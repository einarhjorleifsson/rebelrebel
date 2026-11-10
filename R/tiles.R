# tiles.R — tile makers: vector tiles of a table cut by DuckDB, and elevation images from MBTiles or a grid.
# Moved from ramb's R/map.R on 2026-11-10.

#' A table of geometries as vector tiles
#'
#' Makes a tile function for a `"vector"` [rr_layer()] from a table of points, lines or areas: the table is copied
#' once into a temporary DuckDB table in web mercator, with each feature's bounds, and each tile is cut from it by
#' DuckDB (`ST_AsMVT()`). Every feature carries `kind` (`"point"`, `"line"` or `"area"`) and the `properties`.
#'
#' @param x A lazy DuckDB table or a data frame: a geometry column (DuckDB's GEOMETRY, or WKB), or `lon` and `lat`.
#' @param geometry The geometry column; or `lon`, `lat` for points from two columns.
#' @param lon,lat The position columns, for points.
#' @param properties The columns each feature carries. A vector tile holds text, numbers and logicals only, so a
#'   factor, date or time is carried as text, a small integer as an integer and a decimal as a double.
#' @param layer The tile layer of the features.
#' @param points The tile layer of a point on every feature (`ST_PointOnSurface()`), for the zooms where an area is
#'   too small to see; `NULL` for none.
#' @param thin Points thinned in each tile, where a screen pixel holds many: `NULL` for none, or a list of `px`, the
#'   side in screen pixels of the cells that each keep one point (a tile drawn 512 pixels wide); `by`, columns whose
#'   values each keep their own point in a cell (a colour's classes, a column chosen in the page), carried in the
#'   tiles only if `properties` names them; and `until`, the zoom from which only the points on one spot of a tile's own
#'   grid (4,096 a side, an eighth of a pixel) are dropped, the source's deepest, so that every point that can be told
#'   apart can be clicked there and beyond. The point kept is a cell's first in the table's order, the same every
#'   time. Lines and areas are kept whole.
#' @return A function of `z`, `x`, `y` returning a tile (raw bytes), with the number of features as its `count`
#'   attribute, the temporary table's name as its `table` attribute, and as `tiles_at` a function of a zoom giving the
#'   tiles that hold a feature (`x`, `y`), by which [rr_bundle()] writes only those.
#' @export
#' @examples
#' \donttest{
#' tiles <- rr_tiles_table(rr_example("dansk_harbours"),
#'   properties = "SI_HARB", points = "harbour_points")
#' attr(tiles, "count")
#' length(tiles(7, 68, 39))
#' }
rr_tiles_table <- function(
  x,
  geometry = "geometry",
  lon = NULL,
  lat = NULL,
  properties = NULL,
  layer = "features",
  points = NULL,
  thin = NULL
) {
  if (!is.null(thin)) {
    ok <- is.list(thin) && is.numeric(thin$px) && length(thin$px) == 1 && thin$px > 0 &&
      is.numeric(thin$until) && length(thin$until) == 1 && (is.null(thin$by) || is.character(thin$by))
    if (!ok) {
      stop("`thin` is a list of `px` (pixels), `until` (a zoom) and `by` (columns), or NULL.", call. = FALSE)
    }
  }
  con <- .rr_con_for(x)
  # a data frame registered here is the tile function's for the session (it remakes its table from it)
  .rr_scope()
  .rr_extension(con, "spatial")
  src <- .rr_sql(.rr_as_lazy(x, con))
  q <- function(s) DBI::dbQuoteIdentifier(con, s)
  g <- if (!is.null(lon)) {
    sprintf("ST_Point(%s, %s)", q(lon), q(lat))
  } else {
    .rr_geom_sql(con, src, geometry)
  }
  props <- vapply(properties, function(p) as.character(q(p)), "")
  # the columns a thinning keeps apart, kept in the table beside the properties; and the table's order, for the point
  # each cell keeps
  kept <- vapply(unique(c(properties, thin$by)), function(p) as.character(q(p)), "")
  nm <- .rr_name(con, "rb_tiles_")
  make <- function() {
    DBI::dbExecute(con, create)
  }
  create <- sprintf(
    "CREATE TEMP TABLE %s AS
     WITH p AS (SELECT %s %s AS g%s FROM (%s)),
     m AS (
       SELECT * EXCLUDE (g),
              CASE WHEN ST_GeometryType(g) IN ('POINT', 'MULTIPOINT') THEN 'point'
                   WHEN ST_GeometryType(g) IN ('LINESTRING', 'MULTILINESTRING') THEN 'line' ELSE 'area' END AS kind,
              ST_Transform(g, 'EPSG:4326', 'EPSG:3857', always_xy := true) AS g3857,
              ST_Transform(ST_PointOnSurface(g), 'EPSG:4326', 'EPSG:3857', always_xy := true) AS p3857
       FROM p WHERE g IS NOT NULL)
     SELECT *, ST_XMin(g3857) AS xmin, ST_XMax(g3857) AS xmax, ST_YMin(g3857) AS ymin, ST_YMax(g3857) AS ymax FROM m",
    nm,
    if (length(kept)) paste0(paste(kept, collapse = ", "), ",") else "",
    g,
    if (is.null(thin)) "" else ", row_number() OVER () AS rr_n",
    src
  )
  make()
  n <- DBI::dbGetQuery(
    con,
    sprintf("SELECT count(*)::INTEGER AS n FROM %s", nm)
  )$n
  types <- DBI::dbGetQuery(con, sprintf("DESCRIBE %s", nm))
  fields <- paste(
    c(sprintf(
      "%s: %s",
      DBI::dbQuoteString(con, c(properties, "kind")),
      c(
        .rr_mvt_cast(
          props,
          types$column_type[match(properties, types$column_name)]
        ),
        "kind"
      )
    )),
    collapse = ", "
  )
  # the layers are written apart and joined: a tile's layers are protobuf messages, so their bytes concatenate
  tile <- function(z, x, y) {
    # thinned, the features in the table's order, so a tile's bytes and the order its points are drawn in are the same
    # every time (a parallel aggregate writes them in any order)
    one <- function(geom, name, where) {
      sprintf(
        "(SELECT ST_AsMVT({'geometry': ST_AsMVTGeom(%s, ST_Extent(env), 4096, 64, true), %s}, %s, 4096%s) FROM f, e %s)",
        geom,
        fields,
        DBI::dbQuoteString(con, name),
        if (is.null(thin)) "" else " ORDER BY rr_n",
        where
      )
    }
    # a tile's points one a cell of `px` screen pixels and of each value of `by`, the cell's first; from `until` on,
    # one a unit of the tile's own grid, the points it would draw on one spot
    cells <- if (!is.null(thin)) {
      cell <- sprintf("((ST_XMax(e.env) - ST_XMin(e.env)) / %.6f)", if (z < thin$until) 512 / thin$px else 4096)
      sprintf(
        ",
       f AS (SELECT f0.* FROM f0, e WHERE kind <> 'point'
             UNION ALL
             SELECT f0.* FROM f0, e WHERE kind = 'point'
             QUALIFY row_number() OVER (PARTITION BY floor((xmin - ST_XMin(e.env)) / %1$s), floor((ymin - ST_YMin(e.env)) / %1$s)%2$s
                                        ORDER BY rr_n) = 1)",
        cell,
        if (length(thin$by)) paste0(", ", vapply(thin$by, function(p) as.character(q(p)), ""), collapse = "") else ""
      )
    }
    sql <- sprintf(
      "WITH e AS (SELECT ST_TileEnvelope(%d, %d, %d) AS env),
       %s AS (SELECT t.* FROM %s t, e
             WHERE t.xmax >= ST_XMin(e.env) AND t.xmin <= ST_XMax(e.env) AND t.ymax >= ST_YMin(e.env) AND t.ymin <= ST_YMax(e.env))%s
       SELECT %s AS a%s",
      as.integer(z),
      as.integer(x),
      as.integer(y),
      if (is.null(cells)) "f" else "f0",
      nm,
      .rr_or(cells, ""),
      one("g3857", layer, if (is.null(points)) "" else "WHERE kind <> 'point'"),
      if (is.null(points)) {
        ""
      } else {
        paste0(", ", one("p3857", points, ""), " AS b")
      }
    )
    # a table dropped when its map was replaced on its port (rr_serve()) is made again
    r <- tryCatch(DBI::dbGetQuery(con, sql), error = function(e) {
      if (DBI::dbExistsTable(con, nm)) {
        stop(e)
      }
      make()
      DBI::dbGetQuery(con, sql)
    })
    out <- c(r$a[[1]], if (!is.null(points)) r$b[[1]])
    if (is.null(out)) raw(0) else out
  }
  # the tiles at zoom z that hold a feature, x and y (y from the north), for a bundle (rr_bundle())
  attr(tile, "tiles_at") <- function(z) {
    if (!DBI::dbExistsTable(con, nm)) {
      make()
    }
    w <- 20037508.342789244
    k <- function(v) sprintf("CAST(floor(%s) AS BIGINT)", v)
    cl <- function(v) sprintf("greatest(0, least(%.0f, %s))", 2^z - 1, v)
    DBI::dbGetQuery(con, sprintf(
      "WITH r AS (SELECT %s AS x0, %s AS x1, %s AS y0, %s AS y1 FROM %s),
            a AS (SELECT unnest(range(x0, x1 + 1)) AS x, y0, y1 FROM r)
       SELECT DISTINCT x, y FROM (SELECT x, unnest(range(y0, y1 + 1)) AS y FROM a) ORDER BY x, y",
      cl(k(sprintf("(xmin + %.17e) / %.17e * %.0f", w, 2 * w, 2^z))),
      cl(k(sprintf("(xmax + %.17e) / %.17e * %.0f", w, 2 * w, 2^z))),
      cl(k(sprintf("(%.17e - ymax) / %.17e * %.0f", w, 2 * w, 2^z))),
      cl(k(sprintf("(%.17e - ymin) / %.17e * %.0f", w, 2 * w, 2^z))),
      nm
    ))
  }
  attr(tile, "count") <- n
  attr(tile, "table") <- nm
  .rr_hold(tile)
}

#' Tiles from an MBTiles file
#'
#' A tile function for an [rr_layer()] that reads its tiles from an MBTiles file (SQLite), such as the image tiles
#' of elevations a dem layer draws. The file is opened once, read-only, and stays open while the function lives.
#'
#' @param file An MBTiles file: a `tiles` table (`zoom_level`, `tile_column`, `tile_row` in the TMS order, from the
#'   south, and `tile_data`) and a `metadata` table of `name` and `value`.
#' @return A function `function(z, x, y)` (`y` from the north, as the page asks) returning a tile's bytes, or `NULL`
#'   where the file has none; with the file's `format`, `bounds`, `minzoom`, `maxzoom`, `encoding` and `tile_size` as
#'   attributes where its metadata give them, which [rr_layer()] takes for a dem layer, and `source`, the file's
#'   path, by which [rr_bundle()] writes the tiles of layers on one file once.
#' @seealso [rr_layer()]
#' @export
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".mbtiles")
#'   con <- DBI::dbConnect(RSQLite::SQLite(), f)
#'   DBI::dbWriteTable(con, "metadata", data.frame(
#'     name = c("format", "encoding", "bounds", "minzoom", "maxzoom"),
#'     value = c("png", "terrarium", "-30,60,-5,70", "0", "1")
#'   ))
#'   DBI::dbExecute(con, "CREATE TABLE tiles (zoom_level INTEGER, tile_column INTEGER, tile_row INTEGER,
#'                        tile_data BLOB)")
#'   DBI::dbExecute(con, "INSERT INTO tiles VALUES (1, 0, 1, ?)", params = list(list(as.raw(1:4))))
#'   DBI::dbDisconnect(con)
#'   tiles <- rr_tiles_mbtiles(f)
#'   tiles(1, 0, 0) # the north-west tile at zoom 1, row 1 counted from the south
#'   attributes(tiles)[c("format", "encoding", "bounds")]
#' }
rr_tiles_mbtiles <- function(file) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("rr_tiles_mbtiles() needs the RSQLite package.", call. = FALSE)
  }
  if (!is.character(file) || length(file) != 1 || !file.exists(file)) {
    stop("`file` is the path of an MBTiles file.", call. = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), file, flags = RSQLite::SQLITE_RO)
  md <- DBI::dbGetQuery(con, "SELECT name, value FROM metadata")
  m <- stats::setNames(as.list(md$value), md$name)
  int <- function(v) if (is.null(v)) NULL else as.integer(v)
  tile <- function(z, x, y) {
    r <- DBI::dbGetQuery(
      con,
      "SELECT tile_data FROM tiles WHERE zoom_level = ? AND tile_column = ? AND tile_row = ?",
      params = list(as.integer(z), as.integer(x), as.integer(2^z - 1 - y))
    )
    if (!nrow(r)) NULL else as.raw(r$tile_data[[1]])
  }
  structure(
    tile,
    format = if (is.null(m$format)) "png" else m$format,
    encoding = m$encoding,
    tile_size = int(m$tile_size),
    bounds = if (!is.null(m$bounds)) as.numeric(strsplit(m$bounds, ",", fixed = TRUE)[[1]]),
    minzoom = int(m$minzoom),
    maxzoom = int(m$maxzoom),
    source = normalizePath(file)
  )
}

#' Tiles of elevations from a grid
#'
#' A tile function for a dem or contour [rr_layer()] made from a regular grid of elevations, such as a clip of
#' GEBCO or of NOAA's ETOPO (which `marmap::getNOAA.bathy()` reads), with no MBTiles file and no tool outside R. The
#' grid is copied once into a temporary DuckDB table. Each tile the page asks for is made from the cells under it: their
#' elevations interpolated (bilinear) at the tile's pixels and written as a PNG image in the Terrarium encoding, from
#' which the page draws the relief, the colours and the contours. Where a pixel spans several cells (the map zoomed
#' out), the cells are first averaged in square blocks, two, four, eight ... cells a side.
#'
#' @param x A data frame or lazy DuckDB table of a regular grid in longitude and latitude (WGS 84), a row per cell: its
#'   centre and its elevation. A cell may be missing, a row may not repeat a cell.
#' @param lon,lat,elevation Its columns: the centre's longitude and latitude, and the elevation in metres, negative below
#'   sea level (a depth is the elevation's negative: make the column first).
#' @param tile_size The tiles' side in pixels, 256 or 512.
#' @param minzoom,maxzoom The zooms tiles are made for; the map draws deeper zooms from the deepest's tiles, enlarged. By
#'   default from 0 to the first zoom at which a pixel is no wider than a cell.
#' @param nodata The elevation written where the grid has none: beyond its edge, and where the cell nearest a pixel is
#'   missing. By default 0, sea level, which the colours of a depth leave clear.
#' @return A function `function(z, x, y)` (`y` from the north, as the page asks) returning a tile's bytes, a PNG; a
#'   tile wholly outside the grid is level at `nodata`, for maplibre-contour draws no lines in a tile one of whose eight
#'   neighbours is missing. With the attributes [rr_layer()] takes for a dem or contour layer
#'   (`format`, `encoding`, `tile_size`, `bounds`, `minzoom` and `maxzoom`), `source`, by which [rr_bundle()] writes
#'   the tiles of layers on one grid once, `table`, the temporary table's name, `count`, the number of cells, and
#'   `fit`, a function of a view's `west`, `south`, `east` and `north` (and `of`, `"depth"` or `"elevation"`), which a
#'   layer on these tiles takes when it is given no fit of its own: the floor and the cap at the 2nd and 98th
#'   percentiles of the values in view, 25 quantiles between them, and the shading's intensity, 2 over the 90th
#'   percentile of the slope in degrees, between 0.3 and 1 (the rules [rr_bundle()] applies in the browser). So
#'   the colours, the contours and the shading follow the view with no more code.
#' @seealso [rr_tiles_mbtiles()], for tiles already made; [rr_layer()]
#' @export
#' @examples
#' # a small hill, 21 by 21 cells of 0.01 degrees
#' g <- expand.grid(lon = seq(10, 10.2, by = 0.01), lat = seq(56, 56.2, by = 0.01))
#' g$elevation <- 100 - 5000 * ((g$lon - 10.1)^2 + (g$lat - 56.1)^2)
#' if (requireNamespace("png", quietly = TRUE)) {
#'   tiles <- rr_tiles_dem(g)
#'   attributes(tiles)[c("bounds", "minzoom", "maxzoom", "count")]
#'   png_bytes <- tiles(10, 540, 318) # the tile over the hill at zoom 10
#'   rgb <- png::readPNG(png_bytes) * 255
#'   # the Terrarium encoding: (red * 256 + green + blue / 256) - 32768
#'   range(rgb[, , 1] * 256 + rgb[, , 2] + rgb[, , 3] / 256 - 32768)
#' }
rr_tiles_dem <- function(
  x,
  lon = "lon",
  lat = "lat",
  elevation = "elevation",
  tile_size = 512L,
  minzoom = 0L,
  maxzoom = NULL,
  nodata = 0
) {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("rr_tiles_dem() needs the png package.", call. = FALSE)
  }
  if (!inherits(x, c("data.frame", "tbl_lazy"))) {
    stop("`x` is a data frame or a lazy DuckDB table.", call. = FALSE)
  }
  is_name <- function(v) {
    is.character(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  }
  if (!is_name(lon) || !is_name(lat) || !is_name(elevation)) {
    stop("`lon`, `lat` and `elevation` are column names.", call. = FALSE)
  }
  if (
    !(is.numeric(tile_size) &&
      length(tile_size) == 1 &&
      tile_size %in% c(256, 512))
  ) {
    stop("`tile_size` is 256 or 512.", call. = FALSE)
  }
  zoom_ok <- function(v) {
    is.numeric(v) &&
      length(v) == 1 &&
      !is.na(v) &&
      v >= 0 &&
      v <= 22 &&
      v == round(v)
  }
  if (
    !zoom_ok(minzoom) ||
      (!is.null(maxzoom) && (!zoom_ok(maxzoom) || maxzoom < minzoom))
  ) {
    stop(
      "`minzoom` and `maxzoom` are whole numbers from 0 to 22, `maxzoom` no less than `minzoom`.",
      call. = FALSE
    )
  }
  if (!(is.numeric(nodata) && length(nodata) == 1 && is.finite(nodata))) {
    stop("`nodata` is a number.", call. = FALSE)
  }
  con <- .rr_con_for(x)
  # a data frame registered here is the tile function's for the session (it remakes its table from it)
  .rr_scope()
  lz <- .rr_as_lazy(x, con)
  gone <- setdiff(c(lon, lat, elevation), colnames(lz))
  if (length(gone)) {
    stop(
      sprintf("`x` has no column %s.", paste0("`", gone, "`", collapse = ", ")),
      call. = FALSE
    )
  }
  src <- .rr_sql(lz)
  q <- function(s) as.character(DBI::dbQuoteIdentifier(con, s))
  where <- sprintf(
    "%1$s IS NOT NULL AND %2$s IS NOT NULL AND %3$s IS NOT NULL AND isfinite(CAST(%3$s AS DOUBLE))",
    q(lon),
    q(lat),
    q(elevation)
  )
  # the grid's step each way: the least gap between the distinct positions (a column or row of cells may be missing)
  step <- function(v) {
    DBI::dbGetQuery(
      con,
      sprintf(
        "WITH d AS (SELECT DISTINCT round(%1$s, 7) AS v FROM (%2$s) WHERE %3$s)
       SELECT min(v) AS v0, max(v) AS v1, count(*) AS n, min(g) AS step
       FROM (SELECT v, v - lag(v) OVER (ORDER BY v) AS g FROM d)",
        q(v),
        src,
        where
      )
    )
  }
  gx <- step(lon)
  gy <- step(lat)
  if (gx$n < 2 || gy$n < 2) {
    stop(
      "`x` is a grid of at least two cells each way, east-west and north-south.",
      call. = FALSE
    )
  }
  lon0 <- gx$v0
  lat0 <- gy$v0
  # the step from the whole span, so positions stored rounded do not drift off it across the grid
  nx <- as.integer(round((gx$v1 - lon0) / gx$step)) + 1L
  ny <- as.integer(round((gy$v1 - lat0) / gy$step)) + 1L
  dx <- (gx$v1 - lon0) / (nx - 1L)
  dy <- (gy$v1 - lat0) / (ny - 1L)
  ix <- sprintf("(%s - %.17g) / %.17g", q(lon), lon0, dx)
  iy <- sprintf("(%s - %.17g) / %.17g", q(lat), lat0, dy)
  chk <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT max(abs(%1$s - round(%1$s))) AS off_x, max(abs(%2$s - round(%2$s))) AS off_y, count(*) AS n,
            count(DISTINCT (CAST(round(%1$s) AS BIGINT), CAST(round(%2$s) AS BIGINT))) AS cells
     FROM (%3$s) WHERE %4$s",
      ix,
      iy,
      src,
      where
    )
  )
  if (chk$off_x > 0.01 || chk$off_y > 0.01) {
    stop(
      sprintf(
        "`x` is a regular grid: its positions lie %.3g by %.3g degrees apart, and %s off that lattice.",
        dx,
        dy,
        if (chk$off_x > 0.01) "a longitude lies" else "a latitude lies"
      ),
      call. = FALSE
    )
  }
  if (chk$cells < chk$n) {
    stop(
      sprintf(
        "`x` has one row per cell: %d rows repeat a cell.",
        chk$n - chk$cells
      ),
      call. = FALSE
    )
  }
  nm <- .rr_name(con, "rb_dem_")
  create <- sprintf(
    "CREATE TEMP TABLE %s AS
     SELECT CAST(round(%s) AS INTEGER) AS ix, CAST(round(%s) AS INTEGER) AS iy, CAST(%s AS DOUBLE) AS e
     FROM (%s) WHERE %s ORDER BY iy, ix",
    nm,
    ix,
    iy,
    q(elevation),
    src,
    where
  )
  make <- function() DBI::dbExecute(con, create)
  make()
  S <- as.integer(tile_size)
  bounds <- c(
    max(-180, lon0 - dx / 2),
    max(-85.0511, lat0 - dy / 2),
    min(180, lon0 + (nx - 0.5) * dx),
    min(85.0511, lat0 + (ny - 0.5) * dy)
  )
  if (is.null(maxzoom)) {
    maxzoom <- min(
      22L,
      max(as.integer(minzoom), as.integer(ceiling(log2(360 / (S * dx)))))
    )
  }
  # elevations, a pixel each, as a Terrarium PNG: (red * 256 + green + blue / 256) - 32768
  encode <- function(E) {
    v <- pmin(pmax(E + 32768, 0), 65535.996)
    red <- floor(v / 256)
    green <- floor(v) %% 256
    blue <- floor((v - floor(v)) * 256)
    png::writePNG(array(c(red, green, blue) / 255, dim = c(S, S, 3)))
  }
  # beyond the grid a level tile, not none: maplibre-contour reads a contour tile's eight neighbours and makes no lines
  # in it where one is refused, so a small grid's edge tiles, every tile zoomed out, had none
  flat <- encode(rep(nodata, S * S))
  tile <- function(z, x, y) {
    n <- 2^z
    lat_at <- function(t) atan(sinh(pi * (1 - 2 * t / n))) * 180 / pi
    w <- x / n * 360 - 180
    e <- (x + 1) / n * 360 - 180
    north <- lat_at(y)
    south <- lat_at(y + 1)
    if (
      e <= bounds[1] ||
        w >= bounds[3] ||
        north <= bounds[2] ||
        south >= bounds[4]
    ) {
      return(flat)
    }
    p <- (seq_len(S) - 0.5) / S
    fi <- ((x + p) / n * 360 - 180 - lon0) / dx # the pixels' columns, west to east, in cells
    fj <- (lat_at(y + p) - lat0) / dy # the pixels' rows, north to south, in cells
    # cells to a pixel: averaged in blocks of k by k, k a power of two, so the blocks of neighbouring tiles agree
    per_px <- min((e - w) / S / dx, (north - south) / S / dy)
    k <- if (per_px >= 2) as.integer(2^floor(log2(per_px))) else 1L
    bi <- (fi - (k - 1) / 2) / k # in blocks: block b holds cells b k to b k + k - 1, its centre at b k + (k - 1) / 2
    bj <- (fj - (k - 1) / 2) / k
    last_i <- (nx - 1L) %/% k
    last_j <- (ny - 1L) %/% k
    i0 <- max(0, min(last_i, floor(min(bi))))
    i1 <- max(0, min(last_i, floor(max(bi)) + 1))
    j0 <- max(0, min(last_j, floor(min(bj))))
    j1 <- max(0, min(last_j, floor(max(bj)) + 1))
    sql <- sprintf(
      "SELECT ix // %1$d AS bi, iy // %1$d AS bj, avg(e) AS e FROM %2$s
       WHERE ix >= %3$.0f AND ix < %4$.0f AND iy >= %5$.0f AND iy < %6$.0f GROUP BY ALL",
      k,
      nm,
      i0 * k,
      (i1 + 1) * k,
      j0 * k,
      (j1 + 1) * k
    )
    # a table dropped since (its connection's temporary tables) is made again
    r <- tryCatch(DBI::dbGetQuery(con, sql), error = function(err) {
      if (DBI::dbExistsTable(con, nm)) {
        stop(err)
      }
      make()
      DBI::dbGetQuery(con, sql)
    })
    M <- matrix(NA_real_, i1 - i0 + 1, j1 - j0 + 1)
    M[cbind(r$bi - i0 + 1, r$bj - j0 + 1)] <- r$e
    # the pixels as a matrix, rows north to south and columns west to east; the edge's half cell takes the edge's value
    U <- pmin(pmax(matrix(bi - i0, S, S, byrow = TRUE), 0), nrow(M) - 1)
    V <- pmin(pmax(matrix(bj - j0, S, S), 0), ncol(M) - 1)
    inside <- matrix(fi >= -0.5 & fi <= nx - 0.5, S, S, byrow = TRUE) &
      matrix(fj >= -0.5 & fj <= ny - 0.5, S, S)
    at <- function(a, b) M[cbind(as.vector(a) + 1, as.vector(b) + 1)]
    u0 <- floor(U)
    v0 <- floor(V)
    fu <- as.vector(U - u0)
    fv <- as.vector(V - v0)
    u1 <- pmin(u0 + 1, nrow(M) - 1)
    v1 <- pmin(v0 + 1, ncol(M) - 1)
    num <- 0
    den <- 0
    for (cn in list(
      list(u0, v0, (1 - fu) * (1 - fv)),
      list(u1, v0, fu * (1 - fv)),
      list(u0, v1, (1 - fu) * fv),
      list(u1, v1, fu * fv)
    )) {
      ev <- at(cn[[1]], cn[[2]])
      ok <- !is.na(ev)
      num <- num + ifelse(ok, cn[[3]] * ev, 0)
      den <- den + ifelse(ok, cn[[3]], 0)
    }
    E <- num / den
    # where the cell nearest the pixel is missing, there is no value: a hole stays a hole
    E[
      is.na(at(round(U), round(V))) | !as.vector(inside) | !is.finite(E)
    ] <- nodata
    encode(E)
  }
  attr(tile, "format") <- "png"
  attr(tile, "encoding") <- "terrarium"
  attr(tile, "tile_size") <- S
  attr(tile, "bounds") <- bounds
  attr(tile, "minzoom") <- as.integer(minzoom)
  attr(tile, "maxzoom") <- as.integer(maxzoom)
  attr(tile, "source") <- paste0("dem:", nm)
  attr(tile, "table") <- nm
  attr(tile, "count") <- DBI::dbGetQuery(
    con,
    sprintf("SELECT count(*)::INTEGER AS n FROM %s", nm)
  )$n
  # A view's fit, by the rules rr_bundle() applies in the browser (its `fit`), which a bathymetry map in use
  # settled on: the floor and the cap at the 2nd and 98th percentiles of the values in the box, to two figures, with 25
  # quantiles from one to the other (colours in equal shares, contour levels); the values a depth's (the elevation's
  # negative, the sea only) or an elevation's (above sea level); and the shading's intensity, 2 over the 90th
  # percentile of the slope there in degrees, kept between 0.3 and 1. From at most about a million cells, every k-th
  attr(tile, "fit") <- function(west, south, east, north, of = c("depth", "elevation")) {
    of <- match.arg(of)
    i0 <- max(0, floor((west - lon0) / dx))
    i1 <- min(nx - 1, ceiling((east - lon0) / dx))
    j0 <- max(0, floor((south - lat0) / dy))
    j1 <- min(ny - 1, ceiling((north - lat0) / dy))
    if (i0 > i1 || j0 > j1) {
      return(NULL)
    }
    k <- max(1L, as.integer(ceiling(sqrt((i1 - i0 + 1) * (j1 - j0 + 1) / 1e6))))
    v <- if (of == "depth") "-e" else "e"
    where <- sprintf(
      "ix BETWEEN %.0f AND %.0f AND iy BETWEEN %.0f AND %.0f AND ix %% %d = 0 AND iy %% %d = 0 AND %s > 0",
      i0, i1, j0, j1, k, k, v
    )
    run <- function(sql) {
      tryCatch(DBI::dbGetQuery(con, sql), error = function(err) {
        if (DBI::dbExistsTable(con, nm)) {
          stop(err)
        }
        make()
        DBI::dbGetQuery(con, sql)
      })
    }
    probs <- seq(0.02, 0.98, length.out = 25)
    q <- run(sprintf(
      "SELECT quantile_cont(%s, [%s]) AS q FROM %s WHERE %s",
      v, paste(probs, collapse = ", "), nm, where
    ))$q[[1]]
    out <- list()
    if (length(q) && !anyNA(q)) {
      lo <- max(1, signif(q[1], 2))
      out <- list(floor = lo, cap = min(10000, max(lo * 1.5, signif(q[length(q)], 2))), quantiles = round(q, 1))
    }
    mid <- (lat0 + (j0 + j1) / 2 * dy) * pi / 180
    p90 <- run(sprintf(
      "WITH c AS (SELECT ix, iy, e FROM %1$s WHERE %2$s)
       SELECT quantile_cont(degrees(atan(sqrt(pow((r.e - c.e) / %3$.6f, 2) + pow((u.e - c.e) / %4$.6f, 2)))), 0.9) AS p
       FROM c JOIN c r ON r.ix = c.ix + %5$d AND r.iy = c.iy JOIN c u ON u.ix = c.ix AND u.iy = c.iy + %5$d",
      nm, where, k * dx * 111320 * cos(mid), k * dy * 110574, k
    ))$p
    if (length(p90) && !is.na(p90) && p90 > 0) {
      out$exaggeration <- min(1, max(0.3, 2 / p90))
    }
    if (length(out)) out else NULL
  }
  .rr_hold(tile)
}

# ST_AsMVT() takes VARCHAR, FLOAT, DOUBLE, INTEGER, BIGINT and BOOLEAN properties and refuses every other type: an R
# factor reaches DuckDB as an ENUM, a date as a DATE
.rr_mvt_cast <- function(expr, type) {
  to <- ifelse(
    type %in% c("VARCHAR", "FLOAT", "DOUBLE", "INTEGER", "BIGINT", "BOOLEAN"),
    NA,
    ifelse(
      type %in% c("TINYINT", "SMALLINT", "UTINYINT", "USMALLINT"),
      "INTEGER",
      ifelse(
        type == "UINTEGER",
        "BIGINT",
        ifelse(startsWith(type, "DECIMAL"), "DOUBLE", "VARCHAR")
      )
    )
  )
  ifelse(is.na(to), expr, sprintf("CAST(%s AS %s)", expr, to))
}

