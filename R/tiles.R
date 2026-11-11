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
    # the features in the tile and in its margin of 64 of its 4,096 units (ST_AsMVTGeom's buffer): a point just past
    # its edge, which the tile beside it rounds onto its own edge and so does not draw, is drawn by this one
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
      "WITH e AS (SELECT ST_TileEnvelope(%d, %d, %d) AS env, (ST_XMax(ST_TileEnvelope(%d, %d, %d)) - ST_XMin(ST_TileEnvelope(%d, %d, %d))) / 64 AS m),
       %s AS (SELECT t.* FROM %s t, e
             WHERE t.xmax >= ST_XMin(e.env) - e.m AND t.xmin <= ST_XMax(e.env) + e.m AND t.ymax >= ST_YMin(e.env) - e.m AND t.ymin <= ST_YMax(e.env) + e.m)%s
       SELECT %s AS a%s",
      as.integer(z),
      as.integer(x),
      as.integer(y),
      as.integer(z), as.integer(x), as.integer(y), as.integer(z), as.integer(x), as.integer(y),
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
      # with a tile's margin (1/64 of it), as each tile is cut
      cl(k(sprintf("(xmin + %.17e) / %.17e * %.0f - 0.015625", w, 2 * w, 2^z))),
      cl(k(sprintf("(xmax + %.17e) / %.17e * %.0f + 0.015625", w, 2 * w, 2^z))),
      cl(k(sprintf("(%.17e - ymax) / %.17e * %.0f - 0.015625", w, 2 * w, 2^z))),
      cl(k(sprintf("(%.17e - ymin) / %.17e * %.0f + 0.015625", w, 2 * w, 2^z))),
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
#' A tile function for a dem or contour [rr_layer()] made from a grid of elevations: a GeoTIFF (a Cloud Optimized
#' GeoTIFF when it is large) or any grid GDAL reads, such as GEBCO's netCDF, in any projection; a terra `SpatRaster`;
#' or a data frame of a grid's cells. Each tile the page asks for is warped from the grid by GDAL (through terra), the
#' grid resampled once: averaged where a pixel is wider than a cell (the map zoomed out), interpolated (bilinear) where
#' it is narrower. The tile is written as a PNG image in the Terrarium encoding, from which the page draws the relief,
#' the colours and the contours, and fits them to the view.
#'
#' A grid is read where it is: only the part under a tile, and from a GeoTIFF's overviews where a pixel spans eight
#' cells or more (the full grid nearer the deepest zoom, as overviews average twice). A large grid that is no Cloud
#' Optimized GeoTIFF is read whole for a tile zoomed far out: `gdal_translate -of COG` makes one, losing nothing.
#'
#' @param x The grid: a path of a raster file, a terra `SpatRaster`, or a data frame (or a lazy table of at most 10
#'   million rows, read into R) of a regular grid in longitude and latitude (WGS 84), a row per cell: its centre and
#'   its elevation. A cell may be missing, a row may not repeat a cell.
#' @param lon,lat,elevation For a data frame, its columns: the centre's longitude and latitude, and the elevation in
#'   metres, negative below sea level (a depth is the elevation's negative: make the column first). For a raster of
#'   several layers, `elevation` names the one drawn; a raster's values are metres, negative below sea level.
#' @param tile_size The tiles' side in pixels, 256 or 512.
#' @param minzoom,maxzoom The zooms tiles are made for; the map draws deeper zooms from the deepest's tiles, enlarged. By
#'   default from 0 to the first zoom at which a pixel is no wider than a cell, but no deeper than 8: a pixel of about
#'   300 m at the equator and 130 m at 65 degrees north (512-pixel tiles), an overview rather than every detail of a
#'   fine grid. Give `maxzoom` to go deeper; each zoom takes about four times the bytes of the one before.
#' @param nodata The elevation written where the grid has none: beyond its edge, and where a cell a pixel is drawn from
#'   is missing. By default 0, sea level, which the colours of a depth leave clear.
#' @return A function `function(z, x, y)` (`y` from the north, as the page asks) returning a tile's bytes, a PNG; a
#'   tile wholly outside the grid is level at `nodata`, for maplibre-contour draws no lines in a tile one of whose eight
#'   neighbours is missing. With the attributes [rr_layer()] takes for a dem or contour layer (`format`, `encoding`,
#'   `tile_size`, `bounds`, `minzoom` and `maxzoom`); `source`, by which [rr_bundle()] writes the tiles of layers on
#'   one grid once; `count`, the grid's number of cells, missing ones included; and `fit`, `"page"`: a layer on these
#'   tiles fits its colours, contours and shading to the view in the page, from the tiles in view, with no call to R
#'   (the rules of [rr_bundle()]'s `fit`, by default the floor and the cap at the 2nd and 98th percentiles of the values
#'   in view, 25 quantiles between them, and the shading's intensity 2 over the 90th percentile of the slope in
#'   degrees, between 0.3 and 1).
#' @seealso [rr_tiles_mbtiles()], for tiles already made; [rr_layer()]
#' @export
#' @examples
#' # a small hill, 21 by 21 cells of 0.01 degrees
#' g <- expand.grid(lon = seq(10, 10.2, by = 0.01), lat = seq(56, 56.2, by = 0.01))
#' g$elevation <- 100 - 5000 * ((g$lon - 10.1)^2 + (g$lat - 56.1)^2)
#' if (requireNamespace("png", quietly = TRUE) && requireNamespace("terra", quietly = TRUE)) {
#'   tiles <- rr_tiles_dem(g)
#'   attributes(tiles)[c("bounds", "minzoom", "maxzoom", "count")]
#'   png_bytes <- tiles(10, 540, 318) # the tile over the hill at zoom 10
#'   rgb <- png::readPNG(png_bytes) * 255
#'   # the Terrarium encoding: (red * 256 + green + blue / 256) - 32768
#'   range(rgb[, , 1] * 256 + rgb[, , 2] + rgb[, , 3] / 256 - 32768)
#'
#'   # the same grid as a GeoTIFF, as most grids come
#'   f <- tempfile(fileext = ".tif")
#'   terra::writeRaster(terra::rast(g, type = "xyz", crs = "EPSG:4326"), f)
#'   attributes(rr_tiles_dem(f))[c("bounds", "maxzoom")]
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
  for (p in c("png", "terra")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(sprintf("rr_tiles_dem() needs the %1$s package: install.packages(\"%1$s\").", p), call. = FALSE)
    }
  }
  is_name <- function(v) {
    is.character(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  }
  if (!is_name(lon) || !is_name(lat) || !is_name(elevation)) {
    stop("`lon`, `lat` and `elevation` are column names.", call. = FALSE)
  }
  if (!(is.numeric(tile_size) && length(tile_size) == 1 && tile_size %in% c(256, 512))) {
    stop("`tile_size` is 256 or 512.", call. = FALSE)
  }
  zoom_ok <- function(v) {
    is.numeric(v) && length(v) == 1 && !is.na(v) && v >= 0 && v <= 22 && v == round(v)
  }
  if (!zoom_ok(minzoom) || (!is.null(maxzoom) && (!zoom_ok(maxzoom) || maxzoom < minzoom))) {
    stop("`minzoom` and `maxzoom` are whole numbers from 0 to 22, `maxzoom` no less than `minzoom`.", call. = FALSE)
  }
  if (!(is.numeric(nodata) && length(nodata) == 1 && is.finite(nodata))) {
    stop("`nodata` is a number.", call. = FALSE)
  }
  src <- .rr_dem_raster(x, lon, lat, elevation)
  # the same file opened with no overviews, for the tiles where a pixel spans fewer than eight cells: GDAL's warp takes
  # a file's overviews wherever they are near the tile's resolution, and they average twice (on a Cloud Optimized
  # GeoTIFF of a 50 m grid, 0.25 m from the grid on average at zoom 8, against 0.07 m from its full resolution). The
  # open option, as terra hands the warp utility's own flags (-ovr) no further
  full <- .rr_dem_full(src)
  S <- as.integer(tile_size)
  lonlat <- terra::is.lonlat(src)
  # the grid's box in degrees, its edges followed (a projected grid's edges curve in longitude and latitude)
  e <- if (lonlat) terra::ext(src) else terra::project(terra::ext(src), from = terra::crs(src), to = "EPSG:4326")
  bounds <- c(
    max(-180, terra::xmin(e)),
    max(-85.0511, terra::ymin(e)),
    min(180, terra::xmax(e)),
    min(85.0511, terra::ymax(e))
  )
  # a cell's side on the ground in metres, east-west, at a latitude
  res <- terra::res(src)
  cell_m <- if (lonlat) {
    function(lat) res[1] * 111320 * cos(lat * pi / 180)
  } else {
    unit <- terra::linearUnits(src)
    function(lat) res[1] * (if (is.finite(unit) && unit > 0) unit else 1)
  }
  # by default no deeper than zoom 8, about 128 m a pixel at 65 degrees north (512-pixel tiles): an overview of the
  # seabed, a tenth of zoom 10's bytes, and a file that a bundle opened from a folder holds
  if (is.null(maxzoom)) {
    own <- if (lonlat) {
      ceiling(log2(360 / (S * res[1])))
    } else {
      mid <- (bounds[2] + bounds[4]) / 2
      ceiling(log2(2 * pi * 6378137 * cos(mid * pi / 180) / (S * cell_m(mid))))
    }
    maxzoom <- max(as.integer(minzoom), min(8L, as.integer(own)))
  }
  half <- pi * 6378137
  flat <- .rr_terrarium_png(rep(nodata, S * S), S)
  tile <- function(z, x, y) {
    n <- 2^z
    lat_at <- function(t) atan(sinh(pi * (1 - 2 * t / n))) * 180 / pi
    w <- x / n * 360 - 180
    east <- (x + 1) / n * 360 - 180
    north <- lat_at(y)
    south <- lat_at(y + 1)
    if (east <= bounds[1] || w >= bounds[3] || north <= bounds[2] || south >= bounds[4]) {
      return(flat)
    }
    s <- 2 * half / n
    tpl <- terra::rast(
      nrows = S, ncols = S, crs = "EPSG:3857",
      xmin = -half + x * s, xmax = -half + (x + 1) * s, ymin = half - (y + 1) * s, ymax = half - y * s
    )
    mid <- (north + south) / 2
    ground <- s / S * cos(mid * pi / 180) # a pixel's side on the ground, metres
    cells <- ground / cell_m(mid)
    r <- terra::project(
      if (cells < 8) full else src, tpl,
      method = if (cells > 1) "average" else "bilinear",
      by_util = TRUE
    )
    E <- terra::values(r, mat = FALSE) # by rows, north to south, each west to east
    E[!is.finite(E)] <- nodata
    .rr_terrarium_png(matrix(E, S, S, byrow = TRUE), S)
  }
  attr(tile, "format") <- "png"
  attr(tile, "encoding") <- "terrarium"
  attr(tile, "tile_size") <- S
  attr(tile, "bounds") <- bounds
  attr(tile, "minzoom") <- as.integer(minzoom)
  attr(tile, "maxzoom") <- as.integer(maxzoom)
  files <- terra::sources(src)
  attr(tile, "source") <- paste0(
    "dem:",
    if (length(files) == 1 && nzchar(files)) normalizePath(files) else paste0("grid_", .rr_names$n <- .rr_names$n + 1L),
    sprintf(":%d:%g:%d:%d", S, nodata, as.integer(minzoom), as.integer(maxzoom))
  )
  attr(tile, "count") <- terra::ncell(src)
  attr(tile, "fit") <- "page"
  tile
}

# A grid as a one-layer SpatRaster with a coordinate reference system: a raster file read where it is, a SpatRaster as
# it is, a data frame's cells (or a small lazy table's, read into R) checked as a regular grid in longitude and
# latitude and put on it, each cell at its place, none resampled
.rr_dem_raster <- function(x, lon, lat, elevation) {
  if (inherits(x, "tbl_lazy")) {
    n <- dplyr::pull(dplyr::count(x), 1)
    if (n > 1e7) {
      stop(
        sprintf(
          "`x` is a table of %s cells, too many to read into R: write the grid as a GeoTIFF (terra::writeRaster()) and give its path.",
          format(n, big.mark = ",", scientific = FALSE)
        ),
        call. = FALSE
      )
    }
    x <- dplyr::collect(x)
  }
  if (is.character(x) && length(x) == 1 && !is.na(x)) {
    r <- tryCatch(terra::rast(x), error = function(err) NULL)
    if (is.null(r)) {
      stop(sprintf("`x` is no raster GDAL reads: %s", x), call. = FALSE)
    }
  } else if (inherits(x, "SpatRaster")) {
    r <- x
  } else if (is.data.frame(x)) {
    r <- .rr_dem_grid(as.data.frame(x), lon, lat, elevation)
  } else {
    stop("`x` is a raster file's path, a terra SpatRaster, or a data frame of a grid's cells.", call. = FALSE)
  }
  if (terra::nlyr(r) > 1) {
    if (!elevation %in% names(r)) {
      stop(
        sprintf("`x` has %d layers (%s): `elevation` names the one drawn.", terra::nlyr(r), paste(names(r), collapse = ", ")),
        call. = FALSE
      )
    }
    r <- r[[elevation]]
  }
  if (!nzchar(terra::crs(r))) {
    stop("`x` has no coordinate reference system: give it one (terra::crs()).", call. = FALSE)
  }
  r
}

# A grid read from one file opened again with GDAL's open option OVERVIEW_LEVEL=NONE, so a warp reads its full
# resolution only; its layer by name where the file has several. A grid in memory, or one whose layer cannot be found
# again, is its own: it has no overviews, or keeps them
.rr_dem_full <- function(src) {
  files <- terra::sources(src)
  if (length(files) != 1 || !nzchar(files)) {
    return(src)
  }
  f <- tryCatch(terra::rast(files, opts = "OVERVIEW_LEVEL=NONE"), error = function(err) NULL)
  if (is.null(f)) {
    return(src)
  }
  if (terra::nlyr(f) == 1) {
    return(f)
  }
  if (names(src) %in% names(f)) f[[names(src)]] else src
}

# A data frame's cells as a raster in longitude and latitude: the columns there, at least two cells each way, one row a
# cell, each on the lattice its least steps make (a cell may be missing); put on it exactly, so terra reads it as a grid
.rr_dem_grid <- function(d, lon, lat, elevation) {
  gone <- setdiff(c(lon, lat, elevation), names(d))
  if (length(gone)) {
    stop(sprintf("`x` has no column %s.", paste0("`", gone, "`", collapse = ", ")), call. = FALSE)
  }
  d <- d[is.finite(d[[lon]]) & is.finite(d[[lat]]) & is.finite(d[[elevation]]), c(lon, lat, elevation)]
  lattice <- function(v) {
    u <- sort(unique(round(v, 7)))
    if (length(u) < 2) {
      return(NULL)
    }
    n <- round((u[length(u)] - u[1]) / min(diff(u))) + 1
    list(v0 = u[1], step = (u[length(u)] - u[1]) / (n - 1))
  }
  gx <- lattice(d[[lon]])
  gy <- lattice(d[[lat]])
  if (is.null(gx) || is.null(gy)) {
    stop("`x` is a grid of at least two cells each way, east-west and north-south.", call. = FALSE)
  }
  fi <- (d[[lon]] - gx$v0) / gx$step
  fj <- (d[[lat]] - gy$v0) / gy$step
  if (max(abs(fi - round(fi))) > 0.01 || max(abs(fj - round(fj))) > 0.01) {
    stop(
      sprintf(
        "`x` is a regular grid: its positions lie %.3g by %.3g degrees apart, and %s off that lattice.",
        gx$step, gy$step, if (max(abs(fi - round(fi))) > 0.01) "a longitude lies" else "a latitude lies"
      ),
      call. = FALSE
    )
  }
  key <- paste(round(fi), round(fj))
  if (anyDuplicated(key)) {
    stop(sprintf("`x` has one row per cell: %d rows repeat a cell.", sum(duplicated(key))), call. = FALSE)
  }
  xyz <- data.frame(x = gx$v0 + round(fi) * gx$step, y = gy$v0 + round(fj) * gy$step, z = d[[elevation]])
  terra::rast(xyz, type = "xyz", crs = "EPSG:4326")
}

# Elevations, a pixel each (a matrix by rows north to south, or a vector of them column by column), as a Terrarium PNG:
# (red * 256 + green + blue / 256) - 32768
.rr_terrarium_png <- function(E, S) {
  v <- pmin(pmax(E + 32768, 0), 65535.996)
  red <- floor(v / 256)
  green <- floor(v) %% 256
  blue <- floor((v - floor(v)) * 256)
  png::writePNG(array(c(red, green, blue) / 255, dim = c(S, S, 3)))
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


# The rows of a table as one GeoJSON FeatureCollection, for a layer sent to the page whole: a function giving the
# text, made once; coordinates to six decimals (a tenth of a metre), the properties as the tiles carry them
# (.rr_mvt_cast), the features in `order`. Its `count` attribute is the number of features
.rr_geojson_table <- function(x, geometry = "geometry", lon = NULL, lat = NULL, properties = NULL, order = NULL) {
  con <- .rr_con_for(x)
  .rr_scope()
  .rr_extension(con, "spatial")
  src <- .rr_sql(.rr_as_lazy(x, con))
  q <- function(s) as.character(DBI::dbQuoteIdentifier(con, s))
  g <- if (!is.null(lon)) {
    sprintf("ST_Point(round(%s, 6), round(%s, 6))", q(lon), q(lat))
  } else {
    sprintf("ST_ReducePrecision(%s, 0.000001)", .rr_geom_sql(con, src, geometry))
  }
  has <- if (!is.null(lon)) sprintf("%s IS NOT NULL AND %s IS NOT NULL", q(lon), q(lat)) else sprintf("%s IS NOT NULL", q(geometry))
  types <- DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM (%s)", src))
  props <- if (length(properties)) {
    sprintf("to_json({%s})", paste(sprintf(
      "%s: %s",
      DBI::dbQuoteString(con, properties),
      .rr_mvt_cast(vapply(properties, q, ""), types$column_type[match(properties, types$column_name)])
    ), collapse = ", "))
  } else {
    "'{}'::JSON"
  }
  sql <- sprintf(
    "SELECT count(*) AS n, '{\"type\":\"FeatureCollection\",\"features\":[' ||
       coalesce(string_agg(json_object('type', 'Feature', 'geometry', ST_AsGeoJSON(%s)::JSON, 'properties', %s)::VARCHAR, ','%s), '') ||
       ']}' AS j
     FROM (%s) WHERE %s",
    g, props, if (is.null(order)) "" else sprintf(" ORDER BY %s", q(order)), src, has
  )
  r <- DBI::dbGetQuery(con, sql)
  text <- r$j
  out <- function() text
  attr(out, "count") <- r$n
  attr(out, "bytes") <- nchar(text, type = "bytes")
  out
}
