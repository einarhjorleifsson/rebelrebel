# bundle.R — a map written as a folder that opens from its index.html, with no R and no server.
# Moved from ramb's R/map.R on 2026-11-10.

#' Write a map as a folder that opens with no R and no server
#'
#' `rr_bundle()` writes an [rr_map()] as a folder whose `index.html` opens in a browser with a double click, on any
#' system, with no R and no server: the page with the map's config, and beside it the layers' tiles and data as script
#' files (`tiles/`, `data/`), which a page opened as a file (`file://`) may load where it may not fetch them. It needs
#' the internet only for the base maps: MapLibre, maplibre-contour and uPlot are in its `vendor` folder.
#'
#' What a bundle carries:
#' * a geojson layer, its data;
#' * a dem or contour layer, its tiles from its `minzoom` to its `maxzoom` inside its `bounds`, written once for the
#'   layers that read one file (the `source` of [rr_tiles_mbtiles()]). Past the deepest zoom written, the map draws
#'   that zoom's tiles larger. A view's fit (a colour's, the contours' or a shading's `fit`) is made in the browser from
#'   the layer's tiles in view, by the rules in `fit`, not by the layer's R function, which needs a server;
#' * a vector layer from [rr_tiles_table()], the tiles that hold a feature, to the deepest zoom written;
#' * a time panel ([rr_time()]), each vessel's spans, the whole and each month, a file each (`time/`);
#' * a grid layer named in `grids`, its rows: summed by cell at a few cell sizes and written in blocks of cells
#'   (`grids/`), from which the page makes each tile, its filters applied and its cells summed in the browser, with the
#'   fields its server would send. Its cells hold one to two screen pixels (the filter `px` times that), from the
#'   cell sizes written. A filter the rows have no column for is left out, with a message.
#'
#' Another vector layer, or a grid layer not in `grids`, is left out, with a message: their tiles are made by a server.
#'
#' @param map An [rr_map()].
#' @param dir The folder to write. If it already holds a bundle (`index.html`, `tiles/`, `data/`, `grids/`, `time/` or
#'   `vendor/`),
#'   `overwrite` replaces those and leaves the rest of the folder as it is.
#' @param maxzoom The deepest zoom of the tiles written (dem, contour and vector layers), at most each layer's own;
#'   `NULL` for each layer's own, 14 for a vector layer with none, as the page has it. Past it the map draws that
#'   zoom's tiles larger. A lower zoom writes fewer files: `dansk`'s 395,558 points as tiles to zoom 14 are 4,292
#'   files and 90 MB, to zoom 12 751 and 53 MB, to zoom 10 160 and 23 MB.
#' @param fit A view's fit in the browser: `probs`, the percentiles (0-1) of the floor and the cap, with `n` quantiles
#'   from one to the other (for colours in equal shares and contour levels); `nodata`, an elevation that means no value
#'   (`NULL` for none); `slope`, a shading's intensity: `k` over the `p` percentile of the slope in degrees, kept
#'   between `min` and `max`. Any left out keeps its default: those of a depth map whose tiles hold 0 on land.
#' @param grids A grid layer's rows for the browser, by the layer's id, each a list of:
#'   * `rows`: a data frame or a lazy table ([dplyr::tbl()] on DuckDB), a row for each cell and combination of the
#'     filters' values, with the cell's south-west corner as whole numbers `x` and `y` of `unit` degrees;
#'   * `unit`: the degrees of one step of `x` and `y`;
#'   * `levels`: the cell sizes written, in steps of `unit`, rising, each a multiple of the rows' own cell;
#'   * `columns`: a named character vector, a filter's id to the column it selects on (a range filter's column holds
#'     the values its two parameters bound);
#'   * `sums`: the columns added up within a cell;
#'   * `fields`: a named list, each of the tile's fields a sum's column, or `c(<column>, "km2")` for that sum per
#'     square kilometre of the cell;
#'   * `x`, `y`: the corner's columns, `"x"` and `"y"` unless given;
#'   * `px`: the filter that scales the cells to that many screen pixels, if any.
#' @param overwrite Whether to replace a bundle already in `dir`.
#' @return `dir`, invisibly, with the number of tiles written (`tiles`) and the bundle's size in bytes (`bytes`) as
#'   attributes.
#' @seealso [rr_serve()], which serves the map from this R session instead.
#' @export
#' @examples
#' places <- sf::st_as_sf(
#'   data.frame(name = c("Skagen", "Hirtshals"), lon = c(10.59, 9.96), lat = c(57.72, 57.59)),
#'   coords = c("lon", "lat"), crs = 4326
#' )
#' layer <- rr_layer(
#'   "places", "geojson", label = "Places", data = places,
#'   style = list(list(id = "places-dot", type = "circle", paint = list("circle-color" = "#d9822b"))),
#'   popup = list(click = "places-dot", title = "name")
#' )
#' m <- rr_map(rr_section("ref", "Reference", list(layer)),
#'             title = "Two harbours", view = list(center = c(10.3, 57.6), zoom = 8))
#' d <- rr_bundle(m, file.path(tempdir(), "two_harbours"), overwrite = TRUE)
#' list.files(d, recursive = TRUE)
#' # utils::browseURL(file.path(d, "index.html"))  # opens it in a browser
rr_bundle <- function(
  map,
  dir,
  maxzoom = NULL,
  fit = list(probs = c(0.02, 0.98), n = 25, nodata = 0, slope = list(k = 2, p = 0.9, min = 0.3, max = 1)),
  grids = list(),
  overwrite = FALSE
) {
  if (!inherits(map, "rr_map")) {
    stop("`map` is an rr_map().", call. = FALSE)
  }
  if (!is.character(dir) || length(dir) != 1 || is.na(dir) || !nzchar(dir)) {
    stop("`dir` is the path of a folder.", call. = FALSE)
  }
  if (
    !is.null(maxzoom) &&
      !(is.numeric(maxzoom) && length(maxzoom) == 1 && !is.na(maxzoom) && maxzoom >= 0 && maxzoom <= 22 &&
        maxzoom == round(maxzoom))
  ) {
    stop("`maxzoom` is a whole number from 0 to 22, or NULL.", call. = FALSE)
  }
  if (!isTRUE(overwrite) && !isFALSE(overwrite)) {
    stop("`overwrite` is TRUE or FALSE.", call. = FALSE)
  }
  fit <- .rr_map_bundle_fit(fit)
  if (!is.list(grids) || (length(grids) && (is.null(names(grids)) || any(!nzchar(names(grids)))))) {
    stop("`grids` is a list of a grid layer's rows, by the layer's id.", call. = FALSE)
  }
  parts <- file.path(dir, c("index.html", "tiles", "data", "grids", "time", "vendor"))
  if (any(file.exists(parts))) {
    if (!overwrite) {
      stop(sprintf("%s holds a bundle: `overwrite = TRUE` replaces it.", dir), call. = FALSE)
    }
    unlink(parts, recursive = TRUE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dir)) {
    stop(sprintf("Could not make the folder %s.", dir), call. = FALSE)
  }

  # the layers a bundle carries, and the config without the others
  layers <- map$layers
  kind <- vapply(layers, function(l) l$config$kind, "")
  unknown <- setdiff(names(grids), names(layers)[kind == "grid"])
  if (length(unknown)) {
    stop(sprintf("`grids` names no grid layer of the map: %s.", paste(unknown, collapse = ", ")), call. = FALSE)
  }
  listed <- vapply(layers, function(l) is.function(attr(l$tiles, "tiles_at")), logical(1))
  out <- names(layers)[(kind == "vector" & !listed) | (kind == "grid" & !names(layers) %in% names(grids))]
  if (length(out)) {
    message(sprintf(
      "Left out of the bundle, as their tiles are made by a server: %s.",
      paste(out, collapse = ", ")
    ))
  }
  keep <- setdiff(names(layers), out)
  config <- map$config
  config$layers <- Filter(function(l) l$id %in% keep, config$layers)
  config$groups <- Filter(
    function(g) length(g$layers) > 0,
    lapply(config$groups, function(g) {
      g$layers <- as.list(intersect(unlist(g$layers), keep))
      g
    })
  )
  if (!is.null(config$choose$fields)) {
    config$choose$fields <- Filter(
      function(f) length(f$layers) > 0,
      lapply(config$choose$fields, function(f) {
        f$layers <- as.list(intersect(unlist(f$layers), keep))
        f
      })
    )
    if (!length(config$choose$fields)) {
      config$choose <- NULL
    }
  }

  # a grid's rows, summed by cell and written in blocks; its filters without a column left out
  grid_specs <- list()
  for (id in names(grids)) {
    i <- which(vapply(config$layers, `[[`, "", "id") == id)
    spec <- .rr_map_bundle_grid_spec(grids[[id]], config$layers[[i]])
    dropped <- setdiff(vapply(config$layers[[i]]$filters, `[[`, "", "id"), spec$filters)
    if (length(dropped)) {
      message(sprintf("Layer %s: left out of the bundle, as its rows have no column for them: %s.", id,
                      paste(dropped, collapse = ", ")))
    }
    config$layers[[i]]$filters <- Filter(function(f) f$id %in% spec$filters, config$layers[[i]]$filters)
    grid_specs[[id]] <- .rr_map_bundle_grid(spec, config$layers[[i]], dir, id)
  }

  # the tiles: once for the layers on one source, from the least of their zooms to the most; a dem or contour layer's
  # inside its bounds, a vector layer's those that hold a feature (its tiles' `tiles_at`)
  ids <- vapply(config$layers, `[[`, "", "id")
  dem <- which(vapply(config$layers, function(l) l$kind %in% c("dem", "contour", "vector"), logical(1)))
  for (i in dem) {
    lc <- config$layers[[i]]
    # a vector layer with no deepest zoom: the page's own for its tiles, 14, where the thinning stops too
    z1 <- c(unlist(lc$maxzoom) %||% if (lc$kind == "vector") 14L, maxzoom)
    if (!length(z1)) {
      stop(sprintf("Layer %s: its tiles' deepest zoom is unknown: give `maxzoom`.", lc$id), call. = FALSE)
    }
    z1 <- min(z1)
    if (lc$kind != "vector" && is.null(lc$bounds)) {
      stop(sprintf("Layer %s: a dem or contour layer in a bundle needs its `bounds`.", lc$id), call. = FALSE)
    }
    config$layers[[i]]$maxzoom <- as.integer(z1)
  }
  source <- vapply(ids[dem], function(id) .rr_or(attr(layers[[id]]$tiles, "source"), paste0("layer:", id)), "")
  folder <- stats::setNames(list(), character())
  n_tiles <- 0L
  for (s in unique(source)) {
    on <- dem[source == s]
    lcs <- config$layers[on]
    b <- if (lcs[[1]]$kind != "vector") vapply(lcs, function(lc) as.numeric(unlist(lc$bounds)), numeric(4))
    n_tiles <- n_tiles + .rr_map_bundle_tiles(
      layers[[ids[on[1]]]]$tiles,
      dir,
      ids[on[1]],
      min(vapply(lcs, function(lc) as.integer(.rr_or(lc$minzoom, 0L)), integer(1))),
      max(vapply(lcs, function(lc) lc$maxzoom, integer(1))),
      if (!is.null(b)) c(min(b[1, ]), min(b[2, ]), max(b[3, ]), max(b[4, ]))
    )
    for (i in on) {
      folder[[ids[i]]] <- ids[on[1]]
    }
  }

  # the time panel's series (rr_time()): each vessel's spans, the whole and each month, a file each, named by the
  # vessel's place in the panel's list
  if (!is.null(map$time)) {
    dir.create(file.path(dir, "time"), showWarnings = FALSE)
    for (i in seq_along(map$time$config$vessels)) {
      v <- map$time$config$vessels[[i]]
      for (sp in v$spans) {
        key <- sprintf("time/%d_%s", i - 1L, sp$value)
        writeLines(
          enc2utf8(sprintf(
            'RB_BUNDLE("%s",%s);',
            key,
            .rr_map_json(map$time$series(v$value, sp$value))
          )),
          file.path(dir, paste0(key, ".js")),
          useBytes = TRUE
        )
      }
    }
  }

  # a geojson layer's data
  for (lc in config$layers) {
    if (lc$kind != "geojson") {
      next
    }
    d <- .rr_map_geojson(layers[[lc$id]])
    if (is.null(d)) {
      next
    }
    dir.create(file.path(dir, "data"), showWarnings = FALSE)
    writeLines(
      enc2utf8(sprintf('RB_BUNDLE("data/%s",%s);', lc$id, paste(d, collapse = "\n"))),
      file.path(dir, "data", paste0(lc$id, ".js")),
      useBytes = TRUE
    )
  }

  # a layer's rows for its clicks, whole, under a guard on their size (a server reads them a row at a time)
  rows_written <- character()
  for (lc in config$layers) {
    reader <- layers[[lc$id]]$row
    if (!is.function(reader)) {
      next
    }
    r <- reader()
    cells <- nrow(r$rows) * length(r$cols)
    if (cells > 2e6) {
      message(sprintf(
        "Layer %s: its %s rows are not written (%s values, more than 2 million): a click shows its title alone.",
        lc$id, format(nrow(r$rows), big.mark = ","), format(cells, big.mark = ",")
      ))
      next
    }
    # an array, row n at n - 1: a browser reads an object literal keyed by number slowly (395,558 rows of four columns
    # took 65-71 s in Chrome, as an array 0.1 s), and the keys were written as R prints them ("1e+05")
    vals <- lapply(r$cols, function(cc) r$rows[[cc$name]])
    rows <- vector("list", max(c(0, r$rows$rr_row)))
    rows[as.integer(r$rows$rr_row)] <- lapply(seq_len(nrow(r$rows)), function(i) {
      lapply(vals, function(v) if (is.na(v[i])) NULL else v[i])
    })
    dir.create(file.path(dir, "data"), showWarnings = FALSE)
    writeLines(
      enc2utf8(sprintf('RB_BUNDLE("data/%s_rows",%s);', lc$id, .rr_map_json(list(cols = r$cols, rows = rows)))),
      file.path(dir, "data", paste0(lc$id, "_rows.js")),
      useBytes = TRUE
    )
    rows_written <- c(rows_written, lc$id)
  }

  bundle <- list(
    meta = list(engine = .rr_map_engine, layers = Filter(Negate(is.null), .rr_map_metas(layers[keep]))),
    tiles = folder,
    fit = fit,
    grids = if (length(grid_specs)) grid_specs else stats::setNames(list(), character())
  )
  # the page's libraries, so the folder draws with no network but the base map's
  dir.create(file.path(dir, "vendor"), showWarnings = FALSE)
  vendor <- list.files(system.file("engine", "vendor", package = "rebelrebel"), full.names = TRUE)
  if (!length(vendor) || !all(file.copy(vendor, file.path(dir, "vendor"), overwrite = TRUE))) {
    stop("Could not copy the page's libraries into the bundle.", call. = FALSE)
  }
  writeLines(
    enc2utf8(.rr_map_page(.rr_map_json(config), .rr_map_json(bundle))),
    file.path(dir, "index.html"),
    useBytes = TRUE
  )
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  invisible(structure(dir, tiles = n_tiles, bytes = sum(file.size(files))))
}

# A bundle's rules of fit: the defaults where `fit` leaves one out, each checked
.rr_map_bundle_fit <- function(fit) {
  d <- list(probs = c(0.02, 0.98), n = 25, nodata = 0, slope = list(k = 2, p = 0.9, min = 0.3, max = 1))
  if (!is.list(fit) || (length(fit) && is.null(names(fit))) || length(setdiff(names(fit), names(d))) ||
        (!is.null(fit$slope) && (!is.list(fit$slope) || length(setdiff(names(fit$slope), names(d$slope)))))) {
    stop("`fit` is a list of any of probs, n, nodata and slope (a list of any of k, p, min and max).", call. = FALSE)
  }
  if ("slope" %in% names(fit)) {
    fit$slope <- utils::modifyList(d$slope, fit$slope)
  }
  v <- utils::modifyList(d, fit)
  num <- function(x, n = 1) is.numeric(x) && length(x) == n && !anyNA(x)
  s <- v$slope
  ok <- c(
    probs = num(v$probs, 2) && v$probs[1] >= 0 && v$probs[1] < v$probs[2] && v$probs[2] <= 1,
    n = num(v$n) && v$n >= 2 && v$n == round(v$n),
    nodata = is.null(v$nodata) || num(v$nodata),
    slope = num(s$k) && s$k > 0 && num(s$p) && s$p > 0 && s$p < 1 && num(s$min) && num(s$max) && s$min >= 0 &&
      s$min <= s$max
  )
  if (!all(ok)) {
    stop(
      sprintf(
        "`fit`: %s out of range (probs two rising numbers from 0 to 1; n a whole number of 2 or more; nodata a number or NULL; slope's k above 0, p between 0 and 1, min and max from 0 with min no more than max).",
        paste(names(ok)[!ok], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  v$probs <- as.list(v$probs)
  v$n <- as.integer(v$n)
  v
}

# A source's tiles from zoom z0 to z1, inside `bounds` (west, south, east, north), or with no bounds those its
# `tiles_at` lists, each a script handing its bytes in base64 to RB_BUNDLE(): tiles/<key>/<z>/<x>/<y>.js. Its count
.rr_map_bundle_tiles <- function(tiles, dir, key, z0, z1, bounds = NULL) {
  col <- function(lon, z) min(2^z - 1, max(0, floor((lon + 180) / 360 * 2^z)))
  row <- function(lat, z) {
    r <- max(-85.0511, min(85.0511, lat)) * pi / 180
    min(2^z - 1, max(0, floor((1 - log(tan(r) + 1 / cos(r)) / pi) / 2 * 2^z)))
  }
  n <- 0L
  failed <- character()
  for (z in seq(z0, z1)) {
    at <- if (is.null(bounds)) {
      attr(tiles, "tiles_at")(z)
    } else {
      expand.grid(y = seq(row(bounds[4], z), row(bounds[2], z)), x = seq(col(bounds[1], z), col(bounds[3], z)))
    }
    for (k in seq_len(nrow(at))) {
      x <- at$x[k]
      y <- at$y[k]
      # a tile its function cannot make is left out, as its server would refuse it
      t <- tryCatch(tiles(z, x, y), error = function(e) {
        failed <<- c(failed, sprintf("%d/%.0f/%.0f (%s)", z, x, y, sub("\n.*", "", trimws(conditionMessage(e)))))
        NULL
      })
      if (is.null(t) || !length(t)) {
        next
      }
      f <- file.path(dir, "tiles", key, z, x, paste0(y, ".js"))
      dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
      writeLines(
        sprintf('RB_BUNDLE("tiles/%s/%d/%d/%d","%s");', key, z, x, y, gsub("\n", "", jsonlite::base64_enc(t), fixed = TRUE)),
        f
      )
      n <- n + 1L
    }
  }
  if (length(failed)) {
    message(sprintf("%s: %d tile(s) its function could not make, left out: %s.", key, length(failed),
                    paste(utils::head(failed, 5), collapse = "; ")))
  }
  n
}

# A grid's rows for a bundle, checked against its layer: the filters it can apply (those with a column, the metric
# and the cell scale) in `filters`
.rr_map_bundle_grid_spec <- function(g, lc) {
  bad <- function(what) stop(sprintf("`grids$%s`: %s", lc$id, what), call. = FALSE)
  allowed <- c("rows", "unit", "levels", "columns", "sums", "fields", "x", "y", "px")
  if (!is.list(g) || is.null(names(g)) || length(setdiff(names(g), allowed))) {
    bad(sprintf("a list of any of %s.", paste(allowed, collapse = ", ")))
  }
  lazy <- inherits(g$rows, "tbl_sql")
  if (!lazy && !is.data.frame(g$rows)) {
    bad("`rows` is a data frame or a lazy table.")
  }
  have <- colnames(g$rows)
  x <- .rr_or(g$x, "x")
  y <- .rr_or(g$y, "y")
  if (!all(c(x, y) %in% have)) {
    bad(sprintf("`rows` has no column %s.", paste(setdiff(c(x, y), have), collapse = " or ")))
  }
  if (!is.numeric(g$unit) || length(g$unit) != 1 || is.na(g$unit) || g$unit <= 0) {
    bad("`unit` is the degrees of one step of x and y, above 0.")
  }
  lv <- g$levels
  if (!is.numeric(lv) || !length(lv) || anyNA(lv) || any(lv < 1) || any(lv != round(lv)) || is.unsorted(lv, strictly = TRUE)) {
    bad("`levels` are whole numbers of steps, rising.")
  }
  filters <- lc$filters
  fid <- vapply(filters, `[[`, "", "id")
  ftype <- vapply(filters, function(f) .rr_or(f$type, "options"), "")
  cl <- g$columns
  if (length(cl) && (!is.character(cl) || is.null(names(cl)) || !all(names(cl) %in% fid[ftype %in% c("options", "range")]))) {
    bad("`columns` names the layer's filters (not its metric), each its column.")
  }
  if (!all(cl %in% have)) {
    bad(sprintf("`rows` has no column %s.", paste(setdiff(cl, have), collapse = ", ")))
  }
  if (!is.character(g$sums) || !length(g$sums) || !all(g$sums %in% have)) {
    bad("`sums` are columns of `rows`, to be added up within a cell.")
  }
  fl <- g$fields
  ok <- is.list(fl) && length(fl) && !is.null(names(fl)) && all(vapply(fl, function(f) {
    is.character(f) && length(f) %in% 1:2 && f[1] %in% g$sums && (length(f) == 1 || identical(f[2], "km2"))
  }, logical(1)))
  if (!ok) {
    bad("`fields` are the tile's fields by name, each a sum's column, or c(<column>, \"km2\") per square kilometre.")
  }
  metrics <- vapply(lc$metrics, `[[`, "", "field")
  if (!all(metrics %in% names(fl))) {
    bad(sprintf("the layer's metric %s is not among `fields`.", paste(setdiff(metrics, names(fl)), collapse = ", ")))
  }
  if (!is.null(g$px) && !(is.character(g$px) && length(g$px) == 1 && g$px %in% fid)) {
    bad("`px` is the id of the layer's filter that scales its cells.")
  }
  list(
    rows = g$rows, lazy = lazy, unit = g$unit, levels = as.integer(lv), columns = cl, sums = g$sums, fields = fl,
    x = x, y = y, px = g$px, range = names(cl) %in% fid[ftype == "range"],
    filters = c(names(cl), fid[ftype == "metric"], g$px)
  )
}

# A grid's rows summed by cell at each level, from the finest level that divides it, written in blocks of 128 x 128
# cells: grids/<id>/<level>/<bx>_<by>.js, each handing RB_BUNDLE() its rows compressed (zlib) and in base64: their
# number (uint32), each sum (float32), the cell within the block (uint16, row by row from the south-west), and each
# filter's column as codes into its values (uint16, then uint8), all little-endian. The page's account of them
.rr_map_bundle_grid <- function(spec, lc, dir, id) {
  B <- 128L
  if (spec$lazy) {
    con <- dbplyr::remote_con(spec$rows)
    src <- as.character(dbplyr::sql_render(spec$rows))
  } else {
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    duckdb::duckdb_register(con, "rb_grid_rows", spec$rows)
    src <- "SELECT * FROM rb_grid_rows"
  }
  qi <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  get <- function(...) DBI::dbGetQuery(con, sprintf(...))
  run <- function(...) invisible(DBI::dbExecute(con, sprintf(...)))
  cols <- unname(spec$columns)
  values <- lapply(cols, function(cn) get("SELECT DISTINCT %s AS v FROM (%s) ORDER BY 1 NULLS LAST", qi(cn), src)$v)
  bytes <- vapply(values, function(v) if (length(v) <= 256) 1L else if (length(v) <= 65536) 2L else NA_integer_, 1L)
  if (anyNA(bytes)) {
    stop(sprintf("`grids$%s`: a filter's column holds more than 65,536 values.", id), call. = FALSE)
  }
  keys <- paste(c(vapply(cols, qi, ""), vapply(spec$sums, qi, "")), collapse = ", ")
  dims <- paste(vapply(cols, qi, ""), collapse = ", ")
  sums <- paste(sprintf("sum(%1$s) AS %1$s", vapply(spec$sums, qi, "")), collapse = ", ")
  stem <- sprintf("rb_grid_%s_%d", gsub("[^A-Za-z0-9]", "_", id), as.integer(stats::runif(1, 1, 1e9)))
  made <- integer()
  on.exit(for (L in made) DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s_%d", stem, L)), add = TRUE, after = FALSE)
  blocks <- list()
  for (L in spec$levels) {
    from <- rev(made[L %% made == 0])[1]
    sel <- if (is.na(from)) {
      sprintf("SELECT CAST(floor(%s / %d) AS BIGINT) AS cx, CAST(floor(%s / %d) AS BIGINT) AS cy%s, %s FROM (%s) GROUP BY ALL",
              qi(spec$x), L, qi(spec$y), L, if (nzchar(dims)) paste0(", ", dims) else "", sums, src)
    } else {
      sprintf("SELECT CAST(floor(cx / %d) AS BIGINT) AS cx, CAST(floor(cy / %d) AS BIGINT) AS cy%s, %s FROM %s_%d GROUP BY ALL",
              L %/% from, L %/% from, if (nzchar(dims)) paste0(", ", dims) else "", sums, stem, from)
    }
    run("CREATE TEMP TABLE %s_%d AS SELECT *, CAST(floor(cx / %d) AS BIGINT) AS blx, CAST(floor(cy / %d) AS BIGINT) AS bly
         FROM (%s) ORDER BY blx, bly, cx, cy", stem, L, B, B, sel)
    made <- c(made, L)
    bl <- get("SELECT DISTINCT blx, bly FROM %s_%d ORDER BY blx, bly", stem, L)
    folder <- file.path(dir, "grids", id, L)
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    for (k in seq_len(nrow(bl))) {
      r <- get("SELECT cx, cy, %s FROM %s_%d WHERE blx = %.0f AND bly = %.0f ORDER BY cx, cy%s",
               keys, stem, L, bl$blx[k], bl$bly[k], if (nzchar(dims)) paste0(", ", dims) else "")
      cell <- as.integer((r$cy - bl$bly[k] * B) * B + (r$cx - bl$blx[k] * B))
      code <- lapply(seq_along(cols), function(j) match(r[[cols[j]]], values[[j]]) - 1L)
      f32 <- function(v) {
        v <- as.double(v)
        v[is.na(v)] <- 0
        writeBin(v, raw(), size = 4, endian = "little")
      }
      u16 <- function(v) writeBin(ifelse(v > 32767L, v - 65536L, v), raw(), size = 2, endian = "little")
      payload <- c(
        writeBin(nrow(r), raw(), size = 4, endian = "little"),
        unlist(lapply(spec$sums, function(s) f32(r[[s]]))),
        u16(cell),
        unlist(lapply(which(bytes == 2L), function(j) u16(code[[j]]))),
        unlist(lapply(which(bytes == 1L), function(j) as.raw(code[[j]])))
      )
      key <- sprintf("grids/%s/%d/%.0f_%.0f", id, L, bl$blx[k], bl$bly[k])
      writeLines(
        sprintf('RB_BUNDLE("%s","%s");', key, gsub("\n", "", jsonlite::base64_enc(memCompress(payload, "gzip")), fixed = TRUE)),
        file.path(dir, paste0(key, ".js"))
      )
    }
    blocks[[as.character(L)]] <- as.list(sprintf("%.0f_%.0f", bl$blx, bl$bly))
  }
  list(
    unit = spec$unit, levels = as.list(spec$levels), block = B, px = spec$px, layer = .rr_or(lc$source_layer, "cells"),
    sums = as.list(spec$sums), fields = lapply(spec$fields, as.list),
    columns = lapply(seq_along(cols), function(j) {
      list(id = names(spec$columns)[j], range = spec$range[j], bytes = bytes[j],
           values = as.list(ifelse(is.na(values[[j]]), NA, as.character(values[[j]]))))
    }),
    blocks = blocks
  )
}

