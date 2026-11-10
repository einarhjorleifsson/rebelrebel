# layers.R — the layer functions: a mark each, its data a parquet file, a lazy DuckDB
# table, a data frame or sf, piped onto a map. Their arguments come in one order (§3.4): the map and the data; where
# (lon, lat and the mark's own); how it looks (colour, fill, size, linewidth, opacity, label); its colours (palette,
# limits, stretch, legend); what the reader can do (popup, hover, choose); the sidebar (name, info, section, visible);
# its zooms (minzoom, maxzoom). A bare name is a column; a string is a value.

# A short id from a name: lower-case letters, digits and _
.rr_slug <- function(x) {
  s <- gsub("_+", "_", gsub("[^a-z0-9]+", "_", tolower(iconv(x, to = "ASCII//TRANSLIT", sub = ""))))
  s <- gsub("^_|_$", "", s)
  if (!nzchar(s) || grepl("^[0-9]", s)) paste0("l_", s) else s
}

# A parquet file, folder or glob as a lazy DuckDB table; anything else as given
.rr_data <- function(x) {
  if (is.character(x) && length(x) == 1 && !grepl("\n", x)) {
    path <- if (dir.exists(x)) file.path(x, "**", "*.parquet") else x
    if (!grepl("\\.parquet$", path) && !grepl("[*?]", path)) {
      stop(sprintf("\"%s\" is not a parquet file, folder or glob.", x), call. = FALSE)
    }
    con <- .rr_connection()
    return(dplyr::tbl(con, dplyr::sql(sprintf("SELECT * FROM read_parquet(%s)", DBI::dbQuoteString(con, path)))))
  }
  x
}

# What an aesthetic was given: NULL, a value (a string or a number), or columns (bare names, one or c() of them).
# A string that names a column and is not a colour stops: columns are written bare.
.rr_aes <- function(q, cols, what, colour = FALSE) {
  if (is.null(q) || rlang::quo_is_null(q) || rlang::quo_is_missing(q)) {
    return(NULL)
  }
  e <- rlang::quo_get_expr(q)
  if (is.character(e) || is.numeric(e) || is.logical(e)) {
    if (is.character(e) && length(e) == 1 && e %in% cols && !.rr_is_colour(e)) {
      stop(sprintf("`%1$s = \"%2$s\"` is a value; to use the column, write it bare: `%1$s = %2$s`.", what, e), call. = FALSE)
    }
    if (colour && !.rr_is_colour(e)) {
      stop(sprintf("`%s = \"%s\"` is not a colour, nor a column of the data.", what, e), call. = FALSE)
    }
    return(list(value = e))
  }
  list(columns = .rr_select(q, cols, what))
}

# Columns named bare, one or c() of them, or with tidyselect's helpers
.rr_select <- function(q, cols, what) {
  if (is.null(q) || rlang::quo_is_null(q) || rlang::quo_is_missing(q)) {
    return(character())
  }
  e <- rlang::quo_get_expr(q)
  if (isTRUE(e)) {
    return(cols)
  }
  if (isFALSE(e)) {
    return(character())
  }
  pos <- tryCatch(
    tidyselect::eval_select(q, stats::setNames(as.list(seq_along(cols)), cols), allow_rename = FALSE),
    error = function(err) {
      stop(sprintf("`%s`: %s The data have %s.", what, conditionMessage(err), paste(cols, collapse = ", ")),
           call. = FALSE)
    }
  )
  names(pos)
}

# The id and the name of a new layer: the name the call gives, else the mark's, numbered where taken
.rr_layer_names <- function(state, name, kind) {
  label <- .rr_quick_label(state, .rr_or(name, kind))
  taken <- vapply(state$layers, function(l) l$config$id, "")
  id <- .rr_slug(label)
  while (id %in% taken) id <- paste0(id, "_2")
  list(id = id, label = label)
}

# The marks' MapLibre layer types, the paint property each is coloured through, and its legend swatch
.rr_marks <- list(
  point = list(type = "circle", paint = "circle-color", suffix = "dot", kind = "Points", count = "points"),
  line = list(type = "line", paint = "line-color", suffix = "line", kind = "Lines", count = "lines"),
  segment = list(type = "line", paint = "line-color", suffix = "line", kind = "Segments", count = "segments"),
  polygon = list(type = "fill", paint = "fill-color", suffix = "fill", kind = "Areas", count = "areas"),
  text = list(type = "symbol", paint = "text-color", suffix = "text", kind = "Text", count = "labels")
)

# A legend's swatches as the mark draws: dots for points, strokes for lines, filled boxes for areas, dots for text
.rr_swatches <- function(legend, mark, outline = NULL) {
  lapply(legend, function(e) {
    col <- .rr_or(e$swatch$dot, .rr_or(e$swatch$fill, e$swatch$stroke))
    e$swatch <- switch(mark,
      line = , segment = list(fill = col, stroke = col),
      polygon = list(fill = col, stroke = .rr_or(outline, col)),
      list(dot = col)
    )
    e
  })
}

# One layer of a mark, from the map (or data starting one) and the call's arguments as the exported functions take
# them; `a` holds the call's quosures and values. What differs between marks is their style; the rest is shared.
.rr_mark <- function(mark, map, data, a) {
  mk <- .rr_marks[[mark]]
  if (!inherits(map, "rr_map")) {
    if (!is.null(data)) {
      stop(sprintf("rr_%s(): give the data once, as the first argument or as `data`.", mark), call. = FALSE)
    }
    data <- map
  }
  s <- .rr_quick(
    map, if (!is.null(data)) .rr_data(data), a$lon, a$lat, NULL, rlang::quo(NULL), "Map", 8790L,
    a$choose, groups = FALSE
  )
  ctx <- s$ctx_layer
  con <- ctx$con
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  nm <- .rr_layer_names(s$state, a$name, mk$kind)
  id <- nm$id
  data_cols <- setdiff(ctx$cols, c(ctx$lon, ctx$lat, ctx$geometry, a$lon_end, a$lat_end))

  # where: points from lon and lat (or a shape), lines and areas from a shape, a segment from two positions
  if (mark %in% c("line", "polygon") && is.null(ctx$geometry)) {
    stop(sprintf("rr_%s() draws shapes: the data need a geometry column (DuckDB's GEOMETRY or WKB) or an sf object.",
                 mark), call. = FALSE)
  }
  if (mark == "segment") {
    ends <- c(a$lon_end, a$lat_end)
    .rr_quick_need(ctx, ends)
  }

  # the coloured aesthetic: colour (fill for areas), a value or columns carried as properties rr_c<i>
  main <- if (mark == "polygon") "fill" else "colour"
  col <- .rr_aes(a[[main]], ctx$cols, main, colour = TRUE)
  cols <- col$columns
  types <- lapply(cols, function(cc) .rr_column_type(con, ctx$src, cc, ordered = cc %in% ctx$ordered))
  props_sql <- vapply(seq_along(cols), function(i) {
    t <- types[[i]]
    sprintf(
      ", %s AS rr_c%d",
      if (t$kind == "discrete") sprintf("CAST(%s AS VARCHAR)", q(cols[i]))
      else if (!is.null(t$time)) sprintf("epoch(%s)", q(cols[i]))
      else sprintf("CAST(%s AS DOUBLE)", q(cols[i])),
      i
    )
  }, "")
  geo_sql <- if (mark == "segment") {
    sprintf(", ST_MakeLine(ST_Point(%s, %s), ST_Point(%s, %s)) AS rr_geometry",
            q(ctx$lon), q(ctx$lat), q(a$lon_end), q(a$lat_end))
  } else ""
  # a point's time in seconds on the time panel's clock, where the map has a time, so a panel (rr_time()) shows only
  # its group's span
  t_s <- if (mark == "point" && !is.null(ctx$time)) .rr_time_s_sql(con, ctx$src, ctx$time)
  src <- sprintf("SELECT *%s%s%s FROM (%s)", paste(props_sql, collapse = ""), geo_sql,
                 if (is.null(t_s)) "" else sprintf(", %s AS rr_t", t_s), ctx$src)
  colours <- lapply(seq_along(cols), function(i) {
    .rr_colour_by(con, src, paste0("rr_c", i), paste0("rr_c", i), a$palette, a$limits, a$stretch, types[[i]])
  })
  default <- if (mark == "text") "#1b2a3a" else .rr_okabe_ito[1]
  fixed <- if (is.null(col)) default else if (!is.null(col$value)) .rr_hex(col$value)
  outline <- if (mark == "polygon") {
    oc <- .rr_aes(a$colour, ctx$cols, "colour", colour = TRUE)
    if (!is.null(oc$columns)) stop("rr_polygon(): `colour` is the outline's, one colour; colour areas by a column with `fill`.", call. = FALSE)
    if (is.null(oc)) "#4d4d4d" else .rr_hex(oc$value)
  }

  # sizes, as values: a point's diameter, a line's width, a text's size, in pixels
  num <- function(qq, what) {
    v <- .rr_aes(qq, ctx$cols, what)
    if (!is.null(v$columns)) stop(sprintf("`%s` by a column comes in a later version; give a number of pixels.", what), call. = FALSE)
    if (!is.null(v$value) && (!is.numeric(v$value) || v$value <= 0)) stop(sprintf("`%s` is a number of pixels.", what), call. = FALSE)
    v$value
  }
  size <- num(a$size, "size")
  linewidth <- num(a$linewidth, "linewidth")

  # text: the label's column
  lab <- if (mark == "text") {
    l <- .rr_select(a$label, ctx$cols, "label")
    if (length(l) != 1) stop("rr_text(): `label` is the one column whose values are written, bare.", call. = FALSE)
    l
  }

  # what the reader can do
  pop <- .rr_select(a$popup, data_cols, "popup")
  hov <- .rr_select(a$hover, ctx$cols, "hover")
  if (length(hov) > 1) stop("`hover` is one column.", call. = FALSE)
  title <- if (length(hov)) hov else if (length(pop)) pop[1]
  fields <- lapply(setdiff(pop, title), function(f) {
    t <- .rr_column_type(con, ctx$src, f)
    Filter(Negate(is.null), list(field = f, label = f, format = if (t$kind == "continuous" && is.null(t$time)) "value"))
  })
  props <- unique(c(
    if (length(cols)) paste0("rr_c", seq_along(cols)), pop, hov, lab,
    intersect(.rr_or(s$state[["choose_fields"]], character()), ctx$cols),
    if (!is.null(t_s)) c("rr_t", ctx$by)
  ))
  if (!is.null(t_s)) {
    s$state$panel_fields[[id]] <- Filter(Negate(is.null), list(by = ctx$by, t = "rr_t"))
  }
  tbl <- dplyr::tbl(con, dplyr::sql(src))
  tiles <- if (mark == "segment") {
    rr_tiles_table(tbl, geometry = "rr_geometry", properties = props)
  } else if (!is.null(ctx$geometry)) {
    rr_tiles_table(tbl, geometry = ctx$geometry, properties = props)
  } else {
    rr_tiles_table(tbl, lon = ctx$lon, lat = ctx$lat, properties = props)
  }

  # the style
  sid <- paste0(id, "-", mk$suffix)
  colour_expr <- if (length(cols)) colours[[1]]$colour else fixed
  zoomed <- function(a0, b0, c0) list("interpolate", list("linear"), list("zoom"), 3, a0, 8, b0, 13, c0)
  style <- switch(mark,
    point = list(list(id = sid, type = "circle", "source-layer" = "features",
      paint = list("circle-color" = colour_expr, "circle-radius" = if (!is.null(size)) size / 2 else zoomed(1.5, 3, 6),
                   "circle-opacity" = a$opacity %||% 0.85,
                   "circle-stroke-color" = "rgba(255,255,255,0.7)", "circle-stroke-width" = 0.5))),
    line = , segment = list(list(id = sid, type = "line", "source-layer" = "features",
      layout = list("line-cap" = "round", "line-join" = "round"),
      paint = list("line-color" = colour_expr, "line-width" = linewidth %||% zoomed(1, 2, 4),
                   "line-opacity" = a$opacity %||% 0.9))),
    polygon = list(
      list(id = sid, type = "fill", "source-layer" = "features",
           paint = list("fill-color" = colour_expr, "fill-opacity" = a$opacity %||% 0.5)),
      list(id = paste0(id, "-outline"), type = "line", "source-layer" = "features",
           paint = list("line-color" = outline, "line-width" = linewidth %||% 1))),
    text = list(list(id = sid, type = "symbol", "source-layer" = "features",
      layout = list("text-field" = list("to-string", list("get", lab)), "text-size" = size %||% 13,
                    "text-allow-overlap" = FALSE, "text-anchor" = "center"),
      paint = list("text-color" = colour_expr, "text-opacity" = a$opacity %||% 1,
                   "text-halo-color" = "rgba(255,255,255,0.9)", "text-halo-width" = 1.5)))
  )
  variants <- if (length(cols) > 1) {
    list(label = "Colour by", options = lapply(seq_along(cols), function(i) {
      list(value = cols[i], label = cols[i],
           paint = stats::setNames(list(stats::setNames(list(colours[[i]]$colour), mk$paint)), sid),
           legend = if (isTRUE(a$legend)) .rr_swatches(colours[[i]]$legend, mark, outline))
    }))
  }
  leg <- if (!isTRUE(a$legend)) NULL else if (length(cols)) .rr_swatches(colours[[1]]$legend, mark, outline) else
    .rr_swatches(list(list(swatch = list(dot = fixed), label = nm$label)), mark, outline)
  what <- switch(mark, point = "point", line = "line", segment = "line from its start to its end",
                 polygon = "area", text = "label")
  layer <- rr_layer(
    id, "vector",
    label = nm$label,
    info = .rr_or(a$info, if (length(cols) == 1) sprintf("Each row a %s, coloured by %s.", what, cols) else
      if (length(cols) > 1) sprintf("Each row a %s, coloured by the column chosen below.", what) else
      sprintf("Each row a %s.", what)),
    visible = a$visible,
    tiles = tiles,
    count = list(text = paste("{1}", mk$count)),
    minzoom = a$minzoom,
    maxzoom = a$maxzoom,
    style = style,
    legend = leg,
    variants = variants,
    popup = if (!is.null(title)) {
      Filter(Negate(is.null), list(click = sid, title = title, fields = if (length(fields)) fields,
                                   hover = if (length(hov)) TRUE))
    }
  )
  s$state$layer_section[[id]] <- a$section
  .rr_hold(.rr_quick_map(s, layer, list(con = con, name = attr(tiles, "table"))))
}

# The call's arguments for .rr_mark(): quosures for what may name columns, values for the rest; `color` for `colour`
.rr_mark_args <- function(env, dq, fname) {
  if ("color" %in% names(dq)) {
    env$colour <- dq[["color"]]
    dq[["color"]] <- NULL
  }
  if (length(dq)) {
    stop(sprintf("%s() has no argument %s.", fname, paste(names(dq), collapse = ", ")), call. = FALSE)
  }
  env
}

#' Points, lines, areas, segments and text
#'
#' A layer of a map, one mark for each row of the data. `rr_point()` draws a circle at each row's position (pings,
#' stations, catches); `rr_line()` draws rows that are lines (tows, transects, cables); `rr_segment()` draws a line
#' from each row's start to its end (a tow from shooting to hauling); `rr_polygon()` draws areas, filled and outlined;
#' `rr_text()` writes a column's values at the rows' positions (place names, station ids). Each is piped onto a map,
#' `rr_map(data) |> rr_point(colour = speed)`, or given the data first, `data |> rr_point()`, which starts a map. The
#' data stay in DuckDB and reach the page as vector tiles, so a map of millions of rows stays fast and its colours
#' change in the page.
#'
#' @param map An [rr_map()], or the data, which start a map.
#' @param data The rows, if not the map's: a parquet file, folder or glob, a lazy DuckDB table, a data frame or an sf
#'   object (points, or lines and areas for `rr_line()` and `rr_polygon()`; a table's shapes in a column `geometry`
#'   as DuckDB's GEOMETRY or WKB).
#' @param lon,lat The position's columns, by default `lon` and `lat`; for `rr_segment()` its start.
#' @param lon_end,lat_end For `rr_segment()`, the columns of its end.
#' @param colour The marks' colour (an area's outline): a colour (`"#c0392b"`), or a column written bare (`colour =
#'   speed`), coloured by `palette`; several columns, `c(behaviour, speed)`, are a choice in the page, the first
#'   shown. A column's colours by default: Okabe-Ito for up to eight values, Paired for up to twelve, Glasbey's 32
#'   beyond, the rest grey as "other"; viridis for an ordered factor and for numbers, dates and times. `color` is the
#'   same.
#' @param fill For `rr_polygon()`, the areas' colour, as `colour` is for the other marks.
#' @param size A point's diameter, a text's size, in pixels.
#' @param linewidth A line's width (an area's outline), in pixels.
#' @param opacity The marks' opacity, 0-1.
#' @param label For `rr_text()`, the column whose values are written, bare.
#' @param palette The colours for a column, in place of the default: a palette's name (from [rr_palettes()],
#'   `palette.pals()` or `hcl.pals()`, written loosely: `"set1"` is Set 1), colours, colours named by value
#'   (`c(Fishing = "#c0392b")`), or a function of n (`viridisLite::mako`).
#' @param limits,stretch For a numeric column: the values the palette's ends stand at (by default its 1st and 99th
#'   percentile), and how the colours spread between them, `"linear"`, `"log"` or `"equal"` (equal shares of the
#'   values).
#' @param legend Whether the layer has a legend.
#' @param popup The columns a click shows, written bare; `TRUE`, every column; `FALSE`, no popup.
#' @param hover A column whose value is shown beside the pointer.
#' @param choose For the map's first layer from data: the columns whose values the reader picks, as for [rr_map()].
#' @param name,info The layer's name in the sidebar and the text behind its ⓘ.
#' @param section The sidebar's box the layer sits in, by its title.
#' @param visible Whether the layer is on when the page opens.
#' @param minzoom,maxzoom The zooms the layer is drawn at.
#' @param ... Not used; `color` is taken for `colour`.
#' @return An [rr_map()] with the layer added.
#' @seealso [rr_map()]; [rr_layer()] for a layer in MapLibre's own terms.
#' @name rr_point
#' @export
#' @examples
#' \donttest{
#' m <- rr_example("dansk") |>
#'   rr_point(colour = c(behaviour, speed), popup = c(vessel_id, time_stamp, speed), minzoom = 9)
#' format(m)
#' rr_map(rr_example("trawlsurveytows")) |> rr_line(name = "Tows") |> format()
#' rr_example("dansk_harbours") |> rr_polygon(fill = "#d9822b", popup = SI_HARB) |> format()
#' }
rr_point <- function(
  map, data = NULL, lon = NULL, lat = NULL,
  colour = NULL, size = NULL, opacity = NULL,
  palette = NULL, limits = NULL, stretch = NULL, legend = TRUE,
  popup = TRUE, hover = NULL, choose = NULL,
  name = NULL, info = NULL, section = "Layers", visible = TRUE,
  minzoom = NULL, maxzoom = NULL, ...
) {
  a <- .rr_mark_args(list(
    lon = lon, lat = lat, colour = rlang::enquo(colour), size = rlang::enquo(size), opacity = opacity,
    palette = palette, limits = limits, stretch = stretch, legend = legend, popup = rlang::enquo(popup),
    hover = rlang::enquo(hover), choose = rlang::enquo(choose), name = name, info = info, section = section,
    visible = visible, minzoom = minzoom, maxzoom = maxzoom
  ), rlang::enquos(...), "rr_point")
  .rr_scope()
  .rr_mark("point", map, data, a)
}

#' @rdname rr_point
#' @export
rr_line <- function(
  map, data = NULL,
  colour = NULL, linewidth = NULL, opacity = NULL,
  palette = NULL, limits = NULL, stretch = NULL, legend = TRUE,
  popup = TRUE, hover = NULL, choose = NULL,
  name = NULL, info = NULL, section = "Layers", visible = TRUE,
  minzoom = NULL, maxzoom = NULL, ...
) {
  a <- .rr_mark_args(list(
    colour = rlang::enquo(colour), linewidth = rlang::enquo(linewidth), opacity = opacity,
    palette = palette, limits = limits, stretch = stretch, legend = legend, popup = rlang::enquo(popup),
    hover = rlang::enquo(hover), choose = rlang::enquo(choose), name = name, info = info, section = section,
    visible = visible, minzoom = minzoom, maxzoom = maxzoom
  ), rlang::enquos(...), "rr_line")
  .rr_scope()
  .rr_mark("line", map, data, a)
}

#' @rdname rr_point
#' @export
rr_segment <- function(
  map, data = NULL, lon = NULL, lat = NULL, lon_end, lat_end,
  colour = NULL, linewidth = NULL, opacity = NULL,
  palette = NULL, limits = NULL, stretch = NULL, legend = TRUE,
  popup = TRUE, hover = NULL, choose = NULL,
  name = NULL, info = NULL, section = "Layers", visible = TRUE,
  minzoom = NULL, maxzoom = NULL, ...
) {
  a <- .rr_mark_args(list(
    lon = lon, lat = lat, lon_end = .rr_one_col(rlang::enquo(lon_end), "lon_end"),
    lat_end = .rr_one_col(rlang::enquo(lat_end), "lat_end"),
    colour = rlang::enquo(colour), linewidth = rlang::enquo(linewidth), opacity = opacity,
    palette = palette, limits = limits, stretch = stretch, legend = legend, popup = rlang::enquo(popup),
    hover = rlang::enquo(hover), choose = rlang::enquo(choose), name = name, info = info, section = section,
    visible = visible, minzoom = minzoom, maxzoom = maxzoom
  ), rlang::enquos(...), "rr_segment")
  .rr_scope()
  .rr_mark("segment", map, data, a)
}

#' @rdname rr_point
#' @export
rr_polygon <- function(
  map, data = NULL,
  colour = NULL, fill = NULL, linewidth = NULL, opacity = NULL,
  palette = NULL, limits = NULL, stretch = NULL, legend = TRUE,
  popup = TRUE, hover = NULL, choose = NULL,
  name = NULL, info = NULL, section = "Layers", visible = TRUE,
  minzoom = NULL, maxzoom = NULL, ...
) {
  a <- .rr_mark_args(list(
    colour = rlang::enquo(colour), fill = rlang::enquo(fill), linewidth = rlang::enquo(linewidth),
    opacity = opacity, palette = palette, limits = limits, stretch = stretch, legend = legend,
    popup = rlang::enquo(popup), hover = rlang::enquo(hover), choose = rlang::enquo(choose), name = name,
    info = info, section = section, visible = visible, minzoom = minzoom, maxzoom = maxzoom
  ), rlang::enquos(...), "rr_polygon")
  .rr_scope()
  .rr_mark("polygon", map, data, a)
}

#' @rdname rr_point
#' @export
rr_text <- function(
  map, data = NULL, lon = NULL, lat = NULL,
  colour = NULL, size = NULL, opacity = NULL, label,
  palette = NULL, limits = NULL, stretch = NULL, legend = FALSE,
  popup = FALSE, hover = NULL, choose = NULL,
  name = NULL, info = NULL, section = "Layers", visible = TRUE,
  minzoom = NULL, maxzoom = NULL, ...
) {
  a <- .rr_mark_args(list(
    lon = lon, lat = lat, colour = rlang::enquo(colour), size = rlang::enquo(size), opacity = opacity,
    label = rlang::enquo(label), palette = palette, limits = limits, stretch = stretch, legend = legend,
    popup = rlang::enquo(popup), hover = rlang::enquo(hover), choose = rlang::enquo(choose), name = name,
    info = info, section = section, visible = visible, minzoom = minzoom, maxzoom = maxzoom
  ), rlang::enquos(...), "rr_text")
  .rr_scope()
  .rr_mark("text", map, data, a)
}

# One column named bare or as a string, for an argument that is always a column (a segment's end)
.rr_one_col <- function(q, what) {
  if (rlang::quo_is_missing(q)) {
    stop(sprintf("`%s` is a column of the data, written bare.", what), call. = FALSE)
  }
  e <- rlang::quo_get_expr(q)
  if (is.character(e) && length(e) == 1) e else rlang::as_name(e)
}

#' Paths: rows joined in time, a line per group
#'
#' A layer of lines made from rows joined in order of `time`, one line for each value of `group` (a float, an animal,
#' a ship, or each of its legs with `group = c(id, leg)`), as ggplot2's `geom_path()` joins rows in their order:
#' tracks of drifters, tagged animals, gliders and ships. A silence longer than `max_gap_s` seconds breaks a line. Coloured by a column, a line is cut where
#' the column's value changes, each run in its value's colour, and stays joined across the change.
#'
#' @inheritParams rr_point
#' @param group The columns, written bare, whose values each have a line: one, or several, `c(id, leg)`. By default
#'   none, one line through all the rows (and the map's `choose` columns, so a choice shows whole lines).
#' @param time The column that orders the rows, written bare: a time, or any column that orders (a distance along a
#'   transect, a sequence number). A lazy table has no order of its own, so it is needed.
#' @param max_gap_s A silence between two rows of a line longer than this many seconds breaks it, as `3600` for an
#'   hour; `Inf`, the default, for none, so no gap is hidden.
#' @param colour The lines' colour: a colour, or one column written bare, whose runs of one value cut and colour the
#'   line (Okabe-Ito and the other defaults of [rr_point()]).
#' @return An [rr_map()] with the layer added.
#' @export
#' @examples
#' \donttest{
#' m <- rr_map(rr_example("dansk")) |>
#'   rr_path(group = vessel_id, time = time_stamp, colour = behaviour, max_gap_s = 3600)
#' format(m)
#' }
rr_path <- function(
  map, data = NULL, lon = NULL, lat = NULL, group = NULL, time, max_gap_s = Inf,
  colour = NULL, linewidth = NULL, opacity = NULL,
  palette = NULL, limits = NULL, stretch = NULL, legend = TRUE,
  popup = TRUE, hover = NULL, choose = NULL,
  name = NULL, info = NULL, section = "Layers", visible = TRUE,
  minzoom = NULL, maxzoom = NULL, ...
) {
  dq <- rlang::enquos(...)
  colour_q <- rlang::enquo(colour)
  if ("color" %in% names(dq)) {
    colour_q <- dq[["color"]]
    dq[["color"]] <- NULL
  }
  if (length(dq)) {
    stop(sprintf("rr_path() has no argument %s.", paste(names(dq), collapse = ", ")), call. = FALSE)
  }
  .rr_scope()
  .rr_check_gap(max_gap_s)
  if (!inherits(map, "rr_map")) {
    if (!is.null(data)) stop("rr_path(): give the data once, as the first argument or as `data`.", call. = FALSE)
    data <- map
  }
  tcol <- .rr_one_col(rlang::enquo(time), "time")
  s <- .rr_quick(
    map, if (!is.null(data)) .rr_data(data), lon, lat, NULL, rlang::quo(NULL), "Map", 8790L,
    rlang::enquo(choose), groups = FALSE
  )
  ctx <- s$ctx_layer
  ctx$time <- if (tcol %in% ctx$cols) tcol # the layer's, not the map's
  grp0 <- .rr_select(rlang::enquo(group), ctx$cols, "group")
  # on the map's own rows, the path's time and group are the map's too, for a time panel added after it (rr_time())
  if (identical(s$state$ctx$src, ctx$src)) {
    s$state$ctx$time <- .rr_or(s$state$ctx$time, ctx$time)
    s$state$ctx$by <- .rr_or(s$state$ctx$by, if (length(grp0)) grp0[1])
  }
  if (!is.null(ctx$geometry)) {
    stop("rr_path() joins rows with positions (lon, lat); for rows that are lines, use rr_line().", call. = FALSE)
  }
  if (is.null(ctx$time) || !identical(ctx$time, tcol)) {
    stop(sprintf("rr_path(): no column %s in the data to order the rows by.", tcol), call. = FALSE)
  }
  con <- ctx$con
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  nm <- .rr_layer_names(s$state, name, "Paths")
  id <- nm$id
  grp <- .rr_select(rlang::enquo(group), ctx$cols, "group")
  keys <- unique(c(grp, intersect(.rr_or(s$state[["choose_fields"]], character()), ctx$cols)))
  col <- .rr_aes(colour_q, ctx$cols, "colour", colour = TRUE)
  if (length(col$columns) > 1) {
    stop("rr_path() is coloured by one column; for another, add another rr_path().", call. = FALSE)
  }
  cc <- col$columns
  type <- if (length(cc)) .rr_column_type(con, ctx$src, cc, ordered = cc %in% ctx$ordered)
  gap <- .rr_gap_sql(max_gap_s)
  t_s <- .rr_time_s_sql(con, ctx$src, ctx$time)
  .rr_extension(con, "spatial")
  lines_src <- .rr_runs_sql(
    con, ctx$src, ctx$lon, ctx$lat, ctx$time, keys,
    values = if (length(cc)) {
      if (type$kind == "discrete") sprintf("CAST(%s AS VARCHAR)", q(cc))
      else if (!is.null(type$time)) sprintf("epoch(%s)", q(cc))
      else sprintf("CAST(%s AS DOUBLE)", q(cc))
    },
    names = "rr_c1", count = "rows", t_names = c("rr_t0", "rr_t1"), gap = gap, t_s = t_s
  )
  colours <- if (length(cc)) .rr_colour_by(con, lines_src, "rr_c1", "rr_c1", palette, limits, stretch, type)
  fixed <- if (is.null(col)) .rr_okabe_ito[1] else if (!is.null(col$value)) .rr_hex(col$value)
  hov <- .rr_select(rlang::enquo(hover), keys, "hover")
  pop <- .rr_select(rlang::enquo(popup), c(keys, if (length(cc)) cc), "popup")
  props <- unique(c(keys, if (length(cc)) "rr_c1", "rows", "start", "stop", if (!is.null(t_s)) c("rr_t0", "rr_t1")))
  if (!is.null(t_s)) {
    s$state$panel_fields[[id]] <- list(by = .rr_or(grp[1], NULL), t0 = "rr_t0", t1 = "rr_t1")
  }
  tiles <- rr_tiles_table(dplyr::tbl(con, dplyr::sql(lines_src)), properties = props)
  sid <- paste0(id, "-line")
  title <- if (length(hov)) hov else if (length(intersect(pop, keys))) intersect(pop, keys)[1] else if (length(pop)) "start"
  fields <- if (length(pop)) {
    c(
      lapply(setdiff(intersect(pop, keys), title), function(f) list(field = f, label = f)),
      if (length(cc) && cc %in% pop) list(Filter(Negate(is.null), list(
        field = "rr_c1", label = cc, format = if (type$kind == "continuous" && is.null(type$time)) "value"
      ))),
      list(list(field = "start", label = "from"), list(field = "stop", label = "to"),
           list(field = "rows", label = "rows", format = "count"))
    )
  }
  layer <- rr_layer(
    id, "vector",
    label = nm$label,
    info = .rr_or(info, paste0(
      if (length(grp)) sprintf("The rows of each %s", paste(grp, collapse = " and ")) else "The rows",
      " joined in order of ", ctx$time,
      if (is.finite(max_gap_s)) sprintf(", broken where two are more than %s apart", .rr_duration_text(max_gap_s)),
      if (length(cc)) sprintf("%s coloured by %s", if (is.finite(max_gap_s)) ", and" else " and", cc),
      "."
    )),
    visible = visible,
    tiles = tiles,
    count = list(text = "{1} lines"),
    minzoom = minzoom,
    maxzoom = maxzoom,
    style = list(list(
      id = sid, type = "line", "source-layer" = "features",
      layout = list("line-cap" = "round", "line-join" = "round"),
      paint = list("line-color" = if (length(cc)) colours$colour else fixed,
                   "line-width" = .rr_or(linewidth, list("interpolate", list("linear"), list("zoom"), 3, 1, 8, 2, 13, 3.5)),
                   "line-opacity" = .rr_or(opacity, 0.85))
    )),
    legend = if (!isTRUE(legend)) NULL else if (length(cc)) .rr_swatches(colours$legend, "line") else
      .rr_swatches(list(list(swatch = list(dot = fixed), label = nm$label)), "line"),
    popup = if (!is.null(title)) {
      Filter(Negate(is.null), list(click = sid, title = title, fields = fields, hover = if (length(hov)) TRUE))
    }
  )
  s$state$max_gap_s <- .rr_or(s$state$max_gap_s, max_gap_s)
  s$state$layer_section[[id]] <- section
  .rr_hold(.rr_quick_map(s, layer, list(con = con, name = attr(tiles, "table"))))
}

# Rows joined into lines, the query a path is drawn from (rr_path(), and the quick look's track): a line per value of
# `keys` and stretch, a stretch broken where two rows are more than the gap apart (`gap`, .rr_gap_sql()) and where any
# of `values` (SQL expressions, each kept as `names[i]`) changes; a run ends at the next run's first row, so the line
# stays joined across a change. Each line carries its rows' count (`count`), its first and last time as text (start,
# stop), and in seconds on the time panel's clock (`t_names`, from `t_s`, .rr_time_s_sql()), and its geometry.
.rr_runs_sql <- function(con, src, lon, lat, time, keys, values, names, count, t_names, gap, t_s) {
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  k <- length(values)
  part <- if (!length(keys)) "" else paste("PARTITION BY", paste(q(keys), collapse = ", "))
  sprintf(
    "WITH s AS (
       SELECT %1$s %2$s AS lon, %3$s AS lat, %4$s AS t, %10$s AS rr_ts %8$s FROM (%7$s)
       WHERE %2$s IS NOT NULL AND %3$s IS NOT NULL AND %4$s IS NOT NULL),
     w AS (
       SELECT *, CASE WHEN epoch(t) - epoch(lag(t) OVER o) > %6$s %9$s THEN 1 ELSE 0 END AS brk,
              lead(lon) OVER o AS lon2, lead(lat) OVER o AS lat2, epoch(lead(t) OVER o) - epoch(t) AS dt_next
       FROM s WINDOW o AS (%5$s ORDER BY t)),
     g AS (SELECT *, sum(brk) OVER (%5$s ORDER BY t ROWS UNBOUNDED PRECEDING) AS stretch FROM w),
     a AS (
       SELECT %1$s %11$s count(*) AS %12$s, strftime(min(t), '%%Y-%%m-%%d %%H:%%M') AS start,
              strftime(max(t), '%%Y-%%m-%%d %%H:%%M') AS stop, min(rr_ts) AS %13$s, max(rr_ts) AS %14$s,
              list(ST_Point(lon, lat) ORDER BY t) AS pts,
              last(CASE WHEN dt_next <= %6$s THEN lon2 END ORDER BY t) AS lon2,
              last(CASE WHEN dt_next <= %6$s THEN lat2 END ORDER BY t) AS lat2
       FROM g GROUP BY %1$s stretch),
     l AS (SELECT * EXCLUDE (pts, lon2, lat2),
                  CASE WHEN lon2 IS NULL THEN pts ELSE list_append(pts, ST_Point(lon2, lat2)) END AS pts FROM a)
     SELECT * EXCLUDE (pts), ST_MakeLine(pts) AS geometry FROM l WHERE len(pts) > 1",
    if (!length(keys)) "" else paste0(paste(q(keys), collapse = ", "), ","),
    q(lon), q(lat), q(time), part, gap, src,
    paste(vapply(seq_len(k), function(i) sprintf(", %s AS v%d", values[i], i), ""), collapse = ""),
    paste(vapply(seq_len(k), function(i) sprintf(" OR v%1$d IS DISTINCT FROM lag(v%1$d) OVER o", i), ""), collapse = ""),
    .rr_or(t_s, "CAST(NULL AS DOUBLE)"),
    paste(vapply(seq_len(k), function(i) sprintf("any_value(v%d) AS %s, ", i, names[i]), ""), collapse = ""),
    count, t_names[1], t_names[2]
  )
}
