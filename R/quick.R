# quick.R — what the layer functions and the time panel build on: a map that carries its making (its data, layers,
# extents and choices) from call to call, so each call adds to the map piped in, and the map's config is made again.
# Moved from ramb's R/map.R on 2026-11-10.

# The map piped in, if any, and the rows and settings of the layer being added
.rr_quick <- function(
  x,
  data,
  lon,
  lat,
  time,
  by_q,
  title,
  port,
  choose_q = rlang::quo(NULL),
  groups = TRUE
) {
  state <- if (inherits(x, "rr_map")) attr(x, "quick")
  # a map started from data by rr_map(data): its data the map's, its `choose` the map's lists
  if (!is.null(state$pending)) {
    p <- state$pending
    state <- list(
      ctx = .rr_quick_data(p$data, NULL, NULL, NULL, rlang::quo(NULL), NULL),
      layers = list(), tables = list(), boxes = list(), title = p$title, port = p$port, sections = p$sections
    )
    if (!groups) {
      state$ctx$groups <- character()
    }
    if (rlang::quo_is_null(choose_q)) {
      choose_q <- p$choose_q
    }
  }
  if (inherits(x, "rr_map") && is.null(state)) {
    stop(
      "This map was not made by rr_map(data) or a layer function; add its layers to it with rr_layer() and rr_map().",
      call. = FALSE
    )
  }
  base <- state$ctx
  rows <- if (!is.null(data)) {
    data
  } else if (is.null(state)) {
    x
  }
  ctx <- if (is.null(rows)) {
    if (
      !rlang::quo_is_null(by_q) ||
        !is.null(lon) ||
        !is.null(lat) ||
        !is.null(time)
    ) {
      stop(
        "`lon`, `lat`, `time` and `by` are the map's; give `data` to set them for other rows.",
        call. = FALSE
      )
    }
    base
  } else {
    .rr_quick_data(rows, lon, lat, time, by_q, base)
  }
  if (!groups) {
    ctx$groups <- character() # rebelrebel's layer functions read no group_by(): `choose` names the lists
  }
  if (is.null(state)) {
    state <- list(
      ctx = ctx,
      layers = list(),
      tables = list(),
      boxes = list(),
      title = title,
      port = as.integer(port)
    )
  }
  # the map's choices: the groups of the rows that start it, and the columns `choose` names
  wanted <- unique(c(
    ctx$groups,
    if (!rlang::quo_is_null(choose_q)) .rr_track_cols(choose_q, ctx$cols)
  ))
  if (length(state$layers) == 0) {
    .rr_quick_need(ctx, wanted)
    state[["choose_fields"]] <- wanted
    state[["choose"]] <- .rr_quick_choose(ctx, wanted)
  } else if (length(setdiff(wanted, state[["choose_fields"]]))) {
    warning(
      "The map's choices are set by the call that starts it; ",
      paste(setdiff(wanted, state[["choose_fields"]]), collapse = ", "),
      " is not added.",
      call. = FALSE
    )
  }
  list(state = state, ctx_layer = ctx)
}

# The values of each field to choose by, with the rows' count and extent of each, and the extent of each combination
# that occurs where there are several fields. A number or a logical is matched as it is, any other type as the text
# the tiles carry it as
.rr_quick_choose <- function(ctx, fields) {
  if (!length(fields)) {
    return(NULL)
  }
  q <- function(v) as.character(DBI::dbQuoteIdentifier(ctx$con, v))
  as_is <- vapply(
    fields,
    function(f) {
      v <- dplyr::collect(utils::head(
        dplyr::select(ctx$x, dplyr::all_of(f)),
        1
      ))[[1]]
      is.numeric(v) || is.logical(v)
    },
    logical(1)
  )
  value_sql <- function(i) {
    if (as_is[i]) q(fields[i]) else sprintf("CAST(%s AS VARCHAR)", q(fields[i]))
  }
  b <- .rr_box_sql(ctx$con, ctx$src, ctx$lon, ctx$lat, ctx$geometry)
  box_sql <- b$select
  where <- b$where
  key <- function(v, i) {
    if (is.na(v)) {
      "<NA>"
    } else if (is.numeric(v)) {
      format(v, scientific = FALSE, trim = TRUE, digits = 15)
    } else {
      as.character(v)
    }
  }
  box <- function(r, j) c(r$lon0[j], r$lat0[j], r$lon1[j], r$lat1[j])
  out <- list(
    fields = lapply(seq_along(fields), function(i) {
      r <- DBI::dbGetQuery(
        ctx$con,
        sprintf(
          "SELECT %s AS v, %s FROM (%s) WHERE %s GROUP BY 1 ORDER BY v NULLS LAST",
          value_sql(i),
          box_sql,
          ctx$src,
          where
        )
      )
      list(
        field = fields[i],
        label = fields[i],
        multi = nrow(r) <= 12, # few values: chips that tick on and off, so several show at once
        options = lapply(seq_len(nrow(r)), function(j) {
          v <- r$v[j]
          list(
            value = key(v, i),
            text = if (is.na(v)) "none" else key(v, i), # the value alone, for the page's counts as the lists narrow
            label = sprintf(
              "%s (%s)",
              if (is.na(v)) "none" else key(v, i),
              format(r$n[j], big.mark = ",", trim = TRUE)
            ),
            match = if (is.na(v)) NULL else v,
            bbox = box(r, j)
          )
        })
      )
    })
  )
  if (length(fields) > 1) {
    r <- DBI::dbGetQuery(
      ctx$con,
      sprintf(
        "SELECT %s, %s FROM (%s) WHERE %s GROUP BY ALL",
        paste(
          vapply(
            seq_along(fields),
            function(i) sprintf("%s AS v%d", value_sql(i), i),
            ""
          ),
          collapse = ", "
        ),
        box_sql,
        ctx$src,
        where
      )
    )
    out$extents <- lapply(seq_len(nrow(r)), function(j) {
      list(
        key = paste(
          vapply(
            seq_along(fields),
            function(i) key(r[[paste0("v", i)]][j], i),
            ""
          ),
          collapse = intToUtf8(31)
        ),
        n = r$n[j], # the rows of the combination, for the linked lists' counts
        bbox = box(r, j)
      )
    })
  }
  out
}

# Pings read for a layer: the lazy table, its columns, the settings (from the arguments, else the map's, else the
# usual names) and its bounds
.rr_quick_data <- function(rows, lon, lat, time, by_q, base) {
  wkt <- FALSE
  if (inherits(rows, "sf")) {
    if (all(as.character(sf::st_geometry_type(rows)) == "POINT")) {
      xy <- sf::st_coordinates(rows)
      rows <- sf::st_drop_geometry(rows)
      rows[[.rr_or(lon, "lon")]] <- xy[, 1]
      rows[[.rr_or(lat, "lat")]] <- xy[, 2]
    } else {
      # lines and areas: their shapes as text, made DuckDB's GEOMETRY again on the connection (WGS 84)
      g <- sf::st_geometry(rows)
      if (!is.na(sf::st_crs(g)) && !isTRUE(sf::st_is_longlat(g))) {
        g <- sf::st_transform(g, 4326)
      }
      rows <- sf::st_drop_geometry(rows)
      rows$rr_wkt <- sf::st_as_text(g)
      wkt <- TRUE
    }
  }
  groups <- dplyr::group_vars(rows)
  # an ordered factor's order, which DuckDB's ENUM does not keep, and a column's label and units, its attributes
  ordered <- if (is.data.frame(rows)) names(Filter(is.ordered, rows)) else character()
  dict <- if (is.data.frame(rows)) .rr_dict_attributes(rows) else list()
  con <- .rr_con_for(rows)
  x <- dplyr::ungroup(.rr_as_lazy(rows, con))
  if (wkt) {
    .rr_extension(con, "spatial")
    x <- dplyr::tbl(con, dplyr::sql(sprintf(
      "SELECT * EXCLUDE (rr_wkt), ST_GeomFromText(rr_wkt) AS geometry FROM (%s)", .rr_sql(x)
    )))
  }
  cols <- colnames(x)
  pick <- function(given, from_map, usual) {
    if (!is.null(given)) {
      given
    } else if (!is.null(from_map) && from_map %in% cols) {
      from_map
    } else {
      usual
    }
  }
  lon <- pick(lon, base$lon, "lon")
  lat <- pick(lat, base$lat, "lat")
  time <- pick(time, base$time, "time")
  if (!time %in% cols) {
    time <- NULL
  }
  by <- if (rlang::quo_is_null(by_q)) {
    if (!is.null(base$by) && base$by %in% cols) {
      base$by
    }
  } else {
    rlang::as_name(by_q)
  }
  # rows with no lon and lat but a shape: lines, areas or points as DuckDB's GEOMETRY or as WKB
  geometry <- if (!(lon %in% cols && lat %in% cols)) {
    intersect(c("geometry", "geom", "wkb_geometry", "the_geom"), cols)[1]
  }
  if (identical(geometry, NA_character_)) {
    geometry <- NULL
  }
  ctx <- list(
    x = x,
    con = con,
    groups = groups,
    ordered = ordered,
    dict = .rr_dict_merge(dict, .rr_dict_parquet(con, .rr_sql(x))),
    src = .rr_sql(x),
    cols = cols,
    lon = if (is.null(geometry)) lon,
    lat = if (is.null(geometry)) lat,
    geometry = geometry,
    time = time,
    by = by
  )
  .rr_quick_need(ctx, c(if (is.null(geometry)) c(lon, lat), by))
  b <- .rr_box_sql(con, ctx$src, ctx$lon, ctx$lat, geometry)
  box <- DBI::dbGetQuery(con, sprintf("SELECT %s FROM (%s) WHERE %s", b$select, ctx$src, b$where))
  if (box$n == 0) {
    stop(if (is.null(geometry)) "No row has a position." else "No row has a shape.", call. = FALSE)
  }
  ctx$box <- box
  ctx
}

# The SQL of rows' count and extent, from lon and lat or from a shape (DuckDB's GEOMETRY or WKB), and of the rows
# that have one: `select` to aggregate, `where` to keep
.rr_box_sql <- function(con, src, lon, lat, geometry) {
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  if (is.null(geometry)) {
    return(list(
      select = sprintf(
        "count(*) AS n, min(%1$s) AS lon0, max(%1$s) AS lon1, min(%2$s) AS lat0, max(%2$s) AS lat1",
        q(lon), q(lat)
      ),
      where = sprintf("%s IS NOT NULL AND %s IS NOT NULL", q(lon), q(lat))
    ))
  }
  .rr_extension(con, "spatial")
  type <- DBI::dbGetQuery(con, sprintf("SELECT typeof(%s) AS t FROM (%s) LIMIT 1", q(geometry), src))$t
  g <- if (length(type) && startsWith(type, "GEOMETRY")) q(geometry) else sprintf("ST_GeomFromWKB(%s)", q(geometry))
  list(
    select = sprintf(
      "count(*) AS n, min(ST_XMin(%1$s)) AS lon0, max(ST_XMax(%1$s)) AS lon1, min(ST_YMin(%1$s)) AS lat0, max(ST_YMax(%1$s)) AS lat1",
      g
    ),
    where = sprintf("%s IS NOT NULL", q(geometry))
  )
}

.rr_quick_need <- function(ctx, need) {
  missing <- setdiff(unique(need), ctx$cols)
  if (length(missing)) {
    stop(
      sprintf(
        "No column %s in the data; it has %s.",
        paste(missing, collapse = ", "),
        paste(ctx$cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

# A layer's name, numbered only where an earlier layer has the same
.rr_quick_label <- function(state, label) {
  taken <- vapply(state$layers, function(l) .rr_or(l$config$label, ""), "")
  if (!label %in% taken) {
    return(label)
  }
  n <- 2
  while (paste(label, n) %in% taken) {
    n <- n + 1
  }
  paste(label, n)
}

# The map with the layer added over the others: the first view covers every layer's rows
.rr_quick_map <- function(s, layer = NULL, table = NULL) {
  state <- s$state
  if (!is.null(layer)) {
    state$layer_fields[[layer$config$id]] <- intersect(
      .rr_or(state[["choose_fields"]], character()),
      s$ctx_layer$cols
    )
    state$layers <- c(state$layers, list(layer))
    state$tables <- c(state$tables, list(table))
  }
  state$boxes <- c(state$boxes, list(s$ctx_layer$box))
  lon0 <- min(vapply(state$boxes, `[[`, 1, "lon0"))
  lon1 <- max(vapply(state$boxes, `[[`, 1, "lon1"))
  lat0 <- min(vapply(state$boxes, `[[`, 1, "lat0"))
  lat1 <- max(vapply(state$boxes, `[[`, 1, "lat1"))
  span <- max(lon1 - lon0, 2 * (lat1 - lat0)) # a degree of latitude draws taller
  bases <- rr_basemaps()
  # the layers by their sections, in the order first named (the quick look's all under "Track")
  secs <- vapply(state$layers, function(l) .rr_or(state$layer_section[[l$config$id]], "Track"), "")
  sections <- lapply(unique(secs), function(sn) {
    spec <- state$sections[[sn]]
    rr_section(paste0("s_", .rr_slug(sn)), .rr_or(spec$title, sn), state$layers[secs == sn], open = .rr_or(spec$open, TRUE))
  })
  if (!length(sections)) {
    sections <- list(rr_section("track", "Track", list())) # a time panel alone
  }
  m <- do.call(rr_map, c(sections, list(
    title = state$title,
    view = list(
      center = c((lon0 + lon1) / 2, (lat0 + lat1) / 2),
      zoom = if (span > 0) max(1, min(15, floor(log2(360 / span)) - 1)) else 12
    ),
    choose = if (!is.null(state[["choose"]])) {
      cfg <- state[["choose"]]
      cfg$fields <- lapply(cfg$fields, function(f) {
        f$layers <- names(Filter(
          function(fl) f$field %in% fl,
          state$layer_fields
        ))
        f
      })
      cfg
    },
    basemaps = bases
  )))
  # the time panel (rr_time()): its config for the page, its series for the server and its tables
  if (!is.null(state$time)) {
    m$config$time <- state$time$config
    # the layers whose features carry a vessel and a time: the page shows on them only the panel's vessel and span
    m$config$time$layers <- unname(lapply(names(state$panel_fields), function(id) {
      c(list(id = id), Filter(Negate(is.null), state$panel_fields[[id]]))
    }))
    m$time <- state$time
  }
  attr(m, "quick") <- state
  attr(m, "tables") <- c(state$tables, state$time$tables)
  attr(m, "port") <- state$port
  m
}

# The columns `col` names: a name, a string, c() of them, or a variable holding strings
.rr_track_cols <- function(q, cols) {
  e <- rlang::quo_get_expr(q)
  if (rlang::is_call(e, "c")) {
    return(vapply(
      rlang::call_args(e),
      function(a) if (is.character(a)) a else rlang::as_name(a),
      ""
    ))
  }
  if (is.character(e)) {
    return(e)
  }
  if (rlang::is_symbol(e)) {
    # a column, else a variable holding names, else the name as given, for the column check to report
    nm <- rlang::as_name(e)
    if (nm %in% cols) {
      return(nm)
    }
    v <- tryCatch(rlang::eval_tidy(q), error = function(err) NULL)
    return(if (is.character(v)) v else nm)
  }
  v <- rlang::eval_tidy(q)
  if (is.character(v)) {
    return(v)
  }
  stop(
    "`col` is a column, by name or as a string, or c() of them.",
    call. = FALSE
  )
}

