# quick.R — the quick look at pings.
# Moved from ramb's R/map.R on 2026-11-10.

#' Pings on a map, layer by layer
#'
#' The quick look at pings. `.rr_pings()` draws them as points coloured by a column, `.rr_track()` joins them
#' into each vessel's track, and each adds its layer to the map piped into it, so they compose:
#' `pings |> .rr_track() |> .rr_pings()`.
#'
#' Given pings, each starts a map; given a map from either, it adds a layer drawn over the earlier ones, from the map's
#' pings and settings (`lon`, `lat`, `time`, `by`) unless `data` gives others. The result is an [rr_map()], shown when
#' printed: at the console in RStudio's Viewer (or a browser), a map printed on the same port replacing the last and
#' dropping its temporary tables. [rr_serve()] serves it on a port of one's choice.
#'
#' Points: a numeric column is coloured on a continuous palette spread over its 1st to 99th percentile; any other
#' column, or with `discrete = TRUE`, takes a colour per value (the 12 most frequent; the rest grey). Several columns,
#' `c(speed, behaviour)`, are offered under "Colour by" in the page, the first shown. A point's radius is in metres,
#' so it grows as the map zooms in, from 1 pixel to `radius_max` pixels.
#'
#' Track: each vessel's pings joined in time order, one line however long the vessel falls silent; with `max_gap_s`,
#' broken where two pings are more than that many seconds apart (`max_gap_s = 3600` at an hour). With a column, such
#' as `fishing_flow`, it breaks where the value changes too, each run coloured by its value (as the points are) and
#' ending at the next run's first fix, so the line stays joined. A second call adds a second track, by another column,
#' each one turned on and off in the page: `pings |> .rr_track(fishing_flow) |> .rr_track(provider)`.
#'
#' Grouped pings, `group_by(trip)` or `group_by(trip, gear)`, give the page a control per group column above the
#' layers, each value with its number of pings: for a column of 12 values or fewer, chips that tick on and off, so
#' several show at once (setting and hauling); for one of more, a list to show one at a time; and buttons that step to
#' the previous and next. A choice shows those groups' pings and lines on every layer that has the column, fits the
#' view to them, and goes into the address. The groups cut the track too, so each group's line is its own.
#'
#' The map opens on the first base map, EMODnet's sea floor. Each layer copies its pings into a temporary DuckDB table, as
#' [rr_tiles_table()] does.
#'
#' @param x Pings: a data frame or lazy DuckDB table with `lon` and `lat` (WGS 84), or an sf object of points; or a
#'   map made by either function.
#' @param col For the points, the column they are coloured by, a name or a string, or several,
#'   `c(fishing_flow, speed)`, to choose between in the page ("Colour by"), the first shown. For the track, one column,
#'   whose runs of equal value cut and colour the line (a numeric one cuts it at every ping, each step coloured by its
#'   first ping's value), or `NULL` for one colour; for another column, call `.rr_track()` again.
#' @param popup Further columns a click on a point shows.
#' @param discrete Whether a column of `col` takes a colour per value; by default each that is not numeric.
#' @param radius,radius_max The point radius in metres, and its largest size in pixels.
#' @param palette A palette of [grDevices::hcl.colors()] for a numeric column, light at its high end on the dark base
#'   map.
#' @param max_gap_s A silence between two of a vessel's pings longer than this many seconds breaks its line, as
#'   `3600` for an hour; `Inf`, the default, for none, so no gap is hidden.
#' @param data Pings for this layer, in place of the map's.
#' @param lon,lat,time The position and time columns: by default the map's, else `"lon"`, `"lat"` and `"time"`.
#'   The track needs the time; a point's popup shows it first.
#' @param by The vessel column, a name or a string: one track each. By default the map's, else `vid` where the data
#'   have it, else one track through all the pings.
#' @param choose Columns whose values the page offers to show, a name, a string or `c()` of them: by default
#'   the groups of the pings (`dplyr::group_by()`). Set by the call that starts the map.
#' @param title,port The page's title, and where the map is served when printed: set by the call that starts it.
#' @return An [rr_map()], with the layer added.
#' @seealso [rr_time()] for a panel of a vessel's pings over time under the map; [rr_map()] for a map of other
#'   layers, groups and languages.
#' @noRd
#' @examples
#' \donttest{
#' m <- rr_example("dansk") |>
#'   .rr_track(time = "time_stamp", by = vessel_id) |>
#'   .rr_pings(c(speed, behaviour))
#' m
#' }
#' # At the console a map opens in RStudio's Viewer when printed:
#' if (interactive()) {
#'   rr_example("dansk") |> .rr_pings(time = "time_stamp")
#'   rr_example("dansk") |>
#'     .rr_track(behaviour, time = "time_stamp", by = vessel_id) |>
#'     .rr_pings(c(behaviour, speed))
#' }
.rr_pings <- function(
  x,
  col = "speed",
  popup = NULL,
  discrete = NULL,
  radius = 200,
  radius_max = 8,
  palette = "Inferno",
  data = NULL,
  lon = NULL,
  lat = NULL,
  time = NULL,
  by = NULL,
  choose = NULL,
  title = "Track",
  port = 8790L
) {
  # a data frame registered for the map stays with the map; one registered by a call that fails is released
  .rr_scope()
  s <- .rr_quick(
    x,
    data,
    lon,
    lat,
    time,
    rlang::enquo(by),
    title,
    port,
    rlang::enquo(choose)
  )
  ctx <- s$ctx_layer
  col <- .rr_track_cols(rlang::enquo(col), ctx$cols)
  .rr_quick_need(ctx, c(col, popup))
  id <- .rr_quick_id(s$state, "pings")
  con <- ctx$con
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  numeric <- vapply(
    col,
    function(cc) {
      is.numeric(dplyr::collect(utils::head(
        dplyr::select(ctx$x, dplyr::all_of(cc)),
        1
      ))[[1]])
    },
    logical(1)
  )
  is_discrete <- if (is.null(discrete)) {
    !numeric
  } else {
    rep(isTRUE(discrete), length(col))
  }
  colours <- lapply(seq_along(col), function(i) {
    .rr_quick_colours(
      ctx,
      col[i],
      is_discrete[i],
      paste0("rb_class_", i),
      palette
    )
  })
  dot <- paste0(id, "-dot")
  # several columns: a choice in the page, each column's colours and legend a variant of the points' style
  variants <- if (length(col) > 1) {
    list(
      label = "Colour by",
      options = lapply(seq_along(col), function(i) {
        list(
          value = col[i],
          label = col[i],
          paint = stats::setNames(
            list(list("circle-color" = colours[[i]]$colour)),
            dot
          ),
          legend = colours[[i]]$legend
        )
      })
    )
  }
  # the radius in metres: pixels at zoom 0 at the data's middle latitude, doubling with each zoom, from 1 to radius_max
  r0 <- radius /
    (156543.03392 * cos((ctx$box$lat0 + ctx$box$lat1) / 2 * pi / 180))
  za <- min(max(log2(1 / r0), 0), 23)
  zb <- min(max(log2(radius_max / r0), za + 0.01), 24)
  size <- list(
    "interpolate",
    list("exponential", 2),
    list("zoom"),
    za,
    max(1, r0 * 2^za),
    zb,
    min(radius_max, r0 * 2^zb)
  )
  classes <- vapply(
    which(is_discrete),
    function(i) sprintf(", CAST(%s AS VARCHAR) AS rb_class_%d", q(col[i]), i),
    ""
  )
  # each ping's time in seconds on the time panel's clock, so a panel shows only its vessel's span (rr_time())
  t_s <- if (!is.null(ctx$time)) .rr_time_s_sql(con, ctx$src, ctx$time)
  pts <- dplyr::tbl(
    con,
    dplyr::sql(sprintf(
      "SELECT *%s%s FROM (%s)",
      paste(classes, collapse = ""),
      if (is.null(t_s)) "" else sprintf(", %s AS rb_t", t_s),
      ctx$src
    ))
  )
  props <- unique(c(
    col,
    ctx$time,
    popup,
    ctx$by,
    intersect(.rr_or(s$state[["choose_fields"]], character()), ctx$cols),
    if (any(is_discrete)) paste0("rb_class_", which(is_discrete)),
    if (!is.null(t_s)) "rb_t"
  ))
  if (!is.null(t_s)) {
    s$state$panel_fields[[id]] <- list(by = ctx$by, t = "rb_t")
  }
  tiles <- rr_tiles_table(
    pts,
    lon = ctx$lon,
    lat = ctx$lat,
    properties = props
  )
  fields <- c(
    lapply(seq_along(col), function(i) {
      Filter(
        Negate(is.null),
        list(
          field = col[i],
          label = col[i],
          format = if (is_discrete[i]) NULL else "value"
        )
      )
    }),
    lapply(setdiff(unique(c(popup, ctx$by)), col), function(f) {
      list(field = f, label = f)
    })
  )
  layer <- rr_layer(
    id,
    "vector",
    label = .rr_quick_label(s$state, "Pings"),
    info = if (length(col) > 1) {
      "Each ping, coloured by the column chosen below; a numeric one over its 1st to 99th percentile."
    } else {
      sprintf(
        "Each ping, coloured by %s%s.",
        col,
        if (is_discrete[1]) {
          ""
        } else {
          ", its 1st to 99th percentile across the palette"
        }
      )
    },
    tiles = tiles,
    count = list(text = "{1} pings"),
    style = list(list(
      id = dot,
      type = "circle",
      "source-layer" = "features",
      paint = list(
        "circle-color" = colours[[1]]$colour,
        "circle-radius" = size,
        "circle-opacity" = 0.85
      )
    )),
    legend = colours[[1]]$legend,
    variants = variants,
    popup = list(click = dot, title = .rr_or(ctx$time, col[1]), fields = fields)
  )
  .rr_hold(.rr_quick_map(
    s,
    layer,
    list(con = con, name = attr(tiles, "table"))
  ))
}

#' @noRd
.rr_track <- function(
  x,
  col = NULL,
  max_gap_s = Inf,
  discrete = NULL,
  palette = "Inferno",
  data = NULL,
  lon = NULL,
  lat = NULL,
  time = NULL,
  by = NULL,
  choose = NULL,
  title = "Track",
  port = 8790L
) {
  # as .rr_pings(): the frames of a call that fails are released
  .rr_scope()
  .rr_check_gap(max_gap_s)
  s <- .rr_quick(
    x,
    data,
    lon,
    lat,
    time,
    rlang::enquo(by),
    title,
    port,
    rlang::enquo(choose)
  )
  ctx <- s$ctx_layer
  col_q <- rlang::enquo(col)
  col <- if (rlang::quo_is_null(col_q)) {
    NULL
  } else {
    .rr_track_cols(col_q, ctx$cols)
  }
  if (length(col) > 1) {
    stop(
      "The track is coloured by one column; for another, call .rr_track() again: each call adds a track to turn on and off.",
      call. = FALSE
    )
  }
  if (is.null(ctx$time)) {
    stop(
      "The track joins the pings in time order: name their time column in `time`.",
      call. = FALSE
    )
  }
  .rr_quick_need(ctx, col)
  id <- .rr_quick_id(s$state, "path")
  con <- ctx$con
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  by <- ctx$by
  # the line is cut by the fields to choose by as well as by the vessel, so a choice shows whole lines
  keys <- unique(c(
    by,
    intersect(.rr_or(s$state[["choose_fields"]], character()), ctx$cols)
  ))
  k <- length(col)
  numeric <- vapply(
    col,
    function(cc) {
      is.numeric(dplyr::collect(utils::head(
        dplyr::select(ctx$x, dplyr::all_of(cc)),
        1
      ))[[1]])
    },
    logical(1)
  )
  is_discrete <- if (is.null(discrete)) !numeric else rep(isTRUE(discrete), k)
  # each column's value on the run, as rb_path_<i>: text for a colour per value, a number for a palette
  vals <- vapply(
    seq_len(k),
    function(i) {
      sprintf(
        ", CAST(%s AS %s) AS v%d",
        q(col[i]),
        if (is_discrete[i]) "VARCHAR" else "DOUBLE",
        i
      )
    },
    ""
  )
  changed <- vapply(
    seq_len(k),
    function(i) sprintf(" OR v%1$d IS DISTINCT FROM lag(v%1$d) OVER o", i),
    ""
  )
  kept <- vapply(
    seq_len(k),
    function(i) sprintf("any_value(v%1$d) AS rb_path_%1$d, ", i),
    ""
  )
  part <- if (!length(keys)) {
    ""
  } else {
    paste("PARTITION BY", paste(q(keys), collapse = ", "))
  }
  gap <- .rr_gap_sql(max_gap_s)
  # each line's first and last time in seconds on the time panel's clock, so a panel shows only its vessel's span
  t_s <- .rr_time_s_sql(con, ctx$src, ctx$time)
  # a line per vessel and stretch: a silence of more than max_gap_s starts a new one, and so does a change of any
  # column's value, so each run carries one value of each; a run ends at the next run's first fix, so the line stays
  # joined across a change
  .rr_extension(con, "spatial")
  lines <- dplyr::tbl(
    con,
    dplyr::sql(sprintf(
      "WITH s AS (
         SELECT %1$s %2$s AS lon, %3$s AS lat, %4$s AS t, %11$s AS rb_ts %8$s FROM (%7$s)
         WHERE %2$s IS NOT NULL AND %3$s IS NOT NULL AND %4$s IS NOT NULL),
       w AS (
         SELECT *, CASE WHEN epoch(t) - epoch(lag(t) OVER o) > %6$s %9$s THEN 1 ELSE 0 END AS brk,
                lead(lon) OVER o AS lon2, lead(lat) OVER o AS lat2, epoch(lead(t) OVER o) - epoch(t) AS dt_next
         FROM s WINDOW o AS (%5$s ORDER BY t)),
       g AS (SELECT *, sum(brk) OVER (%5$s ORDER BY t ROWS UNBOUNDED PRECEDING) AS stretch FROM w),
       a AS (
         SELECT %1$s %10$s count(*) AS pings, strftime(min(t), '%%Y-%%m-%%d %%H:%%M') AS start,
                strftime(max(t), '%%Y-%%m-%%d %%H:%%M') AS stop, min(rb_ts) AS rb_t0, max(rb_ts) AS rb_t1,
                list(ST_Point(lon, lat) ORDER BY t) AS pts,
                last(CASE WHEN dt_next <= %6$s THEN lon2 END ORDER BY t) AS lon2,
                last(CASE WHEN dt_next <= %6$s THEN lat2 END ORDER BY t) AS lat2
         FROM g GROUP BY %1$s stretch),
       l AS (SELECT * EXCLUDE (pts, lon2, lat2),
                    CASE WHEN lon2 IS NULL THEN pts ELSE list_append(pts, ST_Point(lon2, lat2)) END AS pts FROM a)
       SELECT * EXCLUDE (pts), ST_MakeLine(pts) AS geometry FROM l WHERE len(pts) > 1",
      if (!length(keys)) "" else paste0(paste(q(keys), collapse = ", "), ","),
      q(ctx$lon),
      q(ctx$lat),
      q(ctx$time),
      part,
      gap,
      ctx$src,
      paste(vals, collapse = ""),
      paste(changed, collapse = ""),
      paste(kept, collapse = ""),
      .rr_or(t_s, "CAST(NULL AS DOUBLE)")
    ))
  )
  props <- c(
    keys,
    if (k) paste0("rb_path_", seq_len(k)),
    "pings",
    "start",
    "stop",
    if (!is.null(t_s)) c("rb_t0", "rb_t1")
  )
  if (!is.null(t_s)) {
    s$state$panel_fields[[id]] <- list(by = by, t0 = "rb_t0", t1 = "rb_t1")
  }
  tiles <- rr_tiles_table(lines, properties = props)
  line <- paste0(id, "-line")
  colours <- lapply(seq_len(k), function(i) {
    p <- paste0("rb_path_", i)
    v <- .rr_quick_colours(ctx, col[i], is_discrete[i], p, palette, get = p)
    v$legend <- lapply(v$legend, function(x) {
      list(swatch = list(fill = x$swatch$dot), label = x$label)
    })
    v
  })
  layer <- rr_layer(
    id,
    "vector",
    label = .rr_quick_label(
      s$state,
      if (k == 1) paste("Track by", col) else "Track"
    ),
    info = paste0(
      if (is.null(by)) "The pings" else sprintf("The pings of each %s", by),
      " joined in time order",
      if (is.finite(max_gap_s)) {
        sprintf(
          ", broken where two are more than %s apart",
          .rr_duration_text(max_gap_s)
        )
      },
      if (k > 0) {
        sprintf(
          "%s coloured by %s",
          if (is.finite(max_gap_s)) ", and" else " and",
          col
        )
      },
      "."
    ),
    tiles = tiles,
    style = list(list(
      id = line,
      type = "line",
      "source-layer" = "features",
      paint = if (k == 0) {
        list("line-color" = "#00e5ff", "line-width" = 1.5, "line-opacity" = 0.5)
      } else {
        list(
          "line-color" = colours[[1]]$colour,
          "line-width" = 3,
          "line-opacity" = 0.85
        )
      }
    )),
    legend = if (k == 0) {
      list(list(swatch = list(fill = "#00e5ff"), label = "track"))
    } else {
      colours[[1]]$legend
    },
    popup = list(
      click = line,
      title = .rr_or(by, "start"),
      fields = c(
        lapply(seq_len(k), function(i) {
          Filter(
            Negate(is.null),
            list(
              field = paste0("rb_path_", i),
              label = col[i],
              format = if (is_discrete[i]) NULL else "value"
            )
          )
        }),
        list(
          list(field = "start", label = "from"),
          list(field = "stop", label = "to"),
          list(field = "pings", label = "pings", format = "count")
        )
      )
    )
  )
  # the first track's gap, which a time panel added after it takes by default
  s$state$max_gap_s <- .rr_or(s$state$max_gap_s, max_gap_s)
  .rr_hold(.rr_quick_map(
    s,
    layer,
    list(con = con, name = attr(tiles, "table"))
  ))
}

# The map piped in, if any, and the pings and settings of the layer being added
.rr_quick <- function(
  x,
  data,
  lon,
  lat,
  time,
  by_q,
  title,
  port,
  choose_q = rlang::quo(NULL)
) {
  state <- if (inherits(x, "rr_map")) attr(x, "quick")
  if (inherits(x, "rr_map") && is.null(state)) {
    stop(
      "This map was not made by .rr_pings() or .rr_track(); give its layers to rr_map().",
      call. = FALSE
    )
  }
  base <- state$ctx
  pings <- if (!is.null(data)) {
    data
  } else if (is.null(state)) {
    x
  }
  ctx <- if (is.null(pings)) {
    if (
      !rlang::quo_is_null(by_q) ||
        !is.null(lon) ||
        !is.null(lat) ||
        !is.null(time)
    ) {
      stop(
        "`lon`, `lat`, `time` and `by` are the map's; give `data` to set them for other pings.",
        call. = FALSE
      )
    }
    base
  } else {
    .rr_quick_data(pings, lon, lat, time, by_q, base)
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
  # the map's choices: the groups of the pings that start it, and the columns `choose` names
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

# The values of each field to choose by, with the pings' count and extent of each, and the extent of each combination
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
  box_sql <- sprintf(
    "count(*) AS n, min(%1$s) AS lon0, min(%2$s) AS lat0, max(%1$s) AS lon1, max(%2$s) AS lat1",
    q(ctx$lon),
    q(ctx$lat)
  )
  where <- sprintf("%s IS NOT NULL AND %s IS NOT NULL", q(ctx$lon), q(ctx$lat))
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
        bbox = box(r, j)
      )
    })
  }
  out
}

# Pings read for a layer: the lazy table, its columns, the settings (from the arguments, else the map's, else the
# usual names) and its bounds
.rr_quick_data <- function(pings, lon, lat, time, by_q, base) {
  if (inherits(pings, "sf")) {
    xy <- sf::st_coordinates(pings)
    pings <- sf::st_drop_geometry(pings)
    pings[[.rr_or(lon, "lon")]] <- xy[, 1]
    pings[[.rr_or(lat, "lat")]] <- xy[, 2]
  }
  groups <- dplyr::group_vars(pings)
  con <- .rr_con_for(pings)
  x <- dplyr::ungroup(.rr_as_lazy(pings, con))
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
    } else if ("vid" %in% cols) {
      "vid"
    }
  } else {
    rlang::as_name(by_q)
  }
  ctx <- list(
    x = x,
    con = con,
    groups = groups,
    src = .rr_sql(x),
    cols = cols,
    lon = lon,
    lat = lat,
    time = time,
    by = by
  )
  .rr_quick_need(ctx, c(lon, lat, by))
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  box <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT count(*) AS n, min(%1$s) AS lon0, max(%1$s) AS lon1, min(%2$s) AS lat0, max(%2$s) AS lat1
       FROM (%3$s) WHERE %1$s IS NOT NULL AND %2$s IS NOT NULL",
      q(lon),
      q(lat),
      ctx$src
    )
  )
  if (box$n == 0) {
    stop("No ping has a position.", call. = FALSE)
  }
  ctx$box <- box
  ctx
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

# A layer's id: the kind's name, numbered from the second of its kind on the map
.rr_quick_id <- function(state, kind) {
  ids <- vapply(state$layers, function(l) l$config$id, "")
  n <- sum(ids == kind | startsWith(ids, paste0(kind, "_")))
  if (n == 0) kind else paste0(kind, "_", n + 1)
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

# A column's colours and legend: a colour per value (the 12 most frequent; the rest grey), matched on the text
# property `prop`, or a continuous palette over its 1st to 99th percentile, read from the property `get`
.rr_quick_colours <- function(
  ctx,
  cc,
  discrete,
  prop,
  palette = "Inferno",
  get = cc
) {
  grey <- "#808080"
  q <- function(v) as.character(DBI::dbQuoteIdentifier(ctx$con, v))
  if (discrete) {
    vals <- DBI::dbGetQuery(
      ctx$con,
      sprintf(
        "SELECT CAST(%1$s AS VARCHAR) AS v, count(*) AS n FROM (%2$s) WHERE %1$s IS NOT NULL GROUP BY 1 ORDER BY n DESC, v",
        q(cc),
        ctx$src
      )
    )
    k <- min(nrow(vals), 12L)
    pal <- grDevices::hcl.colors(max(k, 2L), "Dynamic")[seq_len(k)]
    legend <- lapply(seq_len(k), function(j) {
      list(swatch = list(dot = pal[j]), label = vals$v[j])
    })
    if (nrow(vals) > k) {
      legend <- c(
        legend,
        list(list(swatch = list(dot = grey), label = "other"))
      )
    }
    return(list(
      colour = c(
        list("match", list("get", prop)),
        unlist(
          lapply(seq_len(k), function(j) list(vals$v[j], pal[j])),
          recursive = FALSE
        ),
        list(grey)
      ),
      legend = legend
    ))
  }
  qs <- DBI::dbGetQuery(
    ctx$con,
    sprintf(
      "SELECT quantile_cont(%1$s, 0.01) AS lo, quantile_cont(%1$s, 0.99) AS hi FROM (%2$s) WHERE %1$s IS NOT NULL",
      q(cc),
      ctx$src
    )
  )
  pal <- grDevices::hcl.colors(9, palette)
  lo <- qs$lo
  hi <- if (isTRUE(qs$hi > qs$lo)) qs$hi else lo + 1
  stops <- seq(lo, hi, length.out = 9)
  fmt <- function(v) format(signif(v, 3), big.mark = ",", trim = TRUE)
  list(
    colour = list(
      "case",
      list("has", get),
      c(
        list("interpolate", list("linear"), list("get", get)),
        unlist(
          lapply(1:9, function(j) list(stops[j], pal[j])),
          recursive = FALSE
        )
      ),
      grey
    ),
    legend = lapply(c(1, 3, 5, 7, 9), function(j) {
      list(
        swatch = list(dot = pal[j]),
        # less than or equal, greater than or equal: an escape would be rewritten by the formatter
        label = paste0(
          if (j == 1) {
            paste0(intToUtf8(8804), " ")
          } else if (j == 9) {
            paste0(intToUtf8(8805), " ")
          } else {
            ""
          },
          fmt(stops[j])
        )
      )
    })
  )
}

# The map with the layer added over the others: the first view covers every layer's pings, on a dark base map
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
  m <- rr_map(
    rr_section("track", "Track", state$layers),
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
  )
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

