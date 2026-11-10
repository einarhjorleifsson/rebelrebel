# time.R — the time panel under the map: a series over time, linked to the map.
# Moved from ramb's R/map.R on 2026-11-10.

#' A time panel under the map: one group's rows over time, linked to the map
#'
#' `rr_time()` adds a panel under a map, or starts a map with it, that shows one group's rows over time (a vessel's,
#' a float's, an animal's: the values of `by`):
#' * a column or several as lines, the first shown and the others ticked on in the panel;
#' * marks where a column says so (a flag), on the chart and on the map;
#' * lanes of intervals under the chart (visits, legs, phases), a click on one setting the window to it.
#'
#' The panel and the map are linked. A drag across the chart sets the time window, and a double click goes back to
#' the whole span. The map draws the window's path and points, and fits to them. A hover on the chart marks that row
#' on the map, and a click on a point of the window marks its time on the chart. The group, the span and the window go
#' into the page's address.
#'
#' The page holds one group's span at a time: by default its whole series; with `span`, also each of its intervals,
#' chosen under *Span*. A group with more than `span_max` rows is offered by its spans only, or by month where there
#' are none. The map's other layers that carry the group and a time ([rr_path()], [rr_point()]) show only the chosen
#' group and span; a path's line that runs over a span's start or end is shown whole. The window's path is drawn whole,
#' its points thinned to about 5,000 (every k-th, a marked one always), so the map stays quick.
#'
#' Times are shown as the data hold them, in no time zone, labelled by `time_label`.
#'
#' @param x Rows: a data frame or lazy DuckDB table with `lon` and `lat` (WGS 84), or an sf object of points; or a
#'   map, whose rows, time and group (from [rr_path()]) the panel takes.
#' @param data Rows for this panel, in place of the map's.
#' @param lon,lat,time The position and time columns: by default the map's, else `"lon"`, `"lat"` and `"time"`.
#' @param by The column whose values each have a series, a name or a string: by default the map's (a path's
#'   `group`), else one series of all the rows.
#' @param title,port The page's title, and where the map is served when printed: set by the call that starts it.
#' @param col The numeric columns drawn over time, a name, a string or `c()` of them; the first shown.
#' @param marks Columns that mark rows, as `col`: a logical column marks its `TRUE` rows, any other its values that
#'   are not missing (such as a flag's label). Each column is a row of ticks along the chart's top and a ring on the
#'   map in a colour of its own, named in the sidebar; `c(truth = "wrong", clean = "flag")` names them.
#' @param lanes Intervals drawn as lanes under the chart: a named list (the names are the lanes' labels), each
#'   * a column of the rows, as a string: its runs of one value (a phase, a leg), missing values left out and a run
#'     broken by a silence longer than `max_gap_s`;
#'   * a table of intervals with the group's column (`by`), and its start and end in `start` and `end`;
#'   * or `list(data = , start = , end = , label = )`, naming the columns, `label` a column that names each interval.
#' @param max_gap_s A silence between two rows longer than this many seconds breaks the panel's line, on the chart
#'   and on the map, and a lane's run of one value. By default a path's ([rr_path()]'s `max_gap_s`) when the map has
#'   one already, else `Inf`: nothing broken, so no gap is hidden.
#' @param span Units the page holds one at a time, chosen besides the whole series: a column of the rows that names
#'   them, as a string (a leg, a deployment: `"leg"`, or `c(leg = "leg_id")` to name the list), one span per value from
#'   its first row to its last; or a table of intervals, or `list(data = , start = , end = , label = )`, as
#'   for `lanes`, named as a column is with `list(legs = <table>)`. A span holds every row of its group from its start
#'   to its end, those whose value is missing too. Spans that overlap are reported: a state that comes and goes, such as behaviour,
#'   belongs in `lanes`. `NULL`, the default, offers the whole series alone.
#' @param span_max The most rows the page holds at once: a group with more is offered by its spans, else by month.
#'   The browser holds far more than the default is likely to meet: a series of 2.2 million rows loaded in about
#'   8 seconds, most of it the transfer.
#' @param time_label The clock the times are on, shown beside them.
#' @return An [rr_map()] with the time panel.
#' @seealso [rr_path()] and [rr_point()], the layers it pairs with; [rr_bundle()], which writes the panel's series
#'   beside the page.
#' @export
#' @examples
#' \donttest{
#' d <- dplyr::filter(dplyr::collect(rr_example("dansk")), vessel_id %in% c("EX_1", "EX_12"))
#' m <- rr_map(d) |>
#'   rr_path(group = vessel_id, time = time_stamp) |>
#'   rr_time(speed, lanes = list(behaviour = "behaviour"))
#' format(m)
#' # m  # printed at the console, the map opens with the panel under it
#' }
rr_time <- function(
  x,
  col,
  marks = NULL,
  lanes = NULL,
  span = NULL,
  max_gap_s = NULL,
  span_max = 3000000,
  time_label = "UTC",
  data = NULL,
  lon = NULL,
  lat = NULL,
  time = NULL,
  by = NULL,
  title = "Track",
  port = 8790L
) {
  # the frames of a call that fails are released
  .rr_scope()
  s <- .rr_quick(
    x,
    data,
    lon,
    lat,
    time,
    rlang::enquo(by),
    title,
    port
  )
  ctx <- s$ctx_layer
  if (!is.null(s$state$time)) {
    stop("A map has one time panel.", call. = FALSE)
  }
  max_gap_s <- .rr_or(max_gap_s, .rr_or(s$state$max_gap_s, Inf))
  .rr_check_gap(max_gap_s)
  if (is.null(ctx$time)) {
    stop(
      "The time panel draws the pings over time: name their time column in `time`.",
      call. = FALSE
    )
  }
  # unnamed: the page's lists are arrays
  if (missing(col)) {
    stop("rr_time(): name the numeric columns drawn over time in `col`, bare: rr_time(speed).", call. = FALSE)
  }
  col <- unname(.rr_track_cols(rlang::enquo(col), ctx$cols))
  marks_q <- rlang::enquo(marks)
  marks <- if (rlang::quo_is_null(marks_q)) {
    character()
  } else {
    .rr_track_cols(marks_q, ctx$cols)
  }
  # a mark's label: the name it is given, c(truth = "wrong"), else its column's
  mark_names <- names(marks)
  if (is.null(mark_names)) {
    mark_names <- rep("", length(marks))
  }
  mark_labels <- ifelse(is.na(mark_names) | mark_names == "", marks, mark_names)
  marks <- unname(marks)
  mark_labels <- unname(mark_labels)
  .rr_quick_need(ctx, c(col, marks))
  if (
    !is.numeric(span_max) ||
      length(span_max) != 1 ||
      is.na(span_max) ||
      span_max < 1
  ) {
    stop("`span_max` is a number of pings.", call. = FALSE)
  }
  if (
    !is.character(time_label) || length(time_label) != 1 || is.na(time_label)
  ) {
    stop("`time_label` is a text.", call. = FALSE)
  }
  if (
    !is.null(lanes) &&
      (!is.list(lanes) || is.null(names(lanes)) || any(!nzchar(names(lanes))))
  ) {
    stop(
      "`lanes` is a named list: a column's name, a table or a list(data = ) each.",
      call. = FALSE
    )
  }
  con <- ctx$con
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  types <- DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM (%s)", ctx$src))
  type_of <- function(cc) types$column_type[match(cc, types$column_name)]
  is_num <- function(t) {
    grepl("^(U?(TINY|SMALL|BIG|HUGE)?INT(EGER)?|FLOAT|DOUBLE|REAL|DECIMAL)", t)
  }
  is_time <- function(t) grepl("^(TIMESTAMP|DATE)", t)
  not_num <- col[!is_num(type_of(col))]
  if (length(not_num)) {
    stop(
      sprintf(
        "`col` is numeric columns; %s is not.",
        paste(not_num, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!is_time(type_of(ctx$time))) {
    stop(
      sprintf("The time column, %s, is not a date-time.", ctx$time),
      call. = FALSE
    )
  }
  # a time as microseconds since 1970 on its own clock: a date at its midnight, a time with a zone as its instant
  us <- function(v, t) {
    if (startsWith(t, "DATE")) {
      sprintf("epoch_us(CAST(%s AS TIMESTAMP))", v)
    } else {
      sprintf("epoch_us(%s)", v)
    }
  }
  vessel <- if (is.null(ctx$by)) {
    "'all'"
  } else {
    .rr_time_key(q(ctx$by), type_of(ctx$by))
  }
  mark_sql <- vapply(
    seq_along(marks),
    function(i) {
      if (type_of(marks[i]) == "BOOLEAN") {
        sprintf(
          ", CASE WHEN %s THEN %s END AS rb_k%d",
          q(marks[i]),
          DBI::dbQuoteString(con, marks[i]),
          i
        )
      } else {
        sprintf(", CAST(%s AS VARCHAR) AS rb_k%d", q(marks[i]), i)
      }
    },
    ""
  )
  nm <- .rr_name(con, "rb_time_")
  create <- sprintf(
    "CREATE TEMP TABLE %1$s AS
     SELECT %2$s AS rb_v, %3$s / 1e6 AS rb_t, strftime(make_timestamp(%3$s), '%%Y-%%m') AS rb_m,
            round(%4$s, 6) AS rb_lon, round(%5$s, 6) AS rb_lat %6$s %7$s
     FROM (%8$s) WHERE %9$s IS NOT NULL AND %4$s IS NOT NULL AND %5$s IS NOT NULL
     ORDER BY rb_v, rb_t",
    nm,
    vessel,
    us(q(ctx$time), type_of(ctx$time)),
    q(ctx$lon),
    q(ctx$lat),
    paste(
      sprintf(
        ", CAST(%s AS DOUBLE) AS rb_c%d",
        vapply(col, q, ""),
        seq_along(col)
      ),
      collapse = ""
    ),
    paste(mark_sql, collapse = ""),
    ctx$src,
    q(ctx$time)
  )
  # the lanes' intervals, each vessel's, in one table: the runs of a column, or a table's rows
  lane_sql <- unlist(lapply(seq_along(lanes), function(k) {
    .rr_time_lane(
      lanes[[k]],
      names(lanes)[k],
      k,
      ctx,
      vessel,
      us,
      type_of,
      max_gap_s
    )
  }))
  nm_l <- if (length(lane_sql)) .rr_name(con, "rb_lanes_")
  create_l <- if (length(lane_sql)) {
    sprintf(
      "CREATE TEMP TABLE %s AS %s",
      nm_l,
      paste(lane_sql, collapse = " UNION ALL ")
    )
  }
  # a table dropped when its map was replaced on its port (rr_serve()) is made again
  make <- function() {
    DBI::dbExecute(con, create)
    if (!is.null(create_l)) DBI::dbExecute(con, create_l)
  }
  make()
  # what the spans are called in the page: a column's name, or the name given it, c(trip = "trip_id")
  # a table of intervals, or list(data = ), named as a column is: list(fault = <table>)
  named_table <- is.list(span) && !is.data.frame(span) && !inherits(span, "tbl_lazy") && length(span) == 1 &&
    !is.null(names(span)) && nzchar(names(span)) && names(span) != "data" &&
    (is.data.frame(span[[1]]) || inherits(span[[1]], "tbl_lazy") || (is.list(span[[1]]) && !is.null(span[[1]]$data)))
  span_label <- if (is.character(span)) {
    if (!is.null(names(span)) && nzchar(names(span)[1])) names(span)[1] else span[1]
  } else if (named_table) {
    names(span)
  } else if (!is.null(span)) {
    "Span"
  }
  if (is.character(span)) {
    span <- unname(span)
  }
  if (named_table) {
    span <- span[[1]]
  }
  # the spans (`span`): each vessel's intervals in time order, numbered from 1, with the pings each holds; an interval
  # that holds none is left out. Kept here, where a span's series finds its start and end. A column gives one span per
  # value, from its first ping to its last: a trip whose id is missing on a ping, as on one a speed filter removed, stays
  # one span, and that ping is in it to be seen
  span_tab <- if (!is.null(span)) {
    sql_s <- if (is.character(span)) {
      if (length(span) != 1 || !span %in% ctx$cols) {
        stop(
          sprintf("`span`: no column %s in the pings.", paste(span, collapse = ", ")),
          call. = FALSE
        )
      }
      sprintf(
        "(SELECT %1$s AS rb_v, min(%2$s) / 1e6 AS rb_t0, max(%2$s) / 1e6 AS rb_t1, CAST(%3$s AS VARCHAR) AS rb_label
          FROM (%4$s) WHERE %5$s IS NOT NULL AND %3$s IS NOT NULL GROUP BY 1, 4)",
        vessel,
        us(q(ctx$time), type_of(ctx$time)),
        q(span),
        ctx$src,
        q(ctx$time)
      )
    } else {
      .rr_time_lane(
        span,
        "span",
        0L,
        ctx,
        vessel,
        us,
        type_of,
        Inf,
        what = "`span`"
      )
    }
    sp <- DBI::dbGetQuery(
      con,
      sprintf(
        "WITH s AS (SELECT rb_v, rb_t0, rb_t1, rb_label FROM %s WHERE rb_t1 >= rb_t0)
         SELECT s.rb_v, s.rb_t0, s.rb_t1, any_value(s.rb_label) AS rb_label, count(*) AS rb_n
         FROM s JOIN %s p ON p.rb_v = s.rb_v AND p.rb_t BETWEEN s.rb_t0 AND s.rb_t1
         GROUP BY s.rb_v, s.rb_t0, s.rb_t1 ORDER BY s.rb_v, s.rb_t0, s.rb_t1",
        sql_s,
        nm
      )
    )
    sp$rb_k <- stats::ave(seq_len(nrow(sp)), sp$rb_v, FUN = seq_along)
    # a unit's spans do not overlap; a state's do (Fishing from its first ping to its last, over Other), and so do two
    # trips that claim the same hours: said, not refused
    over <- sum(unlist(lapply(split(sp, sp$rb_v), function(x) {
      if (nrow(x) < 2) {
        return(0L)
      }
      ends <- cummax(x$rb_t1)
      sum(x$rb_t0[-1] < ends[-nrow(x)])
    })))
    if (over > 0) {
      message(sprintf(
        "%s of %s spans of %s begin before an earlier one has ended. A span is a unit such as a trip or a voyage; a state that comes and goes, such as behaviour, belongs in `lanes`.",
        format(over, big.mark = ","),
        format(nrow(sp), big.mark = ","),
        span_label
      ))
    }
    sp
  }
  vs <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT rb_v AS v, count(*) AS n, min(rb_t) AS t0, max(rb_t) AS t1 FROM %s GROUP BY 1",
      nm
    )
  )
  if (!nrow(vs)) {
    stop("No ping has a time and a position.", call. = FALSE)
  }
  # vessels in their order as numbers where every one is a number, else in natural order (EX_2 before EX_10)
  num_v <- suppressWarnings(as.numeric(vs$v))
  vs <- vs[if (anyNA(num_v)) order(.rr_natural_key(vs$v)) else order(num_v), ]
  months <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT rb_v AS v, rb_m AS m, count(*) AS n FROM %s GROUP BY ALL ORDER BY 1, 2",
      nm
    )
  )
  fmt_n <- function(n) format(n, big.mark = ",", trim = TRUE)
  # a span's name in the list: its label where it has one, and its start on the data's own clock
  span_name <- function(label, t0) {
    start <- format(
      as.POSIXct(t0, origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%d %H:%M"
    )
    if (is.na(label)) start else sprintf("%s \u00b7 %s", label, start)
  }
  # each vessel's spans: the whole series where it holds no more than span_max, then its own spans (`span`), else, for
  # a vessel too long to hold whole, its months
  vessels <- lapply(seq_len(nrow(vs)), function(j) {
    whole <- vs$n[j] <= span_max
    sj <- if (!is.null(span_tab)) span_tab[span_tab$rb_v == vs$v[j], ]
    own <- if (!is.null(sj) && nrow(sj)) {
      lapply(seq_len(nrow(sj)), function(i) {
        nmi <- span_name(sj$rb_label[i], sj$rb_t0[i])
        list(
          value = sprintf("s%d", sj$rb_k[i]),
          name = nmi,
          label = sprintf("%s (%s)", nmi, fmt_n(sj$rb_n[i])),
          n = sj$rb_n[i]
        )
      })
    }
    by_month <- if (!whole && !length(own)) {
      mj <- months[months$v == vs$v[j], ]
      lapply(seq_len(nrow(mj)), function(i) {
        list(
          value = mj$m[i],
          label = sprintf("%s (%s)", mj$m[i], fmt_n(mj$n[i])),
          n = mj$n[i]
        )
      })
    }
    spans <- c(
      if (whole) {
        list(list(
          value = "all",
          label = sprintf("whole (%s)", fmt_n(vs$n[j])),
          n = vs$n[j]
        ))
      },
      own,
      by_month
    )
    list(
      value = vs$v[j],
      label = sprintf("%s (%s)", vs$v[j], fmt_n(vs$n[j])),
      n = vs$n[j],
      t0 = vs$t0[j],
      t1 = vs$t1[j],
      # the whole series where it is held, else the latest span
      span = if (whole) "all" else spans[[length(spans)]]$value,
      spans = spans
    )
  })
  series <- function(v, span) {
    if (!DBI::dbExistsTable(con, nm)) {
      make()
    }
    where <- sprintf(
      "rb_v = %s%s",
      DBI::dbQuoteString(con, v),
      if (identical(span, "all")) {
        ""
      } else if (grepl("^s[0-9]+$", span) && !is.null(span_tab)) {
        # one of the vessel's own spans: its pings from its start to its end, as numbers DuckDB reads as DOUBLE
        sk <- span_tab[
          span_tab$rb_v == v & span_tab$rb_k == as.integer(substring(span, 2)),
        ]
        sprintf(
          " AND rb_t BETWEEN %s AND %s",
          sprintf("%.17e", sk$rb_t0[1]),
          sprintf("%.17e", sk$rb_t1[1])
        )
      } else {
        sprintf(" AND rb_m = %s", DBI::dbQuoteString(con, span))
      }
    )
    r <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT rb_t, rb_lon, rb_lat%s%s FROM %s WHERE %s ORDER BY rb_t",
        paste(sprintf(", rb_c%d", seq_along(col)), collapse = ""),
        paste(sprintf(", rb_k%d", seq_along(marks)), collapse = ""),
        nm,
        where
      )
    )
    # I() keeps a span of one ping an array in JSON
    out <- list(
      t = I(r$rb_t),
      lon = I(r$rb_lon),
      lat = I(r$rb_lat),
      y = stats::setNames(
        lapply(seq_along(col), function(i) {
          I(signif(r[[paste0("rb_c", i)]], 6))
        }),
        col
      ),
      marks = stats::setNames(
        lapply(seq_along(marks), function(i) {
          k <- r[[paste0("rb_k", i)]]
          w <- which(!is.na(k))
          list(i = I(w - 1L), v = I(k[w]))
        }),
        marks
      )
    )
    if (length(marks) == 0) {
      out$marks <- stats::setNames(list(), character())
    }
    out$lanes <- stats::setNames(list(), character())
    if (!is.null(nm_l) && nrow(r)) {
      l <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT rb_lane, rb_t0, rb_t1, rb_label FROM %s WHERE rb_v = %s AND rb_t1 >= %s AND rb_t0 <= %s ORDER BY rb_lane, rb_t0",
          nm_l,
          DBI::dbQuoteString(con, v),
          format(min(r$rb_t), digits = 15),
          format(max(r$rb_t), digits = 15)
        )
      )
      out$lanes <- stats::setNames(
        lapply(seq_along(lanes), function(k) {
          lk <- l[l$rb_lane == k, ]
          list(t0 = I(lk$rb_t0), t1 = I(lk$rb_t1), label = I(lk$rb_label))
        }),
        as.character(seq_along(lanes))
      )
    }
    out
  }
  s$state$time <- list(
    config = list(
      by = ctx$by,
      time_label = time_label,
      span_max = span_max,
      span_label = span_label,
      # none for Inf, which JSON cannot hold: the page breaks the line nowhere
      max_gap_s = if (is.finite(max_gap_s)) max_gap_s,
      cols = lapply(col, function(cc) list(field = cc, label = cc)),
      marks = lapply(seq_along(marks), function(k) list(field = marks[k], label = mark_labels[k])),
      lanes = lapply(seq_along(lanes), function(k) {
        list(id = as.character(k), label = names(lanes)[k])
      }),
      vessels = vessels
    ),
    series = series,
    tables = c(
      list(list(con = con, name = nm)),
      if (!is.null(nm_l)) list(list(con = con, name = nm_l))
    )
  )
  .rr_hold(.rr_quick_map(s))
}

# A time column as seconds since 1970 on its own clock, as the time panel holds its pings (rr_time()'s `us`): a
# date at its midnight, a time with a zone as its instant; NULL for a column that is not a date-time
.rr_time_s_sql <- function(con, src, col) {
  qc <- as.character(DBI::dbQuoteIdentifier(con, col))
  ty <- DBI::dbGetQuery(
    con,
    sprintf("DESCRIBE SELECT %s FROM (%s)", qc, src)
  )$column_type[1]
  if (!grepl("^(TIMESTAMP|DATE)", ty)) {
    return(NULL)
  }
  if (startsWith(ty, "DATE")) {
    sprintf("epoch_us(CAST(%s AS TIMESTAMP)) / 1e6", qc)
  } else {
    sprintf("epoch_us(%s) / 1e6", qc)
  }
}

# The gap that breaks a line, in seconds: a positive number, Inf for none
.rr_check_gap <- function(max_gap_s) {
  if (
    !is.numeric(max_gap_s) ||
      length(max_gap_s) != 1 ||
      is.na(max_gap_s) ||
      max_gap_s <= 0
  ) {
    stop(
      "`max_gap_s` is a number of seconds, as 3600 for an hour, or Inf for no break.",
      call. = FALSE
    )
  }
  invisible(max_gap_s)
}

# The gap as an SQL number of seconds; Inf as DuckDB's infinity, which no silence exceeds
.rr_gap_sql <- function(max_gap_s) {
  if (is.finite(max_gap_s)) {
    format(max_gap_s, scientific = FALSE)
  } else {
    "'infinity'::DOUBLE"
  }
}

# A number of seconds in words for a page's text: whole hours, minutes or seconds where it is one, else hours
.rr_duration_text <- function(s) {
  unit <- function(n, word) {
    sprintf("%s %s%s", format(n), word, if (n == 1) "" else "s")
  }
  if (s %% 3600 == 0) {
    unit(s / 3600, "hour")
  } else if (s >= 3600) {
    unit(signif(s / 3600, 3), "hour")
  } else if (s %% 60 == 0) {
    unit(s / 60, "minute")
  } else {
    unit(signif(s, 3), "second")
  }
}

# A text whose runs of digits are padded with zeros, so it sorts in natural order: EX_2 before EX_10
.rr_natural_key <- function(x) {
  m <- gregexpr("[0-9]+", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(d) formatC(d, width = 20, flag = "0"))
  x
}

# A vessel's key as text, the same from the pings and from a lane's table: a whole number written without decimals,
# whether it is stored as an integer or a double
.rr_time_key <- function(v, type) {
  if (grepl("^(FLOAT|DOUBLE|REAL|DECIMAL)", type)) {
    sprintf(
      "CASE WHEN %1$s = trunc(%1$s) THEN CAST(CAST(%1$s AS HUGEINT) AS VARCHAR) ELSE CAST(%1$s AS VARCHAR) END",
      v
    )
  } else {
    sprintf("CAST(%s AS VARCHAR)", v)
  }
}

# One lane's intervals as SQL: rb_v, rb_lane (its number), rb_t0 and rb_t1 (seconds on the pings' clock), rb_label
.rr_time_lane <- function(
  lane,
  name,
  k,
  ctx,
  vessel,
  us,
  type_of,
  max_gap_s,
  what = sprintf("Lane %s", name)
) {
  con <- ctx$con
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  if (is.character(lane)) {
    if (length(lane) != 1 || !lane %in% ctx$cols) {
      stop(
        sprintf(
          "%s: no column %s in the pings.",
          what,
          paste(lane, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    # the runs of one value, each vessel's in time order, broken by a silence longer than max_gap_s as the page's
    # line is
    return(sprintf(
      "(WITH s AS (SELECT %1$s AS rb_v, %2$s / 1e6 AS t, CAST(%3$s AS VARCHAR) AS val FROM (%4$s) WHERE %5$s IS NOT NULL),
             w AS (SELECT *, CASE WHEN val IS DISTINCT FROM lag(val) OVER o OR t - lag(t) OVER o > %7$s THEN 1 ELSE 0 END AS chg
                   FROM s WINDOW o AS (PARTITION BY rb_v ORDER BY t)),
             g AS (SELECT *, sum(chg) OVER (PARTITION BY rb_v ORDER BY t ROWS UNBOUNDED PRECEDING) AS run FROM w)
        SELECT rb_v, %6$d AS rb_lane, min(t) AS rb_t0, max(t) AS rb_t1, any_value(val) AS rb_label
        FROM g WHERE val IS NOT NULL GROUP BY rb_v, run)",
      vessel,
      us(q(ctx$time), type_of(ctx$time)),
      q(lane),
      ctx$src,
      q(ctx$time),
      k,
      .rr_gap_sql(max_gap_s)
    ))
  }
  spec <- if (is.data.frame(lane) || inherits(lane, "tbl_lazy")) {
    list(data = lane)
  } else {
    lane
  }
  if (!is.list(spec) || is.null(spec$data)) {
    stop(
      sprintf(
        "%s: a column's name, a table, or list(data = , start = , end = ).",
        what
      ),
      call. = FALSE
    )
  }
  x <- dplyr::ungroup(.rr_as_lazy(spec$data, con))
  cols <- colnames(x)
  pairs <- list(c("start", "end"))
  se <- if (!is.null(spec$start) && !is.null(spec$end)) {
    c(spec$start, spec$end)
  } else {
    hit <- Filter(function(p) all(p %in% cols), pairs)
    if (!length(hit)) {
      stop(
        sprintf(
          "%s: no start and end; name their columns, list(data = , start = , end = ), or call them start and end.",
          what
        ),
        call. = FALSE
      )
    }
    hit[[1]]
  }
  need <- c(se, ctx$by, spec$label)
  if (length(setdiff(need, cols))) {
    stop(
      sprintf(
        "%s: no column %s in its table.",
        what,
        paste(setdiff(need, cols), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  src <- .rr_sql(x)
  types <- DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM (%s)", src))
  ty <- function(cc) types$column_type[match(cc, types$column_name)]
  if (!all(grepl("^(TIMESTAMP|DATE)", ty(se)))) {
    stop(
      sprintf("%s: %s and %s are date-times.", what, se[1], se[2]),
      call. = FALSE
    )
  }
  sprintf(
    "(SELECT %s AS rb_v, %d AS rb_lane, %s / 1e6 AS rb_t0, %s / 1e6 AS rb_t1, %s AS rb_label
      FROM (%s) WHERE %s IS NOT NULL AND %s IS NOT NULL)",
    if (is.null(ctx$by)) "'all'" else .rr_time_key(q(ctx$by), ty(ctx$by)),
    k,
    us(q(se[1]), ty(se[1])),
    us(q(se[2]), ty(se[2])),
    if (is.null(spec$label)) {
      "CAST(NULL AS VARCHAR)"
    } else {
      sprintf("CAST(%s AS VARCHAR)", q(spec$label))
    },
    src,
    q(se[1]),
    q(se[2])
  )
}

