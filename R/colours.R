# colours.R — a layer's colours from its data: a palette chosen by the column's kind and the
# number of its values, or the one the call names, turned into a MapLibre expression and a legend.
#
# Discrete values: Okabe-Ito to eight (black last, as it vanishes on a dark base map), Paired to twelve, Glasbey's 32
# beyond, the rest grey as "other"; an ordered factor takes viridis, in the order of its levels. Numbers, dates and
# times: viridis, over the 1st to 99th percentile by default. A call's `palette` overrides every default.

.rr_okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
.rr_paired <- c(
  "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
  "#FFFF99", "#B15928"
)
# Glasbey et al. (2007), 32 colours each far from the others, as pals::glasbey(32) gives them
.rr_glasbey <- c(
  "#0000FF", "#FF0000", "#00FF00", "#000033", "#FF00B6", "#005300", "#FFD300", "#009FFF", "#9A4D42", "#00FFBE",
  "#783FC1", "#1F9698", "#FFACFD", "#B1CC71", "#F1085C", "#FE8F42", "#DD00FF", "#201A01", "#720055", "#766C95",
  "#02AD24", "#C8FF00", "#886C00", "#FFB79F", "#858567", "#A10300", "#14F9FF", "#00479E", "#DC5E93", "#93D4FF",
  "#004CFF", "#F2F318"
)
.rr_viridis <- c("#440154", "#472D7B", "#3B528B", "#2C728E", "#21908C", "#27AD81", "#5DC863", "#AADC32", "#FDE725")
.rr_other <- "#999999" # Okabe-Ito's and Set 1's grey, for the values past a palette's colours

`%||%` <- function(a, b) if (is.null(a)) b else a

# A palette's name as written loosely: "Set 1", "set1", "set_1" and "Set-1" are one
.rr_pal_key <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# `n` colours of a palette given as a name, colours, or a function of n; NULL when `palette` is NULL
.rr_palette_colours <- function(palette, n, call = "palette") {
  if (is.null(palette)) {
    return(NULL)
  }
  if (is.function(palette)) {
    return(substr(palette(n), 1, 7))
  }
  if (!is.character(palette) || !length(palette)) {
    stop(sprintf("`%s` is a palette's name, colours, or a function of n.", call), call. = FALSE)
  }
  if (length(palette) > 1) {
    ok <- vapply(palette, .rr_is_colour, logical(1))
    if (!all(ok)) {
      stop(sprintf("`%s` holds what is not a colour: %s.", call, paste(palette[!ok], collapse = ", ")), call. = FALSE)
    }
    return(palette)
  }
  key <- .rr_pal_key(palette)
  own <- list(okabeito = .rr_okabe_ito, paired = .rr_paired, glasbey = .rr_glasbey, viridis = .rr_viridis)
  for (p in rr_palettes()) own[[.rr_pal_key(p$id)]] <- own[[.rr_pal_key(p$id)]] %||% rev(p$colors)
  if (!is.null(own[[key]])) {
    return(own[[key]])
  }
  pp <- grDevices::palette.pals()
  hit <- pp[.rr_pal_key(pp) == key]
  if (length(hit)) {
    return(unname(grDevices::palette.colors(NULL, hit[1])))
  }
  hp <- grDevices::hcl.pals()
  hit <- hp[.rr_pal_key(hp) == key]
  if (length(hit)) {
    return(grDevices::hcl.colors(max(n, 2), hit[1]))
  }
  stop(
    sprintf(
      "No palette called \"%s\": give colours, a function of n, or a name among rr_palettes(), palette.pals() and hcl.pals().",
      palette
    ),
    call. = FALSE
  )
}

.rr_is_colour <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) &&
    !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
}

# A colour as "#rrggbb" for the page
.rr_hex <- function(x) {
  v <- grDevices::col2rgb(x)
  grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255)
}

# The colours of a column with discrete values `vals` (by frequency, most first) and the palette for them: a call's
# palette (named colours taken by value), else by the number of values. Returns colours for the values that get one.
.rr_discrete_colours <- function(vals, palette = NULL, ordered = FALSE) {
  n <- length(vals)
  if (!is.null(names(palette)) && all(nzchar(names(palette)))) {
    hit <- palette[match(vals, names(palette))]
    keep <- !is.na(hit)
    return(list(values = vals[keep], colours = vapply(unname(hit[keep]), .rr_hex, "", USE.NAMES = FALSE)))
  }
  pal <- if (!is.null(palette)) {
    .rr_palette_colours(palette, n)
  } else if (ordered) {
    grDevices::colorRampPalette(.rr_viridis)(max(n, 2))[seq_len(n)]
  } else if (n <= 8) {
    .rr_okabe_ito
  } else if (n <= 12) {
    .rr_paired
  } else {
    .rr_glasbey
  }
  if (ordered && !is.null(palette) && length(pal) < n) {
    pal <- grDevices::colorRampPalette(pal)(n)
  }
  k <- min(n, length(pal))
  list(values = vals[seq_len(k)], colours = vapply(pal[seq_len(k)], .rr_hex, "", USE.NAMES = FALSE))
}

# A column's colours and legend, read from the data on `con`: list(colour = a MapLibre expression, legend, kind).
# `src` holds `column` as the tiles carry it, the property `prop`: text for discrete values, a number for continuous
# ones (a date or a time as its seconds); `type` is the original column's (.rr_column_type()).
.rr_colour_by <- function(con, src, column, prop, palette = NULL, limits = NULL, stretch = NULL, type = NULL) {
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  if (is.null(type)) {
    type <- .rr_column_type(con, src, column)
  }
  if (type$kind == "discrete") {
    vals <- if (!is.null(type$levels)) {
      type$levels
    } else {
      DBI::dbGetQuery(con, sprintf(
        "SELECT CAST(%1$s AS VARCHAR) AS v, count(*) AS n FROM (%2$s) WHERE %1$s IS NOT NULL GROUP BY 1 ORDER BY n DESC, v",
        q(column), src
      ))$v
    }
    dc <- .rr_discrete_colours(vals, palette, ordered = isTRUE(type$ordered))
    rest <- length(vals) > length(dc$values)
    legend <- lapply(seq_along(dc$values), function(j) list(swatch = list(dot = dc$colours[j]), label = dc$values[j]))
    if (rest) {
      legend <- c(legend, list(list(swatch = list(dot = .rr_other), label = "other")))
    }
    colour <- c(
      list("match", list("get", prop)),
      unlist(lapply(seq_along(dc$values), function(j) list(dc$values[j], dc$colours[j])), recursive = FALSE),
      list(.rr_other)
    )
    return(list(colour = colour, legend = legend, kind = "discrete", n = length(vals), shown = length(dc$values)))
  }
  # continuous: over the limits, the 1st to 99th percentile by default, stretched linearly, on a log scale, or in
  # equal shares of the values
  stretch <- match.arg(stretch %||% "linear", c("linear", "log", "equal"))
  v <- DBI::dbGetQuery(con, sprintf(
    "SELECT quantile_cont(%1$s, [0.01, 0.99]) AS lim, quantile_cont(%1$s, [%3$s]) AS qs FROM (%2$s) WHERE %1$s IS NOT NULL",
    q(column), src, paste(seq(0.01, 0.99, length.out = 9), collapse = ", ")
  ))
  lim <- if (!is.null(limits)) {
    if (!is.numeric(limits) || length(limits) != 2 || anyNA(limits) || limits[1] >= limits[2]) {
      stop("`limits` is two numbers, the lower first.", call. = FALSE)
    }
    limits
  } else {
    unlist(v$lim)
  }
  if (length(lim) != 2 || anyNA(lim)) {
    lim <- c(0, 1)
  }
  if (!(lim[2] > lim[1])) {
    lim[2] <- lim[1] + 1
  }
  pal <- .rr_palette_colours(palette, 9) %||% .rr_viridis
  pal <- vapply(grDevices::colorRampPalette(pal)(9), .rr_hex, "", USE.NAMES = FALSE)
  stops <- switch(
    stretch,
    linear = seq(lim[1], lim[2], length.out = 9),
    log = {
      if (lim[1] <= 0) stop("A log stretch needs values above 0: give `limits` above 0.", call. = FALSE)
      exp(seq(log(lim[1]), log(lim[2]), length.out = 9))
    },
    equal = {
      s <- pmin(pmax(unlist(v$qs), lim[1]), lim[2])
      s <- cummax(s)
      s + seq(0, 1e-9, length.out = 9) # rising, as MapLibre's interpolate asks
    }
  )
  fmt <- function(x) format(signif(x, 3), big.mark = ",", trim = TRUE, scientific = FALSE)
  colour <- list(
    "case", list("has", prop),
    c(list("interpolate", list("linear"), list("get", prop)),
      unlist(lapply(1:9, function(j) list(stops[j], pal[j])), recursive = FALSE)),
    .rr_other
  )
  legend <- lapply(c(1, 3, 5, 7, 9), function(j) {
    list(swatch = list(dot = pal[j]),
         label = paste0(if (j == 1) paste0(intToUtf8(8804), " ") else if (j == 9) paste0(intToUtf8(8805), " ") else "",
                        type$format(stops[j]) %||% fmt(stops[j])))
  })
  list(colour = colour, legend = legend, kind = "continuous", limits = lim, stretch = stretch)
}

# What a column is, for its colours: discrete (text, a logical, a factor, its levels when it has them, ordered or not)
# or continuous (a number; a date or a time, coloured by its seconds and labelled as a date or a time)
# What a data dictionary's type (data-dict.tidyverse.org's) says a column is for choosing and colouring: "list" for an
# id, a code, text or a logical (`number(id)`, `enum`, `string`, `boolean`), "range" for a measure, a date or a time
# (`number`, `number(quantity)`, `date`, `datetime`, `time`), "" where it says neither or there is none
.rr_declared <- function(type) {
  if (!is.character(type) || length(type) != 1 || is.na(type)) {
    return("")
  }
  if (grepl("\\(id\\)|^enum|^string|^boolean", type)) {
    "list"
  } else if (grepl("^number(\\(quantity\\))?$|^date|^time", type)) {
    "range"
  } else {
    ""
  }
}

# A category's text in SQL: a number written whole where it is whole (an id stored as a double, 1131 not 1131.0)
.rr_category_sql <- function(col, type) {
  if (isTRUE(type$numeric)) {
    sprintf("CASE WHEN %1$s = round(%1$s) THEN CAST(CAST(%1$s AS BIGINT) AS VARCHAR) ELSE CAST(%1$s AS VARCHAR) END", col)
  } else {
    sprintf("CAST(%s AS VARCHAR)", col)
  }
}

.rr_column_type <- function(con, src, column, ordered = FALSE, declared = NULL) {
  q <- as.character(DBI::dbQuoteIdentifier(con, column))
  one <- DBI::dbGetQuery(con, sprintf("SELECT %s AS v FROM (%s) LIMIT 0", q, src))$v
  # a number the data's dictionary calls an id or a code (data-dict's `number(id)`, `enum`): a category
  if (is.numeric(one) && .rr_declared(declared) == "list") {
    return(list(kind = "discrete", numeric = TRUE))
  }
  if (is.factor(one)) {
    return(list(kind = "discrete", levels = levels(one), ordered = ordered || is.ordered(one)))
  }
  if (inherits(one, "POSIXct")) {
    return(list(kind = "continuous", time = "timestamp",
                format = function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M")))
  }
  if (inherits(one, "Date")) {
    return(list(kind = "continuous", time = "date",
                format = function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")))
  }
  if (is.numeric(one)) {
    return(list(kind = "continuous", format = function(x) NULL))
  }
  list(kind = "discrete")
}
