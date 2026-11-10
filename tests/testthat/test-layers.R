# The layer functions (layers.R) and their colours (colours.R): the pipe, the vocabulary,
# the colours by the data and by the call, and the one order of arguments.

layer <- function(m, i = 1) m$config$layers[[i]]
paint <- function(m, i = 1) layer(m, i)$style[[1]]$paint

test_that("every layer function takes its arguments in the plan's one order", {
  order <- c(
    "map", "data", "lon", "lat", "lon_end", "lat_end", "group", "time", "max_gap_s", "elevation", "of", "cell", "fun",
    "colour", "fill", "size", "linewidth", "opacity", "label",
    "palette", "limits", "breaks", "stretch", "legend",
    "popup", "labels", "codes", "hover", "choose",
    "name", "info", "section", "visible",
    "minzoom", "maxzoom", "..."
  )
  for (f in c("rr_point", "rr_line", "rr_segment", "rr_polygon", "rr_text", "rr_path")) {
    args <- names(formals(get(f)))
    expect_true(all(args %in% order), info = f)
    expect_equal(args, intersect(order, args), info = f)
  }
})

test_that("a point layer starts a map from data, or adds to one, in sections", {
  skip_if_no_spatial()
  d <- rr_example("trawlsurvey")
  m <- rr_point(d)
  expect_s3_class(m, "rr_map")
  expect_equal(layer(m)$id, "points")
  expect_equal(paint(m)[["circle-color"]], "#E69F00") # Okabe-Ito's first, for one colour
  # from rr_map(data), then a second layer in a section of its own; the map's data are the layers'
  m2 <- rr_map(d, title = "Survey") |>
    rr_point(colour = behaviour) |>
    rr_point(colour = speed, name = "Speed", section = "More")
  expect_equal(vapply(m2$config$layers, `[[`, "", "id"), c("points", "speed"))
  expect_equal(vapply(m2$config$groups, `[[`, "", "title"), c("Layers", "More"))
  expect_equal(m2$config$title, "Survey")
  # a map started from data alone prints as one with no layers yet
  expect_match(format(rr_map(d))[2], "no layers yet", fixed = TRUE)
  # a parquet file's path is data too
  f <- system.file("extdata", "trawlsurvey.parquet", package = "rebelrebel")
  expect_equal(attr(layer(rr_point(f))$tiles, "count"), NULL) # the count is the tile function's, not the config's
  expect_s3_class(rr_point(f), "rr_map")
  expect_error(rr_point(d, data = d), "once")
  expect_error(rr_point(d, nonsense = 1), "no argument nonsense")
})

test_that("a bare name is a column, a string is a value", {
  skip_if_no_spatial()
  d <- rr_example("trawlsurvey")
  expect_equal(paint(rr_point(d, colour = "#336699"))[["circle-color"]], "#336699")
  expect_equal(paint(rr_point(d, color = "red"))[["circle-color"]], "#FF0000")
  expect_error(rr_point(d, colour = "behaviour"), "write it bare")
  expect_error(rr_point(d, colour = "nonsense"), "not a colour")
  expect_error(rr_point(d, colour = nonsense), "colour")
  m <- rr_point(d, colour = c(behaviour, speed))
  expect_equal(vapply(layer(m)$variants$options, `[[`, "", "value"), c("behaviour", "speed"))
})

test_that("colours by the data: Okabe-Ito to eight values, Paired to twelve, Glasbey beyond, viridis for numbers", {
  skip_if_no_spatial()
  con <- .rr_connection()
  vals <- function(n) data.frame(lon = seq_len(n) / 100, lat = 60, k = sprintf("v%02d", seq_len(n)))
  cols_of <- function(m) {
    e <- paint(m)[["circle-color"]]
    unlist(e[seq(4, length(e) - 1, by = 2)])
  }
  expect_equal(cols_of(rr_point(vals(8), colour = k)), .rr_okabe_ito)
  expect_equal(cols_of(rr_point(vals(12), colour = k)), .rr_paired)
  m <- rr_point(vals(40), colour = k)
  expect_equal(cols_of(m), .rr_glasbey)
  expect_equal(utils::tail(layer(m)$legend, 1)[[1]]$label, "other") # the 8 past Glasbey's 32
  # the most frequent value takes the first colour
  d <- data.frame(lon = 1:5 / 100, lat = 60, k = c("rare", "common", "common", "common", "mid"))
  d$k[5] <- "common"
  expect_equal(paint(rr_point(d, colour = k))[["circle-color"]][[3]], "common")
  # numbers: viridis over the 1st to 99th percentile, or the limits given
  n <- data.frame(lon = 1:101 / 100, lat = 60, v = 0:100)
  e <- paint(rr_point(n, colour = v))[["circle-color"]]
  expect_equal(e[[1]], "case")
  stops <- e[[3]]
  expect_equal(stops[[4]], 1)
  expect_equal(stops[[5]], .rr_viridis[1])
  expect_equal(stops[[length(stops) - 1]], 99)
  e2 <- paint(rr_point(n, colour = v, limits = c(10, 50)))[["circle-color"]][[3]]
  expect_equal(c(e2[[4]], e2[[length(e2) - 1]]), c(10, 50))
  expect_error(rr_point(n, colour = v, limits = c(5, 1)), "lower first")
  e3 <- paint(rr_point(n, colour = v, limits = c(1, 100), stretch = "log"))[["circle-color"]][[3]]
  expect_equal(e3[[6]], exp(log(100) / 8), tolerance = 1e-9)
  # an ordered factor: viridis in the order of its levels
  o <- data.frame(lon = 1:3 / 100, lat = 60, size = factor(c("small", "large", "medium"), c("small", "medium", "large"), ordered = TRUE))
  e4 <- paint(rr_point(o, colour = size))[["circle-color"]]
  expect_equal(unlist(e4[c(3, 5, 7)]), c("small", "medium", "large"))
  expect_equal(e4[[4]], .rr_viridis[1])
})

test_that("a call's palette overrides the default: a name written loosely, colours, colours by value, a function", {
  skip_if_no_spatial()
  d <- data.frame(lon = 1:3 / 100, lat = 60, k = c("a", "b", "c"))
  first <- function(m) paint(m)[["circle-color"]][[4]]
  expect_equal(first(rr_point(d, colour = k, palette = "set1")), "#E41A1C")
  expect_equal(first(rr_point(d, colour = k, palette = "Set 1")), "#E41A1C")
  expect_equal(first(rr_point(d, colour = k, palette = c("#111111", "#222222", "#333333"))), "#111111")
  m <- rr_point(d, colour = k, palette = c(b = "#00FF00"))
  e <- paint(m)[["circle-color"]]
  expect_equal(e[[3]], "b")
  expect_equal(e[[4]], "#00FF00")
  expect_equal(e[[length(e)]], .rr_other) # the rest grey
  expect_equal(first(rr_point(d, colour = k, palette = function(n) rep("#ABCDEF", n))), "#ABCDEF")
  expect_error(rr_point(d, colour = k, palette = "no such palette"), "No palette called")
})

test_that("popup, hover and the sidebar", {
  skip_if_no_spatial()
  d <- rr_example("trawlsurvey")
  m <- rr_point(d, popup = c(vid, speed), hover = vid, name = "Survey pings", info = "Each ping.", visible = FALSE,
                minzoom = 6)
  l <- layer(m)
  expect_equal(l$id, "survey_pings")
  expect_equal(l$label, "Survey pings")
  expect_equal(l$info, "Each ping.")
  expect_false(l$visible)
  expect_equal(l$minzoom, 6)
  expect_equal(l$popup$title, "vid")
  expect_equal(unlist(l$popup$hover), "survey_pings-dot")
  expect_true(l$popup$row)
  expect_equal(vapply(m$layers$survey_pings$row(1)$cols, `[[`, "", "name"), c("vid", "speed"))
  # every column by default, none with FALSE
  expect_setequal(
    vapply(rr_point(d)$layers$points$row(1)$cols, `[[`, "", "name"),
    c("vid", "speed", "heading", "time", "behaviour")
  )
  expect_null(layer(rr_point(d, popup = FALSE))$popup)
})

test_that("rr_map(data, choose = ) names the columns the reader picks", {
  skip_if_no_spatial()
  m <- rr_map(rr_example("trawlsurvey"), choose = vid) |> rr_point(colour = behaviour)
  expect_equal(vapply(m$config$choose$fields, `[[`, "", "field"), "vid")
  expect_equal(m$config$choose$fields[[1]]$layers, list("points"))
  # a grouping is not read
  g <- dplyr::group_by(dplyr::collect(rr_example("trawlsurvey")), vid)
  expect_null(rr_point(g)$config$choose)
})

test_that("lines, segments, areas and text: their style, legend and data", {
  skip_if_no_spatial()
  m <- rr_map(rr_example("trawlsurveytows")) |> rr_line(name = "Tows", hover = id.lgs, linewidth = 3)
  l <- layer(m)
  expect_equal(l$style[[1]]$type, "line")
  expect_equal(l$style[[1]]$paint[["line-width"]], 3)
  expect_equal(unlist(l$popup$hover), "tows-line")
  expect_equal(l$legend[[1]]$swatch, list(fill = "#E69F00", stroke = "#E69F00"))
  expect_error(rr_line(rr_example("trawlsurvey")), "geometry column")
  # areas: the fill by a column or a colour, the outline one colour
  h <- rr_polygon(rr_example("dansk_harbours"), fill = "#d9822b", colour = "black", popup = SI_HARB)
  st <- layer(h)$style
  expect_equal(vapply(st, `[[`, "", "type"), c("fill", "line"))
  expect_equal(st[[2]]$paint[["line-color"]], "#000000")
  expect_error(rr_polygon(rr_example("dansk_harbours"), colour = SI_HARB), "outline")
  # an sf object's areas
  sfp <- sf::st_as_sf(
    data.frame(id = c("a", "b"), wkt = c("POLYGON((-22 64, -21 64, -21 64.5, -22 64))", "POLYGON((-20 64, -19 64, -19 64.5, -20 64))")),
    wkt = "wkt", crs = 4326
  )
  p <- rr_polygon(sfp, fill = id)
  expect_equal(vapply(layer(p)$legend, `[[`, "", "label"), c("a", "b"))
  expect_equal(attr(p$layers[[1]]$tiles, "count"), 2L)
  # a segment from each row's start to its end
  seg <- data.frame(lon = c(-22, -21), lat = c(64, 64.2), lon2 = c(-21.9, -20.8), lat2 = c(64.05, 64.3))
  s <- rr_segment(seg, lon_end = lon2, lat_end = lat2)
  expect_equal(layer(s)$style[[1]]$type, "line")
  expect_equal(attr(s$layers[[1]]$tiles, "count"), 2L)
  expect_error(rr_segment(seg, lon_end = nope, lat_end = lat2), "No column nope")
  # text: a column's values, with a halo
  tx <- rr_text(rr_example("trawlsurvey"), label = vid, size = 15)
  ts <- layer(tx)$style[[1]]
  expect_equal(ts$type, "symbol")
  expect_equal(ts$layout[["text-field"]], list("to-string", list("get", "vid")))
  expect_equal(ts$layout[["text-size"]], 15)
  expect_true(ts$paint[["text-halo-width"]] > 0)
  expect_error(rr_text(rr_example("trawlsurvey")), "label")
})

test_that("paths: rows joined in time, a line per group, broken at silences and where the colour's value changes", {
  skip_if_no_spatial()
  d <- rr_example("dansk")
  m <- rr_map(d) |> rr_path(group = vessel_id, time = time_stamp, colour = behaviour, max_gap_s = 3600)
  l <- layer(m)
  expect_equal(l$style[[1]]$type, "line")
  expect_match(l$info, "each vessel_id joined in order of time_stamp, broken where two are more than 1 hour apart")
  # the lines of ramb's track on the same rows, as counted when the two shared the query (2026-11-10)
  expect_equal(attr(m$layers[[1]]$tiles, "count"), 1209L)
  # several columns make a line's group
  t <- dplyr::mutate(dplyr::collect(rr_example("trawlsurvey")), day = format(time, "%Y-%m-%d"))
  n_vid <- attr(rr_path(t, group = vid, time = time)$layers[[1]]$tiles, "count")
  n_day <- attr(rr_path(t, group = c(vid, day), time = time)$layers[[1]]$tiles, "count")
  expect_equal(n_vid, 4L)
  expect_gt(n_day, n_vid)
  expect_error(rr_path(d, group = vessel_id), "`time`")
  expect_error(rr_path(d, time = nope), "no column nope")
  expect_error(rr_path(rr_example("trawlsurveytows"), time = id.lgs), "rr_line")
  expect_error(rr_path(d, time = time_stamp, colour = c(behaviour, speed)), "one column")
})

test_that("a click shows the row: read from the server by its number, or from a bundle's rows file", {
  skip_if_no_spatial()
  d <- data.frame(
    lon = c(-22, -21.5), lat = c(64, 64.2), station = c("A1", "B2"), depth = c(120.25, NA),
    when = as.POSIXct(c("2019-03-01 10:00:00", "2019-03-02 11:30:00"), tz = "UTC"), ok = c(TRUE, FALSE),
    photo = c("https://example.org/a.jpg", NA), n = c(3L, 4L)
  )
  m <- rr_point(d, popup = c(station, depth, when, ok, photo, n))
  l <- layer(m)
  expect_true(l$popup$row)
  expect_equal(l$popup$title, "station")
  expect_null(l$popup$fields)
  # the tiles carry the number and the title, not the other columns
  expect_true(is.function(m$layers$points$row))
  app <- .rr_map_app(m)
  get <- function(path) app$call(list(REQUEST_METHOD = "GET", PATH_INFO = path, QUERY_STRING = ""))
  r <- get("/row/points/2")
  expect_equal(r$status, 200L)
  v <- jsonlite::fromJSON(r$body, simplifyVector = FALSE)$columns
  expect_equal(vapply(v, `[[`, "", "name"), c("station", "depth", "when", "ok", "photo", "n"))
  expect_equal(vapply(v, `[[`, "", "type"), c("text", "number", "time", "logical", "text", "integer"))
  expect_equal(v[[1]]$value, "B2")
  expect_null(v[[2]]$value) # missing
  expect_equal(v[[3]]$value, "2019-03-02 11:30:00")
  expect_false(v[[4]]$value)
  expect_equal(get("/row/points/9")$status, 404L)
  expect_equal(get("/row/nope/1")$status, 404L)
  # no popup, no rows
  expect_null(rr_point(d, popup = FALSE)$layers$points$row)
  # a bundle writes the rows beside its tiles
  dir <- tempfile()
  rr_bundle(m, dir, maxzoom = 2)
  js <- readLines(file.path(dir, "data", "points_rows.js"), encoding = "UTF-8")
  expect_match(js[1], '^RB_BUNDLE\\("data/points_rows",')
  b <- jsonlite::fromJSON(sub('^RB_BUNDLE\\("data/points_rows",(.*)\\);$', "\\1", paste(js, collapse = "")), simplifyVector = FALSE)
  expect_equal(b$rows[["1"]][[1]], "A1")
  expect_equal(b$rows[["1"]][[5]], "https://example.org/a.jpg")
  # the page has the table's code and texts
  page <- .rr_map_page("{}")
  expect_match(page, "function rowHtml(cols)", fixed = TRUE)
  expect_match(page, "fetch(`/row/${l.id}/${p.rr_row}`)", fixed = TRUE)
})

test_that("a click's row: names, units and descriptions from the data's dictionary or the call, codes with their labels", {
  skip_if_no_spatial()
  # a parquet file's own dictionary: the survey's speed in knots, its time described
  m <- rr_point(rr_example("trawlsurvey"), popup = c(vid, speed, time))
  cols <- m$layers$points$row(1)$cols
  speed <- Filter(function(x) x$name == "speed", cols)[[1]]
  expect_equal(speed$unit, "knots")
  expect_match(speed$description, "Speed over ground")
  expect_match(Filter(function(x) x$name == "time", cols)[[1]]$description, "UTC")
  # a data frame's column attributes, and the call's labels and codes over them
  d <- data.frame(lon = c(-22, -21), lat = c(64, 64.1), gear = c(6L, 9L), depth = c(120, 85))
  attr(d$depth, "label") <- "Bottom depth"
  attr(d$depth, "units") <- "m"
  m2 <- rr_point(d, popup = c(gear, depth), codes = list(gear = data.frame(code = c(6, 9), label = c("bottom trawl", "longline"))))
  app <- .rr_map_app(m2)
  v <- jsonlite::fromJSON(app$call(list(REQUEST_METHOD = "GET", PATH_INFO = "/row/points/2", QUERY_STRING = ""))$body,
                          simplifyVector = FALSE)$columns
  expect_equal(v[[1]]$code, "longline")
  expect_equal(c(v[[2]]$label, v[[2]]$unit), c("Bottom depth", "m"))
  m3 <- rr_point(d, popup = c(gear, depth), labels = c(depth = "Depth of the haul (m)"), codes = list(gear = c("6" = "bottom trawl")))
  r3 <- m3$layers$points$row(1)$cols
  expect_equal(r3[[2]]$label, "Depth of the haul (m)")
  expect_null(r3[[2]]$unit) # the call's label says it all
  expect_equal(r3[[1]]$codes[["6"]], "bottom trawl")
  expect_error(rr_point(d, labels = "Depth"), "named character")
  expect_error(rr_point(d, codes = c(gear = "x")), "named list")
  # a path's popup fields take the labels and the codes as the field lookup
  p <- data.frame(lon = c(-22, -21.9, -21.8), lat = 64, t = 1:3, boat = c(1L, 1L, 1L))
  attr(p$boat, "label") <- "Boat"
  f <- layer(rr_path(p, group = boat, time = t, popup = boat, hover = NULL, codes = list(boat = c("1" = "Bjarni"))))$popup
  expect_equal(f$title, "boat")
  # the page shows the label, the unit and the code
  page <- .rr_map_page("{}")
  expect_match(page, "esc(c.label ?? c.name)", fixed = TRUE)
  expect_match(page, "c.code ?", fixed = TRUE)
})
