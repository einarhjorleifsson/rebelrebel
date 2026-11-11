# The layer functions (layers.R) and their colours (colours.R): the pipe, the vocabulary,
# the colours by the data and by the call, and the one order of arguments.

layer <- function(m, i = 1) m$config$layers[[i]]
paint <- function(m, i = 1) layer(m, i)$style[[1]]$paint

test_that("every layer function takes its arguments in the plan's one order", {
  order <- c(
    "map", "data", "lon", "lat", "lon_end", "lat_end", "group", "time", "max_gap_s", "elevation", "of", "cell", "fun",
    "colour", "fill", "size", "linewidth", "opacity", "whole", "thin", "label",
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

test_that("two lists are linked by the combinations the data hold, with their rows", {
  skip_if_no_spatial()
  x <- rr_example("trawlsurvey") |> dplyr::mutate(day = as.character(as.Date(time)))
  ch <- (rr_map(x, choose = c(vid, day)) |> rr_point())$config$choose
  rows <- dplyr::pull(dplyr::count(x), n)
  # each option's value alone, for the page's counts as the lists narrow
  expect_equal(ch$fields[[1]]$options[[1]]$text, ch$fields[[1]]$options[[1]]$value)
  # every combination once, its rows summing to the table's
  keys <- vapply(ch$extents, `[[`, "", "key")
  expect_false(anyDuplicated(keys) > 0)
  expect_equal(sum(vapply(ch$extents, `[[`, 0, "n")), rows)
  # a vessel's combinations are its days
  truth <- dplyr::collect(dplyr::distinct(dplyr::filter(x, vid == 1131), day))$day
  mine <- sub("^1131\u001f", "", grep("^1131\u001f", keys, value = TRUE))
  expect_setequal(mine, truth)
})

test_that("an id or a code is a list, a measure a range: as declared, else as stored", {
  skip_if_no_spatial()
  kind <- function(m) vapply(m$config$choose$fields, function(f) .rr_or(f$range, "list"), "")
  # the survey's vid: an integer, and its dictionary's number(id)
  m <- rr_map(rr_example("trawlsurvey"), choose = c(vid, speed)) |> rr_point()
  expect_equal(kind(m), c("list", "number"))
  expect_equal(m$config$choose$fields[[1]]$options[[1]]$label, "1131 (2,164)")
  # stored: a whole number a list, a fraction a range
  d <- data.frame(id = 1:3, n = c(2.5, 3, 4), lon = c(-20, -21, -22), lat = c(64, 65, 66))
  expect_equal(kind(rr_map(d, choose = c(id, n)) |> rr_point()), c("list", "number"))
  # a double the file's dictionary declares an id: a list, written whole, and coloured as categories
  f <- tempfile(fileext = ".parquet")
  con <- DBI::dbConnect(duckdb::duckdb())
  dict <- "columns:\n  - name: id\n    type: number(id)\n  - name: n\n    type: number(quantity)\n"
  DBI::dbExecute(con, sprintf(
    "COPY (SELECT CAST(i AS DOUBLE) AS id, i * 1.5 AS n, -20 - i AS lon, 64 + i / 10 AS lat FROM range(1, 4) t(i))
     TO '%s' (FORMAT parquet, KV_METADATA {'x.data_dict': '%s'})", f, dict))
  DBI::dbDisconnect(con, shutdown = TRUE)
  m <- rr_map(f, choose = c(id, n)) |> rr_point(colour = id)
  expect_equal(kind(m), c("list", "number"))
  expect_equal(vapply(m$config$choose$fields[[1]]$options, `[[`, "", "value"), c("1", "2", "3"))
  expect_equal(paint(m)[["circle-color"]][[1]], "match")
  expect_true(all(c("1", "2", "3") %in% unlist(paint(m)[["circle-color"]])))
})

test_that("a number, a date or a time is a range of bins, linked as the lists are", {
  skip_if_no_spatial()
  x <- rr_example("trawlsurvey") |> dplyr::mutate(day = as.Date(time))
  rows <- dplyr::pull(dplyr::count(x), n)
  ch <- (rr_map(x, choose = c(vid, day, speed, time)) |> rr_point())$config$choose
  f <- stats::setNames(ch$fields, vapply(ch$fields, `[[`, "", "field"))
  expect_null(f$vid$range)
  expect_equal(c(f$day$range, f$speed$range, f$time$range), c("date", "number", "time"))
  # 22 days, each a bin of its own; speed and time in bins between pretty breaks, end to end
  expect_true(f$day$closed)
  expect_length(f$day$bins, 22)
  expect_equal(f$day$bins[[1]]$lo, "2019-02-27")
  expect_false(f$speed$closed)
  lo <- vapply(f$speed$bins, `[[`, 0, "lo")
  hi <- vapply(f$speed$bins, `[[`, 0, "hi")
  expect_equal(lo[-1], hi[-length(hi)])
  sp <- dplyr::collect(dplyr::select(x, speed))$speed
  expect_true(lo[1] <= min(sp) && hi[length(hi)] >= max(sp))
  # each bin's rows are the rows its ends take, the last bin closed
  n <- vapply(f$speed$bins, `[[`, 0, "n")
  took <- vapply(seq_along(lo), function(j) sum(sp >= lo[j] & (if (j == length(lo)) sp <= hi[j] else sp < hi[j])), 0)
  expect_equal(n, took)
  for (g in f[c("day", "speed", "time")]) expect_equal(sum(vapply(g$bins, `[[`, 0, "n")), rows)
  # the combinations carry a range by its bin
  parts <- strsplit(ch$extents[[1]]$key, "\u001f")[[1]]
  expect_length(parts, 4)
  expect_match(parts[2:4], "^[0-9]+$")
  expect_equal(sum(vapply(ch$extents, `[[`, 0, "n")), rows)
  # a path is split by a range's bins, each run carrying its bin's start, which the page's range compares
  m <- rr_map(x, choose = c(vid, speed)) |> rr_path(group = vid, time = time)
  tb <- attr(m, "tables")[[1]]
  starts <- DBI::dbGetQuery(tb$con, sprintf("SELECT DISTINCT speed FROM %s", tb$name))$speed
  expect_true(all(starts %in% lo))
  expect_gt(length(starts), 10)
})

test_that("a layer's own choose: its lists, over it alone", {
  skip_if_no_spatial()
  x <- rr_example("trawlsurvey") |> dplyr::mutate(day = as.character(as.Date(time)))
  m <- rr_map(x) |> rr_path(group = vid, time = time, choose = day) |> rr_point(choose = c(vid, day))
  expect_null(m$config$choose)
  ls <- m$config$layers
  expect_equal(vapply(ls[[1]]$choose$fields, `[[`, "", "field"), "day")
  expect_length(ls[[1]]$choose$extents, 0) # one list: nothing to link
  expect_equal(vapply(ls[[2]]$choose$fields, `[[`, "", "field"), c("vid", "day"))
  expect_equal(sum(vapply(ls[[2]]$choose$extents, `[[`, 0, "n")), dplyr::pull(dplyr::count(x), n))
  # the paths are split by the chosen day, so a day shows whole lines of it
  tb <- attr(m, "tables")[[1]]
  expect_true("day" %in% DBI::dbListFields(tb$con, tb$name))
  # shapes: each value's extent from its shapes, a list's and a range's
  tows <- rr_example("trawlsurveytows") |> dplyr::mutate(tow = as.character(as.integer(id.lgs)))
  o <- (tows |> rr_line(choose = tow))$config$layers[[1]]$choose$fields[[1]]$options
  expect_length(o, 581)
  expect_true(all(lengths(lapply(o, `[[`, "bbox")) == 4))
  r <- (tows |> dplyr::mutate(half = id.lgs / 2) |> rr_line(choose = half))$config$layers[[1]]$choose$fields[[1]]
  expect_equal(r$range, "number")
  expect_true(all(lengths(lapply(Filter(function(b) b$n > 0, r$bins), `[[`, "bbox")) == 4))
  expect_error(rr_point(x, choose = nope), "No column nope")
})

test_that("points thinned in a tile: one a pixel and class, and at the deepest zoom one a spot", {
  skip_if_no_spatial()
  x <- dplyr::mutate(rr_example("dansk"), colour = behaviour)
  whole <- rr_tiles_table(x, lon = "lon", lat = "lat", properties = "colour")
  one <- rr_tiles_table(x, lon = "lon", lat = "lat", properties = "colour", thin = list(px = 1, until = 14))
  by <- rr_tiles_table(x, lon = "lon", lat = "lat", properties = "colour", thin = list(px = 1, by = "colour", until = 14))
  n <- function(f, z, x, y) unname(mvt_count(f(z, x, y))["features"])
  # the busiest tile at zoom 6: 140,734 points in it, 141,036 with its margin
  # (each tile holds the features within 64 of its 4,096 units past its edge); 3,528 a pixel and colour
  expect_equal(n(whole, 6, 33, 19), 141036)
  expect_equal(n(by, 6, 33, 19), 3528)
  expect_lt(n(one, 6, 33, 19), n(by, 6, 33, 19)) # each colour keeps its own
  expect_identical(by(6, 33, 19), by(6, 33, 19)) # the same points every time
  # at the deepest zoom, a harbour's 14,579 points (with the margin) on 857 spots of the tile's own grid
  expect_equal(n(whole, 14, 8712, 4910), 14579)
  expect_equal(n(by, 14, 8712, 4910), 857)
  expect_error(rr_tiles_table(x, lon = "lon", lat = "lat", thin = list(px = 1)), "`thin` is a list")
})

test_that("whole: a layer sent to the page as one GeoJSON file", {
  skip_if_no_spatial()
  m <- rr_point(rr_example("trawlsurvey"), colour = behaviour, popup = c(vid, speed), whole = TRUE)
  l <- layer(m)
  expect_equal(l$kind, "geojson")
  expect_equal(l$features, 8918) # its count, so the page need not read the file
  expect_null(l$style[[1]][["source-layer"]]) # a GeoJSON source has none
  g <- jsonlite::fromJSON(m$layers[[1]]$data(), simplifyVector = FALSE)
  expect_length(g$features, 8918)
  # the properties the tiles would carry, and coordinates to six decimals
  t <- rr_point(rr_example("trawlsurvey"), colour = behaviour, popup = c(vid, speed), whole = FALSE)
  expect_setequal(names(g$features[[1]]$properties), c("rr_c1", "rr_row", "vid", "rr_t"))
  expect_true(all(abs(unlist(g$features[[1]]$geometry$coordinates) * 1e6 - round(unlist(g$features[[1]]$geometry$coordinates) * 1e6)) < 1e-6))
  expect_equal(layer(t)$kind, "vector")
  # paths, lines and areas too
  p <- rr_map(rr_example("trawlsurvey")) |> rr_path(group = vid, time = time, whole = TRUE)
  expect_equal(layer(p)$kind, "geojson")
  expect_equal(attr(p$layers[[1]]$data, "count"), layer(p)$features)
  h <- rr_polygon(rr_example("dansk_harbours"), whole = TRUE)
  expect_equal(layer(h)$features, 942)
  # a bundle writes the file, and no tiles for the layer
  dir <- tempfile()
  suppressMessages(rr_bundle(m, dir))
  expect_true(file.exists(file.path(dir, "data", paste0(l$id, ".js"))))
  expect_false(dir.exists(file.path(dir, "tiles", l$id)))
  unlink(dir, recursive = TRUE)
})

test_that("a tile holds the features in its margin: a point just past its edge is drawn by one tile", {
  skip_if_no_spatial()
  # 61 m north of a zoom-5 tile edge (66.51326 N): the tile above rounds it onto its bottom edge, where MapLibre does
  # not draw a point, and the tile below has it in its margin, within its extent once rounded
  d <- data.frame(lon = -21.56479, lat = 66.51381)
  tl <- rr_tiles_table(d, lon = "lon", lat = "lat")
  n <- function(z, x, y) sum(mvt_count(tl(z, x, y)))
  expect_equal(n(5, 14, 7), 1)
  expect_equal(n(5, 14, 8), 1)
  expect_true(any(attr(tl, "tiles_at")(5)$y == 8)) # a bundle writes the tile below too
  expect_equal(n(5, 14, 9), 0)
})

test_that("a point layer: thinned by default, its size following the zoom", {
  skip_if_no_spatial()
  d <- rr_example("dansk")
  m <- rr_point(d, colour = behaviour)
  r <- paint(m)[["circle-radius"]]
  expect_equal(r[1:3], list("interpolate", list("exponential", 2), list("zoom")))
  expect_equal(c(r[[5]], r[[7]]), c(1, 4)) # 2 to 8 pixels across
  expect_equal(r[[6]] - r[[4]], 2) # doubling with each zoom in
  expect_equal(r[[4]], 12) # dansk's pings lie two pixels apart at zoom 12
  expect_equal(paint(rr_point(d, size = 6))[["circle-radius"]], 3)
  expect_equal(paint(rr_point(d, size = 6))[["circle-stroke-width"]], 0.5)
  b <- paint(rr_point(rr_example("trawlsurvey"), size = c(1, 12)))[["circle-radius"]]
  expect_equal(c(b[[5]], b[[7]]), c(0.5, 6))
  expect_error(rr_point(d, size = c(8, 2)), "the least first")
  expect_error(rr_point(d, thin = "yes"), "`thin` is TRUE")
  # the busiest tile at zoom 6 thinned, and whole with thin = FALSE
  n <- function(m) unname(mvt_count(m$layers[[1]]$tiles(6, 33, 19))["features"])
  expect_equal(n(m), 3528)
  expect_equal(n(rr_point(d, colour = behaviour, thin = FALSE)), 141036)
  # a choose list keeps its values apart: a vessel chosen loses none of its points to another's
  expect_gt(n(rr_point(d, colour = behaviour, choose = vessel_id)), 3528)
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
  # an array, row n at n - 1 (an object keyed by number took Chrome a minute to read)
  expect_length(b$rows, 2)
  expect_equal(b$rows[[1]][[1]], "A1")
  expect_equal(b$rows[[1]][[5]], "https://example.org/a.jpg")
  expect_equal(b$rows[[2]][[1]], "B2")
  expect_null(b$rows[[2]][[2]]) # missing
  # the page has the table's code and texts
  page <- .rr_map_page("{}")
  expect_match(page, "function rowHtml(cols)", fixed = TRUE)
  expect_match(page, "fetch(`/row/${l.id}/${p.rr_row}`)", fixed = TRUE)
  expect_match(page, "d.rows[p.rr_row - 1]", fixed = TRUE)
})

test_that("a bundle of a layer function: its tiles to the page's zoom 14 by default, its rows by number past 99,999", {
  skip_if_no_spatial()
  # a row's number written as R prints it would be "1e+05", and the row lost to a click
  n <- 100001L
  d <- data.frame(lon = 10 + (seq_len(n) %% 1000) / 1000, lat = 56 + (seq_len(n) %/% 1000) / 1000, k = seq_len(n))
  m <- rr_point(d, popup = k)
  dir <- tempfile()
  rr_bundle(m, dir, maxzoom = 0)
  js <- paste(readLines(file.path(dir, "data", "points_rows.js"), encoding = "UTF-8"), collapse = "")
  b <- jsonlite::fromJSON(sub('^RB_BUNDLE\\("data/points_rows",(.*)\\);$', "\\1", js), simplifyVector = FALSE)
  expect_length(b$rows, n)
  rows_k <- vapply(b$rows[c(1, 99999, 100000, 100001)], function(r) r[[1]], 0)
  # the reader's numbering is the table's: row n holds the k of the nth row as the layer numbered them
  expect_equal(rows_k, m$layers$points$row()$rows$k[c(1, 99999, 100000, 100001)])
  expect_false(grepl("e+0", js, fixed = TRUE))
  # no maxzoom given: a vector layer's tiles to 14, as the page has them, not an error
  m2 <- rr_point(data.frame(lon = c(10, 10.5), lat = c(56, 56.5), k = 1:2), popup = k)
  dir2 <- tempfile()
  rr_bundle(m2, dir2)
  expect_true(dir.exists(file.path(dir2, "tiles", "points", "14")))
  expect_false(dir.exists(file.path(dir2, "tiles", "points", "15")))
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
