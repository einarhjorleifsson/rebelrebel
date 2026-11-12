# The map viewer (map.R): the config, the server's routes called as httpuv calls them (no network), the checks on a
# grid's parameters, the table tiles, and a real server started and stopped. All on the example data.

get <- function(app, path, query = "") {
  app$call(list(REQUEST_METHOD = "GET", PATH_INFO = path, QUERY_STRING = query))
}

# the tile holding a point
tile_of <- function(lon, lat, z) {
  n <- 2^z
  c(
    z = z,
    x = floor((lon + 180) / 360 * n),
    y = floor(
      (1 - log(tan(lat * pi / 180) + 1 / cos(lat * pi / 180)) / pi) / 2 * n
    )
  )
}

# a map of the example pings by cell (a grid, filtered by behaviour) and the harbours (as tiles and as GeoJSON)
example_map <- function() {
  con <- .rr_connection()
  pings <- dbplyr::sql_render(rr_example("dansk"))
  grid_tile <- function(z, x, y, params) {
    where <- if (is.null(params$behaviour)) {
      ""
    } else {
      sprintf("WHERE behaviour = %s", DBI::dbQuoteString(con, params$behaviour))
    }
    DBI::dbGetQuery(
      con,
      sprintf(
        "WITH c AS (SELECT floor(lon * 100) AS cx, floor(lat * 100) AS cy, count(*) AS n FROM (%s) %s GROUP BY ALL),
       m AS (SELECT n, ST_Transform(ST_MakeEnvelope(cx / 100, cy / 100, (cx + 1) / 100, (cy + 1) / 100),
                                    'EPSG:4326', 'EPSG:3857', always_xy := true) AS env FROM c)
       SELECT ST_AsMVT({'geometry': ST_AsMVTGeom(env, ST_Extent(ST_TileEnvelope(%d, %d, %d)), 4096, 64, true),
                        'n': n::INTEGER}, 'cells', 4096) AS tile FROM m",
        pings,
        where,
        z,
        x,
        y
      )
    )$tile[[1]]
  }
  grid <- rr_layer(
    "pings",
    "grid",
    label = "Pings",
    tiles = grid_tile,
    meta = list(
      options = list(behaviour = data.frame(value = c("Fishing", "Other"))),
      v = 1
    ),
    filters = list(
      list(
        id = "behaviour",
        label = "Behaviour",
        options = list(list(value = "", label = "all")),
        meta = TRUE
      ),
      list(id = "metric", type = "metric", label = "Metric")
    ),
    metrics = list(list(field = "n", label = "pings")),
    popup = list(
      fields = list(list(field = "n", label = "pings", format = "count"))
    )
  )
  h <- rr_example("dansk_harbours")
  harbours <- rr_layer(
    "harbours",
    "vector",
    label = "Harbours",
    visible = FALSE,
    tiles = rr_tiles_table(
      h,
      properties = "SI_HARB",
      points = "harbour_points"
    ),
    style = list(list(
      id = "harbours-fill",
      type = "fill",
      "source-layer" = "features",
      paint = list("fill-color" = "#d9822b")
    )),
    popup = list(
      click = "harbours-fill",
      title = "SI_HARB",
      fields = list(list(field = "kind", label = "kind"))
    )
  )
  x <- dplyr::collect(h)
  sfh <- sf::st_sf(
    x["SI_HARB"],
    geometry = sf::st_as_sfc(structure(x$geometry, class = "WKB"), crs = 4326)
  )
  outline <- rr_layer(
    "outline",
    "geojson",
    label = "Outlines",
    data = sfh[1:5, ],
    style = list(list(
      id = "outline-line",
      type = "line",
      paint = list("line-color" = "#333")
    ))
  )
  rr_map(
    rr_section("data", "Data", list(grid)),
    rr_section("ref", "Reference", list(harbours, outline)),
    title = "Example",
    view = list(center = c(12.5, 56), zoom = 7),
    texts = list(Pings = list(en = "Pings", da = "Positioner")),
    languages = c("da", "en"),
    language = "en"
  )
}

test_that("a layer and a map are checked as they are made", {
  noop <- function(z, x, y) raw(0)
  st <- list(list(id = "a", type = "line"))
  expect_error(
    rr_layer("Bad-id", "vector", tiles = noop, style = st),
    "lower case"
  )
  expect_error(
    rr_layer("lang", "vector", tiles = noop, style = st),
    "address"
  )
  expect_error(rr_layer("a", "vector", style = st), "needs `tiles`")
  expect_error(rr_layer("a", "grid", tiles = noop), "needs `metrics`")
  expect_error(rr_layer("a", "geojson", style = st), "needs `data`")
  expect_error(rr_layer("a", "vector", tiles = noop), "needs `style`")
  a <- rr_layer("a", "vector", tiles = noop, style = st)
  expect_error(rr_map(rr_section("g", layers = list(a, a))), "used twice")
  g <- function(id) {
    rr_layer(
      id,
      "grid",
      tiles = noop,
      metrics = list(list(field = "n", label = "n"))
    )
  }
  expect_error(
    rr_map(rr_section("g", layers = list(g("a"), g("b")))),
    "one grid"
  )
  expect_error(
    rr_map(rr_section("g", layers = list(a)), palette = "nope"),
    "palette"
  )
  expect_error(rr_map(list()), "rr_section")
})

test_that("the config keeps one-element arrays as arrays", {
  skip_if_not_installed("jsonlite")
  a <- rr_layer(
    "a",
    "vector",
    tiles = function(z, x, y) raw(0),
    popup = list(click = "a-line", title = "x"),
    style = list(list(
      id = "a-line",
      type = "line",
      filter = list("==", list("get", "k"), "v"),
      minzoom = 3,
      layout = list("text-field" = list("zoom"))
    ))
  )
  m <- rr_map(
    rr_section("g", "G", list(a)),
    languages = "en",
    about = "about1"
  )
  j <- as.character(jsonlite::toJSON(
    m$config,
    auto_unbox = TRUE,
    null = "null"
  ))
  expect_match(j, '"layers":["a"]', fixed = TRUE)
  expect_match(j, '"click":["a-line"]', fixed = TRUE)
  expect_match(j, '"languages":["en"]', fixed = TRUE)
  expect_match(j, '"about":["about1"]', fixed = TRUE)
  expect_match(j, '"text-field":["zoom"]', fixed = TRUE)
  expect_match(j, '"filter":["==",["get","k"],"v"]', fixed = TRUE)
  expect_length(m$config$palettes, 18) # twelve for the grid, seven looks for a depth, deep in both
  expect_equal(
    vapply(m$config$basemaps, `[[`, "", "id"),
    c("emodnet", "emodnet_dark", "emodnet_light", "ocean", "none")
  )
})

test_that("the base maps are those that show the sea floor, each a picture with names over it", {
  b <- rr_basemaps()
  names(b) <- vapply(b, `[[`, "", "id")
  # a map opens on EMODnet's sea floor; none is the one that is not a map
  expect_equal(names(b)[1], "emodnet")
  for (id in setdiff(names(b), "none")) {
    expect_match(b[[id]]$tiles, "^https://.*\\{z\\}.*\\{x\\}|^https://.*\\{z\\}/\\{y\\}/\\{x\\}")
    expect_match(b[[id]]$labels, "^https://basemaps\\.cartocdn\\.com/gl/.*/style\\.json$")
    expect_true(nzchar(b[[id]]$attribution))
    expect_null(b[[id]]$style)
  }
  expect_match(b$emodnet$attribution, "EMODnet", fixed = TRUE)
  expect_match(b$emodnet_dark$labels, "dark-matter", fixed = TRUE)
  expect_lt(b$emodnet_dark$paint[["raster-brightness-max"]], 1)
  expect_null(b$none$tiles)
  # the page opens on a picture: its names' style first, then the picture put under the names
  page <- .rr_map_page("{}")
  expect_match(page, "style: BASEMAPS[0].style ?? BASEMAPS[0].labels ?? baseStyle(BASEMAPS[0])", fixed = TRUE)
  expect_match(page, "if (!BASEMAPS[0].style) setBase(BASEMAPS[0].id);", fixed = TRUE)
  expect_match(page, "next = compose(b, next);", fixed = TRUE)
})

test_that("the server's routes: the page, its config, meta, tiles and data", {
  skip_if_not_installed("jsonlite")
  skip_if_no_spatial()
  m <- example_map()
  app <- .rr_map_app(m)
  page <- get(app, "/")
  expect_equal(page$status, 200L)
  expect_false(grepl("__CONFIG__", page$body, fixed = TRUE))
  expect_match(page$body, '"title":"Example"', fixed = TRUE)
  cfg <- jsonlite::fromJSON(
    get(app, "/config.json")$body,
    simplifyVector = FALSE
  )
  expect_equal(
    vapply(cfg$layers, `[[`, "", "id"),
    c("pings", "harbours", "outline")
  )
  meta <- jsonlite::fromJSON(get(app, "/meta")$body)
  expect_equal(meta$engine, .rr_map_engine)
  expect_equal(meta$layers$harbours$count, 942L)
  expect_true(nzchar(meta$layers$harbours$v)) # the server's start, in the tiles' address
  expect_equal(
    get(app, "/data/outline.geojson")$headers$`Cache-Control`,
    "no-store"
  )
  expect_equal(meta$layers$pings$options$behaviour$value, c("Fishing", "Other"))
  t <- tile_of(12.6, 56.0, 7)
  all <- get(app, sprintf("/tiles/pings/%d/%d/%d.pbf", t["z"], t["x"], t["y"]))
  fishing <- get(
    app,
    sprintf("/tiles/pings/%d/%d/%d.pbf", t["z"], t["x"], t["y"]),
    "behaviour=Fishing&v=1"
  )
  expect_equal(all$status, 200L)
  expect_gt(length(all$body), length(fishing$body))
  expect_gt(length(fishing$body), 0)
  # a harbour's own tile has features in both layers
  con <- .rr_connection()
  p <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT ST_X(ST_PointOnSurface(geometry)) AS x, ST_Y(ST_PointOnSurface(geometry)) AS y FROM (%s) LIMIT 1",
      dbplyr::sql_render(rr_example("dansk_harbours"))
    )
  )
  h <- tile_of(p$x, p$y, 10)
  ht <- get(
    app,
    sprintf("/tiles/harbours/%d/%d/%d.pbf", h["z"], h["x"], h["y"])
  )
  expect_equal(ht$status, 200L)
  text <- rawToChar(ht$body[ht$body >= as.raw(32) & ht$body < as.raw(127)]) # the layers' names, among the bytes
  expect_match(text, "features", fixed = TRUE)
  expect_match(text, "harbour_points", fixed = TRUE)
  expect_match(
    get(app, "/data/outline.geojson")$body,
    "FeatureCollection",
    fixed = TRUE
  )
})

test_that("a table's properties of any type reach its tiles, cast to what a tile holds", {
  skip_if_no_spatial()
  con <- .rr_connection()
  duckdb::duckdb_register(
    con,
    "rb_test_types",
    data.frame(
      lon = 12.6,
      lat = 56.0,
      gear = factor("gillnet"), # an ENUM in DuckDB
      day = as.Date("2026-10-24"),
      time = as.POSIXct("2026-10-24 21:00:00", tz = "UTC")
    )
  )
  on.exit(duckdb::duckdb_unregister(con, "rb_test_types"))
  x <- dplyr::tbl(
    con,
    dplyr::sql(
      "SELECT *, 3::TINYINT AS n, 2.5::DECIMAL(4, 1) AS kg, 7::UINTEGER AS id FROM rb_test_types"
    )
  )
  tiles <- rr_tiles_table(
    x,
    lon = "lon",
    lat = "lat",
    properties = c("gear", "day", "time", "n", "kg", "id")
  )
  t <- tile_of(12.6, 56.0, 9)
  b <- tiles(t["z"], t["x"], t["y"])
  expect_gt(length(b), 0)
  text <- rawToChar(b[b >= as.raw(32) & b < as.raw(127)])
  expect_match(text, "gillnet", fixed = TRUE)
  expect_match(text, "2026-10-24", fixed = TRUE)
  expect_equal(
    .rr_mvt_cast(
      c("a", "b", "c", "d", "e"),
      c("INTEGER", "SMALLINT", "UINTEGER", "DECIMAL(4,1)", "ENUM('x')")
    ),
    c(
      "a",
      "CAST(b AS INTEGER)",
      "CAST(c AS BIGINT)",
      "CAST(d AS DOUBLE)",
      "CAST(e AS VARCHAR)"
    )
  )
})

test_that("tables cut into tiles never share a name, after set.seed() or a reload", {
  skip_if_no_spatial()
  h <- rr_example("dansk_harbours")
  set.seed(1)
  x <- stats::runif(1)
  set.seed(1)
  a <- rr_tiles_table(h, properties = "SI_HARB")
  expect_equal(stats::runif(1), x) # the user's random numbers are as they were
  set.seed(1)
  b <- rr_tiles_table(h, properties = "SI_HARB")
  .rr_tiles$n <- 0L # as after the package is loaded again, the connection's tables still there
  d <- rr_tiles_table(h, properties = "SI_HARB")
  expect_equal(
    c(attr(a, "count"), attr(b, "count"), attr(d, "count")),
    rep(942L, 3)
  )
})

test_that("a tile function that fails replies with its reason", {
  skip_if_not_installed("jsonlite")
  boom <- rr_layer(
    "boom",
    "vector",
    label = "Boom",
    tiles = function(z, x, y) stop("no tile here\nsecond line"),
    style = list(list(
      id = "boom-line",
      type = "line",
      "source-layer" = "features",
      paint = list("line-color" = "#333")
    ))
  )
  app <- .rr_map_app(rr_map(rr_section("g", "G", list(boom)), title = "Boom"))
  r <- get(app, "/tiles/boom/1/0/0.pbf")
  expect_equal(r$status, 500L)
  expect_equal(r$body, "no tile here")
})

test_that("a grid's parameters are checked before its tile function sees them", {
  skip_if_not_installed("jsonlite")
  skip_if_no_spatial()
  app <- .rr_map_app(example_map())
  bad <- get(app, "/tiles/pings/7/68/39.pbf", "behaviour=Fishing%27%3B%20DROP")
  expect_equal(bad$status, 400L)
  expect_match(bad$body, "behaviour is Fishing, Other", fixed = TRUE)
  expect_equal(get(app, "/tiles/pings/30/0/0.pbf")$status, 400L)
  expect_equal(get(app, "/tiles/pings/2/9/0.pbf")$status, 400L)
  expect_equal(get(app, "/tiles/nope/2/1/1.pbf")$status, 404L)
  expect_equal(get(app, "/tiles/outline/2/1/1.pbf")$status, 404L)
  expect_equal(get(app, "/data/pings.geojson")$status, 404L)
  expect_equal(get(app, "/elsewhere")$status, 404L)
  # the page's libraries, from the package, so a map draws with no network but the base map's
  js <- get(app, "/vendor/maplibre-gl.js")
  expect_equal(js$status, 200L)
  expect_match(js$headers$`Content-Type`, "text/javascript", fixed = TRUE)
  expect_identical(js$body, readBin(system.file("engine", "vendor", "maplibre-gl.js", package = "rebelrebel"), "raw", 2e6))
  expect_match(get(app, "/vendor/uPlot.min.css")$headers$`Content-Type`, "text/css", fixed = TRUE)
  expect_equal(get(app, "/vendor/maplibre-contour.min.js")$status, 200L)
  expect_equal(get(app, "/vendor/nope.js")$status, 404L)
  expect_equal(get(app, "/vendor/../DESCRIPTION")$status, 404L)
  expect_equal(get(app, "/vendor/LICENSE-uplot.txt")$status, 404L)
  page <- get(app, "/")$body
  expect_false(grepl("cdn.jsdelivr", page, fixed = TRUE))
  for (f in c("vendor/maplibre-gl.js", "vendor/maplibre-gl.css", "vendor/maplibre-contour.min.js",
              "vendor/uPlot.iife.min.js", "vendor/uPlot.min.css")) {
    expect_match(page, f, fixed = TRUE)
  }
  expect_equal(
    app$call(list(
      REQUEST_METHOD = "POST",
      PATH_INFO = "/",
      QUERY_STRING = ""
    ))$status,
    405L
  )
})

test_that("a grid's range filter takes its bounds together and within its meta", {
  f <- list(list(id = "years", type = "range", params = list("y1", "y2")))
  meta <- list(ranges = list(years = c(2007, 2026)))
  expect_equal(
    .rr_map_params(f, meta, list(y1 = "2010", y2 = "2012")),
    list(y1 = "2010", y2 = "2012")
  )
  expect_match(.rr_map_params(f, meta, list(y1 = "2010")), "given together")
  expect_match(
    .rr_map_params(f, meta, list(y1 = "1990", y2 = "2012")),
    "lie in 2007 to 2026"
  )
  expect_equal(.rr_map_params(f, meta, list()), list())
})

test_that("a server starts in the background and stops", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("jsonlite")
  a <- rr_layer(
    "a",
    "vector",
    tiles = function(z, x, y) raw(0),
    style = list(list(id = "a-line", type = "line"))
  )
  port <- httpuv::randomPort()
  url <- rr_serve(
    rr_map(rr_section("g", layers = list(a))),
    port = port,
    open = FALSE,
    query = list(lang = "en")
  )
  expect_equal(url, sprintf("http://localhost:%d/?lang=en", port))
  expect_equal(rr_stop(port), port)
  expect_equal(rr_stop(port), integer(0))
})

test_that("a layer's variants: styles to choose between in the page, checked as they are made", {
  skip_if_not_installed("jsonlite")
  st <- list(list(id = "a-dot", type = "circle", "source-layer" = "features"))
  a <- rr_layer(
    "a",
    "vector",
    tiles = function(z, x, y) raw(0),
    style = st,
    variants = list(
      label = "Colour by",
      options = list(
        list(
          value = "x",
          paint = list("a-dot" = list("circle-color" = "#000000"))
        ),
        list(
          value = "y",
          label = "Y",
          paint = list("a-dot" = list("circle-color" = "#ffffff"))
        )
      )
    )
  )
  expect_equal(
    vapply(a$config$variants$options, `[[`, "", "label"),
    c("x", "Y")
  ) # a label defaults to the value
  cfg <- jsonlite::fromJSON(
    .rr_map_app(rr_map(rr_section("g", layers = list(a))))$call(
      list(
        REQUEST_METHOD = "GET",
        PATH_INFO = "/config.json",
        QUERY_STRING = ""
      )
    )$body,
    simplifyVector = FALSE
  )
  expect_equal(
    cfg$layers[[1]]$variants$options[[2]]$paint$`a-dot`$`circle-color`,
    "#ffffff"
  )
  bad <- function(v) {
    rr_layer(
      "a",
      "vector",
      tiles = function(z, x, y) raw(0),
      style = st,
      variants = v
    )
  }
  expect_error(
    bad(list(options = list(list(value = "x", paint = list(nope = list()))))),
    "style ids"
  )
  expect_error(bad(list(options = list())), "options")
  expect_error(
    bad(list(
      options = list(
        list(value = "x", paint = list("a-dot" = list())),
        list(value = "x", paint = list("a-dot" = list()))
      )
    )),
    "distinct"
  )
})

test_that("a layer's state: values its style reads, checked as they are made", {
  skip_if_not_installed("jsonlite")
  gj <- '{"type": "FeatureCollection", "features": []}'
  st <- list(
    list(id = "n-area", type = "fill", filter = list("global-state", "n_areas")),
    list(id = "n-label", type = "symbol",
         filter = list("<=", list("-", list("get", "min_zoom"), list("global-state", "n_early")), list("zoom")),
         paint = list("text-color" = list("global-state", "n_colour")))
  )
  state <- list(
    list(id = "areas", label = "Areas", default = FALSE),
    list(id = "early", label = "Names appear",
         options = list(list(value = -1, label = "later"), list(value = 0, label = "as set"),
                        list(value = 1))),
    list(id = "colour", options = list(list(value = "#0b3954", label = "dark blue"),
                                       list(value = "#ffffff", label = "white")), default = "#ffffff")
  )
  n <- rr_layer("n", "geojson", data = gj, style = st, state = state)
  s <- n$config$state
  expect_equal(vapply(s, `[[`, "", "id"), c("areas", "early", "colour"))
  expect_equal(s[[2]]$default, -1) # the first option's value when not given
  expect_equal(s[[2]]$options[[3]]$label, "1") # a label defaults to the value
  expect_equal(s[[3]]$label, "colour") # and a control's to its id
  cfg <- jsonlite::fromJSON(
    .rr_map_app(rr_map(rr_section("g", layers = list(n))))$call(
      list(REQUEST_METHOD = "GET", PATH_INFO = "/config.json", QUERY_STRING = "")
    )$body,
    simplifyVector = FALSE
  )
  expect_identical(cfg$layers[[1]]$state[[1]]$default, FALSE)
  expect_identical(cfg$layers[[1]]$state[[2]]$options[[1]]$value, -1L) # a number stays a number in the page
  bad <- function(state, style = st, id = "n") {
    rr_layer(id, "geojson", data = gj, style = style, state = state)
  }
  expect_error(bad(list(list(id = "nope", default = TRUE))), "read by none")
  expect_error(bad(state[1], style = st[2]), "global-state")
  expect_error(bad(list(list(id = "areas"))), "TRUE or FALSE")
  expect_error(bad(list(list(id = "areas", default = "yes"))), "TRUE or FALSE")
  expect_error(bad(list(list(id = "Areas", default = TRUE))), "lower-case")
  expect_error(bad(list(list(id = "by", default = TRUE))), "own controls")
  expect_error(bad(list(state[[1]], state[[1]])), "once")
  expect_error(bad(list(list(id = "early", options = list(list(value = 1), list(value = 1))))), "distinct")
  expect_error(bad(list(list(id = "early", options = list(list(value = 1)), default = 2))), "one of its options")
  expect_error(bad(list(list(id = "early", options = list(list(value = c(1, 2)))))), "options")
  expect_error(bad("areas"), "list of controls")
  expect_error(bad(list()), "list of controls")
  expect_error(
    rr_layer("g", "grid", tiles = function(z, x, y, p) raw(0), metrics = list(list(field = "n")), state = state),
    "drawn by the page"
  )
  # a layer named as another's state would share its place in the address
  other <- rr_layer("n_areas", "geojson", data = gj, style = list(list(id = "o", type = "fill")))
  expect_error(rr_map(rr_section("g", layers = list(n, other))), "another layer's state")
})

test_that("a gap in words: whole hours, minutes or seconds, else hours", {
  expect_equal(.rr_duration_text(3600), "1 hour")
  expect_equal(.rr_duration_text(7200), "2 hours")
  expect_equal(.rr_duration_text(5400), "1.5 hours")
  expect_equal(.rr_duration_text(1800), "30 minutes")
  expect_equal(.rr_duration_text(60), "1 minute")
  expect_equal(.rr_duration_text(90), "90 seconds")
})

test_that("a map is served when printed at a console; a replaced map's tables are dropped and made again", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("jsonlite")
  skip_if_no_spatial()
  con <- .rr_connection()
  exists <- function(m) {
    vapply(
      attr(m, "tables"),
      function(t) DBI::dbExistsTable(t$con, t$name),
      logical(1)
    )
  }
  df <- dplyr::collect(utils::head(rr_example("dansk"), 500))
  port <- httpuv::randomPort()
  # as tiles, whose tables are the subject (a layer sent whole has none)
  m1 <- rr_path(df, group = vessel_id, time = time_stamp, whole = FALSE) |> rr_point(whole = FALSE)
  m2 <- rr_point(df, colour = behaviour, whole = FALSE)
  attr(m1, "port") <- port
  attr(m2, "port") <- port
  rlang::with_interactive(print(m1))
  expect_true(all(exists(m1)))
  rlang::with_interactive(print(m2)) # on the same port: m1's tables go
  expect_false(any(exists(m1)))
  expect_gt(length(m1$layers$points$tiles(10, 547, 320)), 0) # shown again, a table is made again
  expect_true(exists(m1)[2])
  expect_equal(rr_stop(port), port)
  expect_false(any(exists(m2)))
})

test_that("a dem layer: elevation images from an MBTiles file, served as images under their own extension", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")
  f <- tempfile(fileext = ".mbtiles")
  on.exit(unlink(f))
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  DBI::dbWriteTable(con, "metadata", data.frame(
    name = c("format", "encoding", "bounds", "minzoom", "maxzoom", "tile_size"),
    value = c("png", "terrarium", "-30,60,-5,70", "0", "1", "256")
  ))
  DBI::dbExecute(con, "CREATE TABLE tiles (zoom_level INTEGER, tile_column INTEGER, tile_row INTEGER, tile_data BLOB)")
  # zoom 1, the north-west tile: row 1 counted from the south
  DBI::dbExecute(con, "INSERT INTO tiles VALUES (1, 0, 1, ?)", params = list(list(as.raw(1:4))))
  DBI::dbDisconnect(con)

  tiles <- rr_tiles_mbtiles(f)
  expect_identical(tiles(1, 0, 0), as.raw(1:4))
  expect_null(tiles(1, 0, 1))
  expect_equal(attr(tiles, "bounds"), c(-30, 60, -5, 70))
  expect_error(rr_tiles_mbtiles(tempfile()), "path of an MBTiles file")

  shade <- list(list(id = "depth-shade", type = "hillshade"))
  l <- rr_layer("depth", "dem", tiles = tiles, style = shade)
  expect_equal(l$config[c("encoding", "tile_size", "format", "minzoom", "maxzoom")],
               list(encoding = "terrarium", tile_size = 256L, format = "png", minzoom = 0L, maxzoom = 1L))
  expect_equal(l$config$bounds, list(-30, 60, -5, 70)) # an array in the config's JSON
  # given arguments win over the file's
  expect_equal(rr_layer("depth", "dem", tiles = tiles, style = shade, tile_size = 512)$config$tile_size, 512L)
  expect_error(rr_layer("depth", "dem", style = shade), "needs `tiles`, a function returning an image tile")
  expect_error(rr_layer("depth", "dem", tiles = tiles, style = shade, encoding = "lerc"), "terrarium")
  expect_error(
    rr_layer("lines", "vector", tiles = function(z, x, y) raw(0), style = shade, bounds = c(0, 0, 1, 1)),
    "for a dem or contour layer"
  )

  app <- .rr_map_app(rr_map(rr_section("g", "G", list(l))))
  ok <- get(app, "/tiles/depth/1/0/0.png")
  expect_equal(ok$status, 200L)
  expect_equal(ok$headers$`Content-Type`, "image/png")
  expect_identical(ok$body, as.raw(1:4))
  expect_equal(get(app, "/tiles/depth/1/0/1.png")$status, 404L) # not in the file: no empty image
  expect_equal(get(app, "/tiles/depth/1/0/0.pbf")$status, 404L) # not its format
  meta <- jsonlite::fromJSON(get(app, "/meta")$body)
  expect_true(nzchar(meta$layers$depth$v))
})

test_that("elevation tiles from a grid: Terrarium images that decode to the grid, interpolated, holes and edges kept", {
  skip_if_not_installed("png")
  decode <- function(b) {
    a <- png::readPNG(b) * 255
    a[,, 1] * 256 + a[,, 2] + a[,, 3] / 256 - 32768
  }
  # a tile's pixel centres: rows north to south, columns west to east
  pixels <- function(z, x, y, S = 512) {
    p <- (seq_len(S) - 0.5) / S
    list(
      lon = (x + p) / 2^z * 360 - 180,
      lat = atan(sinh(pi * (1 - 2 * (y + p) / 2^z))) * 180 / pi
    )
  }
  # a plane: bilinear interpolation gives it exactly, to the encoding's 1/256 m
  g <- expand.grid(
    lon = seq(10, 10.4, by = 0.02),
    lat = seq(56, 56.3, by = 0.01)
  )
  plane <- function(lon, lat) -50 + 100 * (lon - 10) - 200 * (lat - 56)
  g$elevation <- plane(g$lon, g$lat)
  tiles <- rr_tiles_dem(g)
  expect_equal(attr(tiles, "bounds"), c(9.99, 55.995, 10.41, 56.305))
  expect_equal(attr(tiles, "count"), nrow(g))
  expect_equal(attr(tiles, "format"), "png")
  expect_equal(attr(tiles, "encoding"), "terrarium")
  expect_equal(attr(tiles, "maxzoom"), 6L) # a pixel no wider than a cell of 0.02 degrees: 360 / (512 * 2^6)
  # a fine grid stops at zoom 8 unless asked deeper; a minzoom deeper than 8 is kept
  fine <- expand.grid(lon = seq(10, 10.01, by = 0.0005), lat = seq(56, 56.01, by = 0.0005))
  fine$elevation <- -50
  expect_equal(attr(rr_tiles_dem(fine), "maxzoom"), 8L) # its own limit is 11
  expect_equal(attr(rr_tiles_dem(fine, maxzoom = 11), "maxzoom"), 11L)
  expect_equal(attr(rr_tiles_dem(fine, minzoom = 9), "maxzoom"), 9L)
  t <- tile_of(10.2, 56.15, 9)
  z <- t[["z"]]
  tx <- t[["x"]]
  ty <- t[["y"]]
  E <- decode(tiles(z, tx, ty))
  px <- pixels(z, tx, ty)
  truth <- outer(px$lat, px$lon, function(la, lo) plane(lo, la))
  inside <- outer(px$lat, px$lon, function(la, lo) {
    lo >= 10 & lo <= 10.4 & la >= 56 & la <= 56.3
  })
  expect_lt(max(abs(E - truth)[inside]), 1 / 256)
  beyond <- outer(px$lat, px$lon, function(la, lo) {
    lo < 9.99 | lo > 10.41 | la < 55.995 | la > 56.305
  })
  expect_true(any(beyond))
  expect_true(all(E[beyond] == 0)) # nodata: sea level
  # wholly outside the grid a level tile, not none: maplibre-contour draws no lines in a tile whose neighbour is refused
  expect_true(all(decode(tiles(z, 0, 0)) == 0))
  expect_true(all(decode(rr_tiles_dem(g, nodata = -5)(z, 0, 0)) == -5))

  # the same tile from a lazy table, and from the rows shuffled
  con <- .rr_connection()
  duckdb::duckdb_register(con, "rb_test_dem_grid", g)
  on.exit(duckdb::duckdb_unregister(con, "rb_test_dem_grid"), add = TRUE)
  expect_identical(
    rr_tiles_dem(dplyr::tbl(con, "rb_test_dem_grid"))(z, tx, ty),
    tiles(z, tx, ty)
  )
  shuffled <- g[withr::with_seed(1, sample(nrow(g))), ]
  expect_identical(rr_tiles_dem(shuffled)(z, tx, ty), tiles(z, tx, ty))

  # zoomed out, the cells averaged in blocks: a level grid stays level
  flat <- transform(g, elevation = -20)
  expect_true(all(decode(rr_tiles_dem(flat)(0, 0, 0)) %in% c(-20, 0)))

  # a missing cell is a hole: the pixels nearest it carry nodata, which may be given
  holed <- flat[!(abs(flat$lon - 10.2) < 1e-9 & abs(flat$lat - 56.15) < 1e-9), ]
  Eh <- decode(rr_tiles_dem(holed, nodata = -1)(z, tx, ty))
  near <- outer(px$lat, px$lon, function(la, lo) {
    abs(lo - 10.2) < 0.009 & abs(la - 56.15) < 0.0045
  })
  expect_true(any(near))
  expect_true(all(Eh[near] == -1))
  expect_true(all(Eh[inside & !near] %in% c(-20, -1)))
  expect_gt(mean(Eh[inside] == -20), 0.9)

  # columns by other names; the checks
  names(g) <- c("x", "y", "z")
  expect_identical(
    rr_tiles_dem(g, lon = "x", lat = "y", elevation = "z")(z, tx, ty),
    tiles(z, tx, ty)
  )
  expect_error(rr_tiles_dem(g), "no column `lon`, `lat`, `elevation`")
  names(g) <- c("lon", "lat", "elevation")
  expect_error(rr_tiles_dem(rbind(g, g[1, ])), "1 rows repeat a cell")
  off <- g
  off$lon[5] <- off$lon[5] + 0.003 # half a cell would be a finer grid with holes
  expect_error(rr_tiles_dem(off), "a longitude lies off that lattice")
  expect_error(
    rr_tiles_dem(g[g$lon == 10, ]),
    "at least two cells each way"
  )
  expect_error(rr_tiles_dem(g, tile_size = 300), "256 or 512")
  expect_error(
    rr_tiles_dem(g, minzoom = 5, maxzoom = 2),
    "no less than `minzoom`"
  )

  # a dem layer takes its attributes, and the server sends its tiles as images
  skip_if_not_installed("jsonlite")
  l <- rr_layer(
    "relief",
    "dem",
    tiles = tiles,
    style = list(list(id = "relief-shade", type = "hillshade"))
  )
  expect_equal(
    l$config[c("encoding", "tile_size", "format", "minzoom", "maxzoom")],
    list(
      encoding = "terrarium",
      tile_size = 512L,
      format = "png",
      minzoom = 0L,
      maxzoom = 6L
    )
  )
  app <- .rr_map_app(rr_map(rr_section("g", "G", list(l))))
  ok <- get(app, sprintf("/tiles/relief/%d/%.0f/%.0f.png", z, tx, ty))
  expect_equal(ok$status, 200L)
  expect_identical(ok$body, tiles(z, tx, ty))
  expect_identical(get(app, sprintf("/tiles/relief/%d/0/0.png", z))$body, tiles(z, 0, 0))

  # the tiles' fit is made in the page, from the tiles in view: a layer on them takes it where it is given none
  # (colours in equal shares, the shading and the contours following the view), the map carries its rules, and the
  # server has no fit to answer; a layer given its own function keeps it; `fit = TRUE` asks the page's of any tiles;
  # tiles with no fit leave the scale on log
  d <- expand.grid(lon = seq(10, 10.4, by = 0.01), lat = seq(56, 56.3, by = 0.01))
  d$elevation <- -(1 + 100 * (d$lon - 10) / 0.4) # 1 to 101 m deep, west to east
  dt <- rr_tiles_dem(d)
  expect_identical(attr(dt, "fit"), "page")
  colour_style <- list(list(id = "depth-colour", type = "color-relief"))
  dl <- rr_layer("depth", "dem", tiles = dt, style = colour_style, colour = TRUE)
  expect_equal(dl$config$colour[c("scale", "fit")], list(scale = "equal", fit = TRUE))
  expect_identical(dl$config$fit, "page")
  expect_identical(dl$fit, "page")
  expect_equal(rr_layer("depth", "dem", tiles = dt, style = colour_style,
                            colour = list(scale = "log"))$config$colour$scale, "log")
  own <- function(west, south, east, north) list(floor = 5, cap = 50)
  ol <- rr_layer("depth", "dem", tiles = dt, style = colour_style, colour = list(fit = own))
  expect_identical(ol$fit, own)
  expect_null(ol$config$fit)
  rl <- rr_layer("relief", "dem", tiles = dt, style = list(list(id = "relief-shade", type = "hillshade")),
                     shade = TRUE)
  expect_true(rl$config$shade$fit)
  expect_identical(rl$config$fit, "page")
  cl <- rr_layer("contours", "contour", tiles = dt, contours = list(thresholds = list("5" = 10), multiplier = -1),
                     style = list(list(id = "contours-line", type = "line", "source-layer" = "contours")))
  expect_true(cl$config$contours$fit)
  expect_identical(cl$config$fit, "page")
  bare <- function(z, x, y) NULL
  expect_equal(rr_layer("depth", "dem", tiles = bare, style = colour_style, colour = TRUE)$config$colour$scale, "log")
  tl <- rr_layer("depth", "dem", tiles = bare, style = colour_style, colour = list(fit = TRUE))
  expect_equal(tl$config$colour[c("scale", "fit")], list(scale = "equal", fit = TRUE))
  expect_identical(tl$config$fit, "page")
  expect_error(rr_layer("depth", "dem", tiles = dt, style = colour_style, colour = list(fit = "yes")),
               "is TRUE \\(made in the page")
  m <- rr_map(rr_section("g", "G", list(dl)))
  expect_equal(m$config$fit, .rr_map_bundle_fit(list()))
  expect_null(rr_map(rr_section("g", "G", list(ol)))$config$fit)
  app <- .rr_map_app(m)
  expect_equal(get(app, "/fit/depth", "bbox=10,56,10.4,56.3")$status, 404L)
  expect_equal(get(.rr_map_app(rr_map(rr_section("g", "G", list(ol)))), "/fit/depth", "bbox=10,56,10.4,56.3")$status,
               200L)
})

test_that("elevation tiles from a raster: a GeoTIFF in any projection, a SpatRaster, a layer named; read where it is", {
  skip_if_not_installed("png")
  skip_if_not_installed("terra")
  decode <- function(b) {
    a <- png::readPNG(b) * 255
    a[,, 1] * 256 + a[,, 2] + a[,, 3] / 256 - 32768
  }
  plane <- function(lon, lat) -50 + 100 * (lon - 10) - 200 * (lat - 56)
  g <- expand.grid(lon = seq(10, 10.4, by = 0.02), lat = seq(56, 56.3, by = 0.01))
  g$elevation <- plane(g$lon, g$lat)
  t <- tile_of(10.2, 56.15, 9)
  z <- t[["z"]]
  tx <- t[["x"]]
  ty <- t[["y"]]
  # the data frame's grid as a GeoTIFF gives the same tiles
  r <- terra::rast(g, type = "xyz", crs = "EPSG:4326")
  f <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(r, f)
  from_file <- rr_tiles_dem(f)
  expect_identical(from_file(z, tx, ty), rr_tiles_dem(g)(z, tx, ty))
  expect_equal(attr(from_file, "bounds"), c(9.99, 55.995, 10.41, 56.305))
  expect_identical(attr(rr_tiles_dem(f), "source"), attr(from_file, "source")) # layers on one file share its tiles
  expect_identical(rr_tiles_dem(r)(z, tx, ty), from_file(z, tx, ty))
  # a projected grid (Lambert azimuthal, metres): a plane in its own coordinates comes out at the pixels within a few
  # centimetres, each warped from the grid once (GDAL's bilinear warp of a projected grid is off by about 0.013 m here,
  # about 6 m of position on cells of 500 m; a grid in longitude and latitude, above, is exact)
  laea <- terra::rast(
    nrows = 60, ncols = 60, crs = "EPSG:3035",
    xmin = 4.30e6, xmax = 4.30e6 + 60 * 500, ymin = 3.66e6, ymax = 3.66e6 + 60 * 500
  )
  xy <- terra::xyFromCell(laea, seq_len(terra::ncell(laea)))
  pl <- function(x, y) -100 + 0.002 * (x - 4.30e6) - 0.001 * (y - 3.66e6)
  terra::values(laea) <- pl(xy[, 1], xy[, 2])
  fl <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(laea, fl)
  tl <- rr_tiles_dem(fl)
  b <- attr(tl, "bounds")
  expect_true(b[1] < b[3] && b[2] < b[4])
  expect_equal(attr(tl, "maxzoom"), 7L) # at 56 N zoom 7's pixels, about 340 m, are the first no wider than 500 m
  mid <- tile_of((b[1] + b[3]) / 2, (b[2] + b[4]) / 2, 10)
  E <- decode(tl(10, mid[["x"]], mid[["y"]]))
  S <- 512
  p <- (seq_len(S) - 0.5) / S
  lon <- (mid[["x"]] + p) / 2^10 * 360 - 180
  lat <- atan(sinh(pi * (1 - 2 * (mid[["y"]] + p) / 2^10))) * 180 / pi
  pts <- terra::project(terra::vect(cbind(rep(lon, S), rep(lat, each = S)), crs = "EPSG:4326"), "EPSG:3035")
  pxy <- terra::crds(pts)
  truth <- matrix(pl(pxy[, 1], pxy[, 2]), S, S, byrow = TRUE)
  inside <- matrix(pxy[, 1] > 4.30e6 + 250 & pxy[, 1] < 4.33e6 - 250 & pxy[, 2] > 3.66e6 + 250 & pxy[, 2] < 3.69e6 - 250,
                   S, S, byrow = TRUE)
  expect_gt(mean(inside), 0.5)
  expect_lt(max(abs(E - truth)[inside]), 0.05)
  # a raster of several layers: `elevation` names the one drawn
  two <- c(r, r * 2)
  names(two) <- c("elevation", "double")
  expect_identical(rr_tiles_dem(two)(z, tx, ty), from_file(z, tx, ty))
  expect_identical(decode(rr_tiles_dem(two, elevation = "double")(z, tx, ty)) <= 0, decode(from_file(z, tx, ty)) <= 0)
  names(two) <- c("a", "b")
  expect_error(rr_tiles_dem(two), "2 layers \\(a, b\\): `elevation` names the one drawn")
  # a file's overviews are read only where a pixel spans eight cells or more: at zoom 4, where a pixel spans two to four
  # cells of 0.01 degrees, the tile of a file with overviews is the tile of the same grid without, though GDAL's warp
  # alone would have taken them
  if (requireNamespace("sf", quietly = TRUE)) {
    rough <- terra::rast(nrows = 200, ncols = 200, xmin = 10, xmax = 12, ymin = 56, ymax = 58, crs = "EPSG:4326")
    terra::values(rough) <- withr::with_seed(1, -100 - stats::runif(40000, 0, 50))
    plain <- withr::local_tempfile(fileext = ".tif")
    terra::writeRaster(rough, plain)
    pyramid <- withr::local_tempfile(fileext = ".tif")
    terra::writeRaster(rough, pyramid)
    utils::capture.output(sf::gdal_addo(pyramid, overviews = c(2, 4, 8), method = "average")) # its progress, quietly
    t4 <- tile_of(11, 57, 4)
    expect_identical(rr_tiles_dem(pyramid)(4, t4[["x"]], t4[["y"]]), rr_tiles_dem(plain)(4, t4[["x"]], t4[["y"]]))
    m4 <- 2 * pi * 6378137 / 2^4
    tpl <- terra::rast(nrows = 512, ncols = 512, crs = "EPSG:3857",
                       xmin = -pi * 6378137 + t4[["x"]] * m4, xmax = -pi * 6378137 + (t4[["x"]] + 1) * m4,
                       ymin = pi * 6378137 - (t4[["y"]] + 1) * m4, ymax = pi * 6378137 - t4[["y"]] * m4)
    warped <- function(f) terra::values(terra::project(terra::rast(f), tpl, method = "average", by_util = TRUE))
    expect_gt(max(abs(warped(pyramid) - warped(plain)), na.rm = TRUE), 1)
  }
  # the checks: a raster with no coordinate reference system, a path GDAL cannot read, a table too large to read
  bare <- r
  terra::crs(bare) <- ""
  expect_error(rr_tiles_dem(bare), "no coordinate reference system")
  expect_error(rr_tiles_dem(withr::local_tempfile(fileext = ".tif")), "no raster GDAL reads")
  expect_error(rr_tiles_dem(list(1)), "a raster file's path, a terra SpatRaster, or a data frame")
  con <- .rr_connection()
  big <- dplyr::tbl(con, dplyr::sql("SELECT range AS lon, range AS lat, 0 AS elevation FROM range(10000001)"))
  expect_error(rr_tiles_dem(big), "10,000,001 cells, too many to read into R")
})

test_that("a dem layer's relief controls: MapLibre's defaults filled in, each value checked, its address names taken", {
  dem <- function(z, x, y) NULL
  shade_style <- list(list(id = "relief-shade", type = "hillshade"))
  l <- rr_layer("relief", "dem", tiles = dem, style = shade_style, shade = TRUE)
  expect_equal( # MapLibre's, but the intensity and the shadow a bathymetry map in use settled on
    l$config$shade,
    list(method = "standard", direction = 335, altitude = 45, exaggeration = 0.6, opacity = 100,
         shadow = "#1b2a3a", highlight = "#ffffff", auto = TRUE)
  )
  given <- rr_layer("relief", "dem", tiles = dem, style = shade_style,
                        shade = list(method = "igor", direction = 315, highlight = "#FFF8E0"))$config$shade
  expect_equal(given[c("method", "direction", "altitude", "highlight")],
               list(method = "igor", direction = 315, altitude = 45, highlight = "#fff8e0"))
  expect_null(rr_layer("relief", "dem", tiles = dem, style = shade_style)$config$shade)

  colour_style <- list(list(id = "depth-colour", type = "color-relief"))
  expect_error(rr_layer("depth", "dem", tiles = dem, style = colour_style, shade = TRUE), "`hillshade` style")
  expect_error(
    rr_layer("lines", "vector", tiles = function(z, x, y) raw(0), style = shade_style, shade = TRUE),
    "`hillshade` style"
  )
  expect_error(rr_layer("relief", "dem", tiles = dem, style = shade_style, shade = list(method = "sun")), "method")
  expect_error(rr_layer("relief", "dem", tiles = dem, style = shade_style, shade = list(altitude = 95)), "altitude")
  expect_error(rr_layer("relief", "dem", tiles = dem, style = shade_style, shade = list(shadow = "black")), "shadow")
  expect_error(rr_layer("relief", "dem", tiles = dem, style = shade_style, shade = list(sun = 1)), "TRUE or a list")

  # a choice may not take a name the relief's controls write into the address
  expect_error(
    rr_map(
      rr_section("g", "G", list(l)),
      choose = list(fields = list(list(field = "relief_dir", options = list(list(value = "a", match = "a")),
                                       layers = "relief")))
    ),
    "relief_dir"
  )
})

test_that("a dem layer's colour controls: defaults filled in, each value checked, the palette the map's, names taken", {
  dem <- function(z, x, y) NULL
  colour_style <- list(list(id = "depth-colour", type = "color-relief"))
  l <- rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = TRUE)
  expect_equal(
    l$config$colour[c("of", "floor", "cap", "scale", "mode", "cut", "cut_as", "opacity")],
    list(of = "depth", floor = 10, cap = 2000, scale = "log", mode = "ramp", cut = 0, cut_as = "clear", opacity = 100)
  )
  expect_null(l$config$colour[["palette"]]) # the map settles it
  expect_equal(l$config$colour$bands, list(10, 20, 50, 100, 200, 500, 1000, 2000)) # an array in the config's JSON

  # the palettes it offers: for a depth the seven looks, quiet first; for an elevation every palette; a palette given
  # outside the list put first in it; one the map lacks refused
  offered <- function(colour, ...) {
    m <- rr_map(rr_section("g", "G", list(rr_layer("depth", "dem", tiles = dem, style = colour_style,
                                                         colour = colour))), ...)
    co <- m$config$layers[[1]]$colour
    list(palette = co[["palette"]], palettes = unlist(co$palettes))
  }
  looks <- c("quiet", "seabed", "deep", "chart", "grey", "drained", "haxby")
  expect_equal(offered(TRUE), list(palette = "quiet", palettes = looks))
  expect_equal(offered(list(palette = "chart")), list(palette = "chart", palettes = looks))
  expect_equal(offered(list(palette = "magma")), list(palette = "magma", palettes = c("magma", looks)))
  expect_equal(offered(list(palettes = c("haxby", "magma"))), list(palette = "haxby", palettes = c("haxby", "magma")))
  expect_equal(offered(list(of = "elevation"))$palettes, vapply(rr_palettes(), `[[`, "", "id"))
  expect_error(offered(list(palettes = c("quiet", "rainbow"))), "rainbow is not among the map's `palettes`")
  expect_error(offered(list(palettes = c("quiet", "quiet"))), "each once")
  # one of your own, added to the map's; a palette given again under an id replaces the earlier, in its place
  mine <- list(list(id = "rainbow", label = "rainbow", colors = grDevices::rainbow(9)))
  expect_equal(offered(list(palette = "rainbow"), palettes = c(rr_palettes(), mine))$palettes, c("rainbow", looks))
  quiet2 <- list(list(id = "quiet", label = "quiet, mine", colors = c("#ffffff", "#000000")))
  p <- rr_map(rr_section("g", "G", list(l)), palettes = c(rr_palettes(), quiet2))$config$palettes
  expect_length(p, 18)
  expect_equal(p[[13]]$label, "quiet, mine")
  # the grid's palettes and a depth's, by `use`
  use <- vapply(rr_palettes(), function(p) paste(p$use, collapse = "+"), "")
  expect_equal(unname(use[vapply(rr_palettes(), `[[`, "", "id") %in% looks]),
               c("grid+depth", rep("depth", 6)))
  expect_equal(sum(grepl("grid", use)), 12)
  given <- rr_layer("depth", "dem", tiles = dem, style = colour_style,
                        colour = list(palette = "blues", cut = 20, cut_as = "grey", mode = "bands"))$config$colour
  expect_equal(given[c("palette", "cut", "cut_as", "mode")], list(palette = "blues", cut = 20, cut_as = "grey", mode = "bands"))

  shade_style <- list(list(id = "relief-shade", type = "hillshade"))
  expect_error(rr_layer("relief", "dem", tiles = dem, style = shade_style, colour = TRUE), "`color-relief` style")
  expect_error(rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(cut = 15)), "cut")
  expect_error(rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(bands = c(50, 20))), "bands")
  expect_error(rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(scale = "sqrt")), "scale")
  expect_error(rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(floor = 0)), "floor")
  expect_error(rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(hue = 1)), "TRUE or a list")

  expect_error(
    rr_map(rr_section("g", "G", list(rr_layer("depth", "dem", tiles = dem, style = colour_style,
                                                    colour = list(palette = "rainbow"))))),
    "palette"
  )
  expect_error(
    rr_map(
      rr_section("g", "G", list(l)),
      choose = list(fields = list(list(field = "depth_cut", options = list(list(value = "a", match = "a")),
                                       layers = "depth")))
    ),
    "depth_cut"
  )
})

test_that("a contour layer: its intervals by zoom checked and kept as an object, its tiles served as images", {
  skip_if_not_installed("jsonlite")
  png1 <- as.raw(1:4)
  tiles <- structure(function(z, x, y) if (z == 1) png1 else NULL, format = "png", encoding = "terrarium", maxzoom = 1L)
  lines <- list(list(id = "contours-line", type = "line", "source-layer" = "contours"))
  l <- rr_layer("contours", "contour", tiles = tiles, style = lines,
                    contours = list(thresholds = list("9" = c(50, 200), "6" = c(500, 1000)), multiplier = -1))
  expect_equal(names(l$config$contours$thresholds), c("6", "9")) # in zoom order
  expect_equal(l$config$contours$multiplier, -1)
  expect_equal(l$config[c("format", "encoding", "maxzoom")], list(format = "png", encoding = "terrarium", maxzoom = 1L))
  cfg <- jsonlite::fromJSON(.rr_map_app(rr_map(rr_section("g", "G", list(l))))$call(
    list(REQUEST_METHOD = "GET", PATH_INFO = "/config.json", QUERY_STRING = "")
  )$body, simplifyVector = FALSE)
  expect_equal(cfg$layers[[1]]$contours$thresholds, list("6" = list(500, 1000), "9" = list(50, 200)))

  expect_error(rr_layer("contours", "contour", tiles = tiles, style = lines), "`contours` is a list of `thresholds`")
  expect_error(
    rr_layer("contours", "contour", tiles = tiles, style = lines, contours = list(thresholds = list("x" = 100))),
    "thresholds"
  )
  expect_error(
    rr_layer("contours", "contour", tiles = tiles, style = lines, contours = list(thresholds = list("6" = c(500, 100)))),
    "thresholds"
  )
  expect_error(
    rr_layer("contours", "contour", tiles = tiles, style = lines,
                 contours = list(thresholds = list("6" = 100), multiplier = 0)),
    "multiplier"
  )
  dem <- list(list(id = "d", type = "hillshade"))
  expect_error(rr_layer("d", "dem", tiles = tiles, style = dem, contours = list(thresholds = list("6" = 100))),
               "for a contour layer")

  app <- .rr_map_app(rr_map(rr_section("g", "G", list(l))))
  ok <- get(app, "/tiles/contours/1/0/0.png")
  expect_equal(ok$status, 200L)
  expect_equal(ok$headers$`Content-Type`, "image/png")
  expect_equal(get(app, "/tiles/contours/2/0/0.png")$status, 404L)
  expect_equal(get(app, "/tiles/contours/1/0/0.pbf")$status, 404L)
})

test_that("a dem layer's colours fitted to the view: the server asks the layer's fit for the view's box", {
  skip_if_not_installed("jsonlite")
  dem <- function(z, x, y) NULL
  colour_style <- list(list(id = "depth-colour", type = "color-relief"))
  seen <- NULL
  fit <- function(west, south, east, north) {
    seen <<- c(west, south, east, north)
    if (west < -100) NULL else c(12.5, 480)
  }
  l <- rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(fit = fit))
  expect_true(l$config$colour$fit) # the page's button; the function stays on the server
  expect_true(is.function(l$fit))
  app <- .rr_map_app(rr_map(rr_section("g", "G", list(l))))
  r <- get(app, "/fit/depth", "bbox=-25.5,63.5,-20,65")
  expect_equal(r$status, 200L)
  expect_equal(jsonlite::fromJSON(r$body), list(floor = 12.5, cap = 480))
  expect_equal(seen, c(-25.5, 63.5, -20, 65))
  expect_equal(get(app, "/fit/depth", "bbox=-120,63.5,-110,65")$status, 404L) # no values there
  expect_equal(get(app, "/fit/depth", "bbox=-20,63.5,-25,65")$status, 400L) # west east of east
  expect_equal(get(app, "/fit/depth", "")$status, 400L)
  expect_equal(get(app, "/fit/other", "bbox=-25,63,-20,65")$status, 404L)
  expect_error(rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(fit = 3)), "a function")
  expect_null(rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = TRUE)$config$colour$fit)
})

test_that("a dem layer's colours follow the view by default, and its address name is taken", {
  dem <- function(z, x, y) NULL
  colour_style <- list(list(id = "depth-colour", type = "color-relief"))
  expect_true(rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = TRUE)$config$colour$auto)
  expect_false(rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(auto = FALSE))$config$colour$auto)
  expect_error(rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(auto = "yes")), "auto")
  l <- rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = TRUE)
  expect_error(
    rr_map(rr_section("g", "G", list(l)),
           choose = list(fields = list(list(field = "depth_auto", options = list(list(value = "a", match = "a")),
                                            layers = "depth")))),
    "depth_auto"
  )
})

test_that("a fit may give the values' quantiles in view, for colours in equal shares", {
  skip_if_not_installed("jsonlite")
  dem <- function(z, x, y) NULL
  colour_style <- list(list(id = "depth-colour", type = "color-relief"))
  fit <- function(west, south, east, north) {
    if (west < -100) list(floor = 1, cap = 9, quantiles = c(5, 3)) else list(floor = 3, cap = 80, quantiles = c(3, 20, 40, 80))
  }
  l <- rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(fit = fit, scale = "equal"))
  expect_equal(l$config$colour$scale, "equal")
  app <- .rr_map_app(rr_map(rr_section("g", "G", list(l))))
  r <- get(app, "/fit/depth", "bbox=-25,63,-20,65")
  expect_equal(r$status, 200L)
  expect_equal(jsonlite::fromJSON(r$body, simplifyVector = FALSE), list(floor = 3, cap = 80, q = list(3, 20, 40, 80)))
  expect_equal(get(app, "/fit/depth", "bbox=-120,63,-110,65")$status, 500L) # quantiles not rising
  expect_error(rr_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(scale = "equal")),
               "needs `colour\\$fit`")
})

test_that("a contour layer's levels from the view: its fit kept on the server, following by default, its name taken", {
  skip_if_not_installed("jsonlite")
  tiles <- structure(function(z, x, y) NULL, format = "png")
  lines <- list(list(id = "contours-line", type = "line", "source-layer" = "contours"))
  fit <- function(west, south, east, north) list(floor = 3, cap = 80, quantiles = c(3, 20, 40, 80))
  l <- rr_layer("contours", "contour", tiles = tiles, style = lines,
                    contours = list(thresholds = list("5" = 100), multiplier = -1, fit = fit))
  expect_true(l$config$contours$fit)
  expect_true(l$config$contours$auto)
  expect_true(is.function(l$fit))
  app <- .rr_map_app(rr_map(rr_section("g", "G", list(l))))
  expect_equal(jsonlite::fromJSON(get(app, "/fit/contours", "bbox=-25,63,-20,65")$body)$q, c(3, 20, 40, 80))
  expect_false(rr_layer("contours", "contour", tiles = tiles, style = lines,
                            contours = list(thresholds = list("5" = 100), auto = FALSE))$config$contours$auto)
  expect_null(rr_layer("contours", "contour", tiles = tiles, style = lines,
                           contours = list(thresholds = list("5" = 100)))$config$contours$fit)
  expect_error(rr_layer("contours", "contour", tiles = tiles, style = lines,
                            contours = list(thresholds = list("5" = 100), fit = 1)), "a function")
  expect_error(
    rr_map(rr_section("g", "G", list(l)),
           choose = list(fields = list(list(field = "contours_auto", options = list(list(value = "a", match = "a")),
                                            layers = "contours")))),
    "contours_auto"
  )
})

test_that("a shading's intensity from the view: its fit answered by the route, one fit a layer", {
  skip_if_not_installed("jsonlite")
  dem <- function(z, x, y) NULL
  shade_style <- list(list(id = "relief-shade", type = "hillshade"))
  fit <- function(west, south, east, north) if (west < -100) list(exaggeration = 2) else list(exaggeration = 0.45)
  l <- rr_layer("relief", "dem", tiles = dem, style = shade_style, shade = list(exaggeration = 0.6, fit = fit))
  expect_true(l$config$shade$fit)
  expect_true(l$config$shade$auto)
  expect_true(is.function(l$fit))
  app <- .rr_map_app(rr_map(rr_section("g", "G", list(l))))
  expect_equal(jsonlite::fromJSON(get(app, "/fit/relief", "bbox=-25,63,-20,65")$body), list(exaggeration = 0.45))
  expect_equal(get(app, "/fit/relief", "bbox=-120,63,-110,65")$status, 500L) # out of 0-1
  both <- list(list(id = "c", type = "color-relief"), list(id = "s", type = "hillshade"))
  expect_error(rr_layer("d", "dem", tiles = dem, style = both, shade = list(fit = fit), colour = list(fit = fit)),
               "one fit")
  expect_error(rr_layer("relief", "dem", tiles = dem, style = shade_style, shade = list(fit = "x")), "a function")
})

test_that("a bundle: the page, its data and tiles as scripts, the tiles once for layers on one file", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")
  f <- tempfile(fileext = ".mbtiles")
  d <- tempfile("bundle_")
  on.exit(unlink(c(f, d), recursive = TRUE))
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  DBI::dbWriteTable(con, "metadata", data.frame(
    name = c("format", "encoding", "bounds", "minzoom", "maxzoom"),
    value = c("png", "terrarium", "-30,60,-5,70", "0", "2")
  ))
  DBI::dbExecute(con, "CREATE TABLE tiles (zoom_level INTEGER, tile_column INTEGER, tile_row INTEGER, tile_data BLOB)")
  # the tiles inside the bounds: zoom 0 the world, zoom 1 its north-west, zoom 2 two (rows counted from the south)
  for (t in list(c(0, 0, 0), c(1, 0, 1), c(2, 1, 3), c(2, 1, 2))) {
    DBI::dbExecute(con, "INSERT INTO tiles VALUES (?, ?, ?, ?)", params = list(t[1], t[2], t[3], list(as.raw(c(t, 9)))))
  }
  DBI::dbDisconnect(con)
  expect_equal(attr(rr_tiles_mbtiles(f), "source"), normalizePath(f))

  # two layers on one file, each from its own reader, and a geojson layer
  depth <- rr_layer("depth", "dem", tiles = rr_tiles_mbtiles(f), style = list(list(id = "d", type = "hillshade")))
  relief <- rr_layer("relief", "dem", tiles = rr_tiles_mbtiles(f), style = list(list(id = "r", type = "hillshade")))
  gj <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"name":"Þórshöfn"},"geometry":{"type":"Point","coordinates":[-15.3,66.2]}}]}'
  places <- rr_layer("places", "geojson", data = gj, style = list(list(id = "p", type = "circle")))
  m <- rr_map(rr_section("g", "G", list(depth, relief, places)))
  out <- rr_bundle(m, d, maxzoom = 1)
  expect_equal(attr(out, "tiles"), 2L) # zooms 0 and 1, once for both layers
  expect_true(attr(out, "bytes") > 0)
  # one archive, for both layers on the file: a PNG's, to zoom 1 (none past maxzoom), the tiles as the file holds them
  expect_equal(list.files(file.path(d, "tiles")), "depth.pmtiles.js")
  a <- pm_archive(d, "depth")
  expect_equal(a$header$magic, "PMTiles")
  expect_equal(a$header$version, 3L)
  expect_equal(a$header$tile_type, 2L)
  expect_equal(c(a$header$minzoom, a$header$maxzoom), c(0L, 1L))
  expect_equal(a$header$bounds, c(-30, 60, -5, 70))
  expect_equal(nrow(a$entries), 2)
  expect_identical(pm_tile(a, 0, 0, 0), as.raw(c(0, 0, 0, 9)))
  expect_identical(pm_tile(a, 1, 0, 0), as.raw(c(1, 0, 1, 9)))
  expect_null(pm_tile(a, 2, 1, 0))
  expect_null(a$meta$vector_layers) # an image's archive names no vector layers
  data <- paste(readLines(file.path(d, "data", "places.js"), encoding = "UTF-8"), collapse = "\n")
  expect_match(data, '^RB_BUNDLE\\("data/places",\\{')
  expect_match(data, "Þórshöfn", fixed = TRUE)

  page <- paste(readLines(file.path(d, "index.html"), encoding = "UTF-8"), collapse = "\n")
  block <- function(id) {
    jsonlite::fromJSON(
      sub(sprintf('(?s).*<script id="%s" type="application/json">(.*?)</script>.*', id), "\\1", page, perl = TRUE),
      simplifyVector = FALSE
    )
  }
  config <- block("config")
  bundle <- block("bundle")
  expect_equal(vapply(config$layers, `[[`, "", "id"), c("depth", "relief", "places"))
  expect_equal(config$layers[[1]]$maxzoom, 1L) # the map draws zoom 1's tiles larger past it
  expect_equal(bundle$tiles, list(depth = "depth", relief = "depth"))
  expect_equal(bundle$fit$probs, list(0.02, 0.98))
  expect_equal(bundle$fit$slope$k, 2)
  expect_equal(names(bundle$meta$layers), c("depth", "relief"))
  expect_false(grepl("__BUNDLE__", page, fixed = TRUE))
  # the page's libraries travel with it
  expect_setequal(
    list.files(file.path(d, "vendor")),
    list.files(system.file("engine", "vendor", package = "rebelrebel"))
  )
  expect_false(grepl("cdn.jsdelivr", page, fixed = TRUE))
  # the server's page has none
  expect_match(.rr_map_page("{}"), '<script id="bundle" type="application/json">null</script>', fixed = TRUE)

  # a bundle is written over only when asked, and then only its own files go
  writeLines("the caller's", file.path(d, "README.md"))
  expect_error(rr_bundle(m, d), "holds a bundle")
  rr_bundle(m, d, maxzoom = 2, overwrite = TRUE)
  expect_true(file.exists(file.path(d, "README.md")))
  a <- pm_archive(d, "depth")
  expect_identical(pm_tile(a, 2, 1, 0), as.raw(c(2, 1, 3, 9)))
  expect_identical(pm_tile(a, 2, 1, 1), as.raw(c(2, 1, 2, 9)))

  # its arguments checked
  expect_error(rr_bundle(list(), d), "rr_map")
  expect_error(rr_bundle(m, d, maxzoom = 1.5, overwrite = TRUE), "whole number")
  expect_error(rr_bundle(m, d, overwrite = NA), "TRUE or FALSE")
  expect_error(rr_bundle(m, d, fit = list(probs = c(0.9, 0.1)), overwrite = TRUE), "probs out of range")
  expect_error(rr_bundle(m, d, fit = list(slope = list(q = 1)), overwrite = TRUE), "a list of any of")
  expect_error(rr_bundle(m, d, fit = list(n = 1), overwrite = TRUE), "n out of range")
  v <- .rr_map_bundle_fit(list(nodata = NULL, slope = list(k = 3)))
  expect_null(v$nodata) # no value left out
  expect_equal(v$slope, list(k = 3, p = 0.9, min = 0.3, max = 1))
  no_bounds <- rr_layer("depth", "dem", tiles = function(z, x, y) NULL, maxzoom = 2,
                            style = list(list(id = "d", type = "hillshade")))
  expect_error(rr_bundle(rr_map(rr_section("g", "G", list(no_bounds))), tempfile()), "needs its `bounds`")
})

test_that("a bundle leaves out the layers a server draws, with a message, and keeps a table's tiles", {
  skip_if_not_installed("jsonlite")
  skip_if_no_spatial()
  d <- tempfile("bundle_")
  on.exit(unlink(d, recursive = TRUE))
  expect_message(rr_bundle(example_map(), d, maxzoom = 8), "Left out of the bundle, as their tiles are made by a server: pings[.]")
  page <- paste(readLines(file.path(d, "index.html"), encoding = "UTF-8"), collapse = "\n")
  config <- jsonlite::fromJSON(
    sub('(?s).*<script id="config" type="application/json">(.*?)</script>.*', "\\1", page, perl = TRUE),
    simplifyVector = FALSE
  )
  expect_equal(vapply(config$layers, `[[`, "", "id"), c("harbours", "outline"))
  expect_equal(vapply(config$groups, `[[`, "", "id"), "ref") # the grid's group, left empty, is gone
  expect_true(file.exists(file.path(d, "data", "outline.js")))
  # the harbours' tiles: only those that hold one, to zoom 8, the page drawing zoom 8's larger past it
  expect_equal(config$layers[[1]]$maxzoom, 8L)
  at <- attr(rr_example("dansk_harbours") |> rr_tiles_table(properties = "SI_HARB"), "tiles_at")(8)
  expect_true(nrow(at) > 0)
  a <- pm_archive(d, "harbours")
  expect_equal(a$header$tile_type, 1L) # vector tiles
  expect_equal(a$meta$vector_layers$id, "features")
  expect_equal(a$header$maxzoom, 8L)
  expect_setequal(pm_ids_at(a, 8), .rr_pmtiles_id(rep(8, nrow(at)), at$x, at$y))
  m <- example_map()
  for (i in c(1, nrow(at))) expect_identical(pm_tile(a, 8, at$x[i], at$y[i]), m$layers$harbours$tiles(8, at$x[i], at$y[i]))
  expect_equal(list.files(file.path(d, "tiles")), "harbours.pmtiles.js")
})

test_that("a PMTiles archive: tile ids as the reference reader's, leaf directories, each tile as given", {
  # ids from pmtiles.js 4.5.0's zxyToTileId(), the reader the page loads
  z <- c(0, 1, 1, 1, 1, 2, 2, 7, 8, 14, 20)
  x <- c(0, 0, 0, 1, 1, 0, 3, 68, 130, 8700, 524287)
  y <- c(0, 0, 1, 1, 0, 0, 1, 39, 70, 5100, 0)
  expect_equal(.rr_pmtiles_id(z, x, y), c(0, 1, 2, 3, 4, 5, 17, 19428, 77799, 318331125, 458129844906))
  # more tiles than a leaf directory holds (4,096) under a small root: leaf directories, each tile found, given in no
  # order
  set.seed(1)
  at <- expand.grid(x = 0:69, y = 0:69)
  at <- at[sample(nrow(at)), ]
  data <- lapply(seq_len(nrow(at)), function(i) as.raw(c(i %% 256, i %/% 256, sample(0:255, i %% 7))))
  f <- tempfile(fileext = ".pmtiles")
  on.exit(unlink(f))
  .rr_pmtiles_write(rep(7, nrow(at)), at$x, at$y, data, f, c(-10, 50, 10, 60), type = "webp", name = "t", root_max = 50)
  a <- pm_archive(f)
  expect_true(a$leaves > 1)
  expect_equal(a$header$tile_type, 4L)
  expect_equal(nrow(a$entries), nrow(at))
  for (i in c(1, 2, 500, 4097, nrow(at))) expect_identical(pm_tile(a, 7, at$x[i], at$y[i]), data[[i]])
  expect_null(pm_tile(a, 6, 0, 0))
  expect_null(pm_tile(a, 7, 100, 100))
})

test_that("a bundle's grid: its rows summed by cell at each level, in blocks the page reads", {
  skip_if_not_installed("jsonlite")
  rows <- data.frame(
    x = c(0L, 1L, 2L, -1L, 300L), y = c(0L, 0L, 1L, -1L, 200L),
    behaviour = c("F", "F", "O", "F", NA), n = c(1, 2, 4, 8, 16), h = c(0.5, 0.5, 1, 1, 2)
  )
  grid <- rr_layer(
    "cells", "grid", label = "Cells", tiles = function(z, x, y, p) raw(0),
    meta = list(options = list(behaviour = data.frame(value = c("F", "O"))), v = 1),
    filters = list(
      list(id = "behaviour", label = "Behaviour", options = list(list(value = "", label = "all")), meta = TRUE),
      list(id = "speed", label = "Speed", options = list(list(value = "any"), list(value = "slow"))),
      list(id = "metric", type = "metric", label = "Metric")
    ),
    metrics = list(list(field = "n", label = "count"), list(field = "h_km2", label = "hours per km2"))
  )
  m <- rr_map(rr_section("g", "G", list(grid)))
  spec <- list(rows = rows, unit = 0.01, levels = c(1, 2), columns = c(behaviour = "behaviour"), sums = c("n", "h"),
               fields = list(n = "n", h_km2 = c("h", "km2")))
  d <- tempfile("bundle_")
  on.exit(unlink(d, recursive = TRUE))
  expect_message(rr_bundle(m, d, grids = list(cells = spec)), "left out of the bundle, as its rows have no column for them: speed")
  page <- paste(readLines(file.path(d, "index.html"), encoding = "UTF-8"), collapse = "\n")
  block <- function(id) {
    jsonlite::fromJSON(
      sub(sprintf('(?s).*<script id="%s" type="application/json">(.*?)</script>.*', id), "\\1", page, perl = TRUE),
      simplifyVector = FALSE
    )
  }
  config <- block("config")
  g <- block("bundle")$grids$cells
  expect_equal(vapply(config$layers[[1]]$filters, `[[`, "", "id"), c("behaviour", "metric"))
  expect_equal(unlist(g$levels), c(1, 2))
  expect_equal(g$block, 128)
  expect_equal(g$columns[[1]][c("id", "range", "bytes")], list(id = "behaviour", range = FALSE, bytes = 1))
  expect_equal(g$columns[[1]]$values, list("F", "O", NULL)) # NA last, as null
  expect_equal(g$fields, list(n = list("n"), h_km2 = list("h", "km2")))
  expect_setequal(unlist(g$blocks[["1"]]), c("0_0", "-1_-1", "2_1"))
  expect_setequal(unlist(g$blocks[["2"]]), c("0_0", "-1_-1", "1_0"))

  # a block read as the page reads it: rows, each sum (float32), the cell in the block (uint16), the codes (uint8)
  read_block <- function(key, sums = 2) {
    js <- readLines(file.path(d, "grids", "cells", paste0(key, ".js")))
    expect_match(js, sprintf('^RB_BUNDLE\\("grids/cells/%s",', key))
    bytes <- memDecompress(jsonlite::base64_dec(sub('^RB_BUNDLE\\("[^"]+","(.*)"\\);$', "\\1", js)), "gzip")
    con <- rawConnection(bytes)
    on.exit(close(con))
    n <- readBin(con, "integer", 1, size = 4, endian = "little")
    s <- lapply(seq_len(sums), function(i) readBin(con, "double", n, size = 4, endian = "little"))
    cell <- readBin(con, "integer", n, size = 2, signed = FALSE, endian = "little")
    code <- readBin(con, "integer", n, size = 1, signed = FALSE)
    data.frame(cell = cell, code = code, n = s[[1]], h = s[[2]])
  }
  b <- read_block("2/0_0")
  # level 2: x 0 and 1 in cell 0 (F, n 1 + 2), x 2 at y 1 in cell 1 (O, n 4)
  expect_equal(b[order(b$cell), ], data.frame(cell = c(0L, 1L), code = c(0L, 1L), n = c(3, 4), h = c(1, 1)),
               ignore_attr = TRUE)
  b <- read_block("1/-1_-1") # x -1, y -1: the block to the south-west, its north-east corner
  expect_equal(b$cell, 127L * 128L + 127L)
  b <- read_block("1/2_1") # x 300, y 200, no behaviour: block 2, 1, its code the last
  expect_equal(b[c("cell", "code", "n")], data.frame(cell = (200L - 128L) * 128L + (300L - 256L), code = 2L, n = 16))

  # its checks
  bad <- function(msg, ...) {
    g <- spec
    v <- list(...)
    g[names(v)] <- v
    expect_error(rr_bundle(m, tempfile(), grids = list(cells = g)), msg)
  }
  bad("rising", levels = c(2, 1))
  bad("metric h_km2", fields = list(n = "n"))
  bad("`fields`", fields = list(n = "x", h_km2 = c("h", "km2")))
  bad("names the layer's filters", columns = c(colour = "behaviour"))
  bad("`sums`", sums = "speed")
  expect_error(rr_bundle(m, tempfile(), grids = list(other = spec)), "names no grid layer")
  expect_message(rr_bundle(m, tempfile()), "made by a server: cells")
})

# The time panel (rr_time()) -------------------------------------------------------------------------------------

time_pings <- function() {
  d <- dplyr::collect(rr_example("dansk"))
  d <- d[
    d$vessel_id %in% c("EX_1", "EX_12"),
    c("vessel_id", "time_stamp", "lon", "lat", "speed", "course", "behaviour")
  ]
  names(d)[1:2] <- c("vid", "time")
  d$flag <- ifelse(d$speed > 5, "fast", NA)
  d
}

test_that("a time panel: each vessel and its spans in the config, a span's series from /time", {
  skip_if_no_spatial()
  d <- time_pings()
  m <- rr_time(
    d,
    c(speed, course),
    by = vid,
    marks = flag,
    lanes = list(behaviour = "behaviour"),
    span_max = 10000
  )
  tp <- m$config$time
  expect_equal(vapply(tp$vessels, `[[`, "", "value"), c("EX_1", "EX_12"))
  expect_equal(vapply(tp$cols, `[[`, "", "field"), c("speed", "course"))
  # a mark's label: its column's, or the name it is given
  expect_equal(tp$marks, list(list(field = "flag", label = "flag")))
  d$fast <- d$speed > 8
  named <- rr_time(d, speed, by = vid, marks = c(slow = flag, quick = "fast"))$config$time$marks
  expect_equal(named, list(list(field = "flag", label = "slow"), list(field = "fast", label = "quick")))
  expect_equal(rr_time(d, speed, by = vid, marks = c(flag, quick = fast))$config$time$marks[[1]]$label, "flag")
  expect_equal(tp$lanes[[1]]$label, "behaviour")
  # a vessel with more than span_max pings is offered by month only, its latest month first
  ex12 <- tp$vessels[[2]]
  expect_equal(ex12$n, sum(d$vid == "EX_12"))
  expect_false("all" %in% vapply(ex12$spans, `[[`, "", "value"))
  expect_equal(ex12$span, "2014-12")
  expect_equal(tp$vessels[[1]]$span, "all")
  app <- .rr_map_app(m)
  r <- get(app, "/time", "v=EX_1&span=all")
  expect_equal(r$status, 200L)
  s <- jsonlite::fromJSON(r$body)
  e1 <- d[d$vid == "EX_1", ]
  e1 <- e1[order(e1$time), ]
  expect_equal(length(s$t), nrow(e1))
  expect_equal(s$t, as.numeric(e1$time))
  expect_equal(s$y$speed, signif(e1$speed, 6))
  expect_true(any(!is.na(e1$flag)))
  expect_equal(s$marks$flag$i, which(!is.na(e1$flag)) - 1L)
  expect_equal(unique(s$marks$flag$v), "fast")
  # the lane: runs of one value, broken by a silence of over an hour, each interval within the pings' times
  l <- s$lanes[["1"]]
  expect_true(length(l$t0) > 1)
  expect_true(
    all(l$t0 <= l$t1) && min(l$t0) >= min(s$t) && max(l$t1) <= max(s$t)
  )
  expect_setequal(unique(l$label), unique(e1$behaviour))
  # no gap breaks the panel's line or a lane's run by default; max_gap_s does, the panel's own or the track's
  expect_null(tp$max_gap_s)
  m2 <- rr_time(d, speed, by = vid, lanes = list(behaviour = "behaviour"), max_gap_s = 3600)
  expect_equal(m2$config$time$max_gap_s, 3600)
  l2 <- jsonlite::fromJSON(get(.rr_map_app(m2), "/time", "v=EX_1&span=all")$body)$lanes[["1"]]
  expect_gt(length(l2$t0), length(l$t0))
  t1 <- as.numeric(e1$time)
  inside <- function(a, b) max(c(0, diff(t1[t1 >= a & t1 <= b])))
  expect_true(all(mapply(inside, l2$t0, l2$t1) <= 3600))
  expect_gt(max(mapply(inside, l$t0, l$t1)), 3600)
  expect_equal(
    (d |> rr_path(group = vid, time = time, max_gap_s = 1800) |> rr_time(speed))$config$time$max_gap_s,
    1800
  )
  expect_equal(
    (d |> rr_path(group = vid, time = time, max_gap_s = 1800) |> rr_time(speed, max_gap_s = 60))$config$time$max_gap_s,
    60
  )
  expect_error(rr_time(d, speed, by = vid, max_gap_s = -1), "number of seconds")
  # a month's span, and the refusals
  r <- get(app, "/time", "v=EX_12&span=2014-12")
  expect_equal(r$status, 200L)
  expect_equal(
    length(jsonlite::fromJSON(r$body)$t),
    sum(d$vid == "EX_12" & format(d$time, "%Y-%m") == "2014-12")
  )
  expect_equal(get(app, "/time", "v=nope&span=all")$status, 400L)
  expect_match(
    get(app, "/time", "v=EX_12&span=all")$body,
    "span is one of 2014-12"
  )
  expect_equal(get(app, "/time", "")$status, 400L)
  expect_equal(
    get(.rr_map_app(rr_point(d)), "/time", "v=EX_1&span=all")$status,
    404L
  )
})

test_that("a time panel pipes with the layers, either way round, one to a map", {
  skip_if_no_spatial()
  d <- time_pings()
  m <- d |> rr_path(group = vid, time = time, colour = behaviour) |> rr_time(speed) |> rr_point(colour = speed)
  expect_equal(vapply(m$config$layers, `[[`, "", "id"), c("paths", "points"))
  expect_false(is.null(m$config$time))
  expect_true(is.function(m$time$series))
  # the panel's tables go with the map's, dropped when it is replaced on its port
  expect_true(all(
    vapply(m$time$tables, `[[`, "", "name") %in%
      vapply(attr(m, "tables"), `[[`, "", "name")
  ))
  expect_error(rr_time(m, speed), "one time panel")
  expect_match(format(m)[3], "time panel: speed over time, 2 groups")
  # vessels in natural order where their keys are not all numbers
  expect_equal(order(.rr_natural_key(c("EX_10", "EX_2", "EX_1"))), c(3L, 2L, 1L))
  # a map of the panel alone
  m0 <- rr_time(d, speed, by = vid)
  expect_length(m0$config$layers, 0)
  expect_equal(m0$config$time$cols[[1]]$field, "speed")
})

test_that("a time panel's lanes from interval tables, its vessel keys matched across number types", {
  skip_if_no_spatial()
  d <- time_pings()
  d$v <- as.numeric(factor(d$vid)) # 1 and 2, as doubles
  first <- c(min(d$time[d$v == 1]), min(d$time[d$v == 2]))
  calls <- data.frame(
    v = c(1L, 2L),
    T_in = first,
    T_out = first + 1800,
    port = c("a", "b")
  )
  voyages <- data.frame(
    v = 2L,
    start = as.POSIXct("2014-12-08", tz = "UTC"),
    end = as.POSIXct("2014-12-09", tz = "UTC")
  )
  m <- rr_time(
    d,
    speed,
    by = v,
    lanes = list(calls = list(data = calls, start = "T_in", end = "T_out", label = "port"), voyages = voyages)
  )
  app <- .rr_map_app(m)
  s1 <- jsonlite::fromJSON(get(app, "/time", "v=1&span=all")$body)
  expect_equal(s1$lanes[["1"]]$label, "a")
  s2 <- jsonlite::fromJSON(get(app, "/time", "v=2&span=all")$body)
  expect_equal(s2$lanes[["2"]]$t0, as.numeric(voyages$start))
  expect_error(
    rr_time(d, speed, by = vid, lanes = list(x = data.frame(v = 1, a = Sys.time()))),
    "no start and end"
  )
  expect_error(rr_time(d, speed, by = vid, lanes = list(x = "nope")), "no column nope")
  expect_error(rr_time(d, speed, by = vid, lanes = list("behaviour")), "named list")
  expect_error(rr_time(d, behaviour, by = vid), "numeric")
  expect_error(rr_time(d[, c("vid", "lon", "lat", "speed")], speed, by = vid), "time column")
})

test_that("a time panel's spans: the whole series of raw pings, or a column's runs or a table's intervals", {
  skip_if_no_spatial()
  d <- time_pings()
  # raw pings, nothing but time and position: each vessel's whole series alone, no months, no list in the page
  tp <- rr_time(d, speed, by = vid)$config$time
  expect_equal(
    lapply(tp$vessels, function(v) vapply(v$spans, `[[`, "", "value")),
    list("all", "all")
  )
  expect_null(tp$span_label)
  # a column's runs of one value: numbered in time order, named by value and start, a span holding every ping of its
  # vessel from its start to its end; the column's name, or the name given, labels them
  d$haul <- ifelse(d$behaviour == "Fishing", paste0("h", cumsum(c(1, diff(d$behaviour == "Fishing") == 1))), NA)
  m <- rr_time(d, speed, by = vid, span = c(Haul = "haul"))
  tp <- m$config$time
  expect_equal(tp$span_label, "Haul")
  expect_equal(rr_time(d, speed, by = vid, span = "haul")$config$time$span_label, "haul")
  e1 <- d[d$vid == "EX_1", ]
  e1 <- e1[order(e1$time), ]
  runs <- rle(ifelse(is.na(e1$haul), "", e1$haul))
  n_runs <- sum(runs$values != "")
  sp <- tp$vessels[[1]]$spans
  expect_equal(vapply(sp, `[[`, "", "value"), c("all", sprintf("s%d", seq_len(n_runs))))
  first <- which(!is.na(e1$haul))[1]
  last <- first + runs$lengths[which(runs$values != "")[1]] - 1
  expect_equal(
    sp[[2]]$name,
    sprintf("%s · %s", e1$haul[first], format(e1$time[first], "%Y-%m-%d %H:%M"))
  )
  s <- jsonlite::fromJSON(get(.rr_map_app(m), "/time", "v=EX_1&span=s1")$body)
  expect_equal(s$t, as.numeric(e1$time[first:last]))
  expect_equal(sp[[2]]$n, length(s$t))
  expect_match(format(m)[3], "spans: Haul")
  # one span per value: an id missing on a ping inside its unit, as on one a speed filter removed, leaves the unit one
  # span, and that ping in it to be seen
  d2 <- d
  mid <- which(d2$vid == "EX_1" & d2$time == e1$time[first + 1])
  d2$haul[mid] <- NA
  sp2 <- rr_time(d2, speed, by = vid, span = "haul")$config$time$vessels[[1]]$spans
  expect_equal(length(sp2), length(sp))
  s2 <- jsonlite::fromJSON(get(.rr_map_app(rr_time(d2, speed, by = vid, span = "haul")), "/time", "v=EX_1&span=s1")$body)
  expect_equal(s2$t, as.numeric(e1$time[first:last]))
  # a state's values come and go: their spans overlap, which is said
  expect_message(rr_time(d, speed, by = vid, span = "behaviour"), "belongs in `lanes`")
  expect_no_message(rr_time(d, speed, by = vid, span = "haul"))
  # a table's intervals, with a label column; an interval that holds no ping is left out
  tab <- data.frame(
    vid = c("EX_1", "EX_1", "EX_12"),
    start = c(min(e1$time), as.POSIXct("2000-01-01", tz = "UTC"), min(d$time[d$vid == "EX_12"])),
    end = c(min(e1$time) + 3600, as.POSIXct("2000-01-02", tz = "UTC"), max(d$time[d$vid == "EX_12"])),
    trip = c("t1", "none", "t2")
  )
  tp <- rr_time(d, speed, by = vid, span = list(data = tab, label = "trip"))$config$time
  expect_equal(tp$span_label, "Span")
  # a table named as a column is: list(trips = <table or spec>)
  tn <- rr_time(d, speed, by = vid, span = list(trips = list(data = tab, label = "trip")))$config$time
  expect_equal(tn$span_label, "trips")
  expect_equal(tn$vessels[[1]]$spans, tp$vessels[[1]]$spans)
  expect_equal(rr_time(d, speed, by = vid, span = list(trips = tab))$config$time$span_label, "trips")
  expect_equal(vapply(tp$vessels[[1]]$spans, `[[`, "", "value"), c("all", "s1"))
  expect_equal(tp$vessels[[1]]$spans[[2]]$n, sum(e1$time <= min(e1$time) + 3600))
  # a vessel over span_max is offered by its spans alone, the latest first; with none, by month
  tp <- rr_time(d, speed, by = vid, span = list(data = tab, label = "trip"), span_max = 10000)$config$time
  expect_equal(vapply(tp$vessels[[2]]$spans, `[[`, "", "value"), "s1")
  expect_equal(tp$vessels[[2]]$span, "s1")
  expect_error(rr_time(d, speed, by = vid, span = "nope"), "`span`: no column nope")
  # the layers whose features carry the vessel and a time, which the page filters to the vessel and span
  ml <- d |> rr_path(group = vid, time = time, colour = behaviour, whole = FALSE) |> rr_point(colour = speed) |> rr_time(speed)
  expect_equal(
    ml$config$time$layers,
    list(
      list(id = "paths", by = "vid", t0 = "rr_t0", t1 = "rr_t1"),
      list(id = "points", by = "vid", t = "rr_t")
    )
  )
  # their times are the panel's: a track's first and last, a ping's own
  lines <- DBI::dbGetQuery(
    .rr_connection(),
    sprintf(
      "SELECT vid, min(rr_t0) AS t0, max(rr_t1) AS t1 FROM %s GROUP BY vid ORDER BY vid",
      attr(ml$layers$paths$tiles, "table")
    )
  )
  expect_equal(lines$t0, as.numeric(tapply(d$time, d$vid, min)))
  expect_equal(lines$t1, as.numeric(tapply(d$time, d$vid, max)))
})

test_that("a bundle carries the time panel's spans, a file each", {
  skip_if_no_spatial()
  d <- time_pings()
  m <- rr_time(d, speed, by = vid, span_max = 10000)
  dir <- tempfile()
  rr_bundle(m, dir)
  f <- sort(list.files(file.path(dir, "time")))
  # EX_1 held whole, EX_12 over span_max by its one month
  expect_equal(f, c("0_all.js", "1_2014-12.js"))
  js <- readLines(file.path(dir, "time", "0_all.js"), warn = FALSE)
  s <- jsonlite::fromJSON(sub(
    '^RB_BUNDLE\\("time/0_all",(.*)\\);$',
    "\\1",
    paste(js, collapse = "")
  ))
  expect_equal(length(s$t), sum(d$vid == "EX_1"))
  page <- paste(
    readLines(file.path(dir, "index.html"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(page, '"time":\\{', perl = TRUE)
})

test_that("a label on hover: style ids of the layer's own, TRUE for the click's, with a title", {
  data <- '{"type": "FeatureCollection", "features": []}'
  st <- list(list(id = "a-fill", type = "fill"), list(id = "a-line", type = "line"))
  a <- rr_layer("a", "geojson", data = data, style = st,
                    popup = list(click = "a-fill", hover = TRUE, title = "name"))
  expect_equal(a$config$popup$hover, list("a-fill"))
  b <- rr_layer("b", "geojson", data = data, style = st,
                    popup = list(hover = c("a-fill", "a-line"), title = "name", hover_fields = TRUE))
  expect_equal(b$config$popup$hover, list("a-fill", "a-line"))
  expect_null(b$config$popup$click)
  # one id stays an array in the config's JSON
  j <- .rr_map_json(rr_map(rr_section("g", layers = list(a)))$config)
  expect_match(j, '"hover":["a-fill"]', fixed = TRUE)
  expect_error(rr_layer("c", "geojson", data = data, style = st, popup = list(hover = "nope", title = "name")),
               "style ids")
  expect_error(rr_layer("c", "geojson", data = data, style = st, popup = list(hover = "a-fill")), "title")
  expect_error(rr_layer("c", "geojson", data = data, style = st, popup = list(hover = TRUE, title = "name")),
               "click")
  expect_error(rr_layer("c", "geojson", data = data, style = st,
                            popup = list(hover = "a-fill", title = "name", hover_fields = "yes")), "TRUE or FALSE")
  expect_error(rr_layer("c", "geojson", data = data, style = st,
                            popup = list(click = "a-fill", title = "name", hover_fields = TRUE)), "goes with")
  expect_error(rr_layer("c", "dem", tiles = function(z, x, y) NULL,
                            style = list(list(id = "c-shade", type = "hillshade")),
                            popup = list(hover = "c-shade", title = "name")), "vector or geojson")
  # no hover, as before
  expect_null(rr_layer("d", "geojson", data = data, style = st, popup = list(click = "a-fill", title = "x"))$config$popup$hover)
})

# a request to a map's app, as httpuv makes it: a page opened as localhost, its header, and a body to read
notes_call <- function(app, method, path, body = NULL, host = "localhost:8811", page = TRUE, origin = NULL) {
  req <- list(REQUEST_METHOD = method, PATH_INFO = path, QUERY_STRING = "", HTTP_HOST = host)
  if (page) req$HTTP_X_RB_NOTES <- "1"
  req$HTTP_ORIGIN <- origin
  if (!is.null(body)) {
    req$rook.input <- list(read = function(...) charToRaw(enc2utf8(body)))
    req$CONTENT_LENGTH <- as.character(length(charToRaw(enc2utf8(body))))
  }
  app$call(req)
}

test_that("notes from R: a note, a reply at its place, a reply to a reply in the same thread, read back", {
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "notes are written in a UTF-8 locale")
  f <- tempfile(fileext = ".geojson")
  n0 <- rr_notes(f)
  expect_equal(nrow(n0), 0)
  expect_named(n0, c("id", "thread", "reply_to", "time", "author", "text", "lon", "lat", "zoom", "under", "address"))
  expect_equal(rr_notes_add(f, "Er brún Rósagarðs hér?", lon = -14.6, lat = 64.05, author = "reader"), 1L)
  expect_equal(rr_notes_add(f, "Já, við 200 m.", reply_to = 1), 2L)
  expect_equal(rr_notes_add(f, "Og þá?", reply_to = 2, author = "reader"), 3L)
  expect_equal(rr_notes_add(f, "Another place", lon = -20, lat = 65), 4L)
  n <- rr_notes(f)
  expect_equal(n$id, 1:4)
  expect_equal(n$thread, c(1L, 1L, 1L, 4L))
  expect_equal(n$reply_to, c(NA, 1L, 1L, NA))
  expect_equal(n$author, c("reader", "analyst", "reader", "analyst"))
  expect_equal(n$text[1:3], c("Er brún Rósagarðs hér?", "Já, við 200 m.", "Og þá?"))
  expect_equal(n$lon, c(-14.6, -14.6, -14.6, -20))
  expect_s3_class(n$time, "POSIXct")
  expect_equal(attr(n$time, "tzone"), "UTC")
  # plain GeoJSON, a note a line, its letters as written
  lines <- readLines(f, encoding = "UTF-8")
  expect_length(lines, 6)
  expect_match(lines[2], "Rósagarðs", fixed = TRUE)
  expect_equal(jsonlite::fromJSON(f, simplifyVector = FALSE)$type, "FeatureCollection")
  expect_error(rr_notes_add(f, "", lon = 1, lat = 1), "text")
  expect_error(rr_notes_add(f, "x"), "place")
  expect_error(rr_notes_add(f, "x", lon = 200, lat = 1), "place")
  expect_error(rr_notes_add(f, "x", reply_to = 99), "reply_to")
  expect_error(rr_notes_add(f, "x", lon = 1, lat = 1, author = ""), "author")
  expect_error(rr_notes_add("notes.csv", "x", lon = 1, lat = 1), "geojson")
  expect_error(rr_notes_add(file.path(tempfile(), "n.geojson"), "x", lon = 1, lat = 1), "does not exist")
  # a file that is not notes is never overwritten
  g <- tempfile(fileext = ".geojson")
  writeLines('{"type": "Feature"}', g)
  expect_error(rr_notes_add(g, "x", lon = 1, lat = 1), "not a notes file")
  expect_equal(readLines(g), '{"type": "Feature"}')
})

test_that("notes served: the page's notes, replies and deletions through /notes, from the page only", {
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "notes are written in a UTF-8 locale")
  f <- tempfile(fileext = ".geojson")
  a <- rr_layer("a", "geojson", data = '{"type": "FeatureCollection", "features": []}',
                    style = list(list(id = "a-fill", type = "fill")))
  m <- rr_map(rr_section("g", layers = list(a)))
  app <- .rr_map_app(m, notes = f)
  cfg <- jsonlite::fromJSON(get(app, "/config.json")$body, simplifyVector = FALSE)
  expect_equal(cfg$notes$file, basename(f))
  expect_null(m$config$notes) # the map itself is left as it was: a bundle of it has no notes
  r <- notes_call(app, "GET", "/notes")
  expect_equal(r$status, 200L)
  expect_equal(r$headers$`Cache-Control`, "no-store")
  expect_length(jsonlite::fromJSON(r$body, simplifyVector = FALSE)$features, 0)
  expect_false(file.exists(f)) # made at the first note
  r <- notes_call(app, "POST", "/notes", '{"text": "Hér er grunnið", "lon": -20.5, "lat": 65.25, "zoom": 7.333,
                                            "under": ["Areas: Rósagarður"], "address": "?a=1#7.33/65.25/-20.5"}')
  expect_equal(r$status, 200L)
  fc <- jsonlite::fromJSON(r$body, simplifyVector = FALSE)
  expect_equal(fc$added, 1L)
  p <- fc$features[[1]]$properties
  expect_equal(p$author, "reader")
  expect_equal(p$text, "Hér er grunnið")
  expect_equal(p$zoom, 7.33)
  expect_equal(p$under, list("Areas: Rósagarður"))
  expect_equal(p$address, "?a=1#7.33/65.25/-20.5")
  expect_null(p$reply_to)
  r <- notes_call(app, "POST", "/notes", '{"text": "A reply", "reply_to": 1}')
  expect_equal(jsonlite::fromJSON(r$body, simplifyVector = FALSE)$added, 2L)
  rr_notes_add(f, "From R", reply_to = 1, author = "claude") # R and the server write the same file
  n <- rr_notes(f)
  expect_equal(n$author, c("reader", "reader", "claude"))
  expect_equal(n$under[1], "Areas: Rósagarður")
  expect_equal(unique(n$lon), -20.5)
  # refused: no text, no place, a bad reply, not JSON, too big; nothing written
  before <- readLines(f, encoding = "UTF-8")
  expect_equal(notes_call(app, "POST", "/notes", '{"text": " ", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x"}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x", "reply_to": 9}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1, "under": [1]}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/notes", "not json")$status, 400L)
  expect_equal(notes_call(app, "POST", "/notes", strrep("x", 70000))$status, 413L)
  # from elsewhere: no page header, another site's origin, a page opened under another host name
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1}', page = FALSE)$status, 403L)
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1}', origin = "http://evil.example")$status, 403L)
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1}', host = "evil.example:8811")$status, 403L)
  expect_equal(notes_call(app, "GET", "/notes", host = "evil.example:8811")$status, 403L)
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1}', origin = "http://localhost:8811")$status, 200L)
  expect_equal(nrow(rr_notes(f)), 4)
  # a reply deleted alone, a thread's first note with its replies
  expect_equal(notes_call(app, "DELETE", "/notes/2")$status, 200L)
  expect_equal(rr_notes(f)$id, c(1L, 3L, 4L))
  expect_equal(notes_call(app, "DELETE", "/notes/1")$status, 200L)
  expect_equal(rr_notes(f)$id, 4L)
  expect_equal(notes_call(app, "DELETE", "/notes/1")$status, 404L)
  # a number is never given twice: the last note deleted, the next takes the one after it
  expect_equal(notes_call(app, "DELETE", "/notes/4")$status, 200L)
  expect_equal(nrow(rr_notes(f)), 0)
  expect_equal(rr_notes_add(f, "after", lon = 1, lat = 1), 5L)
  expect_equal(jsonlite::fromJSON(f, simplifyVector = FALSE)$last_id, 5L)
  expect_null(jsonlite::fromJSON(notes_call(app, "GET", "/notes")$body, simplifyVector = FALSE)$last_id)
  expect_equal(notes_call(app, "DELETE", "/notes/5", page = FALSE)$status, 403L)
  expect_equal(notes_call(app, "PUT", "/notes")$status, 405L)
  # a file broken by hand: an error to the page, and left as it is
  writeLines("{broken", f)
  r <- notes_call(app, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1}')
  expect_equal(r$status, 500L)
  expect_match(r$body, "not a notes file")
  expect_equal(readLines(f), "{broken")
  # a map served without notes has no such route
  plain <- .rr_map_app(m)
  expect_equal(notes_call(plain, "GET", "/notes")$status, 404L)
  expect_equal(notes_call(plain, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1}')$status, 405L)
  expect_null(jsonlite::fromJSON(get(plain, "/config.json")$body, simplifyVector = FALSE)$notes)
})

# a layer whose features take a verdict
verdict_layer <- function(id = "a", ...) {
  rr_layer(id, "geojson", data = '{"type": "FeatureCollection", "features": []}',
               style = list(list(id = paste0(id, "-dot"), type = "circle")),
               popup = list(click = paste0(id, "-dot"), title = "name",
                            verdict = list(field = "key", text = TRUE,
                                           options = list(list(value = "right", label = "vRight"),
                                                          list(value = "place", label = "vPlace", colour = "#fdae61"),
                                                          list(value = "name", label = "vName")))), ...)
}

test_that("a popup's verdict: checked, its options an array in the config", {
  a <- verdict_layer()
  v <- a$config$popup$verdict
  expect_equal(v$field, "key")
  expect_true(v$text)
  expect_equal(v$questions[[1]]$id, "verdict") # one question given as `options`
  expect_equal(vapply(v$questions[[1]]$options, `[[`, "", "value"), c("right", "place", "name"))
  expect_equal(v$questions[[1]]$options[[2]]$colour, "#fdae61")
  expect_false(v$questions[[1]]$options[[1]]$other)
  cfg <- jsonlite::fromJSON(.rr_map_json(rr_map(rr_section("g", layers = list(a)))$config), simplifyVector = FALSE)
  expect_length(cfg$layers[[1]]$popup$verdict$questions[[1]]$options, 3)
  # a label defaults to its value; text defaults to FALSE
  b <- rr_layer("b", "geojson", data = "{}", style = list(list(id = "b-dot", type = "circle")),
                    popup = list(click = "b-dot", verdict = list(field = "k", options = list(list(value = "y"), list(value = "n")))))
  expect_equal(b$config$popup$verdict$questions[[1]]$options[[1]]$label, "y")
  expect_false(b$config$popup$verdict$text)
  bad <- function(verdict, click = "b-dot", kind = "geojson") {
    rr_layer("b", kind, data = if (kind == "geojson") "{}", tiles = if (kind == "vector") function(z, x, y) raw(0),
                 style = list(list(id = "b-dot", type = "circle")), popup = list(click = click, verdict = verdict))
  }
  ok <- list(list(value = "y"), list(value = "n"))
  expect_error(bad(list(field = "k", options = ok), click = NULL), "click")
  expect_error(bad(list(options = ok)), "field")
  expect_error(bad(list(field = "k", options = ok[1])), "two to six")
  expect_error(bad(list(field = "k", options = rep(ok, 4))), "two to six")
  expect_error(bad(list(field = "k", options = list(list(value = "a b"), list(value = "n")))), "letters")
  expect_error(bad(list(field = "k", options = list(list(value = "y"), list(value = "y")))), "of their own")
  expect_error(bad(list(field = "k", options = list(list(value = "y", colour = "red"), list(value = "n")))), "colour")
  expect_error(bad(list(field = "k", options = ok, text = "yes")), "TRUE or FALSE")
  expect_no_error(bad(list(field = "k", options = ok), kind = "vector"))
})

test_that("verdicts served: given, replaced, withdrawn, read back; from the page only; a file broken by hand left", {
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "verdicts are written in a UTF-8 locale")
  f <- tempfile(fileext = ".csv")
  plain <- rr_layer("p", "geojson", data = '{"type": "FeatureCollection", "features": []}',
                        style = list(list(id = "p-fill", type = "fill")))
  m <- rr_map(rr_section("g", layers = list(verdict_layer(), plain)))
  app <- .rr_map_app(m, verdicts = f)
  cfg <- jsonlite::fromJSON(get(app, "/config.json")$body, simplifyVector = FALSE)
  expect_equal(cfg$verdicts$file, basename(f))
  expect_null(m$config$verdicts) # the map itself is left as it was: a bundle of it has no verdicts
  r <- notes_call(app, "GET", "/verdicts")
  expect_equal(r$status, 200L)
  expect_equal(r$headers$`Cache-Control`, "no-store")
  expect_length(jsonlite::fromJSON(r$body, simplifyVector = FALSE)$verdicts, 0)
  expect_false(file.exists(f)) # made at the first verdict
  r <- notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "eldeyjarbodi", "value": "place", "text": "Hér er það ekki",
                                              "title": "Eldeyjarboði", "lon": -23.7, "lat": 63.45, "zoom": 9.123}')
  expect_equal(r$status, 200L)
  v <- jsonlite::fromJSON(r$body, simplifyVector = FALSE)$verdicts
  expect_length(v, 1)
  expect_equal(v[[1]]$value, "place")
  expect_equal(v[[1]]$text, "Hér er það ekki")
  notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": 7, "value": "right", "lon": 1, "lat": 2}')
  # a later answer replaces the earlier; an empty one withdraws it; every answer kept in the file
  notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "eldeyjarbodi", "value": "name", "lon": -23.7, "lat": 63.45}')
  expect_equal(rr_verdicts(f)$value, c("right", "name"))
  expect_equal(rr_verdicts(f)$id, c("7", "eldeyjarbodi"))
  r <- notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "7", "value": "", "lon": 1, "lat": 2}')
  expect_equal(r$status, 200L)
  expect_length(jsonlite::fromJSON(r$body, simplifyVector = FALSE)$verdicts, 1)
  all <- rr_verdicts(f, all = TRUE)
  expect_equal(all$value, c("place", "right", "name", ""))
  expect_equal(names(all), c("time", "layer", "id", "question", "value", "other", "text", "title", "lon", "lat", "zoom"))
  expect_equal(unique(all$question), "verdict") # a page of one question may leave it out
  expect_equal(all$title[1], "Eldeyjarboði")
  expect_equal(all$zoom[1], 9.12)
  expect_true(is.na(all$zoom[2]))
  expect_equal(all$text[2], "")
  # refused: another layer, a value not offered, no id, no place, too long, not JSON, too big; nothing written
  before <- readLines(f, encoding = "UTF-8")
  expect_equal(notes_call(app, "POST", "/verdicts", '{"layer": "p", "id": "x", "value": "right", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "x", "value": "maybe", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "", "value": "right", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "x", "value": "right"}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/verdicts", sprintf('{"layer": "a", "id": "x", "value": "right", "lon": 1, "lat": 1, "text": "%s"}',
                                                              strrep("x", 1001)))$status, 400L)
  expect_equal(notes_call(app, "POST", "/verdicts", "not json")$status, 400L)
  expect_equal(notes_call(app, "POST", "/verdicts", strrep("x", 20000))$status, 413L)
  # from elsewhere: no page header, another site's origin, a page opened under another host name
  expect_equal(notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "x", "value": "right", "lon": 1, "lat": 1}', page = FALSE)$status, 403L)
  expect_equal(notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "x", "value": "right", "lon": 1, "lat": 1}',
                          origin = "http://evil.example")$status, 403L)
  expect_equal(notes_call(app, "GET", "/verdicts", host = "evil.example:8811")$status, 403L)
  expect_equal(readLines(f, encoding = "UTF-8"), before)
  expect_equal(notes_call(app, "PUT", "/verdicts")$status, 405L)
  # a file broken by hand: an error to the page, and left as it is
  writeLines("a,b\n1,2", f)
  r <- notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "x", "value": "right", "lon": 1, "lat": 1}')
  expect_equal(r$status, 500L)
  expect_match(r$body, "not a verdicts file")
  expect_equal(readLines(f), c("a,b", "1,2"))
  # a map served without verdicts has no such route
  expect_equal(notes_call(.rr_map_app(m), "GET", "/verdicts")$status, 404L)
  expect_null(jsonlite::fromJSON(get(.rr_map_app(m), "/config.json")$body, simplifyVector = FALSE)$verdicts)
})

# a layer whose features take two questions, one skipped by an answer to the first, an answer naming another, a note
two_questions <- function() {
  rr_layer("n", "geojson", data = '{"type": "FeatureCollection", "features": []}',
               style = list(list(id = "n-dot", type = "circle")),
               popup = list(click = "n-dot", title = "name", verdict = list(field = "key", text = TRUE, questions = list(
                 list(id = "name", label = "qName", options = list(
                   list(value = "right", label = "nRight"), list(value = "form", label = "nForm", other = TRUE),
                   list(value = "not", label = "nNot"), list(value = "unsure", label = "nUnsure", colour = "#999999"))),
                 list(id = "place", label = "qPlace", skip = list(name = "not"), options = list(
                   list(value = "right", label = "pRight"), list(value = "wrong", label = "pWrong")))))))
}

test_that("a verdict's questions: checked, an answer naming another, a question skipped by an earlier answer", {
  q <- two_questions()$config$popup$verdict$questions
  expect_equal(vapply(q, `[[`, "", "id"), c("name", "place"))
  expect_true(q[[1]]$options[[2]]$other)
  expect_equal(q[[2]]$skip, list(name = list("not")))
  cfg <- jsonlite::fromJSON(.rr_map_json(rr_map(rr_section("g", layers = list(two_questions())))$config), simplifyVector = FALSE)
  expect_equal(cfg$layers[[1]]$popup$verdict$questions[[2]]$skip$name, list("not")) # an array, one answer or several
  bad <- function(verdict) {
    rr_layer("b", "geojson", data = "{}", style = list(list(id = "b-dot", type = "circle")),
                 popup = list(click = "b-dot", verdict = verdict))
  }
  ok <- list(list(value = "y"), list(value = "n"))
  expect_error(bad(list(field = "k", options = ok, questions = list(list(id = "a", options = ok)))), "not both")
  expect_error(bad(list(field = "k", questions = rep(list(list(id = "a", options = ok)), 5))), "one to four")
  expect_error(bad(list(field = "k", questions = list(list(id = "a", options = ok), list(id = "a", options = ok)))), "ids of their own")
  expect_error(bad(list(field = "k", questions = list(list(id = "note", options = ok)))), "note")
  expect_error(bad(list(field = "k", questions = list(list(id = "a", options = ok), list(id = "b", options = ok, skip = list(c = "y"))))),
               "earlier questions")
  expect_error(bad(list(field = "k", questions = list(list(id = "a", options = ok), list(id = "b", options = ok, skip = list(a = "z"))))),
               "answers of the question")
  expect_error(bad(list(field = "k", questions = list(list(id = "b", options = ok, skip = list(b = "y"))))), "earlier questions")
  expect_error(bad(list(field = "k", options = list(list(value = "y", other = "yes"), list(value = "n")))), "other")
})

test_that("verdicts served by question: an answer naming another, a note, each question's latest; a file of one question read", {
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "verdicts are written in a UTF-8 locale")
  f <- tempfile(fileext = ".csv")
  m <- rr_map(rr_section("g", layers = list(two_questions())))
  app <- .rr_map_app(m, verdicts = f)
  post <- function(body) notes_call(app, "POST", "/verdicts", body)
  expect_equal(post('{"layer": "n", "id": "bodi", "question": "name", "value": "form", "other": "Eldeyjarboði", "lon": -23.7, "lat": 63.4}')$status, 200L)
  expect_equal(post('{"layer": "n", "id": "bodi", "question": "place", "value": "wrong", "lon": -23.7, "lat": 63.4}')$status, 200L)
  expect_equal(post('{"layer": "n", "id": "bodi", "question": "note", "text": "Austan við", "lon": -23.7, "lat": 63.4}')$status, 200L)
  expect_equal(post('{"layer": "n", "id": "sker", "question": "name", "value": "not", "lon": -23.1, "lat": 63.6}')$status, 200L)
  v <- rr_verdicts(f)
  expect_equal(paste(v$id, v$question, v$value, v$other), c("bodi name form Eldeyjarboði", "bodi place wrong ", "bodi note  ", "sker name not "))
  expect_equal(v$text[v$question == "note"], "Austan við")
  # each question's latest: the place withdrawn, the note cleared, the name kept
  post('{"layer": "n", "id": "bodi", "question": "place", "value": "", "lon": -23.7, "lat": 63.4}')
  post('{"layer": "n", "id": "bodi", "question": "note", "text": "", "lon": -23.7, "lat": 63.4}')
  v <- rr_verdicts(f)
  expect_equal(paste(v$id, v$question), c("bodi name", "sker name"))
  expect_equal(nrow(rr_verdicts(f, all = TRUE)), 6)
  r <- jsonlite::fromJSON(notes_call(app, "GET", "/verdicts")$body, simplifyVector = FALSE)$verdicts
  expect_equal(r[[1]]$other, "Eldeyjarboði")
  # refused: no name where the answer names one, a name where it does not, no question, a question not there,
  # a note with a value; nothing written
  before <- readLines(f, encoding = "UTF-8")
  expect_equal(post('{"layer": "n", "id": "x", "question": "name", "value": "form", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(post('{"layer": "n", "id": "x", "question": "name", "value": "form", "other": " ", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(post('{"layer": "n", "id": "x", "question": "name", "value": "right", "other": "B", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(post('{"layer": "n", "id": "x", "value": "right", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(post('{"layer": "n", "id": "x", "question": "depth", "value": "right", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(post('{"layer": "n", "id": "x", "question": "note", "value": "right", "text": "t", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(readLines(f, encoding = "UTF-8"), before)
  # a file of 2026.11.09, one question with no question column: read as "verdict", and written on in the new form
  g <- tempfile(fileext = ".csv")
  writeLines(c('"time","layer","id","value","text","title","lon","lat","zoom"',
               '"2026-11-09T09:19:03Z","a","x","place","Hér","X",-22.78,63.78,10'), g, useBytes = TRUE)
  old <- rr_verdicts(g)
  expect_equal(c(old$question, old$value, old$other, old$text), c("verdict", "place", "", "Hér"))
  app1 <- .rr_map_app(rr_map(rr_section("g", layers = list(verdict_layer()))), verdicts = g)
  expect_equal(notes_call(app1, "POST", "/verdicts", '{"layer": "a", "id": "y", "value": "name", "lon": 1, "lat": 1}')$status, 200L)
  expect_equal(names(utils::read.csv(g)), c("time", "layer", "id", "question", "value", "other", "text", "title", "lon", "lat", "zoom"))
  expect_equal(rr_verdicts(g)$id, c("x", "y"))
})

test_that("a verdict kept under another layer's answers: shared both ways, the same questions asked", {
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "verdicts are written in a UTF-8 locale")
  shared <- function(id, store = NULL, options = list(list(value = "right"), list(value = "not"))) {
    rr_layer(id, "geojson", data = '{"type": "FeatureCollection", "features": []}',
                 style = list(list(id = paste0(id, "-dot"), type = "circle")),
                 popup = list(click = paste0(id, "-dot"), title = "name",
                              verdict = list(field = "key", store = store, options = options)))
  }
  expect_equal(shared("b", "a")$config$popup$verdict$store, "a")
  expect_null(shared("a")$config$popup$verdict$store)
  expect_error(shared("b", "a b"), "store")
  f <- tempfile(fileext = ".csv")
  m <- rr_map(rr_section("g", layers = list(shared("a"), shared("b", "a"))))
  app <- .rr_map_app(m, verdicts = f)
  # an answer given through b is kept as a's, and read as a's
  r <- notes_call(app, "POST", "/verdicts", '{"layer": "b", "id": "x", "value": "not", "lon": 1, "lat": 1}')
  expect_equal(r$status, 200L)
  expect_equal(jsonlite::fromJSON(r$body, simplifyVector = FALSE)$verdicts[[1]]$layer, "a")
  notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "x", "value": "right", "lon": 1, "lat": 1}')
  v <- rr_verdicts(f)
  expect_equal(paste(v$layer, v$id, v$value), "a x right") # one feature, its latest answer, whichever layer gave it
  expect_equal(rr_verdicts(f, all = TRUE)$layer, c("a", "a"))
  # layers sharing answers ask the same questions, or the map is refused
  odd <- rr_map(rr_section("g", layers = list(shared("a"), shared("b", "a", list(list(value = "yes"), list(value = "no"))))))
  expect_error(.rr_map_app(odd, verdicts = f), "different questions")
  expect_error(rr_serve(odd, port = httpuv::randomPort(), open = FALSE, verdicts = tempfile(fileext = ".csv")), "different questions")
})

test_that("a server with verdicts: the file and the map checked as it starts", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "verdicts are written in a UTF-8 locale")
  m <- rr_map(rr_section("g", layers = list(verdict_layer())))
  g <- tempfile(fileext = ".csv")
  writeLines("x,y", g)
  expect_error(rr_serve(m, port = httpuv::randomPort(), open = FALSE, verdicts = g), "not a verdicts file")
  expect_error(rr_serve(m, port = httpuv::randomPort(), open = FALSE, verdicts = tempfile(fileext = ".json")), "csv")
  expect_error(rr_serve(m, port = httpuv::randomPort(), open = FALSE, verdicts = file.path(tempfile(), "v.csv")),
               "does not exist")
  a <- rr_layer("a", "vector", tiles = function(z, x, y) raw(0), style = list(list(id = "a-line", type = "line")))
  expect_error(rr_serve(rr_map(rr_section("g", layers = list(a))), port = httpuv::randomPort(), open = FALSE,
                            verdicts = tempfile(fileext = ".csv")), "has none")
  port <- httpuv::randomPort()
  rr_serve(m, port = port, open = FALSE, verdicts = tempfile(fileext = ".csv"))
  expect_equal(rr_stop(port), port)
})

test_that("a server with notes: the file checked as it starts", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "notes are written in a UTF-8 locale")
  a <- rr_layer("a", "vector", tiles = function(z, x, y) raw(0), style = list(list(id = "a-line", type = "line")))
  m <- rr_map(rr_section("g", layers = list(a)))
  g <- tempfile(fileext = ".geojson")
  writeLines("[]", g)
  expect_error(rr_serve(m, port = httpuv::randomPort(), open = FALSE, notes = g), "not a notes file")
  expect_error(rr_serve(m, port = httpuv::randomPort(), open = FALSE, notes = file.path(tempfile(), "n.geojson")),
               "does not exist")
  port <- httpuv::randomPort()
  rr_serve(m, port = port, open = FALSE, notes = tempfile(fileext = ".geojson"))
  expect_equal(rr_stop(port), port)
})

test_that("a choice's option may name a base map, shown while it is the one value chosen", {
  v <- rr_layer("areas", "vector", tiles = function(z, x, y) NULL,
                style = list(list(id = "areas-fill", type = "fill", "source-layer" = "features")))
  ch <- function(base) list(fields = list(list(field = "scale", multi = FALSE, layers = "areas",
                                               options = list(list(value = "a", match = "a", base = base),
                                                              list(value = "b", match = "b")))))
  m <- rr_map(rr_section("g", "G", list(v)), choose = ch("chart_a"))
  expect_identical(m$config$choose$fields[[1]]$options[[1]]$base, "chart_a")
  expect_null(m$config$choose$fields[[1]]$options[[2]]$base)
  expect_error(rr_map(rr_section("g", "G", list(v)), choose = ch(c("x", "y"))), "base")
  expect_error(rr_map(rr_section("g", "G", list(v)), choose = ch(NA_character_)), "base")
  expect_error(rr_map(rr_section("g", "G", list(v)), choose = ch("")), "base")
})
