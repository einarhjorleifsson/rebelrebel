test_that("a map with no port takes a free one, and replaces the last such map there", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("jsonlite")
  a <- rr_layer("a", "vector", tiles = function(z, x, y) raw(0), style = list(list(id = "a-line", type = "line")))
  m <- rr_map(rr_section("g", layers = list(a)))
  app <- .rr_map_app(m)
  port_of <- function(url) as.integer(sub("^http://localhost:([0-9]+)/.*$", "\\1", url))
  # the first port of the range held by another program
  first <- httpuv::randomPort(min = 20000L, max = 60000L)
  other <- httpuv::startServer("127.0.0.1", first, list(call = function(req) list(status = 200L, body = "")))
  on.exit(httpuv::stopServer(other), add = TRUE)
  p1 <- .rr_serve_free("127.0.0.1", app, ports = first + 0:9)
  expect_true(p1 > first)
  # served again with no port: in the same place, the map there replaced
  expect_equal(.rr_serve_free("127.0.0.1", app, ports = first + 0:9), p1)
  # a map on a port named is never replaced by one with no port
  .rr_serve_last$port <- NULL
  p3 <- .rr_serve_free("127.0.0.1", app, ports = first + 0:9)
  expect_true(p3 > p1)
  expect_true(all(as.character(c(p1, p3)) %in% ls(.rr_maps)))
  expect_equal(sort(rr_stop(c(p1, p3))), sort(c(p1, p3)))
  # a port named and taken is an error, and so is a range with none free
  expect_error(rr_serve(m, port = first, open = FALSE), "Port [0-9]+ is taken: give another port, or none")
  expect_error(.rr_serve_free("127.0.0.1", app, ports = first), "No port free from")
  # rr_serve() with no port, as print() calls it: twice on one port
  url <- rr_serve(m, open = FALSE)
  expect_equal(port_of(rr_serve(m, open = FALSE)), port_of(url))
  expect_equal(rr_stop(port_of(url)), port_of(url))
})

test_that("a map from data carries no port unless its first call gives one", {
  skip_if_not_installed("jsonlite")
  df <- dplyr::collect(utils::head(rr_example("dansk"), 50))
  expect_null(attr(rr_point(df), "port"))
  expect_null(attr(rr_map(df) |> rr_point(), "port"))
  expect_equal(attr(rr_time(df, speed, time = "time_stamp", by = vessel_id, port = 9001), "port"), 9001L)
})
