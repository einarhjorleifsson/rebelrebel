# Skip a test when DuckDB's spatial extension cannot be loaded (no network to install it).
skip_if_no_spatial <- function() {
  con <- .rr_connection()
  ok <- !inherits(
    try(DBI::dbExecute(con, "LOAD spatial"), silent = TRUE),
    "try-error"
  ) ||
    !inherits(
      try(DBI::dbExecute(con, "INSTALL spatial; LOAD spatial"), silent = TRUE),
      "try-error"
    )
  skip_if_not(ok, "DuckDB's spatial extension is not available")
}

# The features of each layer of a vector tile (Mapbox Vector Tile, protobuf): a tile's layers are field 3, a layer's
# name field 1 and its features field 2, each length-delimited
mvt_count <- function(bytes) {
  b <- as.integer(bytes)
  pos <- 1L
  varint <- function() {
    v <- 0
    s <- 0
    repeat {
      x <- b[pos]
      pos <<- pos + 1L
      v <- v + (x %% 128) * 2^s
      s <- s + 7
      if (x < 128) break
    }
    v
  }
  skip <- function(w) if (w == 0) varint() else if (w == 1) pos <<- pos + 8L else if (w == 5) pos <<- pos + 4L
  out <- list()
  while (pos <= length(b)) {
    key <- varint()
    if (key %% 8 != 2) {
      skip(key %% 8)
      next
    }
    end <- pos + varint()
    if (key %/% 8 != 3) {
      pos <- end
      next
    }
    name <- NA_character_
    k <- 0
    while (pos < end) {
      key2 <- varint()
      if (key2 %% 8 != 2) {
        skip(key2 %% 8)
        next
      }
      n2 <- varint()
      if (key2 %/% 8 == 1) name <- rawToChar(bytes[pos:(pos + n2 - 1)])
      if (key2 %/% 8 == 2) k <- k + 1
      pos <- pos + n2
    }
    out[[name]] <- k
  }
  unlist(out)
}
