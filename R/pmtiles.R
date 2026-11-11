# pmtiles.R — a source's tiles (vector tiles, or elevation images) as one PMTiles archive (version 3,
# https://github.com/protomaps/PMTiles/blob/main/spec/v3/spec.md), for a bundle: one file in place of a file a tile.
# Its directories and tiles are written uncompressed, the tiles in the order of their ids. Read in the page by the
# pmtiles library (vendor/pmtiles.js, 4.5.0).

# A tile's id: the count of the tiles of the zooms before it, then its place along the Hilbert curve at its zoom, as
# the reference implementation's zxyToTileId() makes it
.rr_pmtiles_id <- function(z, x, y) {
  vapply(seq_along(z), function(i) {
    n <- 2^z[i]
    px <- x[i]
    py <- y[i]
    d <- 0
    s <- n / 2
    while (s >= 1) {
      rx <- as.numeric((px %/% s) %% 2 == 1)
      ry <- as.numeric((py %/% s) %% 2 == 1)
      d <- d + s * s * bitwXor(3L * as.integer(rx), as.integer(ry))
      if (ry == 0) {
        if (rx == 1) {
          px <- s - 1 - px
          py <- s - 1 - py
        }
        t <- px
        px <- py
        py <- t
      }
      s <- s / 2
    }
    (4^z[i] - 1) / 3 + d
  }, 0)
}

# Unsigned LEB128 varints of whole numbers below 2^53
.rr_varint <- function(v) {
  out <- vector("list", length(v))
  for (i in seq_along(v)) {
    x <- v[i]
    b <- raw(0)
    repeat {
      low <- x %% 128
      x <- (x - low) / 128
      if (x > 0) {
        b <- c(b, as.raw(low + 128))
      } else {
        b <- c(b, as.raw(low))
        break
      }
    }
    out[[i]] <- b
  }
  unlist(out, use.names = FALSE)
}

# A directory: the count of entries, then each column (tile ids as differences, run lengths, lengths, offsets as 0
# where a tile follows the one before it, else one more than the offset)
.rr_pmtiles_dir <- function(id, offset, length, run) {
  k <- length(id)
  follows <- c(FALSE, offset[-1] == offset[-k] + length[-k])
  c(
    .rr_varint(k),
    .rr_varint(diff(c(0, id))),
    .rr_varint(run),
    .rr_varint(length),
    .rr_varint(ifelse(follows, 0, offset + 1))
  )
}

.rr_u64 <- function(x) as.raw(floor(x / 256^(0:7)) %% 256)
.rr_i32 <- function(x) {
  x <- round(x)
  as.raw(floor((if (x < 0) x + 2^32 else x) / 256^(0:3)) %% 256)
}

# The archive of tiles given by zoom, column, row and bytes, of one `type` (vector tiles, "mvt", or images, "png" or
# "webp", "jpg"), written to `path`: a header, the root directory, the metadata, the leaf directories where the root alone
# would pass its limit (16 kB with the header), the tiles
.rr_pmtiles_write <- function(z, x, y, data, path, bounds, type = "mvt", name = "tiles", root_max = 16384 - 127) {
  type_code <- unname(c(mvt = 1, png = 2, jpg = 3, jpeg = 3, webp = 4)[type]) # another, the spec's "unknown"
  if (is.na(type_code)) type_code <- 0
  id <- .rr_pmtiles_id(z, x, y)
  o <- order(id)
  id <- id[o]
  data <- data[o]
  z <- z[o]
  len <- as.numeric(lengths(data))
  off <- c(0, cumsum(len))[seq_along(len)]
  root <- .rr_pmtiles_dir(id, off, len, rep(1, length(id)))
  leaves <- raw(0)
  size <- 4096
  # leaves of 4,096 entries, then 8,192 and on, until the root of their pointers fits (a root of one pointer is the
  # least it can be)
  while (length(root) > root_max && (size == 4096 || length(groups) > 1)) {
    groups <- split(seq_along(id), ceiling(seq_along(id) / size))
    parts <- lapply(groups, function(g) .rr_pmtiles_dir(id[g], off[g], len[g], rep(1, length(g))))
    plen <- as.numeric(lengths(parts))
    leaves <- unlist(parts, use.names = FALSE)
    root <- .rr_pmtiles_dir(vapply(groups, function(g) id[g[1]], 0), c(0, cumsum(plen))[seq_along(plen)], plen, rep(0, length(plen)))
    size <- size * 2
  }
  meta <- charToRaw(enc2utf8(as.character(jsonlite::toJSON(c(
    list(name = name),
    if (type == "mvt") list(vector_layers = list(list(id = "features", fields = structure(list(), names = character()))))
  ), auto_unbox = TRUE))))
  at_root <- 127
  at_meta <- at_root + length(root)
  at_leaves <- at_meta + length(meta)
  at_data <- at_leaves + length(leaves)
  header <- c(
    charToRaw("PMTiles"), as.raw(3),
    .rr_u64(at_root), .rr_u64(length(root)), .rr_u64(at_meta), .rr_u64(length(meta)),
    .rr_u64(at_leaves), .rr_u64(length(leaves)), .rr_u64(at_data), .rr_u64(sum(len)),
    .rr_u64(length(id)), .rr_u64(length(id)), .rr_u64(length(id)),
    as.raw(1), # clustered: the tiles in the order of their ids
    as.raw(1), as.raw(1), # no compression, of the directories and of the tiles
    as.raw(type_code),
    as.raw(min(z)), as.raw(max(z)),
    .rr_i32(bounds[1] * 1e7), .rr_i32(bounds[2] * 1e7), .rr_i32(bounds[3] * 1e7), .rr_i32(bounds[4] * 1e7),
    as.raw(min(z)), .rr_i32((bounds[1] + bounds[3]) / 2 * 1e7), .rr_i32((bounds[2] + bounds[4]) / 2 * 1e7)
  )
  stopifnot(length(header) == 127)
  con <- file(path, "wb")
  on.exit(close(con))
  writeBin(c(header, root, meta, leaves), con)
  for (d in data) writeBin(d, con)
  invisible(path)
}
