# helper-pmtiles.R — a bundle's PMTiles archive read back in R, for the tests: its header, every tile's entry (id,
# offset, length) from the root and leaf directories, and a tile's bytes by zoom, column and row. Read from the
# specification (version 3, https://github.com/protomaps/PMTiles/blob/main/spec/v3/spec.md), apart from rr_bundle()'s
# writer but for the tile id, which is checked against pmtiles.js's own; the page reads with pmtiles.js.

# The archive's bytes, from the script a bundle writes, or a .pmtiles file
pm_bytes <- function(dir, key = NULL) {
  if (is.null(key)) {
    return(readBin(dir, "raw", file.size(dir)))
  }
  js <- readLines(file.path(dir, "tiles", paste0(key, ".pmtiles.js")), warn = FALSE)
  stopifnot(length(js) == 1)
  jsonlite::base64_dec(sub(sprintf('^RB_BUNDLE\\("tiles/%s[.]pmtiles","(.*)"\\);$', key), "\\1", js))
}

pm_archive <- function(dir, key = NULL) {
  a <- pm_bytes(dir, key)
  u64 <- function(at) sum(as.numeric(a[at + 1:8]) * 256^(0:7)) # at: the byte's place from 0
  i32 <- function(at) {
    v <- sum(as.numeric(a[at + 1:4]) * 256^(0:3))
    (if (v >= 2^31) v - 2^32 else v) / 1e7
  }
  h <- list(
    magic = rawToChar(a[1:7]), version = as.integer(a[8]),
    root = c(u64(8), u64(16)), meta = c(u64(24), u64(32)), leaves = c(u64(40), u64(48)), data = c(u64(56), u64(64)),
    addressed = u64(72), entries = u64(80), contents = u64(88), clustered = as.integer(a[97]),
    compression = as.integer(a[98:99]), tile_type = as.integer(a[100]),
    minzoom = as.integer(a[101]), maxzoom = as.integer(a[102]), bounds = c(i32(102), i32(106), i32(110), i32(114))
  )
  directory <- function(from, len) {
    b <- as.integer(a[from + seq_len(len)])
    pos <- 1
    v <- function() {
      x <- 0
      s <- 1
      repeat {
        byte <- b[pos]
        pos <<- pos + 1
        x <- x + (byte %% 128) * s
        s <- s * 128
        if (byte < 128) return(x)
      }
    }
    k <- v()
    id <- cumsum(vapply(seq_len(k), function(i) v(), 0))
    run <- vapply(seq_len(k), function(i) v(), 0)
    len <- vapply(seq_len(k), function(i) v(), 0)
    off <- numeric(k)
    for (i in seq_len(k)) {
      o <- v()
      off[i] <- if (o == 0 && i > 1) off[i - 1] + len[i - 1] else o - 1
    }
    data.frame(id = id, run = run, offset = off, length = len)
  }
  root <- directory(h$root[1], h$root[2])
  leaves <- root[root$run == 0, ]
  entries <- rbind(
    root[root$run > 0, ],
    do.call(rbind, lapply(seq_len(nrow(leaves)), function(i) directory(h$leaves[1] + leaves$offset[i], leaves$length[i])))
  )
  list(bytes = a, header = h, meta = jsonlite::fromJSON(rawToChar(a[h$meta[1] + seq_len(h$meta[2])])),
       entries = entries[order(entries$id), ], leaves = nrow(leaves))
}

# A tile's bytes, NULL where the archive holds none
pm_tile <- function(arch, z, x, y) {
  e <- arch$entries[arch$entries$id == .rr_pmtiles_id(z, x, y), ]
  if (!nrow(e)) {
    return(NULL)
  }
  arch$bytes[arch$header$data[1] + e$offset + seq_len(e$length)]
}

# The ids of the tiles it holds at zoom z
pm_ids_at <- function(arch, z) {
  arch$entries$id[arch$entries$id >= (4^z - 1) / 3 & arch$entries$id < (4^(z + 1) - 1) / 3]
}
