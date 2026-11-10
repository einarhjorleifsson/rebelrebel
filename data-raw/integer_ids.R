# data-raw/integer_ids.R — the example files' ids stored as integers (2026-11-10). trawlsurvey's `vid` and
# trawlsurveytows' `id.lgs` were doubles, as ramb's data-raw/DATASET.R wrote them; their dictionaries call them ids
# (data-dict's `number(id)`). Stored as integers, an id is a list in `choose` by its storage too, and its text has no
# decimal. Every value is checked whole and in an integer's range first; the rows, their order and every other column
# are kept, and the files' metadata key by key (GeoParquet's `geo` written again by DuckDB). Run from the package's
# root: Rscript data-raw/integer_ids.R
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, "INSTALL spatial; LOAD spatial")
s <- function(x) as.character(DBI::dbQuoteString(con, x))
for (x in list(c("trawlsurvey", "vid"), c("trawlsurveytows", "id.lgs"))) {
  p <- file.path("inst", "extdata", paste0(x[1], ".parquet"))
  col <- as.character(DBI::dbQuoteIdentifier(con, x[2]))
  src <- sprintf("read_parquet(%s)", s(p))
  bad <- DBI::dbGetQuery(con, sprintf(
    "SELECT count(*) AS n FROM %1$s WHERE %2$s IS NOT NULL AND (%2$s <> round(%2$s) OR abs(%2$s) > 2147483647)", src, col
  ))$n
  stopifnot(bad == 0)
  kv <- DBI::dbGetQuery(con, sprintf("SELECT decode(key) AS k, decode(value) AS v FROM parquet_kv_metadata(%s)", s(p)))
  own <- kv[kv$k != "geo", ]
  tmp <- tempfile(fileext = ".parquet")
  DBI::dbExecute(con, sprintf(
    "COPY (SELECT * REPLACE (CAST(%1$s AS INTEGER) AS %1$s) FROM %2$s) TO %3$s (FORMAT parquet, COMPRESSION zstd, KV_METADATA {%4$s})",
    col, src, s(tmp), paste(sprintf("%s: %s", s(own$k), s(own$v)), collapse = ", ")
  ))
  # the same rows in the same order, each column equal, the id now an integer; the same metadata
  new <- sprintf("read_parquet(%s)", s(tmp))
  same <- DBI::dbGetQuery(con, sprintf(
    "SELECT count(*) AS n FROM (SELECT *, row_number() OVER () AS rr_i FROM %s) a
     JOIN (SELECT *, row_number() OVER () AS rr_i FROM %s) b USING (rr_i)
     WHERE a.%3$s = b.%3$s AND %4$s",
    src, new, col,
    paste(sprintf("a.%1$s IS NOT DISTINCT FROM b.%1$s", setdiff(
      as.character(DBI::dbQuoteIdentifier(con, DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM %s", src))$column_name)),
      col
    )), collapse = " AND ")
  ))$n
  rows <- DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s", src))$n
  type <- DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT %s FROM %s", col, new))$column_type
  kv2 <- DBI::dbGetQuery(con, sprintf("SELECT decode(key) AS k, decode(value) AS v FROM parquet_kv_metadata(%s)", s(tmp)))
  stopifnot(same == rows, type == "INTEGER", setequal(kv$k, kv2$k),
            identical(own$v[order(own$k)], kv2$v[match(sort(own$k), kv2$k)]))
  file.copy(tmp, p, overwrite = TRUE)
  cat(sprintf("%s: %s as INTEGER, %d rows equal, %d metadata keys kept\n", x[1], x[2], same, nrow(kv2)))
}
DBI::dbDisconnect(con, shutdown = TRUE)
