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
