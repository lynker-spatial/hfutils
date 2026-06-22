test_that(".duckdb_reader_opts formats named reader options for DuckDB", {
  # no options -> empty fragment (back-compat with the original query)
  expect_identical(.duckdb_reader_opts(list()), "")

  # logical -> true/false
  expect_identical(.duckdb_reader_opts(list(union_by_name = TRUE)),
    ", union_by_name=true")
  expect_identical(.duckdb_reader_opts(list(union_by_name = FALSE)),
    ", union_by_name=false")

  # character -> single-quoted; numeric -> bare; order preserved
  expect_identical(
    .duckdb_reader_opts(list(union_by_name = TRUE, filename = "f", n = 3)),
    ", union_by_name=true, filename='f', n=3")

  # unnamed options are rejected
  expect_error(.duckdb_reader_opts(list(TRUE)), "must be named")
  expect_error(.duckdb_reader_opts(list(union_by_name = TRUE, TRUE)),
    "must be named")
})
