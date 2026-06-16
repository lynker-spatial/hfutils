# Contributing to hfutils

## Testing & coverage

Tests use [testthat](https://testthat.r-lib.org/) and run against small,
self-contained fixtures (the `sf::nc` sample and synthetic flow networks built
in `tests/testthat/helper-fixtures.R`) — no network access, cloud
authentication, or external command-line tools are required.

Run the suite with:

```r
devtools::test()
```

### What coverage does and does not include

The headline coverage number reflects the package's **testable logic**. Three
source files are intentionally excluded from coverage measurement (see
`.covrignore`) because they cannot be exercised in a headless CI run without
external services:

| File | Why it is excluded |
|------|--------------------|
| `R/clean_geom.R` | `clean_gpkg_layer()` shells out to `ogr2ogr`, `mapshaper`, and `gdalsrsinfo`. |
| `R/duckdb.R` | `duckdb_connection()` / `tbl_http()` open a live DuckDB connection and read over cloud `httpfs`. |
| `R/auth.R` | `lynker_spatial_*()` drive an interactive OAuth2 browser flow. |

These functions are validated through integration use rather than unit tests;
their error-guard paths (e.g. malformed tokens) *are* unit-tested. Everything
else — I/O, geometry cleaning, network/topology algorithms, versioning, and the
staged invariant checks — is expected to stay well covered, so please add or
extend tests alongside any change to those areas.

### Conventions

- Prefer behavioral assertions (outputs, invariants, error conditions) over
  asserting that a specific internal branch executed.
- Keep fixtures small and deterministic; build them in `helper-fixtures.R` so
  they can be reused.
- User-facing errors should be raised with `cli::cli_abort()`.
