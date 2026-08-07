# Contributing to hfutils

## Testing & coverage

Tests use [testthat](https://testthat.r-lib.org/) and run against small,
self-contained fixtures (the `sf::nc` sample and synthetic flow networks built
in `tests/testthat/helper-fixtures.R`), no network access, cloud
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
else, I/O, geometry cleaning, network/topology algorithms, versioning, and the
staged invariant checks, is expected to stay well covered, so please add or
extend tests alongside any change to those areas.

## Lint and spelling

Both are enforced in CI, so it is worth running them before you push.

```r
lintr::lint_package()          # must report zero lints
spelling::spell_check_package()
```

The `lint` workflow runs with `LINTR_ERROR_ON_LINT=true`, so a single style
finding fails the build. The active linter set, and the exceptions the package
takes deliberately, are documented inline in `.lintr`; `styler` is the
formatter of record and owns indentation.

CI installs the current lintr, so an older local copy can report zero lints on
code CI rejects: new linters arrive in minor releases. Run
`update.packages("lintr")` before trusting a clean local run.

Spelling runs as part of the test suite and fails on an unrecognized word.
Genuine technical terms (`arbolate`, `NHDPlus`, `Strahler`, and so on) belong
in `inst/WORDLIST`; add them there rather than rewording the documentation. The
package declares `Language: en-US`, so use US spellings in prose.

## Documentation

Documentation is roxygen2-generated. After changing any roxygen block, run:

```r
roxygen2::roxygenise()   # regenerates man/ and NAMESPACE
pkgdown::check_pkgdown() # every exported topic must appear in _pkgdown.yml
```

A newly exported function has to be added to the reference index in
`_pkgdown.yml`, otherwise the website build fails. `README.md` is generated
from `README.Rmd` via `knitr::knit()`; edit the `.Rmd`.

### Conventions

- Prefer behavioral assertions (outputs, invariants, error conditions) over
  asserting that a specific internal branch executed.
- Keep fixtures small and deterministic; build them in `helper-fixtures.R` so
  they can be reused.
- User-facing errors should be raised with `cli::cli_abort()`.
