# Changelog

## hfutils 0.3.4

Correctness and quality pass on the base layer.

- [`tbl_http()`](https://lynker-spatial.github.io/hfutils/reference/tbl_http.md)
  no longer errors on its default call — the malformed `read_func`
  default is fixed and validated with
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html).
- [`get_hydroseq()`](https://lynker-spatial.github.io/hfutils/reference/get_hydroseq.md)
  is now character-safe: non-numeric identifiers (`"fp-"`,
  scientific-notation strings) round-trip instead of being coerced to
  `NA`.
- [`write_hydrofabric()`](https://lynker-spatial.github.io/hfutils/reference/write_hydrofabric.md)
  defaults to `enforce_dm = FALSE`; the data-model schema (`hf_dm`) is
  only consulted when a caller supplies it, so the default call no
  longer fails looking for an object hfutils does not ship.
- Fixed
  [`union_polygons()`](https://lynker-spatial.github.io/hfutils/reference/union_polygons.md)
  erroring (`object '.' not found`) in its disjoint-MULTIPOLYGON dedup
  branch — it used the magrittr `.` pronoun under a native `|>` pipe.
  This path fires exactly when a group unions to disjoint parts; now
  regression-tested.
- Removed divergent duplicate definitions of
  [`rename_geometry()`](https://lynker-spatial.github.io/hfutils/reference/rename_geometry.md),
  [`add_areasqkm()`](https://lynker-spatial.github.io/hfutils/reference/add_areasqkm.md),
  and `fast_validity_check()` (kept one canonical copy each).
- [`create_metadata()`](https://lynker-spatial.github.io/hfutils/reference/create_metadata.md)
  no longer warns on every GeoParquet write (now opt-in via
  `quiet = FALSE`); fixed the misspelled `licence` metadata key.
- New vignettes: *Reading and writing a hydrofabric*, *Network
  properties*, and *Cloud access, versioning, and quality assurance*.
- Grouped pkgdown reference index; package is `R CMD check` clean (no
  NOTEs).
- Continuous integration: added an `R-CMD-check` matrix
  (macOS/Windows/Ubuntu, R release/devel/oldrel) and a `test-coverage`
  (covr/Codecov) workflow, with badges. The pkgdown site now builds and
  deploys through `docs/`.
- Test coverage of the package’s testable logic is ~79% (core files
  75-99%). Three files that require external services –
  [`clean_gpkg_layer()`](https://lynker-spatial.github.io/hfutils/reference/clean_gpkg_layer.md)
  (GDAL/ mapshaper CLIs),
  `duckdb`/[`tbl_http()`](https://lynker-spatial.github.io/hfutils/reference/tbl_http.md)
  (live DuckDB + cloud), and the OAuth flow in `auth.R` – are excluded
  from measurement via `.covrignore` and documented in
  `CONTRIBUTING.md`. New suites for
  [`hf_check_invariants()`](https://lynker-spatial.github.io/hfutils/reference/hf_check_invariants.md)
  (all four stages incl. reconciled flow-direction, coverage, and
  fp-\>nexus-\>fp DAG checks, strict/non-strict),
  [`get_hydroseq()`](https://lynker-spatial.github.io/hfutils/reference/get_hydroseq.md),
  the
  [`write_hydrofabric()`](https://lynker-spatial.github.io/hfutils/reference/write_hydrofabric.md)/[`read_hydrofabric()`](https://lynker-spatial.github.io/hfutils/reference/read_hydrofabric.md)
  round trip (incl. mixed sf + attribute-table layers),
  [`clean_geometry()`](https://lynker-spatial.github.io/hfutils/reference/clean_geometry.md)
  (single-part, multi-part, flowline-driven, simplification),
  [`union_polygons()`](https://lynker-spatial.github.io/hfutils/reference/union_polygons.md)
  /
  [`union_linestrings()`](https://lynker-spatial.github.io/hfutils/reference/union_linestrings.md)
  /
  [`flowpaths_to_linestrings()`](https://lynker-spatial.github.io/hfutils/reference/flowpaths_to_linestrings.md),
  the small measure/node helpers, the lazy
  [`as_ogr()`](https://lynker-spatial.github.io/hfutils/reference/as_ogr.md)
  OGR-SQL backend (named layer, raw query, multi-layer), GeoParquet
  round trips, and auth guard paths.
- Standardized user-facing errors in
  [`write_hydrofabric()`](https://lynker-spatial.github.io/hfutils/reference/write_hydrofabric.md)
  and
  [`as_ogr()`](https://lynker-spatial.github.io/hfutils/reference/as_ogr.md)
  on
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).

## hfutils 0.3.3

- Add
  [`gpkg_set_version()`](https://lynker-spatial.github.io/hfutils/reference/gpkg_set_version.md)
  /
  [`gpkg_get_version()`](https://lynker-spatial.github.io/hfutils/reference/gpkg_get_version.md)
  — stamp and read a dataset version (machine integer `Mmmpp` + human
  semver) into the standard GeoPackage metadata extension tables, with
  an optional build-provenance JSON entry and an SPDX license.
  Idempotent; leaves the GeoPackage spec version (`user_version`)
  untouched.
- Add
  [`hf_check_invariants()`](https://lynker-spatial.github.io/hfutils/reference/hf_check_invariants.md)
  — shared staged pipeline invariant checks (`refactored` / `reconciled`
  / `aggregated` / `ngen`) so every layer of the stack can use one
  implementation.
- [`accumulate_downstream()`](https://lynker-spatial.github.io/hfutils/reference/accumulate_downstream.md)
  now builds its topological-sort graph via
  [`igraph::graph_from_data_frame()`](https://r.igraph.org/reference/graph_from_data_frame.html)
  for consistent id/toid handling.
- Add a testthat scaffold with topology and GeoPackage-metadata
  round-trip tests.
- Lynker Spatial pkgdown site with cross-stack navigation.
