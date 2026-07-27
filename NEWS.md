# hfutils 0.4.1

* New `upstream_index()`: a nested-set upstream index (`upstream_id` and
  `num_upstreams`) over a rooted-tree network, so everything upstream of a node
  is an O(1) integer range filter (`upstream_id` in `(u, u + k]`) with no
  traversal. Reuses `accumulate_downstream()` for the count (inheriting its
  acyclic check) and flags divergences or cycles rather than mis-indexing.
* New `merge_groups()`: groups a network into contiguous same-order runs over
  the `upstream_index()` pre-order. Each group is a contiguous `upstream_id`
  range, so a size-budgeted tiler or partitioner can merge groups into balanced
  chunks that are always complete sub-networks.

# hfutils 0.4.0

Completes the topological network-property family.

* New `get_pathlength()`: distance along the network from each reach's outlet
  to the terminal outlet (the NHDPlus `PathLength` attribute).
* New `get_streamlevel()`: stream level, the number of level-path steps from a
  reach to the network terminus (the NHDPlus `StreamLeve` attribute).
* New `get_pfafstetter()`: hierarchical Pfafstetter basin codes, verified
  against a full 746-reach reference basin.
* These join `accumulate_downstream()`, `get_hydroseq()`, `get_streamorder()`,
  and `get_levelpath()`; all are character-safe and return a vector aligned to
  the input rows, and now cross-reference each other in their help pages.

# hfutils 0.3.4

Correctness and quality pass on the base layer.

* `tbl_http()` no longer errors on its default call; the malformed
  `read_func` default is fixed and validated with `match.arg()`.
* `get_hydroseq()` is now character-safe: non-numeric identifiers (`"fp-"`,
  scientific-notation strings) round-trip instead of being coerced to `NA`.
* `write_hydrofabric()` defaults to `enforce_dm = FALSE`; the data-model schema
  (`hf_dm`) is only consulted when a caller supplies it, so the default call no
  longer fails looking for an object hfutils does not ship.
* Fixed `union_polygons()` erroring (`object '.' not found`) in its
  disjoint-MULTIPOLYGON dedup branch; it used the magrittr `.` pronoun under a
  native `|>` pipe. This path fires exactly when a group unions to disjoint
  parts; now regression-tested.
* Removed divergent duplicate definitions of `rename_geometry()`,
  `add_areasqkm()`, and `fast_validity_check()` (kept one canonical copy each).
* `create_metadata()` no longer warns on every GeoParquet write (now opt-in via
  `quiet = FALSE`); fixed the misspelled `licence` metadata key.
* New vignettes: *Reading and writing a hydrofabric*, *Network properties*, and
  *Cloud access, versioning, and quality assurance*.
* Grouped pkgdown reference index; package is `R CMD check` clean (no NOTEs).
* Continuous integration: added an `R-CMD-check` matrix (macOS/Windows/Ubuntu,
  R release/devel/oldrel) and a `test-coverage` (covr/Codecov) workflow, with
  badges. The pkgdown site now builds and deploys through `docs/`.
* Test coverage of the package's testable logic is ~79% (core files 75-99%).
  Three files that require external services -- `clean_gpkg_layer()` (GDAL/
  mapshaper CLIs), `duckdb`/`tbl_http()` (live DuckDB + cloud), and the OAuth
  flow in `auth.R` -- are excluded from measurement via `.covrignore` and
  documented in `CONTRIBUTING.md`. New suites for `hf_check_invariants()`
  (all four stages incl. reconciled flow-direction, coverage, and fp->nexus->fp
  DAG checks, strict/non-strict), `get_hydroseq()`, the
  `write_hydrofabric()`/`read_hydrofabric()` round trip (incl. mixed sf +
  attribute-table layers), `clean_geometry()` (single-part, multi-part,
  flowline-driven, simplification), `union_polygons()` / `union_linestrings()` /
  `flowpaths_to_linestrings()`, the small measure/node helpers, the lazy
  `as_ogr()` OGR-SQL backend (named layer, raw query, multi-layer), GeoParquet
  round trips, and auth guard paths.
* Standardized user-facing errors in `write_hydrofabric()` and `as_ogr()` on
  `cli::cli_abort()`.
* **Attribute-integrity invariants.** `hf_check_merge_invariants()` and the
  per-stage `hf_check_invariants()` (aggregated/ngen) now guard
  `mainstem_id_populated` and `hydroseq_valid` via shared `.hf_mainstem_check()`
  / `.hf_hydroseq_check()` helpers, catching carried/recomputed columns that
  are silently dropped or mis-mapped (the class behind two Stage-4 regressions
  in `hydrofabric`). Tests cover dropped / duplicate / clean at the per-stage
  and merge entry points.
* `dbGetInfo()` on the `OGRSQLDriver` now reports `hfutils`' own version for
  `client.version` (was an undeclared, unguarded `packageVersion("hfsubsetR")`
  that errored on any machine without that sibling package installed).
* `tbl_http()` forwards named reader options through `...` to the DuckDB read
  function (e.g. `union_by_name = TRUE` -> `union_by_name=true`); previously
  `...` was accepted but silently dropped.
* `lynker_spatial_auth()` documents per-library behavior and adds `"arrow"` as
  an opt-in target; arrow authenticates lynker-spatial via the S3 credential
  chain (it has no HTTP-header filesystem for the bearer token).
* `\dontrun{}` examples on every exported function; spell-check setup
  (`Language`, `inst/WORDLIST`, `tests/spelling.R`); `styler` + a documented
  `.lintr` policy (lint-clean) + a `lint` CI workflow.

# hfutils 0.3.3

* Add `gpkg_set_version()` / `gpkg_get_version()`: stamp and read a dataset
  version (machine integer `Mmmpp` + human semver) into the standard GeoPackage
  metadata extension tables, with an optional build-provenance JSON entry and an
  SPDX license. Idempotent; leaves the GeoPackage spec version (`user_version`)
  untouched.
* Add `hf_check_invariants()`: shared staged pipeline invariant checks
  (`refactored` / `reconciled` / `aggregated` / `ngen`) so every layer of the
  stack can use one implementation.
* `accumulate_downstream()` now builds its topological-sort graph via
  `igraph::graph_from_data_frame()` for consistent id/toid handling.
* Add a testthat scaffold with topology and GeoPackage-metadata round-trip
  tests.
* Lynker Spatial pkgdown site with cross-stack navigation.
