# hfutils 0.4.2

First release since 0.3.4. Completes the topological network-property family,
adds a nested-set upstream index, and makes several in-place write paths exact.
0.4.0 and 0.4.1 were development versions and were never released.

* New `get_pathlength()`: distance along the network from each reach's outlet
  to the terminal outlet (the NHDPlus `PathLength` attribute).
* New `get_streamlevel()`: stream level, the number of level-path steps from a
  reach to the network terminus (the NHDPlus `StreamLeve` attribute).
* New `get_pfafstetter()`: hierarchical Pfafstetter basin codes, verified
  against a full 746-reach reference basin.
* These join `accumulate_downstream()`, `get_hydroseq()`, `get_streamorder()`,
  and `get_levelpath()`; all are character-safe and return a vector aligned to
  the input rows, and now cross-reference each other in their help pages.
* New `upstream_index()`: a nested-set upstream index (`upstream_id` and
  `num_upstreams`) over a rooted-tree network, so everything upstream of a node
  is an O(1) integer range filter (`upstream_id` in `(u, u + k]`) with no
  traversal. Reuses `accumulate_downstream()` for the count (inheriting its
  acyclic check) and flags divergences or cycles rather than mis-indexing.
* New `merge_groups()`: groups a network into contiguous same-order runs over
  the `upstream_index()` pre-order. Each group is a contiguous `upstream_id`
  range, so a size-budgeted tiler or partitioner can merge groups into balanced
  chunks that are always complete sub-networks.
* New `hf_upstream_index()`: applies the `upstream_index()` nested set at
  hydrofabric grain, resolving `flowpath -> nexus -> flowpath` hops into a
  direct flowpath graph before indexing. A `flowpath_toid` that is already a
  flowpath is followed directly, so networks written without a nexus layer
  index correctly rather than resolving every node to an isolated terminal. A
  divergent nexus (two distinct downstreams) is counted in `n_divergences` and
  warned, since the nested set cannot represent it.
* `write_hydrofabric()` now stamps `upstream_id` and `num_upstreams` onto every
  flowpath-keyed layer whenever the written list carries a flowpath topology,
  so a written GeoPackage supports O(1) upstream range queries. The step is
  attribute-only and is skipped without failing the write when the topology is
  absent or not acyclic. The index is scoped to whatever topology is written,
  so per-VPU, merged, and subset writes each get a correct index; the values
  are build-specific and are not persistent keys.
* `union_polygons()` is now an exact dissolve. It previously routed geometry
  through `terra::makeValid() |> terra::aggregate()`, which perturbed shared
  boundaries enough that neighboring groups came out overlapping: on one CONUS
  VPU the summed area of the result exceeded the summed area of its inputs by
  20 km2, entirely overlap between adjacent groups. Downstream cleanup then
  existed to remove that overlap. Grouped `sf::st_union()` over already-valid
  inputs preserves the input tiling exactly -- measured on 61,061 real divides
  in 23,910 groups: overlap 20 km2 -> 0.000000 km2, area delta -0.0005 km2, and
  21s instead of ~65s.
* `union_polygons()` no longer casts its result to `POLYGON` and keeps only the
  largest part per group. A group whose members are genuinely disjoint is a
  multipart catchment, not an error, and discarding the smaller parts silently
  deleted ground. Output is `MULTIPOLYGON`. This path was latent rather than
  active in current data, but it is the same defect that had to be fixed
  separately downstream.
* `union_polygons()` repairs input geometry only where `sf::st_is_valid()`
  reports a problem, so valid input passes through untouched.
* `gpkg_update_geom()` no longer leaves its temporary layer's spatial index
  behind. It dropped the temp feature table and its `gpkg_contents` /
  `gpkg_geometry_columns` rows, but not the four `rtree_*` shadow tables GDAL
  creates alongside a spatial layer, their triggers, or the `gpkg_extensions`
  registration. Those accumulated in the GeoPackage on every call: ten geometry
  updates left forty orphan tables in the file. Removal is now complete and
  happens inside the same transaction as the geometry swap.
* `gpkg_update_geom()` names its temporary layer from `tempfile()` rather than
  the clock. The previous `%H%M%S%OS2` name collided when two calls landed in
  the same centisecond and embedded a `.` in a SQL identifier.
* `as_ogr()` now ignores the two tables QGIS writes into a GeoPackage when a
  style or a project is saved to it (`layer_styles`, `qgis_projects`). They are
  a QGIS extension rather than part of the GeoPackage spec, so they were being
  counted as user layers: a styled single-layer fabric stopped auto-resolving
  and began erroring as ambiguous.
* The `as_ogr()` `ignore_lyrs` pattern is now anchored (`^gpkg_`, `^rtree_`,
  `^sqlite_`). Unanchored, it silently dropped any real layer whose name merely
  contained one of those fragments, such as `flowpaths_gpkg_v2`. The default is
  also defined once and shared by the generic and both methods, which
  previously carried three separate copies of the literal.
* First tests for `gpkg_update_col()`, `gpkg_update_geom()`, and `gpkg_exec()`,
  covering targeted-row updates, numeric round-tripping, trigger restoration,
  temp-layer cleanup, and transaction rollback. All three write destructively
  in place and had no coverage.

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
