# hfutils 0.3.3

* Add `gpkg_set_version()` / `gpkg_get_version()` — stamp and read a dataset
  version (machine integer `Mmmpp` + human semver) into the standard GeoPackage
  metadata extension tables, with an optional build-provenance JSON entry and an
  SPDX license. Idempotent; leaves the GeoPackage spec version (`user_version`)
  untouched.
* Add `hf_check_invariants()` — shared staged pipeline invariant checks
  (`refactored` / `reconciled` / `aggregated` / `ngen`) so every layer of the
  stack can use one implementation.
* `accumulate_downstream()` now builds its topological-sort graph via
  `igraph::graph_from_data_frame()` for consistent id/toid handling.
* Add a testthat scaffold with topology and GeoPackage-metadata round-trip
  tests.
* Lynker Spatial pkgdown site with cross-stack navigation.
