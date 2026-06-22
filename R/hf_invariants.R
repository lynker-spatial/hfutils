#' Pipeline invariant checks
#'
#' @description
#' Runs a set of invariant assertions at a named pipeline stage. Designed to
#' be called between pipeline steps so silent failures (orphan divides,
#' unsplit parents, raster artifacts) halt the run at the step that created
#' the problem -- not 200 lines later during visual inspection.
#'
#' @param stage Character. One of `"refactored"`, `"reconciled"`,
#'   `"aggregated"`, `"ngen"`.
#' @param ... Stage-specific named arguments (see details).
#' @param strict Logical. If `TRUE` (default) any failed check throws an
#'   error. If `FALSE`, failures become warnings.
#' @param coverage Logical. If `TRUE`, run the expensive flowpath-in-catchment
#'   coverage check (aggregated and ngen stages only). Default `FALSE`.
#' @param coverage_min Minimum fraction of a flowpath that must lie inside
#'   its assigned catchment. Default `0.90`.
#' @param attr_bounds Logical. If `TRUE`, append a per-attribute physical-range
#'   plausibility pass over the `divides` and `flowpaths` layers (see
#'   [hf_check_attr_bounds()]). Soft/warn-only and off by default, so it never
#'   changes the returned `ok` for existing callers until enabled. Absent
#'   attribute columns are skipped.
#' @param domain Domain code (`"CONUS"`, `"AK"`, `"HI"`, `"PRVI"`) selecting the
#'   `lat`/`lon` bounds for the attribute pass; `NULL` skips lat/lon.
#' @param attr_trust_caveated Logical. Include attribute bounds flagged in the
#'   `caveat` column of the bounds table (default `FALSE`).
#'
#' @details
#' Expected arguments per stage:
#'
#' - `stage = "refactored"`: `refactored` (sf, split NHD flowlines),
#'   `reconciled` (sf, collapsed-and-reconciled lines with integer `ID`).
#' - `stage = "reconciled"`: `reconciled` (sf), `divides` (sf of reconciled
#'   divides).
#' - `stage = "aggregated"`: `flowpaths` (sf), `divides` (sf), `network`
#'   (data.frame, optional).
#' - `stage = "ngen"`: `flowpaths` (sf with `fp-` IDs), `divides` (sf with
#'   `cat-` IDs).
#'
#' @return Invisibly a list with `ok` (logical), `stage`, and `checks`
#'   (named list of per-check results).
#'
#' @importFrom sf st_area st_length st_intersection st_sfc st_crs st_transform
#'   st_is_empty st_geometry st_geometry_type
#' @importFrom igraph graph_from_data_frame topo_sort as_ids V
#' @importFrom lwgeom st_endpoint st_startpoint
#' @importFrom utils head
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#' flowpaths <- sf::read_sf("hydrofabric.gpkg", "flowpaths")
#' divides   <- sf::read_sf("hydrofabric.gpkg", "divides")
#' hf_check_invariants("ngen", flowpaths = flowpaths, divides = divides,
#'                     strict = FALSE)
#' }
#' @export
hf_check_invariants <- function(stage, ..., strict = TRUE,
                                coverage = FALSE, coverage_min = 0.90,
                                attr_bounds = FALSE, domain = NULL,
                                attr_trust_caveated = FALSE) {
  stage <- match.arg(stage, c("refactored", "reconciled", "aggregated", "ngen"))
  args <- list(...)
  checks <- switch(stage,
    refactored = .hf_check_refactored(args),
    reconciled = .hf_check_reconciled(args, coverage = coverage,
                                      coverage_min = coverage_min),
    aggregated = .hf_check_aggregated(args, coverage = coverage,
                                      coverage_min = coverage_min),
    ngen       = .hf_check_ngen(args, coverage = coverage,
                                coverage_min = coverage_min))

  # Optional per-attribute physical-plausibility pass. Soft (warn-only) and
  # off by default, so it never disturbs callers gating on the returned `ok`
  # until explicitly enabled. Absent attribute columns are skipped.
  if (isTRUE(attr_bounds) && stage %in% c("reconciled", "aggregated", "ngen")) {
    checks <- c(checks,
      .hf_attr_bounds_checks(args$divides, "divides", domain = domain,
                             trust_caveated = attr_trust_caveated, prefix = "div"),
      .hf_attr_bounds_checks(args$flowpaths, "flowpaths", domain = domain,
                             trust_caveated = attr_trust_caveated, prefix = "fp"))
  }

  .hf_report_checks(checks, stage, strict)
}

.hf_check_refactored <- function(args) {
  refactored <- args$refactored
  reconciled <- args$reconciled
  checks <- list()

  if (!is.null(refactored)) {
    ids <- as.character(refactored$flowpath_id)
    checks$refactored_unique <- .hf_ok(
      sum(duplicated(ids)) == 0,
      sprintf("refactored flowpath_id has %d duplicate(s)",
              sum(duplicated(ids))))
    toids <- as.character(refactored$flowpath_toid)
    dangling <- setdiff(toids[toids != "0" & !is.na(toids)], ids)
    checks$refactored_topology <- .hf_ok(
      length(dangling) == 0,
      sprintf("%d refactored flowpath_toid values reference nonexistent IDs",
              length(dangling)))
  }

  if (!is.null(reconciled)) {
    rec_ids <- as.character(reconciled$ID)
    checks$reconciled_unique <- .hf_ok(
      sum(duplicated(rec_ids)) == 0,
      sprintf("reconciled ID has %d duplicate(s)", sum(duplicated(rec_ids))))

    mems <- lapply(reconciled$member_flowpath_id, function(m) {
      if (is.null(m) || length(m) == 0) return(character(0))
      s <- if (is.list(m)) unlist(m) else as.character(m)
      trimws(unlist(strsplit(s, ",", fixed = TRUE)))
    })
    n_empty <- sum(lengths(mems) == 0L)
    checks$reconciled_has_members <- .hf_ok(
      n_empty == 0L,
      sprintf("%d reconciled group(s) have no member flowlines", n_empty))

    if (!is.null(refactored)) {
      all_mems <- unique(unlist(mems))
      rfp_ids  <- as.character(refactored$flowpath_id)
      # Base COMIDs whose .N split children are in refactored count as valid;
      # fast_refactor intentionally lists both the split children and the
      # unsplit parent COMID as members for lineage carry-through.
      base_ids <- unique(sub("\\.[0-9]+$", "", rfp_ids))
      valid    <- union(rfp_ids, base_ids)
      missing  <- setdiff(all_mems, valid)
      checks$reconciled_members_exist <- .hf_ok(
        length(missing) == 0L,
        sprintf("%d reconciled member ID(s) not in refactored or split-parents (e.g. %s)",
                length(missing), paste(utils::head(missing, 3), collapse = ", ")))
    }
  }
  checks
}

.hf_check_reconciled <- function(args, coverage, coverage_min) {
  reconciled <- args$reconciled
  divides    <- args$divides
  checks <- list()

  # Flow-direction check: only meaningful for LINESTRING geometries.
  # Collapsed MULTILINESTRING reaches have no guaranteed downstream-last
  # component ordering, so st_endpoint / st_startpoint produce unreliable
  # outlet/inlet proxies and would inflate the disconnected count.
  if (!is.null(reconciled) && inherits(reconciled, "sf") &&
      "toID" %in% names(reconciled) && nrow(reconciled) > 0L) {
    geom_types <- as.character(sf::st_geometry_type(reconciled))
    ls_rows    <- geom_types == "LINESTRING"
    if (any(ls_rows)) {
      rec_ls     <- reconciled[ls_rows, ]
      rec_ids    <- as.character(rec_ls$ID)
      rid_to_idx <- setNames(seq_len(nrow(rec_ls)), rec_ids)
      rtoid      <- as.character(rec_ls$toID)
      has_ds     <- !is.na(rtoid) & rtoid != "0" & rtoid %in% names(rid_to_idx)
      if (any(has_ds)) {
        eps <- sf::st_coordinates(
          lwgeom::st_endpoint(sf::st_geometry(rec_ls)))[, 1:2]
        sps <- sf::st_coordinates(
          lwgeom::st_startpoint(sf::st_geometry(rec_ls)))[, 1:2]
        ds_idx  <- rid_to_idx[rtoid[has_ds]]
        d_end   <- sqrt(rowSums((eps[has_ds, , drop = FALSE] -
                                  sps[ds_idx, , drop = FALSE])^2))
        d_start <- sqrt(rowSums((sps[has_ds, , drop = FALSE] -
                                  sps[ds_idx, , drop = FALSE])^2))
        flagged      <- which(d_end > 100)
        reversed     <- sum(d_start[flagged] < d_end[flagged])
        disconnected <- length(flagged) - reversed
        checks$reconciled_flow_direction <- .hf_ok(
          reversed == 0L,
          sprintf("%d flowline(s) reversed (fix: sf::st_reverse); %d disconnected (geometry gap, not reversal)",
                  reversed, disconnected))
      }
    }
  }

  if (is.null(divides)) return(checks)

  # Split network from landscape divides
  is_lnd <- grepl("^lnd-", as.character(divides$divide_id)) |
            is.na(divides$flowpath_id)
  net_divs <- divides[!is_lnd, ]
  lnd_divs <- divides[ is_lnd, ]

  # Every reconciled flowline has a network divide.
  # Flowpaths that are legitimately divideless (all reference members had
  # has_divide = FALSE) are excluded when the reconciled sf carries has_divide.
  if (!is.null(reconciled)) {
    rec_ids <- as.character(reconciled$ID)
    if ("has_divide" %in% names(reconciled))
      rec_ids <- rec_ids[is.na(reconciled$has_divide) | as.logical(reconciled$has_divide)]
    div_fp  <- as.character(net_divs$flowpath_id)
    missing <- setdiff(rec_ids, div_fp)
    checks$every_flowline_has_divide <- .hf_ok(
      length(missing) == 0L,
      sprintf("%d reconciled flowline(s) have no matching network divide (e.g. %s)",
              length(missing), paste(utils::head(missing, 5), collapse = ", ")))
  }

  # All network divides have area > 0
  if (nrow(net_divs) > 0L) {
    areas <- suppressWarnings(as.numeric(sf::st_area(net_divs)))
    n_tiny <- sum(!is.finite(areas) | areas < 1, na.rm = TRUE)
    checks$network_divides_have_area <- .hf_ok(
      n_tiny == 0L,
      sprintf("%d network divide(s) have < 1 m2 area", n_tiny))
  }

  # No excessive interior rings (raster artifacts)
  n_rings <- .hf_ring_counts(sf::st_geometry(net_divs))
  max_rings <- if (length(n_rings)) max(n_rings) else 0L
  checks$no_excessive_rings <- .hf_ok(
    max_rings <= 50L,
    sprintf("max interior rings in a single divide: %d (threshold: 50)",
            max_rings))

  # Landscape carry-through non-empty (informational, not strict)
  checks$landscape_preserved <- .hf_info(
    sprintf("landscape divides carried: %d", nrow(lnd_divs)))

  checks
}

.hf_check_aggregated <- function(args, coverage, coverage_min) {
  flowpaths <- args$flowpaths
  divides   <- args$divides
  checks <- list()

  if (!is.null(flowpaths)) {
    ids <- as.character(flowpaths$flowpath_id)
    checks$aggregated_flowpath_unique <- .hf_ok(
      sum(duplicated(ids)) == 0L,
      sprintf("aggregated flowpath_id has %d duplicate(s)",
              sum(duplicated(ids))))
  }

  if (!is.null(divides)) {
    dids <- as.character(divides$divide_id)
    checks$aggregated_divide_unique <- .hf_ok(
      sum(duplicated(dids)) == 0L,
      sprintf("aggregated divide_id has %d duplicate(s)",
              sum(duplicated(dids))))
  }

  if (coverage && !is.null(flowpaths) && !is.null(divides)) {
    checks$flowpath_coverage <- .hf_coverage_check(
      flowpaths, divides, coverage_min)
  }

  checks
}

.hf_check_ngen <- function(args, coverage, coverage_min) {
  flowpaths <- args$flowpaths
  divides   <- args$divides
  nexus     <- args$nexus
  checks <- list()

  if (!is.null(flowpaths)) {
    ids <- as.character(flowpaths$flowpath_id)
    n_bad <- sum(!is.na(ids) & !startsWith(ids, "fp-"))
    checks$ngen_fp_prefix <- .hf_ok(n_bad == 0L,
      sprintf("%d ngen flowpath_id(s) do not start with 'fp-'", n_bad))
  }

  if (!is.null(divides)) {
    dids <- as.character(divides$divide_id)
    # 'lnd-' is accepted for landscape (non-flowline) carry-through
    n_bad <- sum(!is.na(dids) &
                  !startsWith(dids, "cat-") &
                  !startsWith(dids, "lnd-"))
    checks$ngen_cat_prefix <- .hf_ok(n_bad == 0L,
      sprintf("%d ngen divide_id(s) are not 'cat-' or 'lnd-' prefixed", n_bad))
  }

  # DAG check -- flatten fp -> nexus -> fp into direct fp -> fp edges and verify
  # the combined graph has no cycles. Including connector (no-divide) fps
  # must not introduce cycles.
  if (!is.null(flowpaths) && !is.null(nexus)) {
    nex_lu <- setNames(as.character(nexus$nexus_toid),
                        as.character(nexus$nexus_id))
    next_fp <- nex_lu[as.character(flowpaths$flowpath_toid)]
    edge_df <- data.frame(
      flowpath_id   = as.character(flowpaths$flowpath_id),
      flowpath_toid = ifelse(is.na(next_fp), "0", as.character(next_fp)),
      stringsAsFactors = FALSE)
    is_dag <- tryCatch(
      .hf_network_is_dag(edge_df,
        id_col = "flowpath_id", toid_col = "flowpath_toid"),
      error = function(e) NA)
    if (isTRUE(is_dag)) {
      checks$network_is_dag <- .hf_ok(TRUE,
        sprintf("fp->nexus->fp graph is a DAG (%d flowpaths, %d connectors)",
                nrow(flowpaths),
                sum(!flowpaths$has_divide, na.rm = TRUE)))
    } else {
      checks$network_is_dag <- .hf_ok(FALSE,
        "fp->nexus->fp graph contains cycle(s)")
    }
  }

  if (coverage && !is.null(flowpaths) && !is.null(divides)) {
    checks$flowpath_coverage <- .hf_coverage_check(
      flowpaths, divides, coverage_min)
  }

  checks
}

# ---- helpers -----------------------------------------------------------------

.hf_network_is_dag <- function(flowpaths, id_col = "flowpath_id",
                                toid_col = "flowpath_toid") {
  ids   <- as.character(flowpaths[[id_col]])
  toids <- as.character(flowpaths[[toid_col]])
  keep  <- !is.na(toids) & toids != "0" & toids %in% ids
  if (!any(keep)) return(TRUE)
  edge_df <- data.frame(from = ids[keep], to = toids[keep],
                        stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(edge_df, directed = TRUE)
  igraph::is_dag(g)
}

.hf_ok <- function(ok, msg) list(ok = isTRUE(ok), msg = msg, kind = "check")

.hf_info <- function(msg) list(ok = TRUE, msg = msg, kind = "info")

.hf_ring_counts <- function(geoms) {
  vapply(geoms, function(g) {
    if (length(g) == 0L || sf::st_is_empty(g)) return(0L)
    if (inherits(g, "POLYGON"))      return(max(0L, length(g) - 1L))
    if (inherits(g, "MULTIPOLYGON")) return(sum(vapply(g, function(p)
      max(0L, length(p) - 1L), integer(1L))))
    0L
  }, integer(1L))
}

.hf_coverage_check <- function(flowpaths, divides, coverage_min) {
  # For each flowpath with a named divide, compute length-coverage ratio.
  # Uses vectorized sf::st_intersects (spatial index) then per-row
  # intersection only for matched pairs.
  has_fp <- !is.na(flowpaths$divide_id) & nchar(as.character(flowpaths$divide_id)) > 0L
  fp_sub <- flowpaths[has_fp, ]
  if (nrow(fp_sub) == 0L) return(.hf_info("no flowpath-divide pairs to check"))

  fp_t  <- sf::st_transform(fp_sub, sf::st_crs(divides))
  d_ix  <- match(as.character(fp_t$divide_id), as.character(divides$divide_id))
  matched <- !is.na(d_ix)
  if (!any(matched))
    return(.hf_ok(FALSE, "no flowpaths matched a divide by divide_id"))

  fp_g <- sf::st_geometry(fp_t)
  d_g  <- sf::st_geometry(divides)
  total_len <- as.numeric(sf::st_length(fp_g))
  inside    <- numeric(length(total_len))
  for (i in which(matched)) {
    inter <- tryCatch(
      sf::st_intersection(fp_g[[i]], d_g[[d_ix[i]]]),
      error = function(e) NULL)
    if (is.null(inter) || length(inter) == 0L) next
    inside[i] <- as.numeric(sf::st_length(sf::st_sfc(inter,
                                                     crs = sf::st_crs(fp_t))))
  }
  frac <- ifelse(total_len > 0, inside / total_len, NA_real_)
  bad  <- which(matched & !is.na(frac) & frac < coverage_min)
  ok   <- length(bad) == 0L
  msg  <- if (ok) sprintf("all %d flowpaths have >= %.0f%% coverage by their catchment",
                          sum(matched), 100 * coverage_min)
          else sprintf("%d flowpath(s) below %.0f%% coverage (worst: %s at %.0f%%)",
                       length(bad), 100 * coverage_min,
                       as.character(fp_sub$flowpath_id[bad[which.min(frac[bad])]]),
                       100 * min(frac[bad], na.rm = TRUE))
  .hf_ok(ok, msg)
}

.hf_report_checks <- function(checks, stage, strict) {
  ok_all <- all(vapply(checks, function(x) isTRUE(x$ok), logical(1L)))
  for (nm in names(checks)) {
    res <- checks[[nm]]
    icon <- if (identical(res$kind, "info")) "i"
            else if (isTRUE(res$ok))         "\u2713"
            else                              "\u2717"
    tag <- sprintf("[invariants:%s]", stage)
    line <- sprintf("%s %s %-32s %s", tag, icon, nm, res$msg)
    if (isTRUE(res$ok) || identical(res$kind, "info")) message(line)
    else if (strict && !isTRUE(res$soft)) stop(line, call. = FALSE)
    else message(line)
  }
  invisible(list(ok = ok_all, stage = stage, checks = checks))
}

#' Global-merge (Stage 4) conservation + completeness invariants
#'
#' Complements the structural merge checks (id-uniqueness, prefixes, toid
#' resolution, divide FK, DAG) with the conservation/completeness class that
#' catches SILENT data loss or corruption when per-VPU fabrics are merged into a
#' global domain: a dropped/duplicated feature, lost catchment area, a missing
#' VPU, zeroed drainage area (the `add_measures` zero-fill class), mixed CRS, or
#' an untagged feature. Reusable across any domain merge.
#'
#' @param merged named list with `flowpaths`, `divides`, `nexus` (sf/data.frame)
#'   for the merged domain product.
#' @param expected optional list of input expectations summed over the per-VPU
#'   inputs: `vpus` (character), `n_flowpaths`, `n_divides`, `area_sqkm`. When a
#'   field is absent the corresponding check reports as info (skipped).
#' @param area_tol fractional tolerance for divide-area conservation (default 0.005).
#' @param stage message label (default "merge").
#' @param strict if TRUE, a failed check stops execution (else warns).
#' @return invisibly `list(ok, stage, checks)`.
#' @examples
#' \dontrun{
#' merged <- list(
#'   flowpaths = sf::read_sf("conus.gpkg", "flowpaths"),
#'   divides   = sf::read_sf("conus.gpkg", "divides"),
#'   nexus     = sf::read_sf("conus.gpkg", "nexus")
#' )
#' hf_check_merge_invariants(merged,
#'   expected = list(vpus = sprintf("%02d", 1:18)))
#' }
#' @export
hf_check_merge_invariants <- function(merged, expected = NULL,
                                      area_tol = 0.005, stage = "merge",
                                      strict = FALSE) {
  fp <- merged$flowpaths; dv <- merged$divides; nx <- merged$nexus
  if (is.null(fp) || is.null(dv)) stop("merged must contain flowpaths and divides")
  checks <- list()
  vcol <- function(x) if (!is.null(x) && "vpuid" %in% names(x)) as.character(x$vpuid) else character(0)

  # completeness: every expected VPU made it into the merge
  seen <- unique(c(vcol(fp), vcol(dv)))
  if (!is.null(expected$vpus)) {
    miss <- setdiff(as.character(expected$vpus), seen)
    checks$all_vpus_present <- .hf_ok(length(miss) == 0L,
      if (!length(miss)) sprintf("all %d expected VPU(s) present", length(expected$vpus))
      else sprintf("%d expected VPU(s) missing: %s", length(miss), paste(miss, collapse = ", ")))
  } else checks$all_vpus_present <- .hf_info("no expected VPU list supplied")

  # every feature tagged with its source VPU
  vfp <- vcol(fp); n_untag <- if (length(vfp)) sum(is.na(vfp) | !nzchar(vfp)) else nrow(fp)
  checks$vpuid_tagged <- .hf_ok(n_untag == 0L,
    if (!n_untag) "every flowpath carries a vpuid" else sprintf("%d flowpath(s) missing vpuid", n_untag))

  # feature-count conservation (flowpaths/divides are 1:1 re-ID'd, never merged)
  if (!is.null(expected$n_flowpaths))
    checks$flowpath_count_conserved <- .hf_ok(nrow(fp) == expected$n_flowpaths,
      sprintf("merged flowpaths %d vs input sum %d", nrow(fp), expected$n_flowpaths))
  else checks$flowpath_count_conserved <- .hf_info(sprintf("merged flowpaths: %d", nrow(fp)))
  if (!is.null(expected$n_divides))
    checks$divide_count_conserved <- .hf_ok(nrow(dv) == expected$n_divides,
      sprintf("merged divides %d vs input sum %d", nrow(dv), expected$n_divides))
  else checks$divide_count_conserved <- .hf_info(sprintf("merged divides: %d", nrow(dv)))

  # divide-area conservation
  if (!is.null(expected$area_sqkm) && "areasqkm" %in% names(dv)) {
    got <- sum(as.numeric(dv$areasqkm), na.rm = TRUE)
    drift <- if (expected$area_sqkm > 0) abs(got - expected$area_sqkm) / expected$area_sqkm else NA_real_
    checks$divide_area_conserved <- .hf_ok(!is.na(drift) && drift <= area_tol,
      sprintf("merged divide area %.1f km2 vs input %.1f km2 (%.3f%% drift, tol %.3f%%)",
              got, expected$area_sqkm, 100 * drift, 100 * area_tol))
  } else checks$divide_area_conserved <- .hf_info("no input area or areasqkm column")

  # drainage area populated + accumulating.
  if ("total_dasqkm" %in% names(fp)) {
    da   <- suppressWarnings(as.numeric(fp$total_dasqkm))
    area <- if ("areasqkm" %in% names(fp)) suppressWarnings(as.numeric(fp$areasqkm))
            else rep(NA_real_, length(da))
    # Correct DA invariant: total_dasqkm must (a) never be NA, (b) be >= its own
    # local area (accumulation never loses own area), and (c) be > 0 wherever the
    # flowpath has a real catchment (areasqkm > 0). A catchment-less connector
    # (areasqkm == 0) fed only by other catchment-less reaches legitimately has
    # total_dasqkm == 0 -- NOT a failure (previously these were wrongly flagged).
    n_bad <- sum(is.na(da) |
                 (!is.na(area) & da < area - 1e-6) |
                 (!is.na(area) & area > 0 & da <= 0))
    checks$drainage_area_populated <- .hf_ok(n_bad == 0L,
      if (!n_bad) sprintf("all %d flowpaths have valid total_dasqkm (max %.1f km2)", length(da), max(da, na.rm = TRUE))
      else sprintf("%d/%d flowpath(s) have invalid total_dasqkm (NA, < own area, or 0 with a catchment)", n_bad, length(da)))
    # Accumulation sanity (informational): a real network has many reaches whose
    # DA exceeds their own local area. ~0% indicates a topology-resolution bug
    # (e.g. passing nexus ids to accumulate_downstream -> DA == own area for all).
    pos <- !is.na(area) & area > 0 & !is.na(da)
    frac_accum <- if (sum(pos)) mean(da[pos] > area[pos] + 1e-6) else 1
    checks$drainage_area_accumulates <- .hf_info(
      sprintf("%.1f%% of area>0 flowpaths have DA > own area%s",
              100 * frac_accum,
              if (frac_accum < 0.10) " -- WARNING: looks unaccumulated (topology resolved?)" else ""))
  } else checks$drainage_area_populated <- .hf_ok(FALSE, "total_dasqkm column absent from flowpaths")

  # single CRS across all merged layers
  crs_of <- function(x) if (inherits(x, "sf")) sf::st_crs(x)$epsg else NA
  crs <- unique(stats::na.omit(c(crs_of(fp), crs_of(dv), crs_of(nx))))
  checks$crs_consistent <- .hf_ok(length(crs) <= 1L,
    if (length(crs) <= 1L) sprintf("single CRS (EPSG:%s)", if (length(crs)) crs else "NA")
    else sprintf("mixed CRS: %s", paste(crs, collapse = ", ")))

  .hf_report_checks(checks, stage, strict)
}
