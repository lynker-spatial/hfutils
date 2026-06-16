#' Physical-range bounds for divide and flowpath attributes
#'
#' Returns the attribute plausibility-bounds table shipped with the package
#' (`inst/extdata/divide_attr_bounds.csv`). Each row gives the physically
#' reasonable range for a divide or flowpath modeling parameter. The `caveat`
#' column flags bounds that are demonstrably too strict or look like
#' placeholders; those are excluded from checks by default. The `aliases`
#' column (`;`-separated) lists alternate column names a bound also matches, so
#' a single bound covers schema naming differences (e.g. `lengthkm` matches
#' `length_km`).
#'
#' @return A data.frame of bounds (one row per attribute, plus per-domain rows
#'   for `lat`/`lon`).
#' @export
hf_attr_bounds <- function() {
  path <- system.file("extdata", "divide_attr_bounds.csv", package = "hfutils")
  if (!nzchar(path)) stop("divide_attr_bounds.csv not found in installed package", call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

# Build a named list of bound checks for a single layer. Returns results shaped
# like .hf_ok() but marked soft = TRUE so they warn rather than abort even under
# strict reporting. `prefix` disambiguates names when divides and flowpaths
# checks are concatenated (both carry e.g. `area_sqkm`).
.hf_attr_bounds_checks <- function(layer, which = c("divides", "flowpaths"),
                                   domain = NULL, trust_caveated = FALSE,
                                   prefix = NULL, max_report = 5L) {
  which <- match.arg(which)
  if (is.null(layer)) return(list())
  df <- as.data.frame(layer)
  b  <- hf_attr_bounds()
  b  <- b[b$layer == which, , drop = FALSE]

  is_domain_row <- b$domain != "all"
  if (is.null(domain)) {
    b <- b[!is_domain_row, , drop = FALSE]
  } else {
    b <- b[!is_domain_row | b$domain == domain, , drop = FALSE]
  }
  if (!trust_caveated) b <- b[is.na(b$caveat), , drop = FALSE]

  has_aliases <- "aliases" %in% names(b)
  checks <- list()
  for (i in seq_len(nrow(b))) {
    a <- b$attribute[i]
    # Match our canonical name first, then any alias (handles schema naming
    # differences, e.g. length_km vs lengthkm, and flowline_* variants).
    cand <- a
    if (has_aliases && !is.na(b$aliases[i]) && nzchar(b$aliases[i]))
      cand <- c(cand, trimws(strsplit(b$aliases[i], ";", fixed = TRUE)[[1]]))
    col <- cand[cand %in% names(df)]
    if (!length(col)) next
    col <- col[1]
    v <- suppressWarnings(as.numeric(df[[col]]))
    v <- v[!is.na(v)]
    if (!length(v)) next

    lo <- b$lower[i]; hi <- b$upper[i]
    bad <- logical(length(v))
    if (!is.na(lo)) bad <- bad | (if (isTRUE(b$lower_inclusive[i])) v < lo else v <= lo)
    if (!is.na(hi)) bad <- bad | (if (isTRUE(b$upper_inclusive[i])) v > hi else v >= hi)

    nm  <- if (is.null(prefix)) a else paste0(prefix, ".", a)
    rng <- sprintf("[%s, %s]",
                   if (is.na(lo)) "-Inf" else lo,
                   if (is.na(hi)) "Inf" else hi)
    if (sum(bad) == 0L) {
      checks[[nm]] <- list(ok = TRUE, msg = sprintf("in range %s", rng),
                           kind = "check", soft = TRUE)
    } else {
      ex <- utils::head(sort(unique(v[bad])), max_report)
      checks[[nm]] <- list(
        ok = FALSE, kind = "check", soft = TRUE,
        msg = sprintf("%d/%d out of range %s (e.g. %s)",
                      sum(bad), length(v), rng, paste(signif(ex, 4), collapse = ", ")))
    }
  }
  checks
}

#' Check divide/flowpath attribute values against physical-range bounds
#'
#' Complements the topology/geometry checks in [hf_check_invariants()] with a
#' per-attribute physical-plausibility pass: every modeling parameter present
#' in the layer is checked against its expected range. Attributes not present
#' in the layer are skipped; bounds carrying a `caveat` are skipped unless
#' `trust_caveated = TRUE`. Failures are reported but never abort, even when
#' `strict = TRUE`.
#'
#' @param layer A data.frame or sf object (the Divides or Flowpaths layer).
#' @param which One of `"divides"` or `"flowpaths"`.
#' @param domain Domain code for `lat`/`lon` bounds: one of `"CONUS"`, `"AK"`,
#'   `"HI"`, `"PRVI"`, or `NULL` to skip lat/lon.
#' @param strict Logical. Passed through to the reporter; attribute-bound
#'   failures are soft and warn regardless.
#' @param trust_caveated Logical. Include bounds flagged in the `caveat` column
#'   (default `FALSE`).
#' @param max_report Integer. Max example offending values to show per attribute.
#'
#' @return Invisibly a list with `ok` and `checks` (same shape as
#'   [hf_check_invariants()]).
#' @export
hf_check_attr_bounds <- function(layer, which = c("divides", "flowpaths"),
                                 domain = NULL, strict = FALSE,
                                 trust_caveated = FALSE, max_report = 5L) {
  which  <- match.arg(which)
  checks <- .hf_attr_bounds_checks(layer, which, domain = domain,
                                   trust_caveated = trust_caveated,
                                   max_report = max_report)
  .hf_report_checks(checks, stage = paste0("attr_bounds:", which), strict = strict)
}
