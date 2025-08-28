.onLoad <- function(...) {

  identifier_classes <- c(
    "default",
    "id",
    "comid",
    "hl_uri",
    "poi_id",
    "nldi_feature",
    "xy"
  )

  op <- getOption("ogr.query.debug")
  if (is.null(op) || is.na(op) || length(op) < 1) {
    op <- FALSE
    options(ogr.query.debug = op)
  }

  invisible()
}
