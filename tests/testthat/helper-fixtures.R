## Shared fixtures for geometry / IO tests.

# A small projected polygon layer (NAD83 / Conus Albers) with a unique ID.
nc_divides <- function(n = 10L) {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc <- sf::st_transform(nc[seq_len(n), ], 5070)
  nc$ID <- seq_len(nrow(nc))
  nc
}

# A two-part (disjoint MULTIPOLYGON) catchment sharing one ID, plus a normal one.
nc_multipart <- function() {
  nc <- nc_divides(50L)
  multi  <- sf::st_union(sf::st_geometry(nc[1, ]), sf::st_geometry(nc[50, ]))
  feat   <- sf::st_sf(ID = 1L, geometry = multi)
  normal <- sf::st_sf(ID = 2L, geometry = sf::st_geometry(nc[2, ]))
  rbind(feat, normal)
}

# ---- synthetic flow network for invariant checks (EPSG:5070, metres) --------

.ls <- function(x1, y1, x2, y2) {
  sf::st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE))
}
.sq <- function(cx, cy, r = 600) {
  sf::st_polygon(list(matrix(
    c(cx - r, cy - r, cx + r, cy - r, cx + r, cy + r,
      cx - r, cy + r, cx - r, cy - r),
    ncol = 2, byrow = TRUE)))
}

# A clean 3-segment chain (1 -> 2 -> 3 -> outlet) of LINESTRINGs, each fully
# enclosed by a square divide. `reverse_first = TRUE` flips segment 1 so the
# reconciled flow-direction check sees a reversed flowline.
recon_fixture <- function(reverse_first = FALSE) {
  segs <- list(.ls(0, 0, 1000, 0), .ls(1000, 0, 2000, 0), .ls(2000, 0, 3000, 0))
  if (reverse_first) segs[[1]] <- .ls(1000, 0, 0, 0)
  reconciled <- sf::st_sf(
    ID = c(1, 2, 3), toID = c(2, 3, 0),
    geometry = sf::st_sfc(segs, crs = 5070))

  divides <- sf::st_sf(
    divide_id   = c("1", "2", "3", "lnd-1"),
    flowpath_id = c("1", "2", "3", NA),
    geometry = sf::st_sfc(.sq(500, 0), .sq(1500, 0), .sq(2500, 0), .sq(5000, 0),
                          crs = 5070))
  list(reconciled = reconciled, divides = divides)
}

# Aggregated/ngen-style layers: flowpaths carry a `divide_id` so the optional
# coverage check has matched pairs; each line lies inside its divide.
agg_fixture <- function() {
  fp <- sf::st_sf(
    flowpath_id = c("1", "2", "3"),
    divide_id   = c("1", "2", "3"),
    geometry = sf::st_sfc(.ls(0, 0, 1000, 0), .ls(1000, 0, 2000, 0),
                          .ls(2000, 0, 3000, 0), crs = 5070))
  divides <- sf::st_sf(
    divide_id = c("1", "2", "3"),
    geometry = sf::st_sfc(.sq(500, 0), .sq(1500, 0), .sq(2500, 0), crs = 5070))
  list(flowpaths = fp, divides = divides)
}

# ngen-prefixed flowpaths + nexus so the fp -> nexus -> fp DAG check runs.
ngen_fixture <- function() {
  flowpaths <- sf::st_sf(
    flowpath_id   = c("fp-1", "fp-2", "fp-3"),
    flowpath_toid = c("nex-1", "nex-2", "nex-3"),
    divide_id     = c("cat-1", "cat-2", "cat-3"),
    has_divide    = c(TRUE, TRUE, TRUE),
    geometry = sf::st_sfc(.ls(0, 0, 1000, 0), .ls(1000, 0, 2000, 0),
                          .ls(2000, 0, 3000, 0), crs = 5070))
  divides <- sf::st_sf(
    divide_id = c("cat-1", "cat-2", "cat-3"),
    geometry = sf::st_sfc(.sq(500, 0), .sq(1500, 0), .sq(2500, 0), crs = 5070))
  nexus <- data.frame(
    nexus_id   = c("nex-1", "nex-2", "nex-3"),
    nexus_toid = c("fp-2", "fp-3", "0"),
    stringsAsFactors = FALSE)
  list(flowpaths = flowpaths, divides = divides, nexus = nexus)
}
