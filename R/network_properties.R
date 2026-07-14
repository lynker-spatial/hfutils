#' Accumulate an attribute downstream over a directed acyclic network
#'
#' Propagates a per-node attribute (e.g. area) downstream along
#' `toid` links and returns the accumulated (upstream-summed) value for each
#' input row. The network must be a DAG (no cycles). This is an O(E) pass after
#' a single topological sort and is fast on large hydro networks.
#'
#' @param x A data frame, tibble, or sf object containing at least the identifier column
#'   given by `id`, the downstream pointer column given by `toid`, and the
#'   attribute column named in `attr`.
#' @param id Character scalar. Column name in `x` with unique node identifiers.
#'   Defaults to `"flowpath_id"`.
#' @param toid Character scalar. Column name in `x` with the *downstream* node
#'   identifier for each row. Use `NA` or `0` for outlets/terminals.
#'   Defaults to `"flowpath_toid"`.
#' @param attr Character scalar. Column name in `x` containing the attribute to
#'   accumulate (e.g., `"incremental_areasqkm"`). Values are coerced to numeric.
#'
#' @return A numeric vector the same length as `nrow(x)` giving the accumulated
#'   totals aligned to the rows of `x`.
#'
#' @details
#' The algorithm builds a vertex set from `id` and non-missing `toid` values,
#' performs a topological sort (using \pkg{igraph}), then processes edges in
#' nondecreasing topological order of their sources. For each edge `u -> v`,
#' the running total at `u` is added to `v`. This naturally handles confluences
#' because multiple upstream sources will contribute to the same downstream
#' target. The function treats `NA` and `0` in `toid` as outlets.
#'
#' The function stops with an error if the network contains cycles. Ensure your
#' graph is acyclic (tree/DAG) before calling.
#'
#' @section Performance:
#' Only a single neighbor-independent edge pass is made after topo sort, and
#' computations are done on integer indices. This avoids per-vertex neighbor
#' lookups and scales well to large hydrologic networks.
#'
#' @examples
#' # Toy: 1 -> 3, 2 -> 3, 3 -> 4 (two headwaters merging into 3, then into 4)
#' df <- data.frame(
#'   flowpath_id   = c(1, 2, 3, 4),
#'   flowpath_toid = c(3, 3, 4, NA),
#'   area           = c(1.0, 2.0, 0.5, 0.0)
#' )
#' accumulate_downstream(df, id = "flowpath_id", toid = "flowpath_toid", attr = "area")
#' # Expected: node 1 stays 1.0, node 2 stays 2.0,
#' # node 3 gets 1.0 + 2.0 + 0.5 = 3.5, node 4 gets 3.5 + 0.0 = 3.5
#'
#' @importFrom igraph graph_from_data_frame is_dag topo_sort as_ids
#' @family network properties
#' @export

accumulate_downstream <- function(x, id   = "flowpath_id", toid = "flowpath_toid", attr) {

  ids   <- as.character(x[[id]])
  toids <- x[[toid]]
  toids[is.na(toids) | toids == 0] <- NA
  toids <- as.character(toids)
  incr  <- as.numeric(x[[attr]])

  # Build vertex set and index maps
  v_names <- unique(c(ids, toids[!is.na(toids)]))
  n <- length(v_names)
  idx <- match(ids,   v_names)        # source vertex indices per row
  jdx <- match(toids, v_names)        # dest vertex indices per row (may be NA)

  # Initialize totals with incremental values
  total <- numeric(n)
  total[idx] <- incr

  # Build graph for topo sort (consistent with graph_from_data_frame convention)
  el_df <- data.frame(from = ids[!is.na(jdx)], to = toids[!is.na(jdx)], stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(el_df, directed = TRUE)
  if (!igraph::is_dag(g)) stop("Network contains cycles; cannot accumulate.")

  # Topological order -> rank for each vertex name
  topo_names <- igraph::as_ids(igraph::topo_sort(g, mode = "out"))
  rank <- integer(n)
  rank[match(topo_names, v_names)] <- seq_along(topo_names)

  # Integer edge arrays (no NAs)
  src <- idx[!is.na(jdx)]
  dst <- jdx[!is.na(jdx)]

  # Process edges in nondecreasing rank of the source vertex
  ord <- order(rank[src])
  src <- src[ord]
  dst <- dst[ord]

  # Single pass over edges; propagation ripples downstream automatically
  for (e in seq_along(src)) {
    total[dst[e]] <- total[dst[e]] + total[src[e]]
  }

  # Return totals aligned to input rows
  as.numeric(total[idx])
}

#' Compute and add the hydrosequence to a directed acyclic network
#'
#' @param x A data frame (or tibble) containing at least the identifier column
#'   given by `id` and the downstream pointer column given by `toid`.
#' @param id Character scalar. Column name in `x` with unique node identifiers.
#'   Defaults to `"flowpath_id"`.
#' @param toid Character scalar. Column name in `x` with the *downstream* node
#'   identifier for each row. Use `NA` or `0` for outlets/terminals.
#'   Defaults to `"flowpath_toid"`.
#'
#' @returns A numeric vector of hydrosequence values aligned to the rows of `x`
#'   (largest values upstream, decreasing downstream).
#'
#' @examples
#' # 1 -> 2 -> 3 (outlet). Headwater "1" gets the largest hydroseq.
#' df <- data.frame(
#'   flowpath_id   = c("1", "2", "3"),
#'   flowpath_toid = c("2", "3", "0")
#' )
#' get_hydroseq(df)
#'
#' @importFrom igraph dfs graph_from_data_frame
#' @family network properties
#' @export

get_hydroseq <- function(x, id = "flowpath_id", toid = "flowpath_toid") {
  # Create a _transposed_ network, where traversing the network
  # is equivalent to traversing the hydrological network upstream.
  #
  # This assumes the outlets of this network all connect to an
  # ephemeral "0" node (forming a rooted tree network).

  # IDs are handled as character throughout so non-numeric identifiers
  # (e.g. "fp-123", scientific-notation strings) survive the round-trip.
  edgelist <- as.data.frame(x)[, c(toid, id)]
  names(edgelist) <- c("id", "toid")
  edgelist$id   <- as.character(edgelist$id)
  edgelist$toid <- as.character(edgelist$toid)

  edgelist$id[is.na(edgelist$id) | edgelist$id == ""] <- "0"

  if (sum(edgelist$toid == "0", na.rm = TRUE) == 0) {
    ind  <- which(!edgelist$id %in% edgelist$toid)
    root <- edgelist$toid[ind]
  } else {
    root <- "0"
  }

  # Perform DFS from each terminal upstream to get a
  # distinct topological sort for the hydrosequence.
  sorted <- data.frame(
    node = names(
      igraph::dfs(
        igraph::graph_from_data_frame(edgelist),
        root = as.character(root),
        mode = "out"
      )$order
    ),
    stringsAsFactors = FALSE
  )

  sorted$hydroseq <- c(0, seq_len(nrow(sorted) - 1))

  # Merge the initial hydrosequence to the edgelist and handle ties in the hydrosequence.
  result <- merge(edgelist, sorted, by.x = "id", by.y = "node", all.x = TRUE)
  result <- result[!is.na(result$hydroseq), ]
  result <- result[order(result$hydroseq, result$id, result$toid), c("toid", "id")]
  result$hydroseq <- seq_len(nrow(result))
  names(result) <- c(id, toid, "hydroseq")

  # Arrange into input order
  result$hydroseq[match(as.character(x[[id]]), as.character(result[[id]]))]
}

#' Compute and add Strahler stream order to a directed acyclic network
#'
#' Same topological approach as [get_hydroseq()] (igraph topo-sort), with no
#' non-dendritic/divergence handling required. Leaves are order 1; at each
#' node the order is the max of its upstream contributors, incremented by 1 when
#' that max is shared by two or more of them (Strahler).
#'
#' @param x A data frame with the identifier column `id` and downstream pointer
#'   `toid`. Terminal/outlet rows use `NA`, `""`, `"0"`, or a `toid` that is not
#'   a known `id`.
#' @param id,toid Column names. Default `"flowpath_id"` / `"flowpath_toid"`.
#' @returns Integer vector of stream orders aligned to the rows of `x`.
#' @examples
#' # two headwaters (1,2) join at 3 -> outlet: 3 is order 2
#' get_streamorder(data.frame(flowpath_id = c("1", "2", "3"),
#'   flowpath_toid = c("3", "3", "0")))
#' @importFrom igraph graph_from_data_frame topo_sort as_ids
#' @family network properties
#' @export
get_streamorder <- function(x, id = "flowpath_id", toid = "flowpath_toid") {
  ids <- as.character(x[[id]])
  tos <- as.character(x[[toid]])
  n   <- length(ids)

  # integer downstream index of each row; NA = terminal / dangling outlet
  di <- match(tos, ids)
  di[is.na(tos) | tos == ""] <- NA_integer_

  # upstream contributors per node, INTEGER-indexed (length n)
  pos    <- seq_len(n)
  has_dn <- !is.na(di)
  up     <- split(pos[has_dn], factor(di[has_dn], levels = seq_len(n)))

  # topological order (upstream-first), mapped to row positions
  g <- igraph::graph_from_data_frame(
    data.frame(from = ids[has_dn], to = tos[has_dn]),
    directed = TRUE, vertices = data.frame(name = ids))
  if (!igraph::is_dag(g)) stop("Network contains cycles; cannot compute stream order.")
  ord <- match(igraph::as_ids(igraph::topo_sort(g, mode = "out")), ids)

  # Strahler order via a single integer-indexed pass. The previous version kept
  # `so` as a CHARACTER-named vector and did so[nd] / so[contribs] / up[[nd]]
  # lookups by name inside the loop -- each an O(n) scan, making the whole thing
  # O(n^2). It stalled mega-basins (~44 min on the Mississippi's 461k reaches;
  # infeasible on Amazon). Positional indexing makes every access O(1).
  so <- integer(n)                       # UNNAMED, indexed by row position
  for (r in ord) {
    contribs <- up[[r]]
    if (!length(contribs)) { so[r] <- 1L; next }   # headwater
    ords <- so[contribs]
    m    <- max(ords)
    so[r] <- if (sum(ords == m) >= 2L) m + 1L else m
  }
  so
}

#' Compute mainstem level paths over a directed acyclic network
#'
#' Same topological approach as [get_hydroseq()] / [get_streamorder()] (igraph
#' topo-sort). A level path is a continuous mainstem from a headwater to
#' an outlet: at each confluence the mainstem continues up the contributor with
#' the largest `weight` (e.g. arbolate sum or total drainage area), and the other
#' contributors begin new level paths. The id of a level path is the
#' hydrosequence of its most-downstream (outlet) reach, matching the NHDPlus
#' convention.
#'
#' @param x A data frame with the identifier column `id`, downstream pointer
#'   `toid`, and a numeric `weight` column. Terminal/outlet rows use `NA`, `""`,
#'   `"0"`, or a `toid` that is not a known `id`.
#' @param id,toid Column names. Default `"flowpath_id"` / `"flowpath_toid"`.
#' @param weight Character scalar. Column name giving the mainstem weight; at each
#'   confluence the mainstem follows the largest-weight upstream contributor.
#'   Typically the arbolate sum (`accumulate_downstream` of `lengthkm`) or total
#'   drainage area.
#' @param hydroseq Optional column name of a precomputed hydrosequence to use for
#'   level path ids. If `NULL` (default), it is computed with [get_hydroseq()].
#' @returns Numeric vector of level path ids aligned to the rows of `x` (the
#'   hydrosequence of each level path's outlet reach).
#' @details The network must be acyclic (errors otherwise, like
#'   [accumulate_downstream()]). Weight ties are broken by first occurrence;
#'   named-river continuity (overriding the weight to hold a named mainstem
#'   together through a confluence) is not modelled.
#' @examples
#' # 4 -> 3 -> 1 (mainstem, longer), 2 -> 1 (tributary); 1 -> outlet
#' df <- data.frame(
#'   flowpath_id   = c(1, 2, 3, 4),
#'   flowpath_toid = c(0, 1, 1, 3),
#'   arb_sum       = c(14, 2, 7, 4)
#' )
#' get_levelpath(df, weight = "arb_sum")
#' # reaches 1,3,4 share one level path; reach 2 is its own
#'
#' @importFrom igraph graph_from_data_frame is_dag topo_sort as_ids
#' @family network properties
#' @export
get_levelpath <- function(x, id = "flowpath_id", toid = "flowpath_toid",
                          weight, hydroseq = NULL) {
  ids <- as.character(x[[id]])
  tos <- as.character(x[[toid]])
  w   <- as.numeric(x[[weight]])
  n   <- length(ids)

  # integer index of each row's downstream row; NA = terminal/dangling outlet
  di <- match(tos, ids)
  di[is.na(tos) | tos == ""] <- NA_integer_

  # hydrosequence supplies the level-path (outlet) ids
  hs <- if (is.null(hydroseq)) {
    get_hydroseq(data.frame(flowpath_id = ids,
                            flowpath_toid = ifelse(is.na(di), "0", tos)))
  } else {
    as.numeric(x[[hydroseq]])
  }

  # main upstream branch of each node = its largest-weight contributor.
  # order rows by (downstream row, -weight); the first per group is the main one.
  has_dn  <- !is.na(di)
  is_main <- logical(n)
  o <- order(di, -w)            # terminals (di NA) sort last and are dropped
  o <- o[has_dn[o]]
  is_main[o[!duplicated(di[o])]] <- TRUE

  # topological order (upstream-first); reverse for downstream-first assignment
  g <- igraph::graph_from_data_frame(
    data.frame(from = ids[has_dn], to = tos[has_dn]),
    directed = TRUE, vertices = data.frame(name = ids))
  if (!igraph::is_dag(g)) stop("Network contains cycles; cannot compute level paths.")
  ord <- rev(match(igraph::as_ids(igraph::topo_sort(g, mode = "out")), ids))

  # integer-indexed pass (no per-iteration name lookups): a reach continues its
  # downstream level path iff it is that node's main branch, else it begins a new
  # level path whose outlet (id) is its own hydrosequence.
  lp <- rep(NA_real_, n)
  for (r in ord) {
    d <- di[r]
    lp[r] <- if (!is.na(d) && is_main[r]) lp[d] else hs[r]
  }
  lp
}

#' Compute downstream path length to the network outlet over a DAG
#'
#' Uses the same topological approach as [get_levelpath()] / [get_hydroseq()]
#' (igraph topo-sort). Path length is the distance along the network from the
#' *downstream end* (outlet) of each reach to the terminal outlet of the
#' network: the sum of the lengths of every reach strictly downstream. It does
#' **not** include the reach's own length, so terminal (outlet) reaches are `0`
#' and the value increases upstream, matching the NHDPlus `PathLength` attribute.
#'
#' @param x A data frame with the identifier column `id`, downstream pointer
#'   `toid`, and a numeric `length` column. Terminal/outlet rows use `NA`, `""`,
#'   `"0"`, or a `toid` that is not a known `id`.
#' @param id,toid Column names. Default `"flowpath_id"` / `"flowpath_toid"`.
#' @param length Character scalar. Column name giving each reach's own length
#'   (e.g. `"lengthkm"`); the returned path length is in the same units.
#' @returns Numeric vector of path lengths aligned to the rows of `x` (`0` at
#'   terminal reaches, increasing upstream).
#' @details The network must be acyclic (errors otherwise, like
#'   [accumulate_downstream()]). The downstream path from any reach is unique in
#'   a dendritic network; the single downstream-first pass finalizes each
#'   downstream reach's path length before it is read, so the whole traversal is
#'   O(E) after one topological sort.
#' @examples
#' # 1 -> 2 -> 3 (outlet); each reach 5 km long
#' df <- data.frame(
#'   flowpath_id   = c("1", "2", "3"),
#'   flowpath_toid = c("2", "3", "0"),
#'   lengthkm      = c(5, 5, 5)
#' )
#' get_pathlength(df, length = "lengthkm")
#' # reach 3 (outlet) = 0; reach 2 = 5 (length of 3); reach 1 = 10 (len 2 + len 3)
#'
#' @importFrom igraph graph_from_data_frame is_dag topo_sort as_ids
#' @family network properties
#' @export
get_pathlength <- function(x, id = "flowpath_id", toid = "flowpath_toid",
                           length = "lengthkm") {
  ids <- as.character(x[[id]])
  tos <- as.character(x[[toid]])
  len <- as.numeric(x[[length]])
  n   <- nrow(x)                         # avoid shadowing base::length via `length`

  # integer index of each row's downstream row; NA = terminal/dangling outlet
  di <- match(tos, ids)
  di[is.na(tos) | tos == "" | tos == "0"] <- NA_integer_

  # topological order (upstream-first); reverse for a downstream-first pass so
  # each reach's downstream path length is finalized before we read it
  has_dn <- !is.na(di)
  g <- igraph::graph_from_data_frame(
    data.frame(from = ids[has_dn], to = tos[has_dn]),
    directed = TRUE, vertices = data.frame(name = ids))
  if (!igraph::is_dag(g)) stop("Network contains cycles; cannot compute path length.")
  ord <- rev(match(igraph::as_ids(igraph::topo_sort(g, mode = "out")), ids))

  # integer-indexed pass: distance from a reach's outlet to the network terminus
  # is the downstream reach's own length plus that reach's path length.
  pl <- numeric(n)                       # 0 at terminals
  for (r in ord) {
    d <- di[r]
    if (!is.na(d)) pl[r] <- pl[d] + len[d]
  }
  pl
}

#' Compute stream level over a directed acyclic network
#'
#' Uses the same topological approach as [get_levelpath()] (igraph topo-sort),
#' operating on the *level-path* graph rather than the reach graph. Stream level
#' counts the number of level-path steps from a reach to the network terminus:
#' the mainstem level path that drains out of the network is level `1`, every
#' level path that empties into a level-`1` path is level `2`, and so on
#' (the NHDPlus `StreamLeve` attribute). All reaches on a level path share its
#' level.
#'
#' @param x A data frame with the identifier column `id`, downstream pointer
#'   `toid`, and a precomputed `levelpath` column (e.g. from [get_levelpath()]).
#'   Terminal/outlet rows use `NA`, `""`, `"0"`, or a `toid` that is not a known
#'   `id`.
#' @param id,toid Column names. Default `"flowpath_id"` / `"flowpath_toid"`.
#' @param levelpath Character scalar. Column name of the level-path id each reach
#'   belongs to. Default `"levelpath"`.
#' @returns Integer vector of stream levels aligned to the rows of `x` (`1` on the
#'   terminal mainstem, increasing up each tributary level path).
#' @details The level-path network must be acyclic (errors otherwise). A level
#'   path is a contiguous mainstem, so it empties into exactly one downstream
#'   level path; the level is a single downstream-first pass over that coarser
#'   graph, mirroring [get_pathlength()] / [get_levelpath()].
#' @examples
#' # mainstem 4 -> 3 -> 1 (level path A), tributary 2 -> 1 (level path B)
#' df <- data.frame(
#'   flowpath_id   = c("1", "2", "3", "4"),
#'   flowpath_toid = c("0", "1", "1", "3"),
#'   levelpath     = c("A", "B", "A", "A")
#' )
#' get_streamlevel(df)
#' # reaches on A (1,3,4) = 1; tributary 2 (level path B) = 2
#'
#' @importFrom igraph graph_from_data_frame is_dag topo_sort as_ids
#' @family network properties
#' @export
get_streamlevel <- function(x, id = "flowpath_id", toid = "flowpath_toid",
                            levelpath = "levelpath") {
  ids <- as.character(x[[id]])
  tos <- as.character(x[[toid]])
  lp  <- as.character(x[[levelpath]])

  # downstream row index of each reach; NA = terminal/dangling outlet
  di <- match(tos, ids)
  di[is.na(tos) | tos == "" | tos == "0"] <- NA_integer_

  # level-path edges: a reach whose downstream reach is on a *different* level
  # path is that level path's outlet. Terminal reaches yield no edge, so their
  # level path drains out of the network.
  has_dn        <- !is.na(di)
  dn_lp         <- rep(NA_character_, length(ids))
  dn_lp[has_dn] <- lp[di[has_dn]]
  cross         <- has_dn & lp != dn_lp
  edges         <- unique(data.frame(from = lp[cross], to = dn_lp[cross],
                                     stringsAsFactors = FALSE))

  ulp <- unique(lp)
  g <- igraph::graph_from_data_frame(edges, directed = TRUE,
    vertices = data.frame(name = ulp))
  if (!igraph::is_dag(g)) stop("Level-path network contains cycles; cannot compute stream level.")
  ord <- rev(match(igraph::as_ids(igraph::topo_sort(g, mode = "out")), ulp))

  # downstream level path of each level path (NA = drains out of the network)
  dn_of <- rep(NA_integer_, length(ulp))
  dn_of[match(edges$from, ulp)] <- match(edges$to, ulp)

  # downstream-first pass: terminal level paths are 1, each step upstream +1
  lvl <- integer(length(ulp))
  for (r in ord) {
    d <- dn_of[r]
    lvl[r] <- if (is.na(d)) 1L else lvl[d] + 1L
  }

  lvl[match(lp, ulp)]
}

#' Compute Pfafstetter basin codes over a directed acyclic network
#'
#' Assigns hierarchical Pfafstetter codes (the NHDPlus basin-coding scheme).
#' At each level the basin's mainstem is found, its four largest tributaries (by
#' total drainage area) are given even digits `2,4,6,8` ordered downstream to
#' upstream, and the five mainstem inter-basins between those junctions take odd
#' digits `1,3,5,7,9`. Each of the nine sub-basins is then subdivided the same
#' way, appending a digit per level, down to `max_level`.
#'
#' @param x A data frame with `id`, downstream pointer `toid`, `total_da`
#'   (total upstream drainage area), `topo_sort` (a hydrosequence; smaller is
#'   more downstream, e.g. from [get_hydroseq()]), and `levelpath` (e.g. from
#'   [get_levelpath()]). Terminal/outlet rows use `NA`, `""`, `"0"`, or an
#'   unknown `toid`.
#' @param id,toid Column names. Default `"flowpath_id"` / `"flowpath_toid"`.
#' @param total_da,topo_sort,levelpath Column names for total drainage area,
#'   hydrosequence, and level-path id. Defaults `"total_da_sqkm"`,
#'   `"topo_sort"`, `"levelpath"`.
#' @param max_level Integer. Number of Pfafstetter levels (digits) to assign.
#'   Default `2`.
#' @returns Numeric vector of `max_level`-digit Pfafstetter codes aligned to the
#'   rows of `x`. Reaches whose sub-basin is deeper than `max_level` levels are
#'   `NA`.
#' @details Requires the drainage-area, hydrosequence, and level-path columns to
#'   be precomputed (see [accumulate_downstream()], [get_hydroseq()],
#'   [get_levelpath()]); this keeps the coding independent of how those were
#'   derived. Ties in the four-largest-tributary cut are resolved by keeping all
#'   tied tributaries.
#' @examples
#' \dontrun{
#' x$total_da_sqkm <- accumulate_downstream(x, attr = "areasqkm")
#' x$topo_sort     <- get_hydroseq(x)
#' x$levelpath     <- get_levelpath(x, weight = "total_da_sqkm")
#' x$pfaf          <- get_pfafstetter(x, max_level = 2)
#' }
#' @importFrom stats setNames ave
#' @family network properties
#' @export
get_pfafstetter <- function(x, id = "flowpath_id", toid = "flowpath_toid",
                            total_da = "total_da_sqkm", topo_sort = "topo_sort",
                            levelpath = "levelpath", max_level = 2) {
  ids <- as.character(x[[id]])
  tos <- as.character(x[[toid]])
  da  <- as.numeric(x[[total_da]])
  ts  <- as.numeric(x[[topo_sort]])
  lp  <- as.character(x[[levelpath]])
  N   <- length(ids)
  tos[is.na(tos) | tos == ""] <- "0"

  ipos <- stats::setNames(seq_len(N), ids)                 # id -> row index

  # level-path outlet id: within each level path, the id of its most-downstream
  # (minimum topo_sort) reach. Reaches sharing an outlet share a level path.
  ord0      <- order(lp, ts)
  firstlp   <- !duplicated(lp[ord0])
  lp_out_of <- stats::setNames(ids[ord0][firstlp], lp[ord0][firstlp])
  lp_outlet <- unname(lp_out_of[lp])                       # per reach

  acc <- vector("list", 0L)                                # (members, code) rows

  # recursive nine-way subdivision of one basin whose mainstem is `ms_ids`
  pfaf9 <- function(ms_ids, pre_pfaf, assigned_even) {
    if (pre_pfaf >= 10^(max_level - 1)) return(invisible())
    ms_lp <- lp[ipos[[ms_ids[1]]]]

    trib <- which(tos %in% ms_ids & lp != ms_lp)           # tributary outlets
    if (length(assigned_even)) trib <- trib[!ids[trib] %in% assigned_even]
    if (length(ms_ids) == 1L && length(trib) == 0L) return(invisible())

    if (length(trib)) {
      k   <- min(4L, length(trib))
      thr <- sort(da[trib], decreasing = TRUE)[k]
      t4  <- trib[da[trib] >= thr]
      t4  <- t4[order(ts[ipos[tos[t4]]])]                  # downstream junction first
    } else t4 <- integer(0)
    jt <- ts[ipos[tos[t4]]]                                # junction topo_sorts
    nt <- length(jt)

    ms_ts_all <- ts[ipos[ms_ids]]
    members   <- vector("list", 9L)
    odd_d     <- c(1L, 3L, 5L, 7L, 9L)
    for (s in 1:5) {                                       # odd digits: interbasins
      if (s > nt + 1L) { members[[odd_d[s]]] <- character(0); next }
      if (s == 1L)                        m <- ms_ts_all <= jt[1]
      else if (s == 5L || s == nt + 1L)   m <- ms_ts_all >  jt[s - 1]
      else                                m <- ms_ts_all >  jt[s - 1] & ms_ts_all <= jt[s]
      members[[odd_d[s]]] <- ms_ids[m]
    }
    even_d <- c(2L, 4L, 6L, 8L)
    for (k in seq_len(min(4L, nt))) {                      # even digits: tributaries
      members[[even_d[k]]] <- ids[lp_outlet == lp_outlet[t4[k]]]
    }

    codes <- (1:9) + pre_pfaf * 10
    if (all(lengths(members) == 0L)) members[[1]] <- ms_ids   # degenerate mainstem

    all_new <- character(0)
    for (p in 1:9) {
      mm <- members[[p]]
      if (!length(mm)) next
      acc[[length(acc) + 1L]] <<- list(m = mm, c = codes[p])
      all_new <- c(all_new, mm)
    }
    if (all(all_new %in% ms_ids)) return(invisible())       # base case: nothing to recurse

    even_members <- unlist(members[even_d], use.names = FALSE)
    for (p in 1:9) {
      mm <- members[[p]]
      if (length(mm)) pfaf9(mm, codes[p], even_members)
    }
    invisible()
  }

  root_lp <- lp[which.min(ts)]                             # outlet's level path
  pfaf9(ids[lp == root_lp], pre_pfaf = 0, assigned_even = character(0))
  if (!length(acc)) return(rep(NA_real_, N))

  member <- unlist(lapply(acc, function(a) a$m), use.names = FALSE)
  code   <- unlist(lapply(acc, function(a) rep(a$c, length(a$m))), use.names = FALSE)
  level  <- nchar(as.character(code))                      # digit count == level
  ok     <- level <= max_level                            # ignore any over-deep codes
  member <- member[ok]; code <- code[ok]; level <- level[ok]

  # per (member, level) keep only the largest code, then pivot to one column
  # per level and back/forward-fill so every coded reach carries a full code.
  key    <- paste(member, level, sep = "\r")
  mx     <- stats::ave(code, key, FUN = max)
  keep   <- code == mx
  member <- member[keep]; level <- level[keep]; code <- code[keep]
  dedup  <- !duplicated(paste(member, level, sep = "\r"))
  member <- member[dedup]; level <- level[dedup]; code <- code[dedup]

  mu <- unique(member)
  M  <- matrix(NA_real_, nrow = length(mu), ncol = max_level)
  M[cbind(match(member, mu), level)] <- code
  if (max_level >= 2L) {
    for (i in 2:max_level) {                               # forward: append 1
      na_i <- is.na(M[, i]) & !is.na(M[, i - 1])
      M[na_i, i] <- 1 + M[na_i, i - 1] * 10
    }
    for (i in (max_level - 1):1) {                         # backward: drop last digit
      na_i <- is.na(M[, i])
      M[na_i, i] <- floor(M[na_i, i + 1] / 10)
    }
  }
  code_of <- stats::setNames(M[, max_level], mu)
  unname(code_of[ids])
}

# ---- DAG / hydrosequence topology helpers -----------------------------------
# Canonical, shared topology primitives (id_col/toid_col API). Terminal toids
# ("0", NA, or values absent from id_col) are outlets, excluded from the edge
# list. Used by hf_check_invariants() and by the hydrofabric build pipeline.

#' Is a flowpath network a directed acyclic graph?
#'
#' @param flowpaths A data.frame/sf with id and downstream-id columns.
#' @param id_col,toid_col Column names for the node id and its downstream id.
#' @return `TRUE` if acyclic (or edge-free), `FALSE` if any cycle exists.
#' @importFrom igraph graph_from_data_frame is_dag
#' @export
hf_network_is_dag <- function(flowpaths, id_col = "flowpath_id",
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

#' Assert a flowpath network is a DAG, reporting any cycle nodes
#'
#' Unlike a strongly-connected-component test, `igraph::is_dag` also catches
#' self-loop cycles (`a -> a`). `cycle_ids` reports every node in a cycle
#' (multi-node SCCs plus self-loops) for diagnostics/repair.
#'
#' @param flowpaths A data.frame/sf with id and downstream-id columns.
#' @param id_col,toid_col Column names for the node id and its downstream id.
#' @return `list(is_dag, message, cycle_ids)`.
#' @importFrom igraph graph_from_data_frame is_dag components
#' @export
hf_assert_network_dag <- function(flowpaths,
                                  id_col   = "flowpath_id",
                                  toid_col = "flowpath_toid") {
  ids   <- as.character(flowpaths[[id_col]])
  toids <- as.character(flowpaths[[toid_col]])
  keep  <- !is.na(toids) & toids != "0" & toids %in% ids
  if (!any(keep)) return(list(is_dag = TRUE, message = "DAG OK", cycle_ids = character(0)))
  edge_df <- data.frame(from = ids[keep], to = toids[keep], stringsAsFactors = FALSE)
  g       <- igraph::graph_from_data_frame(edge_df, directed = TRUE)
  if (igraph::is_dag(g))
    return(list(is_dag = TRUE, message = "DAG OK", cycle_ids = character(0)))
  sccs      <- igraph::components(g, mode = "strong")
  cyc       <- names(sccs$membership)[sccs$csize[sccs$membership] > 1L]
  selfloops <- edge_df$from[edge_df$from == edge_df$to]
  cyc       <- unique(c(cyc, selfloops))
  list(is_dag    = FALSE,
    message   = sprintf("%d node(s) in cycle(s)", length(cyc)),
    cycle_ids = cyc)
}

#' Recompute hydrosequence for a flowpath network by topological sort
#'
#' NHD convention: lower `hydroseq` = more downstream (closer to outlet). Call
#' after any topology-modifying operation (cycle break, toid reassignment) to
#' keep `hydroseq` consistent with the current toid graph.
#'
#' @param flowpaths A data.frame/sf with id and downstream-id columns.
#' @param id_col,toid_col Column names for the node id and its downstream id.
#' @return `flowpaths` with an updated integer `hydroseq` column.
#' @importFrom igraph graph_from_data_frame topo_sort
#' @importFrom stats setNames
#' @export
hf_recompute_hydroseq <- function(flowpaths,
                                  id_col   = "flowpath_id",
                                  toid_col = "flowpath_toid") {
  ids   <- as.character(flowpaths[[id_col]])
  toids <- as.character(flowpaths[[toid_col]])

  keep <- !is.na(toids) & toids != "0" & toids %in% ids
  if (!any(keep)) {
    flowpaths$hydroseq <- seq_len(nrow(flowpaths))
    return(flowpaths)
  }

  edge_df <- data.frame(from = ids[keep], to = toids[keep], stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(edge_df, directed = TRUE,
    vertices = data.frame(name = ids))

  topo_names <- tryCatch(
    names(rev(igraph::topo_sort(g, mode = "out"))),
    error = function(e) {
      warning("hf_recompute_hydroseq: graph has cycles -- hydroseq not recomputed")
      NULL
    }
  )
  if (is.null(topo_names)) return(flowpaths)

  remaining   <- setdiff(ids, topo_names)
  ordered_ids <- c(topo_names, remaining)
  hs_map      <- stats::setNames(seq_along(ordered_ids), ordered_ids)
  flowpaths$hydroseq <- as.integer(hs_map[ids])
  flowpaths
}

#' Break cycles in a flowpath network by severing one back-edge per SCC
#'
#' For each strongly-connected component with >1 member, the node receiving the
#' most in-edges from OUTSIDE the SCC is its outlet; every other member whose
#' toid points to that outlet (the back-edge completing the cycle) has its toid
#' set to `"0"`, preserving as much downstream connectivity as possible while
#' making the graph a DAG.
#'
#' @param flowpaths A data.frame/sf with id and downstream-id columns.
#' @param id_col,toid_col Column names for the node id and its downstream id.
#' @return `flowpaths` with `toid_col` rewritten to remove cycles.
#' @importFrom igraph graph_from_data_frame components neighbors
#' @export
hf_break_cycles <- function(flowpaths,
                            id_col   = "flowpath_id",
                            toid_col = "flowpath_toid") {
  ids   <- as.character(flowpaths[[id_col]])
  toids <- as.character(flowpaths[[toid_col]])
  keep  <- !is.na(toids) & toids != "0" & toids %in% ids
  if (!any(keep)) return(flowpaths)

  edge_df <- data.frame(from = ids[keep], to = toids[keep], stringsAsFactors = FALSE)
  g    <- igraph::graph_from_data_frame(edge_df, directed = TRUE,
    vertices = data.frame(name = ids))
  sccs <- igraph::components(g, mode = "strong")

  cycle_comps <- which(sccs$csize > 1L)
  if (length(cycle_comps) == 0L) return(flowpaths)

  toids_new <- toids
  for (comp_id in cycle_comps) {
    members <- names(sccs$membership)[sccs$membership == comp_id]
    ext_in <- vapply(members, function(m) {
      preds <- names(igraph::neighbors(g, m, mode = "in"))
      sum(!preds %in% members)
    }, integer(1L))
    outlet <- members[which.max(ext_in)]
    back_senders <- members[members != outlet & toids[match(members, ids)] == outlet]
    if (length(back_senders) == 0L) {
      interior <- members[which.min(ext_in)]
      back_senders <- interior
    }
    for (bs in back_senders) {
      idx <- match(bs, ids)
      if (!is.na(idx)) toids_new[idx] <- "0"
    }
    message(sprintf("hf_break_cycles: severed %d back-edge(s) in SCC of %d node(s) [outlet: %s]",
      length(back_senders), length(members), outlet))
  }

  flowpaths[[toid_col]] <- toids_new
  flowpaths
}
