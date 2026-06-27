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
#' Native replacement for `hydroloom::add_streamorder` -- same topological
#' approach as [get_hydroseq()] (igraph topo-sort), with no external dependency
#' and no non-dendritic/divergence handling required. Leaves are order 1; at each
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
#' @export
get_streamorder <- function(x, id = "flowpath_id", toid = "flowpath_toid") {
  el <- as.data.frame(x)[, c(id, toid), drop = FALSE]
  names(el) <- c("id", "toid")
  el$id   <- as.character(el$id)
  el$toid <- as.character(el$toid)
  valid_id <- el$id
  # route terminals (NA / "" / "0" / dangling) to a single outlet sentinel
  el$toid[is.na(el$toid) | el$toid == "" | !(el$toid %in% valid_id)] <- "0"

  # upstream contributors per node: ids grouped by the node they drain to
  up <- split(el$id, el$toid)

  # topological order, upstream before downstream (DAG; network_is_dag holds)
  g    <- igraph::graph_from_data_frame(el[, c("id", "toid")])
  ord  <- igraph::as_ids(igraph::topo_sort(g, mode = "out"))
  ord  <- ord[ord %in% valid_id]                       # drop the "0" sentinel

  so <- stats::setNames(integer(length(valid_id)), valid_id)
  for (nd in ord) {
    contribs <- up[[nd]]
    if (is.null(contribs)) {
      so[nd] <- 1L
      next
    }       # headwater
    ords <- so[contribs]
    m    <- max(ords)
    so[nd] <- if (sum(ords == m) >= 2L) m + 1L else m
  }
  unname(so[as.character(x[[id]])])
}

#' Compute mainstem level paths over a directed acyclic network
#'
#' Native replacement for `hydroloom::add_levelpaths` -- same topological
#' approach as [get_hydroseq()] / [get_streamorder()] (igraph topo-sort), with no
#' external dependency. A level path is a continuous mainstem from a headwater to
#' an outlet: at each confluence the mainstem continues up the contributor with
#' the largest `weight` (e.g. arbolate sum or total drainage area), and the other
#' contributors begin new level paths. The id of a level path is the
#' hydrosequence of its most-downstream (outlet) reach, matching the NHDPlus /
#' hydroloom convention.
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
#'   hydroloom's `override_factor` (named-river continuity) is not modelled.
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
