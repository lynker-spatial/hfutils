#' Accumulate an attribute downstream over a directed acyclic network
#'
#' Propagates a per-node attribute (e.g. area) downstream along
#' `toid` links and returns the accumulated (upstream-summed) value for each
#' input row. The network must be a DAG (no cycles). This is an O(E) pass after
#' a single topological sort and is fast on large hydro networks.
#'
#' @param x A data frame (or tibble) containing at least the identifier column
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
#' accumulate(df, id = "flowpath_id", toid = "flowpath_toid", attr = "area")
#' # Expected: node 1 stays 1.0, node 2 stays 2.0,
#' # node 3 gets 1.0 + 2.0 + 0.5 = 3.5, node 4 gets 3.5 + 0.0 = 3.5
#'
#' @importFrom igraph graph_from_edgelist is_dag topo_sort as_ids
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

  # Build edge list (as names) just for topo sort
  el_chr <- cbind(v_names[idx[!is.na(jdx)]], v_names[jdx[!is.na(jdx)]])
  g <- igraph::graph_from_edgelist(el_chr, directed = TRUE)
  if (!igraph::is_dag(g)) stop("Network contains cycles; cannot accumulate.")

  # Topological order -> rank for each vertex name
  topo_names <- igraph::as_ids(igraph::topo_sort(g, mode = "out"))
  rank <- integer(n); rank[match(topo_names, v_names)] <- seq_along(topo_names)

  # Integer edge arrays (no NAs)
  src <- idx[!is.na(jdx)]
  dst <- jdx[!is.na(jdx)]

  # Process edges in nondecreasing rank of the source vertex
  ord <- order(rank[src])
  src <- src[ord]; dst <- dst[ord]

  # Single pass over edges; propagation ripples downstream automatically
  for (e in seq_along(src)) {
    total[dst[e]] <- total[dst[e]] + total[src[e]]
  }

  # Return totals aligned to input rows
  as.numeric(total[idx])
}


#' Compute and add the hydrosequence to a directed acyclic network.
#'
#' @param topology A data frame (or tibble) containing at least the identifier column
#'   given by `id` and the downstream pointer column given by `toid`.
#' @param id Character scalar. Column name in `topology` with unique node identifiers.
#'   Defaults to `"flowpath_id"`.
#' @param toid Character scalar. Column name in `topology` with the *downstream* node
#'   identifier for each row. Use `NA` or `0` for outlets/terminals.
#'   Defaults to `"flowpath_toid"`.
#' @param colname Character scalar. Column name to use in result.
#'   Defaults to `"hydroseq"`
#' 
#' @returns The data frame `topology` with an additional column, named `colname`, representing the hydrosequence.
#' @importFrom igraph dfs graph_from_data_frame
#' @export
add_hydroseq <- function(topology, id = "flowpath_id", toid = "flowpath_toid", colname = "hydroseq") {
  # Create a _transposed_ network, where traversing the network
  # is equivalent to traversing the hydrological network upstream.
  #
  # This assumes the outlets of this network all connect to an
  # ephemeral "0" node (forming a rooted tree network).
  edgelist <- topology[, c(toid, id)]
  names(edgelist) <- c("id", "toid")
  edgelist$id[is.na(edgelist$id)] <- "0"

  # TODO: Check if multiple components exist. If they do, then
  # we need to add "0" edges for each component not rooted on "0".
  
  # Perform DFS from each terminal upstream to get a
  # distinct topological sort for the hydrosequence.
  sorted <- data.frame(
    node = as.integer(
      names(
        igraph::dfs(
          igraph::graph_from_data_frame(edgelist),
          root = "0",
          mode = "out"
        )$order
      )
    )
  )

  sorted$hydroseq <- c(0, seq_len(nrow(sorted) - 1))

  # Merge the initial hydrosequence to the edgelist and handle ties in the hydrosequence.
  result <- merge(edgelist, sorted, by.x = "id", by.y = "node", all.x = TRUE)
  result <- result[!is.na(result$hydroseq), ]
  result <- result[order(result$hydroseq, result$id, result$toid), c("toid", "id")]
  result$hydroseq <- seq_len(nrow(result))
  names(result) <- c(id, toid, "hydroseq")

  # Arrange into input order
  topology[[colname]] <- result$hydroseq[match(topology[[id]], result[[id]])]
  topology
}