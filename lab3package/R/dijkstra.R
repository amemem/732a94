#' @title Dijkstra's algorithm
#' 
#' @description Computes the shortest paths from an initial node to all other nodes in a graph.
#' 
#' @param graph A data frame with 3 variables labeled \emph{v1}, \emph{v2} and \emph{w}.
#' @param init_node The starting node where all paths are to be computed from.
#' 
#' @return A vector with the distances from the initial node to all nodes.
#' 
#' @export
#' 
#' @source \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#' 
dijkstra = function(graph, init_node) {
  stopifnot(is.data.frame(graph),
            ncol(graph) == 3,
            names(graph) == c("v1", "v2", "w"),
            sapply(graph, is.numeric),
            is.numeric(init_node),
            any(graph[1:2] == init_node))
  unseen = as.vector(unique(unlist(graph[["v1"]])))
  path = rep(Inf, length(unseen))
  path[match(init_node, unseen)] = 0
  root = init_node
  repeat {
    nodes = subset(graph, v1 == root & v2 %in% intersect(v2, unseen))
    nrnodes = length(nodes[["v2"]])
    if (nrnodes > 0) {
      for (i in 1:nrnodes) {
        tentative = path[match(root, unseen)] + nodes[["w"]][i]
        if (tentative < path[match(nodes[["v2"]][i], unseen)]) {
          path[match(nodes[["v2"]][i], unseen)] = tentative
        }
      }
    }
    unseen[match(root, unseen)] = -1
    if (all(unseen %in% -1) || is.infinite(min(path[unseen > 0]))) {
      break
    }
    smallest = Inf
    index = 1
    for (i in 1:length(path)) {
      if (unseen[i] == -1) {
        next
      }
      if (path[i] < smallest) {
        smallest = path[i]
        index = i
      }
    }
    root = unseen[index]
  }
  return(path)
}
