dijkstra = function(graph, init) {
  stopifnot(is.data.frame(graph),
            ncol(graph) == 3,
            names(graph) == c("v1", "v2", "w"),
            sapply(graph, is.numeric),
            is.numeric(init),
            any(graph[1:2] == init))
  unseen = as.vector(unique(unlist(graph[["v1"]])))
  path = rep(Inf, length(unseen))
  path[match(init, unseen)] = 0
  root = init
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

wiki_graph =
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph, 1)
