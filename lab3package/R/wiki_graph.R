#' A simple dataset for Dijkstra's algorithm
#' 
#' This dataset contains 3 variables, where \emph{w} is the cost or distance
#' from the nodes in \emph{v1} to the nodes in \emph{v2}.
#' 
#' @usage data(wiki_graph)
#' 
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{v1}{Move from this node}
#'   \item{v2}{Move to this node}
#'   \item{w}{The cost of moving between two nodes}
#' }
#' 
#' @source \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#' 
"wiki_graph"