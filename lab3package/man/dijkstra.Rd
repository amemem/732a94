% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dijkstra.R
\name{dijkstra}
\alias{dijkstra}
\title{Dijkstra's algorithm}
\source{
\url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
}
\usage{
dijkstra(graph, init_node)
}
\arguments{
\item{graph}{A data frame with 3 variables labeled \emph{v1}, \emph{v2} and \emph{w}.}

\item{init_node}{The starting node where all paths are to be computed from.}
}
\value{
A vector with the distances from the initial node to all nodes.
}
\description{
Computes the shortest paths from an initial node to all other nodes in a graph.
}
