% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Community.R
\name{community.sampler}
\alias{community.sampler}
\title{Sampling Community Matrices}
\usage{
community.sampler(edges, required.groups = c(0))
}
\arguments{
\item{edges}{an edge list}

\item{required.groups}{a vector of integers specifying which groups of edges
must always occur in the community matrix.}
}
\value{
Returns a list with elements
\item{\code{community()}}{a function to generate a random community matrix}
\item{\code{select(p)}}{a function that randomly selects which uncertain
edges will be retained}
\item{\code{weights(W)}}{a function that returns the (non-zero) weights as
a vector}
\item{\code{edge.labels}}{the labels of the edges}
\item{\code{uncertain.labels}}{the labels of the uncertain edges}
}
\description{
Construct functions to generate random community matrices
}
\details{
Given an edge list that specifies a directed graph, this function constructs
a list of functions that can be use to generate random community matrices
corresponding to that directed graph.

Edges in the edge list that do not fall in a required group are considered
uncertain, and may or may not be represented in the community matrix.

Random community matrices are generated in two stages, the first stage
determines which of the uncertain edges will be included or excluded in
subsequent simulations, while the second stage generates random matrices
corresponding to the selected.  The \code{select} function is a function of
a single argument \code{p} that determines which of the uncertain edge pairs
will be included in matrices generated by subsequent calls to
\code{community}. This function always selects either neither or both edges
of a pair and every uncertain pair has likelihood \code{p} of being
selected. The \code{community} function is a function of no arguments that
generates a random community matrix.  The \code{weights} function is a
function of a single argument \code{W} that returns those entries of the
community matrix \code{W} that correspond to edges in the edge list.
}
\examples{
set.seed(32)
## Sample model
edges <- parse.digraph(c(
  "E *-> D",
  "D *-> C",
  "C -> E",
  "E *-> B",
  "B *-> A",
  "A -> E",
  "D --> B"))
edges <- enforce.limitation(edges)
s <- community.sampler(edges)
## Force D --> B edge out
s$select(0)
## Generate community matrices
s$community()
s$community()
## Force D --> B edge in
s$select(1)
## Generate community matrices
s$community()
s$community()
## Select the uncertain D --> B edge with prob 0.6
s$select(0.6)
## Generate community matrices
s$community()
s$community()
}
