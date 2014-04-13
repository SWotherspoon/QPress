\name{enforce.limitation}
\alias{enforce.limitation}
\title{Self Limitation}
\usage{
  enforce.limitation(edges)
}
\arguments{
  \item{edges}{an edge list}
}
\value{
  Returns an edge list augmented with self limiting edges.
}
\description{
  Enforce self limitation
}
\details{
  For stability, the majority of nodes of the directed
  graph should have a self limiting edge. This function
  adds a self limiting edge for every node to an existing
  edge list.
}

