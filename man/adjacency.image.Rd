\name{adjacency.image}
\alias{adjacency.image}
\title{Adjacency Matrix Image}
\usage{
  adjacency.image(edges, required.groups = c(0),
    cex.axis = 1)
}
\arguments{
  \item{edges}{an edge list}

  \item{required.groups}{which edge groups should be
  included?}

  \item{cex.axis}{character expansion factor for the edge
  labels}
}
\value{
  Returns the adjacency matrix for the directed graph.
}
\description{
  Display adjacency matrix of the directed graph as an
  image
}
\details{
  Display the matrix constructed by \code{adjacency.matrix}
  as an image.
}
\examples{
edges <- parse.text(c(
  "E *-> D",
  "D *-> C",
  "C -> E",
  "E *-> B",
  "B *-> A",
  "A -> E",
  "D -> B"))
edges <- enforce.limitation(edges)
adjacency.image(edges)
}
\seealso{
  \code{\link{adjacency.matrix}}
}

