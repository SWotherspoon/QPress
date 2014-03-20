\name{weight.density}
\alias{weight.density}
\title{Weight Density Plots}
\usage{
  weight.density(sim, epsilon = 1e-05, main = "")
}
\arguments{
  \item{sim}{the result from \code{system.simulate}}

  \item{epsilon}{outomes below this in absolute magnitude
  are treated as zero.}

  \item{main}{text for plot title}
}
\description{
  Display weights of valid and invalid matrices as a
  density plots
}
\details{
  This control constructs density plots that show the
  distribution of selected edge weights for the cases that
  meet the selected validation criteria (blue), and those
  that do not (red), following a given perturbation.

  The slider controls the level of smoothing of the
  densities. Edges are labelled by pairs of integers for
  compactness, where the integer codes correspond to the
  ordering of the node labels.
}

