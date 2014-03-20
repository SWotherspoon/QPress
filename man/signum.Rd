\name{signum}
\alias{signum}
\title{Sign classification}
\usage{
  signum(x, epsilon = 1e-05)
}
\arguments{
  \item{x}{vector of values to test}

  \item{epsilon}{magnitude threshold}
}
\value{
  Returns a vector with elements +1,0 or -1.
}
\description{
  Classify the sign of the elements of a vector
}
\details{
  Calculates the sign of the elements of then vector x,
  except that values less than epsilon in magnitude are
  rounded down to zero.
}
\examples{
signum(c(-40,-3,-0.1E-8,0,2,5))
}

