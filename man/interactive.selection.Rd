\name{interactive.selection}
\alias{interactive.selection}
\title{Interactive Selection Widget}
\usage{
  interactive.selection(action, nodes, edges = NULL,
    slider = NULL, checkbox = NULL, perturb = T,
    monitor = T)
}
\arguments{
  \item{action}{function to perform the widgets action}

  \item{nodes}{node labels}

  \item{edges}{edge labels}

  \item{slider}{slider label}

  \item{checkbox}{checkbox label}

  \item{perturb}{should a node perturbation control be
  rendered}

  \item{monitor}{should a node monitoring control be
  rendered}
}
\description{
  Construct control widget
}
\details{
  Constructs a toplevel window that allows the allowing the
  user to interactively select nodes to perturb/monitor,
  from a subset of models, and then perform a given action.

  The \code{action} argument must be function of five
  arguments \itemize{ \item \code{perturb} the nodes that
  were perturbed \item \code{monitor} the outcome of the
  monitoring \item \code{edge} the edges to select \item
  \code{check} the state of a checkbutton \item
  \code{slider} the state of a slider }
}

