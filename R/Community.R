
##' Classify elements as positive, negative or zero.
##'
##' Calculates the sign of x, except that values less than epsilon in
##' magnitude are rounded down to zero.
##' @title Sign classification
##' @param x vector of values to test
##' @param epsilon magnitude threshold
##' @return Returns a vector with elements +1,0 or -1.
##' @examples
##' signum(c(-40,-3,-0.1E-8,0,2,5))
##' @export
signum <- function(x,epsilon=1.0E-5) {
    (x > epsilon) - (x < -epsilon)
}



##' Extract labels for the nodes and edges of the directed graph.
##'
##' These functions construct meaningful labels for the nodes and
##' edges from an edge list.
##' @title Node and Edge Labels
##' @param edges an edge list
##' @param reverse reverse the direction of edges
##' @return Return a vector of node or edge labels
##' @export
node.labels <- function(edges) {
  levels(edges$From)
}

##' @rdname node.labels
##' @export
edge.labels <- function(edges,reverse=F) {

  if(reverse) {
    symb <- c("*",">","<>","")
    paste(edges$From,
          " ",
          "-",
          symb[unclass(edges$Type)],
          " ",
          edges$To,
          sep="")
  } else {
    symb <- c("*","<","<>","")
    paste(edges$To,
          " ",
          symb[unclass(edges$Type)],
          "-",
          " ",
          edges$From,
          sep="")
  }

}




##' Adjacency matrix of the directed graph.
##'
##' This function converts an edge list to an adjacency matrix
##' \code{A}, following the convention that \code{A[i,j]} represents
##' the impact of node \code{j} on node \code{i}.
##' @title Adjacency Matrix
##' @param edges an edge list
##' @param labels add row and column labels
##' @param required.groups which edge groups should be included?
##' @return Returns the adjacency matrix for the directed graph.
##' @export
adjacency.matrix <- function(edges,labels=F,required.groups=c(0)) {

  z <- ifelse(edges$Group %in% required.groups,1L,0L)
  labs <- node.labels(edges)
  n <- length(labs)
  A <- matrix(0L,n,n,dimnames=if(labels) list(labs,labs))
  type <- c("N","P","U","Z")
  weight <- c(-1L,1L,NA,0L)
  A[cbind(edges$To,edges$From)] <- z*weight[match(edges$Type,type)]
  A
}




##' Adjoint matrix and Characteristic Polynomial
##'
##' These functions compute the adjoint matrix and characteristic
##' polynomial of A by the Fedeew-Leverrier algorithm.
##'
##' If A has integer elements and the computations are performed with
##' integer arithmetic the result is exact.
##' @title Fedeew-Leverrier
##' @param A a square matrix
##' @return The adjoint matrix or characteristic polynomial of A.
##' @export
adjoint <- function(A) {
  n <- nrow(A)
  B <- diag(1,n,n)
  for(k in 1:(n-1)) {
    B <- A%*%B
    p <- -sum(diag(B))/k
    diag(B) <- diag(B)+p
  }
  p <- -sum(diag(A%*%B))/n
  sign(p)*B
}


##' @rdname adjoint
##' @export
charpoly <- function(A) {
  n <- nrow(A)
  B <- diag(1,n,n)
  p <- rep(1,n+1)
  for(k in 1:n) {
    B <- A%*%B
    p[k+1] <- -sum(diag(B))/k
    diag(B) <- diag(B)+p[k+1]
  }
  p
}

##' Enforce slef limitation
##'
##' For stability, the majority of nodes of the directed graph should
##' have a self limiting edge. This function adds a self limiting edge
##' for every node to an existing edge list.
##' @title Self Limitation
##' @param edges an edge list
##' @return Returns an edge list augmented with self limiting edges.
##' @export
enforce.limitation <- function(edges) {
    loops <- which(edges$To==edges$From)
    labels <- node.labels(edges)
    limit <- setdiff(labels,edges$From[loops])
    n <- length(limit)
    rbind(edges,
          data.frame(From=factor(limit,levels=labels),
                     To=factor(limit,levels=labels),
                     Group=rep(0,n),
                     Type=factor(rep("N",n),levels(edges$Type)),
                     Pair=max(edges$Pair)+1:n))
}


## Subset the edges by group
subset.groups <- function(edges,groups) {
  edges[edges$Group %in% groups,]
}


## Create functions to generate random community matrices given the
## edge list describing the web topology.  This returns a list of two
## functions, "community" draws a random community matrix, and
## "select" determines which optional edges will be retained in the
## web topology.  The user can specify a list of the edge groups that are
## required to be retained in the model.
community.sampler <- function(edges,required.groups=c(0)) {

  labels <- node.labels(edges)
  n.nodes <- length(labels)
  n.edges <- nrow(edges)
  W <- matrix(0,n.nodes,n.nodes)

  ## Ranges and indices of non-zero matrix entries
  lower <- ifelse(edges$Type=="U" | edges$Type=="N",-1L,0L)
  upper <- ifelse(edges$Type=="U" | edges$Type=="P",1L,0L)
  k.edges <- as.vector(unclass(edges$To)+(unclass(edges$From)-1)*n.nodes)

  ## The indices of the matrix entries that can be omitted (zeroed), the
  ## expansion index that relates matching edges of a pair, and the
  ## number of edges that can be omitted.
  required <- edges$Group %in% required.groups
  k.optional <- k.edges[!required]
  optional <- factor(edges$Pair[!required])
  expand <- as.vector(unclass(optional))
  n.omit <- max(0,expand)
  optional.labels <- edge.labels(edges)[!required]

  zs <- rep(1,n.omit)

  if(n.omit > 0) {
    community <- function() {
      W[k.edges] <- runif(n.edges,lower,upper)
      W[k.optional] <- W[k.optional]*zs[expand]
      W
    }
    select <- function(p) {
      zs <<- rbinom(n.omit,1,p)
      zs
    }
  } else  {
    community <- function() {
      W[k.edges] <- runif(n.edges,lower,upper)
      W
    }
    select <- function(p=0) {
      zs
    }
  }
  weights <- function(W) {
    W[k.edges]
  }
  list(community=community,
       select=select,
       weights=weights,
       optional=optional.labels)
}


## Check the stability of a simulated community matrix W

##' Test community matrix stability
##'
##' The system is stable if the eigenvalues of community matrix all
##' have negative real part. This function tests the eigenvalues of a
##' simulated community matrix to determine the stability of the
##' repreeented system.
##' @title System Stability
##' @param W a simulated community matrix
##' @return Returns TRUE if the system is stable, FALSE otherwise.
##' @export
stable.community <- function(W) {
  all(Re(eigen(W,symmetric=FALSE,only.values=T)$values)<0)
}


## Generate a function to check a press condition.  User must supply a
## vector of named elements that specify the relative magnitude of the
## press perturbation, and a vector of named elements that specify the
## signs of the change in the monitored nodes.
press.validate <- function(edges,perturb,monitor,epsilon=1.0E-5) {
  labels <- node.labels(edges)

  index <- function(name) {
    k <- match(name,labels)
    if(any(is.na(k)))
      warning("Unknown nodes:",paste(name[is.na(k)],collapse=" "))
    k
  }

  ## Indices of perturb
  k.perturb <- index(names(perturb))
  k.monitor <- index(names(monitor))
  S.press <- double(length(labels))
  S.press[k.perturb] <- -perturb
  monitor <- sign(monitor)

  ## Return function to check condition
  function(W) {
    s <- tryCatch(solve(W,S.press),error=function(e) NULL)
    !is.null(s) && all(signum(s[k.monitor],epsilon)==monitor)
  }
}


## Generate a function to determine the impact of a press perturbation
press.impact <- function(edges,perturb,monitor=NULL) {
  labels <- node.labels(edges)

  index <- function(name) {
    k <- match(name,labels)
    if(any(is.na(k)))
      warning("Unknown nodes:",paste(name[is.na(k)],collapse=" "))
    k
  }

  ## Indices of perturb
  k.perturb <- index(names(perturb))
  S.press <- double(length(labels))
  S.press[k.perturb] <- -perturb

  if(length(monitor)==0) {
    impact <- function(W) solve(W,S.press)
  } else {
    k.monitor <- index(names(monitor))
    impact <- function(W) solve(W,S.press)[k.monitor]
  }

  ## Return function to compute impact
  impact
}





