##' Classify the sign of the elements of a vector
##'
##' Calculates the sign of the elements of then vector x, except that
##' values less than epsilon in magnitude are rounded down to zero.
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
edge.labels <- function(edges,reverse=FALSE) {

  if(!reverse) {
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
##' This function converts an edge list to an adjacency matrix \code{A},
##' following the convention that \code{A[i,j]} represents the impact of node
##' \code{j} on node \code{i}.
##' @title Adjacency Matrix
##' @param edges an edge list
##' @param labels add row and column labels
##' @param required.groups which edge groups should be included?
##' @seealso \code{\link{adjacency.image}}
##' @return Returns the adjacency matrix for the directed graph.
##' @examples
##' edges <- parse.digraph(c(
##'   "E *-> D",
##'   "D *-> C",
##'   "C -> E",
##'   "E *-> B",
##'   "B *-> A",
##'   "A -> E",
##'   "D -> B"))
##' edges <- enforce.limitation(edges)
##' adjacency.matrix(edges,labels=TRUE)
##' @export
adjacency.matrix <- function(edges,labels=FALSE,required.groups=c(0)) {

  z <- ifelse(edges$Group %in% required.groups,1L,0L)
  labs <- node.labels(edges)
  n <- length(labs)
  A <- matrix(0L,n,n,dimnames=if(labels) list(labs,labs))
  type <- c("N","P","U","Z")
  weight <- c(-1L,1L,NA,0L)
  A[cbind(edges$To,edges$From)] <- z*weight[match(edges$Type,type)]
  A
}




##' Display adjacency matrix of the directed graph as an image
##'
##' Display the matrix constructed by \code{adjacency.matrix} as an image.
##' @title Adjacency Matrix Image
##' @param edges an edge list
##' @param required.groups which edge groups should be included?
##' @param cex.axis character expansion factor for the edge labels
##' @seealso \code{\link{adjacency.matrix}}
##' @return Returns the adjacency matrix for the directed graph.
##' @examples
##' edges <- parse.digraph(c(
##'   "E *-> D",
##'   "D *-> C",
##'   "C -> E",
##'   "E *-> B",
##'   "B *-> A",
##'   "A -> E",
##'   "D -> B"))
##' edges <- enforce.limitation(edges)
##' adjacency.image(edges)
##' @importFrom graphics axis box image par strwidth
##' @export
adjacency.image <- function(edges,required.groups=c(0),cex.axis=1) {
  pal <- c("#92C5DE", "#FFFFFF", "#F4A582")

  A <- adjacency.matrix(edges,required.groups=required.groups)
  nodes <- node.labels(edges)
  n <- length(nodes)
  lwidth <- max(strwidth(nodes,units="inches",cex=cex.axis))
  opar <- par(mai=c(0,lwidth+0.2,lwidth+0.2,0)+0.1)
  ## Flip image to match matrix ordering
  image(seq_len(n),seq_len(n),t(A)[,rev(seq_len(n))],
        axes=FALSE,xlab="",ylab="",col=pal)
  axis(2,seq_len(n),rev(nodes),las=2,cex.axis=cex.axis)
  axis(3,seq_len(n),nodes,las=2,cex.axis=cex.axis)
  box()
  par(opar)
}



##' Adjoint matrix and Characteristic Polynomial
##'
##' These functions compute the adjoint matrix and characteristic polynomial of
##' A by the Fedeew-Leverrier algorithm.
##'
##' If A has integer elements and the computations are performed with integer
##' arithmetic the result is exact.
##' @title Fedeew-Leverrier
##' @param A a square matrix
##' @return \code{adjoint} returns the adjoint matrix of \code{A}
##' @examples
##' edges <- parse.digraph(c(
##'   "E *-> D",
##'   "D *-> C",
##'   "C -> E",
##'   "E *-> B",
##'   "B *-> A",
##'   "A -> E",
##'   "D -> B"))
##' edges <- enforce.limitation(edges)
##' A <- adjacency.matrix(edges,labels=TRUE)
##' adjoint(A)
##' @export
adjoint <- function(A) {
  n <- nrow(A)
  B <- diag(1,n,n)
  for(k in seq_len(n-1)) {
    B <- A%*%B
    p <- -sum(diag(B))/k
    diag(B) <- diag(B)+p
  }
  p <- -sum(diag(A%*%B))/n
  sign(p)*B
}


##' @rdname adjoint
##' @return \code{charpoly} returns the coefficients of the characteristic
##'   polynomial of \code{A} as a vector.
##' @export
charpoly <- function(A) {
  n <- nrow(A)
  B <- diag(1,n,n)
  p <- rep(1,n+1)
  for(k in seq_len(n)) {
    B <- A%*%B
    p[k+1] <- -sum(diag(B))/k
    diag(B) <- diag(B)+p[k+1]
  }
  p
}

##' Enforce self limitation
##'
##' For stability, the majority of nodes of the directed graph should have a
##' self limiting edge. This function adds a self limiting edge for every node
##' to an existing edge list.
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
                     Pair=seq(max(edges$Pair)+1,length.out=n)))
}



##' Subset an edge list
##'
##' These functions extract a subset of an edge list containing only edges in a
##' specified group, or incident with a specified set of nodes.
##' @title Edge Subsets
##' @param edges an edge list
##' @param groups the groups to retain in the subset
##' @return \code{retain.groups} returns an edge list containing only edges from
##'   the specified groups.
##' @examples
##' edges <- parse.digraph(c("A *-> B","B *-> C","C *--> D"))
##' write.digraph(retain.groups(edges,c(0)))
##' @export
retain.groups <- function(edges,groups) {
  labels <- node.labels(edges)
  edges <- edges[edges$Group %in% groups,]
  labels <- labels[labels %in% c(as.character(edges$From),
                                 as.character(edges$To))]
  edges$From <- factor(edges$From,levels=labels)
  edges$To <- factor(edges$To,levels=labels)
  edges
}

##' @rdname retain.groups
##' @param nodes the nodes to retain in the subset
##' @return \code{retain.nodes} returns an edge list containing only edges
##'   incident on the specified nodes.
##' @export
retain.nodes <- function(edges,nodes) {
  labels <- node.labels(edges)
  edges <- edges[edges$From %in% nodes | edges$To %in% nodes,]
  labels <- labels[labels %in% c(as.character(edges$From),
                                 as.character(edges$To))]
  edges$From <- factor(edges$From,levels=labels)
  edges$To <- factor(edges$To,levels=labels)
  edges
}




## Create functions to generate random community matrices given the edge list
## describing the web topology.  This returns a list of two functions,
## "community" draws a random community matrix, and "select" determines which
## uncertain edges will be retained in the web topology.  The user can specify a
## list of the edge groups that are required to be retained in the model.


##' Construct functions to generate random community matrices
##'
##' Given an edge list that specifies a directed graph, this function constructs
##' a list of functions that can be use to generate random community matrices
##' corresponding to that directed graph.
##'
##' Edges in the edge list that do not fall in a required group are considered
##' uncertain, and may or may not be represented in the community matrix.
##'
##' Random community matrices are generated in two stages, the first stage
##' determines which of the uncertain edges will be included or excluded in
##' subsequent simulations, while the second stage generates random matrices
##' corresponding to the selected.  The \code{select} function is a function of
##' a single argument \code{p} that determines which of the uncertain edge pairs
##' will be included in matrices generated by subsequent calls to
##' \code{community}. This function always selects either neither or both edges
##' of a pair and every uncertain pair has likelihood \code{p} of being
##' selected. The \code{community} function is a function of no arguments that
##' generates a random community matrix.  The \code{weights} function is a
##' function of a single argument \code{W} that returns those entries of the
##' community matrix \code{W} that correspond to edges in the edge list.
##' @title Sampling Community Matrices
##' @param edges an edge list
##' @param required.groups a vector of integers specifying which groups of edges
##'   must always occur in the community matrix.
##'
##' @return Returns a list with elements
##' \item{\code{community()}}{a function to generate a random community matrix}
##' \item{\code{select(p)}}{a function that randomly selects which uncertain
##' edges will be retained}
##' \item{\code{weights(W)}}{a function that returns the (non-zero) weights as
##' a vector}
##' \item{\code{edge.labels}}{the labels of the edges}
##' \item{\code{uncertain.labels}}{the labels of the uncertain edges}
##' @examples
##' set.seed(32)
##' ## Sample model
##' edges <- parse.digraph(c(
##'   "E *-> D",
##'   "D *-> C",
##'   "C -> E",
##'   "E *-> B",
##'   "B *-> A",
##'   "A -> E",
##'   "D --> B"))
##' edges <- enforce.limitation(edges)
##' s <- community.sampler(edges)
##' ## Force D --> B edge out
##' s$select(0)
##' ## Generate community matrices
##' s$community()
##' s$community()
##' ## Force D --> B edge in
##' s$select(1)
##' ## Generate community matrices
##' s$community()
##' s$community()
##' ## Select the uncertain D --> B edge with prob 0.6
##' s$select(0.6)
##' ## Generate community matrices
##' s$community()
##' s$community()
##' @importFrom stats runif rbinom
##' @export
community.sampler <- function(edges,required.groups=c(0)) {
  n.nodes <- length(node.labels(edges))
  weight.labels <- edge.labels(edges)
  n.edges <- nrow(edges)
  W <- matrix(0,n.nodes,n.nodes)

  ## Ranges and indices of non-zero matrix entries
  lower <- ifelse(edges$Type=="U" | edges$Type=="N",-1L,0L)
  upper <- ifelse(edges$Type=="U" | edges$Type=="P",1L,0L)
  k.edges <- as.vector(unclass(edges$To)+(unclass(edges$From)-1)*n.nodes)

  ## The indices of the matrix entries that can be omitted (zeroed), the
  ## expansion index that relates matching edges of a pair, and the
  ## number of edges that can be omitted.
  uncertain <- which(!(edges$Group %in% required.groups))
  expand <- match(edges$Pair[uncertain],unique(edges$Pair[uncertain]))
  n.omit <- max(0,expand)

  zs <- rep(1,n.omit)

  community <- if(n.omit > 0) {
    function() {
      r <- runif(n.edges,lower,upper)
      r[uncertain] <- r[uncertain]*zs
      W[k.edges] <- r
      W
    }
  } else {
    function() {
      W[k.edges] <- runif(n.edges,lower,upper)
      W
    }
  }

  select <- if(n.omit > 0) {
    function(p) {
      zs <<- rbinom(n.omit,1,p)[expand]
    }
  } else {
    function(p=0) {
      zs
    }
  }

  weights <- function(W) {
    W[k.edges]
  }

  list(community=community,
       select=select,
       weights=weights,
       weight.labels=weight.labels,
       uncertain.labels=weight.labels[uncertain])
}



##' Test community matrix stability
##'
##' The system is stable if the eigenvalues of community matrix all have
##' negative real part. This function tests the eigenvalues of a simulated
##' community matrix to determine the stability of the repreeented system.
##' @title System Stability
##' @param W a simulated community matrix
##' @return Returns TRUE if the system is stable, FALSE otherwise.
##' @examples
##' set.seed(32)
##' ## Sample model
##' edges <- parse.digraph(c(
##'   "E *-> D",
##'   "D *-> C",
##'   "C -> E",
##'   "E *-> B",
##'   "B *-> A",
##'   "A -> E",
##'   "D --> B"))
##' edges <- enforce.limitation(edges)
##' s <- community.sampler(edges)
##' s$select(0)
##' ## First sample is stable
##' W <- s$community()
##' stable.community(W)
##' ## Second is not
##' W <- s$community()
##' stable.community(W)
##' @export
stable.community <- function(W) {
  all(Re(eigen(W,symmetric=FALSE,only.values=TRUE)$values)<0)
}


##' Construct a function to test a validation criterion
##'
##' Given the an edge list that specifies a directed graph, a set of nodes to
##' perturb and a set of nodes to monitor, \code{press.validate} constructs a
##' function of a single argument \code{W} to test whether the response to
##' perturbation of the system represented by the community matrix \code{W}
##' matches an observed outcome.  The outcome is only specified up to sign (-1,
##' 0 or +1), where outcomes smaller than \code{epsilon} are treated as zero.
##' @title Validation Criterion
##' @param edges an edge list
##' @param perturb a named vector that indicates which nodes were perturbed and
##'   the relative magnitude of the perturbation.
##' @param monitor n named vector of signs (-1,0,1) that indicates the outcome
##'   of the perturbation.
##' @param epsilon outomes below this in absolute magnitude are treated as zero.
##' @return Returns a function that when applied to a community matrix
##'   determines whether the matrix is consistent with the given validation
##'   criterion.
##' @examples
##' set.seed(32)
##' ## Sample model
##' edges <- parse.digraph(c(
##'   "E *-> D",
##'   "D *-> C",
##'   "C -> E",
##'   "E *-> B",
##'   "B *-> A",
##'   "A -> E",
##'   "D --> B"))
##' edges <- enforce.limitation(edges)
##' s <- community.sampler(edges)
##' s$select(0.5)
##' ## Perturb D, B and C must decrease
##' f <- press.validate(edges,perturb=c(D=1),monitor=c(B=-1,C=-1))
##' W <- s$community()
##' f(W)
##' W <- s$community()
##' f(W)
##' @export
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


##' Construct a function to calculate response to perturbation.
##'
##' Given the an edge list that specifies a directed graph, a set of nodes to
##' perturb and a set of nodes to monitor, \code{press.impact} constructs a
##' function of a single argument \code{W} that determines the response of the
##' monitored nodes to the perturbation for a simulated community matrix
##' \code{W}.
##' @title Response to Press Perturbation
##' @param edges an edge list.
##' @param perturb a named vector that indicates which nodes were perturbed and
##'   the relative magnitude of the perturbation.
##' @param monitor n named vector that indicates the subset of nodes to monitor.
##' @return Returns a function that when applied to a community matrix
##'   calculates the response to a press perturbation.
##' @examples
##' set.seed(32)
##' ## Sample model
##' edges <- parse.digraph(c(
##'   "E *-> D",
##'   "D *-> C",
##'   "C -> E",
##'   "E *-> B",
##'   "B *-> A",
##'   "A -> E",
##'   "D --> B"))
##' edges <- enforce.limitation(edges)
##' s <- community.sampler(edges)
##' s$select(0.5)
##' ## Perturb D, monitor C
##' f <- press.impact(edges,perturb=c(D=1),monitor=c(C=0))
##' W <- s$community()
##' f(W)
##' W <- s$community()
##' f(W)
##' ## Perturb D, monitor all
##' f <- press.impact(edges,perturb=c(D=1))
##' W <- s$community()
##' f(W)
##' W <- s$community()
##' f(W)
##' @export
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



##' Simulate Inverse Community Matrices for a Network
##'
##' Generate sets of edge weights and the inverse community matrices given a
##' directed graph and validation criteria by rejection sampling.  Matrices with
##' a pattern of signs consistent with the given model are generated, and only
##' the matrices that correspond to stable equilibria and consistent with the
##' given validation criteria are retained.  For matrices retained in the
##' sample, the matrix is inverted, and the \emph{inverse} community matrix, and
##' the weights that define the community matrix are returned. The function also
##' returns the total number of matrices generated, the number of these that are
##' stable and the number that are ultimately accepted for the sample.
##'
##' The output of this function may be passed to the interactive exploratory
##' tools.
##'
##' This function is a simple wrapper for \code{community.sampler},
##' \code{stable.community} and the functions generated by
##' \code{press.validate}.
##' @title Simulate System
##' @param n.sims number of matrices to simulate.
##' @param edges an edge list.
##' @param required.groups a vector of integers specifying which groups of edges
##'   must always occur in the community matrix.
##' @param sampler the sampler object used to generate random weights (see
##'   \code{\link{community.sampler}})
##' @param validators an (optional) list of validation functions generated by
##'   \code{press.validate}.
##' @return Returns a list with elements
##' \item{\code{edges}}{The edge list}
##' \item{\code{A}}{A list of inverse community matrices}
##' \item{\code{w}}{A matrix of the corresponding edge weights}
##' \item{\code{total}}{The total number of matrices generated}
##' \item{\code{stable}}{The number of stable matrices generated}
##' \item{\code{accepted}}{The number of matrices accepted for the sample}
##' @examples
##' set.seed(32)
##' ## Sample model
##' edges <- parse.digraph(c(
##'   "E *-> D",
##'   "D *-> C",
##'   "C -> E",
##'   "E *-> B",
##'   "B *-> A",
##'   "A -> E",
##'   "D --> B"))
##' edges <- enforce.limitation(edges)
##' sims <- system.simulate(10,edges,
##'                         validators=list(
##'                           press.validate(edges,
##'                                          perturb=c(D=1),
##'                                          monitor=c(D=1)),
##'                           press.validate(edges,
##'                                          perturb=c(D=1),
##'                                          monitor=c(B=-1,C=1))))
##' @importFrom stats runif
##' @export
system.simulate <- function(n.sims,edges,
                            required.groups=c(0),
                            sampler=community.sampler(edges,required.groups),
                            validators=NULL) {

  As <- vector("list",n.sims)
  ws <- matrix(0,n.sims,nrow(edges))
  total <- 0
  stable <- 0
  accepted <- 0


  while(accepted < n.sims) {
    total <- total+1

    ## Sample community matrix
    ## The select step selects which uncertain edges will be retained,
    ## where r is the probability an uncertain edge is retained
    z <- sampler$select(runif(1))
    ## The community step constructs a random comunity matrix with the
    ## correct pattern of signs and missing edges.
    W <- sampler$community()

    ## Check stability and validation criteria
    if(!stable.community(W)) next
    stable <- stable+1

    if(!all(as.logical(lapply(validators,function(v) v(W))))) next

    ## Store
    accepted <- accepted+1
    As[[accepted]] <- -solve(W)
    ws[accepted,] <- sampler$weights(W)
  }
  colnames(ws) <- sampler$weight.labels
  list(edges=edges,A=As,w=ws,total=total,stable=stable,accepted=accepted)
}



##' Drop one or more nodes from a system
##'
##' This is an experimental function! Given a set of system simulation outputs
##' (from \code{system.simulate}), it will drop one or more nodes and their
##' associated edges, but leave all other elements of the system untouched. Each
##' set of edge weights in \code{sim} is checked for stability after dropping
##' the specified nodes, and any matrices representing unstable systems are
##' removed from the returned set.
##'
##' @param sim the result from \code{system.simulate}
##' @param to.drop the names of the nodes to drop (check
##'   \code{node.labels(sim$edges)})
##' @param method either "remove" (the specified nodes will be fully removed
##'   from the system) or "zeros" (the specified nodes will be left in the
##'   system but all edges from or to these nodes (other than self-interactions)
##'   are set to zero).
##' @return As for \code{system.simulate}
##' @export
drop.nodes <- function(sim, to.drop, method = "remove") {
  method <- match.arg(tolower(method), c("remove", "zeros"))
  if (!all(to.drop %in% node.labels(sim$edges))) 
    stop("not all of the to.drop nodes appear in this system (check `node.labels(sim$edges)`)")
  ## indexes of to.drop nodes
  vidx <- sim$edges$From == to.drop | sim$edges$To == to.drop
  vidx2 <- attr(sim$edges, "node.labels") == to.drop
  ## check that each row in w still represents a stable system
  n.nodes <- length(node.labels(sim$edges))
  k.edges <- as.vector(unclass(sim$edges$To)+(unclass(sim$edges$From)-1)*n.nodes)
  ## each row in w corresponds to W[k.edges] where W is a full (square,
  ## sparse) weights matrix we need W to test stability
  sim_new <- sim
  if (method == "remove") {
    sim_new$edges <- sim_new$edges[!vidx, ]
    sim_new$edges$From <- droplevels(sim_new$edges$From)
    sim_new$edges$To <- droplevels(sim_new$edges$To)
    ## build the same k.edge indexer for the reduced edge set
    k.edges_new <- as.vector(unclass(sim_new$edges$To)+
                               (unclass(sim_new$edges$From)-1)*
                               length(node.labels(sim_new$edges)))
    w_new <- matrix(NA_real_, nrow = nrow(sim$w), ncol = sum(!vidx))
  } else {
    k.edges_new <- k.edges
    w_new <- matrix(NA_real_, nrow = nrow(sim$w), ncol = ncol(sim$w))
  }
  A_new <- list()
  accepted <- 0L
  for (wi in seq_len(nrow(sim$w))) {
    Wnew <- matrix(0, nrow = n.nodes, ncol = n.nodes)
    Wnew[k.edges] <- sim$w[wi, ]
    if (method == "remove") {
      Wnew <- Wnew[!vidx2, !vidx2] ## drop edges connected to our unwanted node(s)
      ## double-check that this indexing is correct:
      stopifnot(all(Wnew[k.edges_new] == sim$w[wi, !vidx]))
    } else {
      ## set edge weights to zero
      self <- diag(Wnew)
      Wnew[vidx2, ] <- 0
      Wnew[, vidx2] <- 0
      ## reinstate all self-lims, including on "removed" nodes  
      diag(Wnew) <- self 
    }
    ## Wnew has to be stable
    if (stable.community(Wnew)) {
      accepted <- accepted + 1L
      w_new[accepted, ] <- Wnew[k.edges_new]
      A_new[[accepted]] <- -solve(Wnew)
    }
  }
  sim_new$A <- A_new
  sim_new$w <- w_new[seq_len(accepted), ]
  sim_new$accepted <- accepted
  sim_new
}
