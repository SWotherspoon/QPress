##' Read text representations of edge weight constraints
##'
##' Read a set of edge constraints from a vector of strings,
##' represented in the following format.  Each string corresponds to a
##' constraint of the form "a < b", and where a and b are two edges.
##' Each edge consists of two node labels separated by an arrow, where
##' the arrow consists of a sequence of dashes "-" followed by one of
##' the character sequences ">","*","<>". The number of dashes used in
##' the arrow is ignored.
##'
##' @title Text Representations of Edge Constraints
##' @param lines a vector of strings representing "a < b" constraints
##' @param edges an edge list
##' @return Returns a constrained edge list - a list with components
##' \item{\code{edges}}{the edge list}
##' \item{\code{a}}{the index of the "a" edge of the constraint}
##' \item{\  code{b}}{the index of the "b" edge of the constraint}
##' @export
parse.constraints <- function(lines,edges) {
  ## Parse lines
  m <- regexec("([^\\*<>-]+)(-+)(\\*|>)?([^\\*<>-]+) < ([^\\*<>-]+)(-+)(\\*|>)?([^\\*<>-]+)",lines)
  err <- sapply(m,"[[",1)== -1
  if(any(err)) {
    warning("Could not parse constraints: ",paste(lines[err],collapse=", "))
    lines <- lines[!err]
    m <- m[!err]
  }
  m <- regmatches(lines,m)
  from1 <- gsub("^\\s+|\\s+$","",lapply(m,"[[",2))
  to1 <-   gsub("^\\s+|\\s+$","",lapply(m,"[[",5))
  line1 <- sapply(m,"[[",3)
  head1 <- sapply(m,"[[",4)
  from2 <- gsub("^\\s+|\\s+$","",lapply(m,"[[",6))
  to2 <-   gsub("^\\s+|\\s+$","",lapply(m,"[[",9))
  line2 <- sapply(m,"[[",7)
  head2 <- sapply(m,"[[",8)



  ## Extract edges
  labels <- node.labels(edges)
  from1 <- factor(from1,levels=labels)
  to1 <- factor(to1,levels=labels)
  from2 <- factor(from2,levels=labels)
  to2 <- factor(to2,levels=labels)

  group1 <- nchar(line1)-1
  group2 <- nchar(line2)-1

  type <- c("N","P","U","Z")
  type1 <- type[match(head1,c("*",">","<>",""))]
  type2 <- type[match(head2,c("*",">","<>",""))]

  e1 <- mapply(function(from,to,type)
    match(TRUE,edges$From==from & edges$To==to & edges$Type==type),
    from1,to1,type1)
  e2 <- mapply(function(from,to,type)
    match(TRUE,edges$From==from & edges$To==to & edges$Type==type),
    from2,to2,type2)

  if(any(is.na(e1) | is.na(e2)))
    warning("Encountered undefined edges: ",paste(lines[is.na(e1) | is.na(e2)],collapse=", "))
   list(edges=edges,a=e1,b=e2)
 }


##' Construct functions to generate constrained random community
##' matrices
##'
##' Given an edge list with simple "a < b" edge weight constraints,
##' this function constructs a list of functions that can be used to
##' generate random community matrices that correspond to the
##' constrained directed graph.
##'
##' If the edge list contains uncertain edges, any constraint
##' involving an uncertain edge is dropped if that edge is not
##' represented in the matrix.
##'
##' This version of generates random community matrices by rejection,
##' and can be extremely inefficient if the constraints are complex.
##'
##' @title Sampling Constrained Community Matrices
##' @param constrained a constrained edge list (see
##'   \code{\link{community.sampler}})
##' @param required.groups a vector of integers specifying which
##'   groups of edges must always occur in the community matrix.
##' @seealso \code{\link{community.sampler}}
##' @return Returns a list with elements
##' \item{\code{community()}}{a function to generate a random community matrix}
##' \item{\code{select(p)}}{a function that randomly selects which uncertain edges will be retained}
##' \item{\code{weights(W)}}{a function that returns the (non-zero) weights as a vector}
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
##' constrained <- parse.constraints(c(
##'   "E -> D < D -* E",
##'   "C -> E < A -> E",
##'   "D -> B < D -* E"),
##'   edges)
##' s <- community.rejection.sampler(constrained)
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
community.rejection.sampler <- function(constrained,required.groups=c(0)) {
  edges <- constrained$edges
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

  a <- constrained$a
  b <- constrained$b

  zs <- rep(1,length(uncertain))

  community <- if(n.omit > 0) {
    function() {
      r <- runif(n.edges,lower,upper)
      while(any(abs(r[b])<abs(r[a]))) {
        r <- runif(n.edges,lower,upper)
      }
      r[uncertain] <- r[uncertain]*zs
      W[k.edges] <- r
      W
    }
  } else {
    function() {
      r <- runif(n.edges,lower,upper)
      while(any(abs(r[b])<abs(r[a]))) {
        r <- runif(n.edges,lower,upper)
      }
      W[k.edges] <- r
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





##' Construct bounding sets used to order weights to meet the edge
##' weight constraints.
##'
##' These bounds sets are used to order a set of random edge weights
##' so that they meet the imposed constraints.
##' @title Bounds sets.
##' @param constrained a constrained edge list (see
##'   \code{\link{community.sampler}})
##' @return Returns sets of weights bounded above by their first
##'   element, in topological order.
bound.sets <- function(constrained) {
  a <- constrained$a
  b <- constrained$b

  ## Find all weights bounded above by the root weight r
  bounded <- function(r) {
    v <- unique(a[b %in% r])
    repeat {
      va <- setdiff(a[b %in% v],v)
      if(length(va)==0) break;
      v <- c(va,v)
    }
    v
  }

  bounds <- list()
  n <- 0

  ## Unvisited weights
  us <- b
  ## Unbounded weights
  vs <- setdiff(b,a)
  while(length(vs) > 0) {
    for(v in vs) {
      ## Find all weights bounded by v
      bs <- bounded(v)
      if(v %in% bs) warning("Cyclic constraints found")
      bounds[[n <- n+1]] <- c(v,bs)
    }
    ## Unvisited weights
    us <- setdiff(us,vs)
    ## Weights bounded only by visited weights
    vs <- setdiff(us,a[b %in% us])
  }
  bounds
}

##' Order a set of absolute edge weights to meet imposed edge weights
##' constraints.
##'
##' @title Constraint Sorting
##' @param w absolute values of the edge weights
##' @param bounds a bounds set generated by \code{\link{bound.sets}}
##' @return The absolute weights ordered to meet the edge weights
##'   constraints represented by the bounds set.
constraint.order <- function(w,bounds) {
  for(bs in bounds) {
    k <- which.max(w[bs])
    if(k!=1) w[bs[c(1,k)]] <- w[bs[c(k,1)]]
  }
  w
}



##' Construct functions to generate constrained random community
##' matrices
##'
##' Given an edge list with simple "a < b" edge weight constraints,
##' this function constructs a list of functions that can be used to
##' generate random community matrices that correspond to the
##' constrained directed graph.
##'
##' If the edge list contains uncertain edges, any constraint
##' involving an uncertain edge is dropped if that edge is not
##' represented in the matrix.
##'
##' This version of generates random community matrices by rejection,
##' and can be extremely inefficient if the constraints are complex.
##'
##' @title Sampling Constrained Community Matrices
##' @param constrained a constrained edge list (see
##'   \code{\link{community.sampler}})
##' @param required.groups a vector of integers specifying which
##'   groups of edges must always occur in the community matrix.
##' @seealso \code{\link{community.sampler}}
##' @return Returns a list with elements
##' \item{\code{community()}}{a function to generate a random community matrix}
##' \item{\code{select(p)}}{a function that randomly selects which uncertain edges will be retained}
##' \item{\code{weights(W)}}{a function that returns the (non-zero) weights as a vector}
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
##' constrained <- parse.constraints(c(
##'   "E -> D < D -* E",
##'   "C -> E < A -> E",
##'   "D -> B < D -* E"),
##'   edges)
##' s <- community.ordering.sampler(constrained)
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
community.ordering.sampler <- function(constrained,required.groups=c(0)) {
  edges <- constrained$edges
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

  bounds <- bound.sets(constrained)

  zs <- rep(1,length(uncertain))

  community <- if(n.omit > 0) {
    function() {
      r <- runif(n.edges,lower,upper)
      r <- sign(r)*constraint.order(abs(r),bounds)
      r[uncertain] <- r[uncertain]*zs
      W[k.edges] <- r
      W
    }
  } else {
    function() {
      r <- runif(n.edges,lower,upper)
      r <- sign(r)*constraint.order(abs(r),bounds)
      W[k.edges] <- r
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


