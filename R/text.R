## Read a text file and translate to edge descriptions


##' Read and write text representations of models
##'
##' The functions \code{model.text} and \code{parse.text} read a model
##' description from a text file and a string respectively, while
##' \code{write.text} writes a text representation of the model to and
##' file.
##'
##' These functions recognize the following text format.  Each line
##' corresponds to an edge, and must consist of two node labels
##' separated by an arrow.  An arrow consists of one of the character
##' sequences "<","*","<>" or "" on the left and ">","*","<>" or "" on
##' the right, separated by a sequence of dashes "-".  The number of
##' dashes used in the arrow defines the group number of the edge.
##'
##' @title Text Representations
##' @param file the name of the file to read or write
##' @param lines a string representation of the model
##' @param labels the sequence of labels to use for the nodes
##' @param edges an edge list.
##' @return The \code{write.text} function invisibly returns the text that was
##' written to the file.
##'
##' The functions \code{model.text} and \code{parse.text} return an
##' edge list - a data frame with columns
##'
##' \item{\code{From}}{a factor indicating the origin of each edge (the node that effects)}
##' \item{\code{To}}{a factor indicating the destination of each edge (the node that is effected)}
##' \item{\code{Group}}{an integer vector that indicates the group each edge belons to}
##' \item{\code{Type}}{a factor indicating the edge type - "N" (negative) ,"P" (positive),"U" (unknown) or "Z" (zero)}
##' \item{\code{Pair}}{an integer vector that indicates the pairing of directed edges}
##'
##' Each edge of the text specification is separated into two directed
##' edges, and every row of an edge list corresponds to a single
##' directed edge.
##'
##' @examples
##' edges <- parse.text(c("A <-* B","C *-> A","C <- D","D -> B","B *--* C","A <--- D"))
##' edges
##' deparse.text(edges)
##' @export
model.text <- function(file,labels=NULL) {
  parse.text(readLines(file),labels=labels)
}

##' @rdname model.text
##' @export
parse.text <- function(lines,labels=NULL) {
  ## Attempt to parse specifications
  m <- regexec("^([^\\*<>-]+)(\\*|<|<>)?(-+)(\\*|>|<>)?([^\\*<>-]+)$",lines)
  err <- sapply(m,"[[",1)== -1
  if(any(err)) {
    warning("Could not parse edges: ",paste(lines[err],collapse=", "))
    lines <- lines[!err]
    m <- m[!err]
  }
  m <- regmatches(lines,m)
  from <- gsub("^\\s+|\\s+$","",lapply(m,"[[",2))
  to <-   gsub("^\\s+|\\s+$","",lapply(m,"[[",6))
  tail <- sapply(m,"[[",3)
  line <- sapply(m,"[[",4)
  head <- sapply(m,"[[",5)

  if(any(head=="" & tail==""))
    warning("Zero edges specified: ",paste(lines[head=="" & tail==""],collapse=", "))

  ## Construct edge dataframe
  if(is.null(labels)) labels <- sort(unique(c(from,to)))
  from <- factor(from,levels=labels)
  to <- factor(to,levels=labels)

  group <- nchar(line)-1

  type <- c("N","P","U","Z")
  backward.type <- type[match(tail,c("*","<","<>",""))]
  forward.type <- type[match(head,c("*",">","<>",""))]

  edges <- rbind(data.frame(From=from,
                            To=to,
                            Group=group,
                            Type=factor(forward.type,type),
                            Pair=seq_along(lines)),
                 data.frame(From=to,
                            To=from,
                            Group=group,
                            Type=factor(backward.type,type),
                            Pair=seq_along(lines)))

  ## Drop zero weight edges
  edges <- edges[edges$Type!="Z",,drop=F]
  ## Add node labels
  attr(edges,"node.labels") <- labels
  edges
}


##' @rdname model.text
##' @export
deparse.text <- function(edges) {

  make.edge <- function(edge) {
    edge <- edge[order(match(edge$Type,c("P","N","U","Z"),4),
                       edge$From),]

    from <- edge$From[1]
    to <- edge$To[1]
    fwd <- (edge$From==from & edge$To==to)
    line <- paste(rep("-",edge$Group[1]+1),collapse="")
    symb <- c(">","*","<>","")
    head <- symb[match(edge$Type[fwd],c("P","N","U","Z"),4)]
    symb <- c("<","*","<>","")
    tail <- symb[match(edge$Type[!fwd],c("P","N","U","Z"),4)]

    paste(from," ",tail,line,head," ",to,sep="")
  }

  sapply(split(edges,interaction(edges$Pair,edges$Group,drop=T)),make.edge)
}


##' @rdname model.text
##' @export
write.text <- function(edges,file="") {
  txt <- deparse.text(edges)
  cat(txt,sep="\n",file=file)
  invisible(txt)
}



##' Write a DOT specification of the model.
##'
##' Write a DOT specification of the model in a form suitable for use
##' with \code{grViz} from \pkg{DiagrammeR}.
##'
##' @title Export to DOT
##' @param edges An edge list
##' @param name The name of the digraph
##' @param fontsize Fontsize for node labels.
##' @param node.style The node style.
##' @param node.shape The node shape.
##' @param node.color The node color.
##' @param edge.color The edge color.
##' @return Returns a string.
##' @export
deparse.grViz <- function(edges,name="web",
                          fontsize=10,node.style="filled",
                          node.shape="oval",node.color="DarkOrange",
                          edge.color="DarkGrey") {

  make.edge <- function(edge) {

    edge <- edge[order(match(edge$Type,c("P","N","U","Z"),4),
                       edge$From),]

    from <- edge$From[1]
    to <- edge$To[1]
    fwd <- (edge$From==from & edge$To==to)
    ln <- c("solid","dashed","dotted","bold")
    hd <- c("normal","dot","diamond","none")
    tl <- c("normal","dot","diamond","none")
    paste("    ",
          from[1],"->",to," [",
          "style=",ln[edge$Group[1]+1],
          ",",
          "arrowtail=",if(any(!fwd)) tl[match(edge$Type[!fwd],c("P","N","U","Z"))] else "none",
          ",",
          "arrowhead=",if(any(fwd))  hd[match(edge$Type[fwd],c("P","N","U","Z"),4)] else "none",
          "]",
          sep="")
  }

  ## Node definitions
  ntxt <- paste("    ",
                levels(edges$From),
                " [",
                "style=",rep(node.style,length.out=nlevels(edges$From)),
                ",",
                "shape=",rep(node.shape,length.out=nlevels(edges$From)),
                ",",
                "color=",rep(node.color,length.out=nlevels(edges$From)),
                "]",
                sep="",collapse="\n")

  ## Edge definitions
  etxt <- paste("    ",
                "edge [dir=both,color=",edge.color,"]\n",
                paste(sapply(split(edges,edges$Pair),make.edge),collapse="\n"),
                sep="",collapse="")



  ## Graph definition
  paste("digraph ",name," {\n",
        "    ",
        "graph [fontsize=",fontsize,"]\n",
        ntxt,
        "\n\n",
        etxt,
        "\n}\n",
        sep="")
}
