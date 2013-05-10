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
##' dashes used in teh arrow defines the group number of the edge.
##' @title Text Representations
##' @param file the name of the file to read or write
##' @param lines a string representation of the model
##' @param labels the sequence of labels to use for the nodes
##' @param edges an edge list.
##' @return The functions \code{model.text} and \code{parse.text}
##' return an edgelist. The \code{write.text} function invisibly
##' returns the text that was written to the file.
##' @export
model.text <- function(file,labels=NULL) {
  parse.text(readLines(file),labels=labels)
}

##' @rdname model.text
##' @export
parse.text <- function(lines,labels=NULL) {
  m <- regmatches(lines,regexec("([^\\*<-]+)(\\*|<|<>)?(-+)(\\*|>|<>)?([^\\*>-]+)",lines))
  from <- gsub("^\\s+|\\s+$","",lapply(m,"[[",2))
  to <-   gsub("^\\s+|\\s+$","",lapply(m,"[[",6))
  tail <- sapply(m,"[[",3)
  line <- sapply(m,"[[",4)
  head <- sapply(m,"[[",5)


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
                            Pair=1:length(lines)),
                 data.frame(From=to,
                            To=from,
                            Group=group,
                            Type=factor(backward.type,type),
                            Pair=1:length(lines)))

  ## Drop zero weight edges
  edges <- edges[edges$Type!="Z",,drop=F]
  edges
}

##' @rdname model.text
##' @export
write.text <- function(edges,file="") {

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

  txt <- sapply(split(edges,interaction(edges$Pair,edges$Group,drop=T)),make.edge)
  cat(txt,sep="\n",file=file)
  invisible(txt)
}
