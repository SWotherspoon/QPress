#' Qualitative press perturbations for signed digraphs
#'
#' This package provides facilities for simulating press
#' perturbation scenarios for qualitative network models
#' specified as signed directed graphs (signed digraphs).
#' @name QPress-package
#' @docType package
#' @author B. Raymond, J. Melbourne-Thomas and S. Wotherspoon
NULL


##' Read and write Dia representations of models
##'
##' These functions read and write Dia representions of model
##' toplogies.
##'
##' These functions should be used with care as no attempt is made to
##' test for model mis-specification.  The \code{model.dia} function
##' only recognizes node shapes "Flowchart - Ellipse", "Flowchart - Box"
##' and "Flowchart - Terminal", line types "Standard - Arc",
##' "Standard - ZigZagLine" and "Standard - Line", and arrow types 8,
##' 1 and 5.  Other node shapes, line or arrow types will be silently
##' ignored leading to a mispecified model.
##' @title Dia Representations
##' @param file name of the file to read or write
##' @param labels the sequence of labels to use for the nodes
##' @param edges an edge list
##' @param width width of the nodes in Dia
##' @param height height of the nodes in Dia
##' @param self should self edges be written.
##' @return The \code{model.dia} function returns an edge list.
##' @seealso \code{\link{model.text}}
##' @export
##' @import XML
model.dia <- function(file,labels=NULL) {
  parse.dia(read.dia(file),labels=labels)
}


## Extract basic graph strcuture from a dia xml file.  Assumes the xml
## is uncompressed.
read.dia <- function(file) {

  ## Read the xml and determine namespace
  dia <- xmlParse(file)
  ns <- xmlNamespaceDefinitions(dia,simplify=TRUE)

  fix.null <- function(x)
    if(is.null(x) || length(x)==0) NA else x

  ## Extract the name of a node in the graph
  node.label <- function(node) {
    label <- xpathSApply(node,"./dia:attribute[@name='text']//dia:string/text()",
                         xmlValue,namespaces=ns)
    label <- paste(gsub("^#(.*)#$","\\1",gsub(",|\n"," ",label)),collapse=" ")
    if(label!="") label else paste("Dia",xmlGetAttr(node,"id"),sep=":")
  }

  ## Extract location on the dia page
  node.position <- function(node)
    xpathSApply(node,"./dia:attribute[@name='obj_pos']/dia:point[@val]",
                xmlGetAttr,name="val",namespaces=ns)

  ## Extract color of node
  node.colour <- function(node)
      fix.null(xpathSApply(node,"./dia:attribute[@name='inner_color']/dia:color[@val]",
                           xmlGetAttr,name="val",namespaces=ns))

  ## Extract an attribute of an edge
  edge.attr <- function(edge,attr)
    fix.null(xpathSApply(edge,paste("./dia:attribute[@name='",attr,"']/dia:enum[@val]",sep=""),
                         xmlGetAttr,name="val",namespaces=ns))

  ## Extract an endpoint of an edge
  edge.connection <- function(edge,handle)
    fix.null(xpathSApply(edge,paste("./dia:connections/dia:connection[@handle='",handle,"']",sep=""),
                         xmlGetAttr,name="to",namespaces=ns))

  ## Extract node definitions
  nodes <- xpathApply(dia,"//dia:object[@type='Flowchart - Ellipse'] | //dia:object[@type='Flowchart - Box'] | //dia:object[@type='Flowchart - Terminal']",
                      function(node) list(id=xmlGetAttr(node,"id"),
                                          label=node.label(node),
                                          colour=node.colour(node),
                                          pos=node.position(node)),
                      namespaces=ns)

  ## Extract edge definitions
  edges <- xpathApply(dia,
	"//dia:object[@type='Standard - Arc'] | //dia:object[@type='Standard - ZigZagLine'] | //dia:object[@type='Standard - Line']",
                      function(edge) list(from=edge.connection(edge,"0"),
                                          to=edge.connection(edge,"1"),
                                          line.style=edge.attr(edge,"line_style"),
                                          start.arrow=edge.attr(edge,"start_arrow"),
                                          end.arrow=edge.attr(edge,"end_arrow")))
  if(any(is.na(sapply(edges,"[[","from"))) ||
     any(is.na(sapply(edges,"[[","to"))))
    warning("Dia file contains unconnected edges")

  list(nodes=nodes,edges=edges)
}


## Translate the dia representation to the weighted edge list we
## require for the simulation.
parse.dia <- function(dia,labels=NULL) {

  ## Node id and labels
  nodes.id <- sapply(dia$nodes,"[[","id")
  nodes.label <- sapply(dia$nodes,"[[","label")
  nodes.colour <- toupper(substr(sapply(dia$nodes,"[[","colour"),2,7))

  ## Get endpoints
  from <- nodes.label[match(sapply(dia$edges,"[[",1),nodes.id)]
  to <- nodes.label[match(sapply(dia$edges,"[[",2),nodes.id)]
  if(is.null(labels)) labels <- sort(unique(c(from,to)))
  from <- factor(from,levels=labels)
  to <- factor(to,levels=labels)

  ## Colour determines node grouping
  colour <- factor(nodes.colour[match(labels,nodes.label)])

  ## Line style determines the edge grouping
  line <- sapply(dia$edges,"[[",3)
  group <- ifelse(is.na(line),0,as.numeric(line))

  ## Edge types are determined by arrow styles
  code <- c("8","1","5",NA)
  start <- match(sapply(dia$edges,"[[",4),code)
  end <- match(sapply(dia$edges,"[[",5),code)
  if(any(is.na(start)|is.na(end)))
    stop("Dia file contains unknown arrow type")

  type <- c("N","P","U","Z")
  backward.type <- type[start]
  forward.type <- type[end]




  ## Construct (directed) edge list
  edges <- rbind(data.frame(From=from,
                            To=to,
                            Group=group,
                            Type=factor(forward.type,type),
                            Pair=1:length(dia$edges)),
                 data.frame(From=to,
                            To=from,
                            Group=group,
                            Type=factor(backward.type,type),
                            Pair=1:length(dia$edges)))

  ## Drop zero weight edges
  edges <- edges[edges$Type!="Z",,drop=F]
  ## Add node labels
  attr(edges,"node.labels") <- labels
  ## Add node colours
  attr(edges,"node.colours") <- colour
  ## Return edge list
  edges
}



##' @rdname model.dia
##' @export
write.dia <- function(edges,file,width=8,height=2,self=T) {

  writeHeader <- function() {
    ## Write Header
    cat('<?xml version="1.0"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:diagramdata>
    <dia:attribute name="background">
      <dia:color val="#ffffff"/>
    </dia:attribute>
    <dia:attribute name="pagebreak">
      <dia:color val="#000099"/>
    </dia:attribute>
    <dia:attribute name="paper">
      <dia:composite type="paper">
        <dia:attribute name="name">
          <dia:string>#A4#</dia:string>
        </dia:attribute>
        <dia:attribute name="tmargin">
          <dia:real val="2.8222"/>
        </dia:attribute>
        <dia:attribute name="bmargin">
          <dia:real val="2.8222"/>
        </dia:attribute>
        <dia:attribute name="lmargin">
          <dia:real val="2.8222"/>
        </dia:attribute>
        <dia:attribute name="rmargin">
          <dia:real val="2.8222"/>
        </dia:attribute>
        <dia:attribute name="is_portrait">
          <dia:boolean val="true"/>
        </dia:attribute>
        <dia:attribute name="scaling">
          <dia:real val="1"/>
        </dia:attribute>
        <dia:attribute name="fitto">
          <dia:boolean val="false"/>
        </dia:attribute>
      </dia:composite>
    </dia:attribute>
    <dia:attribute name="grid">
      <dia:composite type="grid">
        <dia:attribute name="width_x">
          <dia:real val="1"/>
        </dia:attribute>
        <dia:attribute name="width_y">
          <dia:real val="1"/>
        </dia:attribute>
        <dia:attribute name="visible_x">
          <dia:int val="1"/>
        </dia:attribute>
        <dia:attribute name="visible_y">
          <dia:int val="1"/>
        </dia:attribute>
        <dia:composite type="color"/>
      </dia:composite>
    </dia:attribute>
    <dia:attribute name="color">
      <dia:color val="#d8e5e5"/>
    </dia:attribute>
    <dia:attribute name="guides">
      <dia:composite type="guides">
        <dia:attribute name="hguides"/>
        <dia:attribute name="vguides"/>
      </dia:composite>
    </dia:attribute>
  </dia:diagramdata>',file=file,sep="",append=F)
  }


  writeNode <- function(k,xy,label) {
    cat('
    <dia:object type="Flowchart - Terminal" version="1" id="O',k,'">
      <dia:attribute name="meta">
        <dia:composite type="dict"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="',xy[1],',',xy[2],'"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="',width,'"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="',height,'"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.1"/>
      </dia:attribute>
      <dia:attribute name="line_colour">
        <dia:color val="#000000"/>
      </dia:attribute>
      <dia:attribute name="fill_colour">
        <dia:color val="#ffffff"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="true"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="0"/>
        <dia:real val="1"/>
      </dia:attribute>
      <dia:attribute name="padding">
        <dia:real val="0.1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#',label,'#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="sans" style="0" name="Helvetica"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>',file=file,sep="",append=T)
  }

  writeArc <- function(k,xy1,xy2,from,to,line,start,end) {
    cat('
    <dia:object type="Standard - Arc" version="0" id="O',k,'">
      <dia:attribute name="conn_endpoints">
        <dia:point val="',xy1[1],',',xy1[2],'"/>
        <dia:point val="',xy2[1],',',xy2[2],'"/>
      </dia:attribute>
      <dia:attribute name="curve_distance">
        <dia:real val="',if(from==to) "3" else "1.0e-5",'"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="',line,'"/>
      </dia:attribute>',file=file,sep="",append=T)

    if(length(start)>0 && !is.na(start)) {
      cat('
      <dia:attribute name="start_arrow">
        <dia:enum val="',start,'"/>
      </dia:attribute>
      <dia:attribute name="start_arrow_length">
        <dia:real val="0.5"/>
      </dia:attribute>
      <dia:attribute name="start_arrow_width">
        <dia:real val="0.5"/>
      </dia:attribute>',file=file,sep="",append=T)
    }

    if(length(end)>0 && !is.na(end)) {
      cat('
      <dia:attribute name="end_arrow">
        <dia:enum val="',end,'"/>
      </dia:attribute>
      <dia:attribute name="end_arrow_length">
        <dia:real val="0.5"/>
      </dia:attribute>
      <dia:attribute name="end_arrow_width">
        <dia:real val="0.5"/>
      </dia:attribute>',file=file,sep="",append=T)
    }

    cat('
      <dia:connections>
        <dia:connection handle="0" to="O',from,'" connection="',if(from==to) "5" else "16",'"/>
        <dia:connection handle="1" to="O',to,'" connection="',if(from==to) "7" else "16",'"/>
      </dia:connections>
    </dia:object>',file=file,sep="",append=T)

  }

  writeHeader()
  ## Background layer
  cat('
  <dia:layer name="Background" visible="true" active="true">',file=file,sep="",append=T)

  ## Add Nodes
  labels <- node.labels(edges)
  n <- length(labels)
  xy <- 20+15*cbind(cos(2*pi*(1:n)/n),sin(2*pi*(1:n)/n))
  for(k in 1:n)
    writeNode(k,xy[k,],labels[k])

  if(!self) edges <- edges[edges$From!=edges$To,]
  prs <- split(edges,interaction(edges$Pair,edges$Group,drop=T))
  for(k in 1:length(prs)) {
    edge <- prs[[k]]
    edge <- edge[order(match(edge$Type,c("P","N","U","Z"),4),edge$From),]
    from <- edge$From[1]
    to <- edge$To[1]
    fwd <- (edge$From==from & edge$To==to)
    line <- edge$Group[1]
    code <- c("8","1","5",NA)
    end <- code[match(edge$Type[fwd],c("N","P","U","Z"),4)]
    start <- code[match(edge$Type[!fwd],c("N","P","U","Z"),4)]
    from <- as.numeric(from)
    to <- as.numeric(to)
    if(from==to)
      writeArc(k+n,xy[from,],xy[to,]+c(0,height),from,to,line,start,end)
    else
      writeArc(k+n,xy[from,]+c(width,height)/2,xy[to,]+c(width,height)/2,from,to,line,start,end)

  }
  ## Write footer
  cat('
  </dia:layer>
</dia:diagram>',file=file,sep="",append=T)

}

