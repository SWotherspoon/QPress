##' Construct a grid of radio buttons to select from a range of options
##' that are common to many items.
##'
##' @title Grid of Radio Buttons
##' @param parent the parent window
##' @param label the label for the enclosing frame
##' @param rows the labels for the rows/items
##' @param choices the labels for the columns/choices
##' @param initial the initial selection
##' @param label.rows whether to label rows
##' @return Returns an object of class radiogrid with elements
##' \item{\code{window}}{the widget}
##' \item{\code{selected}}{function that returns the state of the radiobuttons}
##' \item{\code{state}}{the tclVars representing the state of the radiobuttons}
##' @import tcltk
##' @import tcltk2
radiogrid <- function(parent,label,rows,choices,initial=1,label.rows=T) {
  if(is.null(names(choices))) names(choices) <- as.character(choices)
  initial <- rep(initial,length=length(rows))
  state <- lapply(initial,function(k) tclVar(names(choices)[k]))
  names(state) <- rows
  tk.frame <- tk2labelframe(parent,text=label)
  for(col in seq_along(choices))
    tkgrid(tk2label(tk.frame,text=names(choices)[col]),row=0,column=col)
  for(row in seq_along(rows)) {
    tkgrid(tk2label(tk.frame,text=if(label.rows) rows[row] else ""),
                    row=row,column=0,sticky="w")
    for(col in seq_along(choices))
      tkgrid(tk2radiobutton(tk.frame,value=names(choices)[col],variable=state[[row]]),row=row,column=col)
  }
  r <- list(window=tk.frame,
            selected=function() {
              r <- choices[sapply(state,tclvalue)]
              names(r) <- rows
              r
            },
            state=state,choices=choices)
  class(r) <- "radiogrid"
  r
}


##' Construct an edge selection widget
##'
##' Makes a widget consisting of a grid of check buttons that allow
##' the user to select edges of the network.
##' @title Edge Selection Widget
##' @param parent the parent window
##' @param label the label for the enclosing frame
##' @param rows the labels for the rows (node names)
##' @param edges an nx2 matrix that defines the edges
##' @param group a numeric vector that groups edges
##' @param label.rows whether to label rows
##' @return Returns an object of class checkedges with elements
##' \item{\code{window}}{the widget}
##' \item{\code{selected}}{function that returns the state of the check buttons}
##' \item{\code{state}}{the tclVars representing the state of the check buttons}
##' @import tcltk
##' @import tcltk2
checkedges <- function(parent,label,rows,edges,group=NULL,label.rows=T) {
  group <- if(is.null(group)) seq_len(nrow(edges)) else match(group,unique(group))
  state <- lapply(unique(group),function(k) tclVar("0"))[group]
  tk.frame <- tk2labelframe(parent,text=label)
  for(k in seq_along(rows)) {
    tkgrid(tk2label(tk.frame,text=if(label.rows) rows[k] else ""),row=k,column=0,sticky="w")
    tkgrid(tk2label(tk.frame,text=k),row=k,column=1)
    tkgrid(tk2label(tk.frame,text=k),row=0,column=k+1)
  }
  for(k in seq_len(nrow(edges)))
    tkgrid(tk2checkbutton(tk.frame,text="",variable=state[[k]]),row=edges[k,2],column=edges[k,1]+1)

  r <- list(window=tk.frame,
            selected=function() sapply(state,tclvalue)=="1",
            state=state)
  class(r) <- "checkedges"
  r
}




##' Construct a checkbox widget
##'
##' The \code{checkbox} function makes a single checkbox widget, while
##' \code{checkcolumn} makes a widget containing a column of
##' checkboxes.
##' @title Checkbox widgets
##' @param parent the parent window.
##' @param label the label for the enclosing frame.
##' @param initial the initial state of the checkbox
##' @param rows the row labels
##' @param label.rows whether to label rows.
##' @return Returns an object of class checkbox or checkcolumn with elements
##' \item{\code{window}}{the widget}
##' \item{\code{selected}}{function that returns the state of the checkboxes}
##' \item{\code{state}}{the tclVars representing the state of the checkboxes}
##' @import tcltk
##' @import tcltk2
checkbox <- function(parent,label,initial=0) {
  state <- tclVar(initial)
  w.check <- tk2checkbutton(parent,text=label,variable=state)
  r <- list(window=w.check,
            selected=function() tclvalue(state)=="1",
            state=state)
  class(r) <- "checkbox"
  r
}

##' @rdname checkbox
checkcolumn <- function(parent,label,rows,label.rows=T) {
  state <- lapply(rows,function(k) tclVar("0"))
  names(state) <- rows
  tk.frame <- tk2labelframe(parent,text=label)
  for(row in seq_along(rows))
    tkgrid(tk2checkbutton(tk.frame,text=if(label.rows) rows[[row]] else "",
                          variable=state[[row]]),row=row-1,column=0,sticky="w")
  r <- list(window=tk.frame,
            selected=function() sapply(state,tclvalue)=="1",
            state=state)
  class(r) <- "checkcolumn"
  r
}




##' Construct a slider widget.
##'
##' The \code{slider} function creates a widget containing a single
##' horizontal slider.
##' @title Slider Widgets
##' @param parent the parent window
##' @param initial the initial values of the sliders
##' @param from minimum slider values
##' @param to maximum slider value
##' @param orient slider orientation
##' @return Returns an object of class slider with elements
##' \item{\code{window}}{the widget}
##' \item{\code{selected}}{function that returns the state of the sliders}
##' \item{\code{state}}{the tclVars representing the state of the sliders}
##' @import tcltk
##' @import tcltk2
slider <- function(parent,initial=1,from=0,to=100,orient="horizontal") {
  state <- tclVar(initial)
  w.slider <- tk2scale(parent,orientation=orient,from=to,to=from,variable=state)
  r <- list(window=w.slider,
            selected=function() as.numeric(tclvalue(state)),
            state=state)
  class(r) <- "slider"
  r
}








##' Construct control widget
##'
##' Constructs a toplevel window that allows the allowing the user to
##' interactively select nodes to perturb/monitor, from a subset of
##' models, and then perform a given action.
##'
##' The \code{action} argument must be function of five arguments
##' \itemize{
##' \item \code{perturb} the nodes that were perturbed
##' \item \code{monitor} the outcome of the monitoring
##' \item \code{edge} the edges to select
##' \item \code{check} the state of a checkbutton
##' \item \code{slider} the state of a slider
##' }
##'
##' @title Interactive Selection Widget
##' @param action function to perform the widgets action
##' @param nodes node labels
##' @param edges edge labels
##' @param slider slider label
##' @param checkbox checkbox label
##' @param perturb should a node perturbation control be rendered
##' @param monitor should a node monitoring control be rendered
##' @import tcltk
interactive.selection <- function(action,nodes,edges=NULL,
                                  slider=NULL,checkbox=NULL,
                                  perturb=T,monitor=T) {

  tk.top <- tktoplevel()
  tktitle(tk.top) <- "Node Selector"
  label <- T
  w.perturb <- if(perturb) radiogrid(tk.top,"Perturb",nodes,c("-"=-1,"0"=0,"+"=1),initial=2,label.rows=label && !(label <- F))
  w.monitor <- if(monitor) radiogrid(tk.top,"Monitor",nodes,c("-"=-1,"0"=0,"+"=1,"?"=NA),initial=4,label.rows=label && !(label <- F))
  w.edges <- if(!is.null(edges)) checkedges(tk.top,"Edges",nodes,edges,label.rows=label && !(label <- F))
  w.checkbox <- if(!is.null(checkbox)) checkbox(tk.top,checkbox,0)
  w.slider <- if(!is.null(slider)) slider(tk.top,slider$initial,slider$to,slider$from)

  update <- function() {
    action(perturb=if(!is.null(w.perturb)) w.perturb$selected(),
           monitor=if(!is.null(w.monitor)) w.monitor$selected(),
           edges=if(!is.null(w.edges)) w.edges$selected(),
           check=if(!is.null(w.checkbox)) w.checkbox$selected(),
           slider=if(!is.null(w.slider)) w.slider$selected())
    Sys.sleep(0.1)
    tkfocus(tk.top)
  }

  close <- function() {
    tkdestroy(tk.top)
  }

  col <- -1
  if(!is.null(w.perturb)) tkgrid(w.perturb$window,padx=2,pady=2,row=0,column=(col <- col+1),sticky="n")
  if(!is.null(w.monitor)) tkgrid(w.monitor$window,padx=2,pady=2,row=0,column=(col <- col+1),sticky="n")
  if(!is.null(w.edges)) tkgrid(w.edges$window,padx=2,pady=2,row=0,column=(col <- col+1),sticky="n")
  tk.frame <- tkframe(tk.top)

  tkgrid(tk2button(tk.frame,text="Update",command=update),
         tk2button(tk.frame,text="Close",command=close))
  tkgrid(tk.frame,
         if(!is.null(w.checkbox)) w.checkbox$window,
         if(!is.null(w.slider)) w.slider$window)

  tkfocus(tk.top)
}



##' Display the impact of a perturbation as a barplot
##'
##' This control constructs a barplot that shows the fraction of
##' simulations in which a positive (orange), negative (blue) or zero
##' (off white) outcome occurs at each node following a given
##' perturbation.
##'
##' The user may specify the perturbation of the nodes, and any
##' outcome known from monitoring the network, and then construct a
##' barplot of the frequency table of outcomes at each node.
##'
##' @title Impact Barplot
##' @param sim the result from \code{system.simulate}
##' @param epsilon outomes below this in absolute magnitude are treated as zero.
##' @param main text for plot title
##' @param cex.axis character expansion factor for the edge labels
##' @export
impact.barplot <- function(sim,epsilon=1.0E-5,main="",cex.axis=1) {
  edges <- sim$edges
  As <- sim$A
  nodes <- node.labels(edges)
  action <- function(perturb,monitor,edges,check,slider) {
    impact.barplot.action(nodes,As,perturb,monitor,epsilon=epsilon,
                          main=main,cex.axis=cex.axis)
  }

  interactive.selection(action,nodes,perturb=T,monitor=T)
}


impact.barplot.action <- function(nodes,As,perturb,monitor,
                                  epsilon=1.0E-5,main="",cex.axis=1) {
  pal <- c("#92C5DE", "#808080", "#F4A582")
  results <- matrix(0,length(nodes),3)

  for(i in 1:length(As)) {
    impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
    if(all(monitor==impact,na.rm=T)) {
      results <- results + outer(impact,-1:1,'==')
    }
  }
  rownames(results) <- nodes
  lwidth <- max(strwidth(nodes,units="inches",cex=cex.axis))
  opar <- par(mai=c(1,lwidth+0.2,0.4,0.4)+0.2)
  barplot(t(results),horiz=T,las=1,border=F,col=pal,
          xlab="Simulations",main=main,cex.axis=cex.axis)
  par(opar)
}




##' Display weights of valid and invalid matrices as a density plots
##'
##' This control constructs density plots that show the distribution
##' of selected edge weights for the cases that meet the selected
##' validation criteria (blue), and those that do not (red), following
##' a given perturbation.
##'
##' The slider controls the level of smoothing of the densities.
##' Edges are labelled by pairs of integers for compactness, where the
##' integer codes correspond to the ordering of the node labels.
##'
##' @title Weight Density Plots
##' @param sim the result from \code{system.simulate}
##' @param epsilon outomes below this in absolute magnitude are treated as zero.
##' @param main text for plot title
##' @export
weight.density <- function(sim,epsilon=1.0E-5,main="") {
  edges <- sim$edges
  As <- sim$A
  ws <- sim$w

  nodes <- node.labels(edges)
  colnames(ws) <- paste(unclass(edges$To),unclass(edges$From),sep=":")

  action <- function(perturb,monitor,edges,check,slider) {
    weight.density.action(As,ws,perturb,monitor,edges,slider,epsilon=epsilon,main=main)
  }
  interactive.selection(action,nodes,cbind(edges$From,edges$To),
                        slider=list(initial=1,from=0,to=2),perturb=T,monitor=T)
}


weight.density.action <- function(As,ws,perturb,monitor,edges,slider,epsilon=1.0E-5,main="") {
  pal <- c("#0571B0", "#CA0020")
  if(any(edges)) {
    keep <- rep(F,nrow(ws))
    for(i in 1:length(As)) {
      impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
      if(all(monitor==impact,na.rm=T)) keep[i] <- T
    }
    n <- ceiling(sqrt(sum(edges)))
    m <- ceiling(sum(edges)/n)

    opar <- par(mfrow=c(m,n),mar=c(5,4,1,1)+0.1)
    for(k in which(edges)) {
      d1 <- if(sum(keep) > 10) density(ws[keep,k],adjust=slider) else list(x=c(),y=c())
      d2 <- if(sum(!keep) > 10) density(ws[!keep,k],adjust=slider) else list(x=c(),y=c())
      plot(NULL,xlab=colnames(ws)[k],main="",
           xlim=range(d1$x,d2$x),
           ylim=range(d1$y,d2$y))
      lines(d1,col=pal[1])
      lines(d2,col=pal[2])
      title(main=main,outer=T)
    }
    par(opar)
  }
}


