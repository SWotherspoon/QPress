#' A Shiny app to display the impact of a perturbation as a barplot
#'
#' This control constructs a barplot that shows the fraction of
#' simulations in which a positive (orange), negative (blue) or zero
#' (off white) outcome occurs at each node following a given
#' perturbation.
#'
#' The user may specify the perturbation of the nodes, and any
#' outcome known from monitoring the network, and then construct a
#' barplot of the frequency table of outcomes at each node.
#'
#' @title Shiny Impact Barplot
#' @param sim the result from \code{system.simulate}
#' @param epsilon outomes below this in absolute magnitude are treated as zero.
#' @param main text for plot title
#' @param cex.axis character expansion factor for the edge labels
#' @examples
#' \dontrun{
#'   set.seed(32)
#'   ## Sample model
#'   edges <- parse.digraph(c(
#'     "E *-> D",
#'     "D *-> C",
#'     "C -> E",
#'     "E *-> B",
#'     "B *-> A",
#'     "A -> E",
#'     "D --> B"))
#'   edges <- enforce.limitation(edges)
#'   sims <- system.simulate(10, edges)
#'   impact.barplot.shiny(sims)
#' }
#'
#' @export
impact.barplot.shiny <- function(sim, epsilon = 1.0E-5, main = "", cex.axis = 1) {
    ui <- fluidPage(theme = shinytheme("spacelab"),
                    tags$style(".radio-inline input[type=\"radio\"] { display:none; }"), ## hide radio buttons
                    tags$style(".radio-inline { display:inline; padding: 0px; margin-left: 0px;} .radio-inline+.radio-inline { margin-left: 0px;}"), ## unselected appearance
                    tags$style(".radio-inline span { display:inline-block; background-color:#ddd; padding: 5px 10px; border: 2px solid #444; border-radius: 4px; }"), ## unselected appearance of label
                    tags$style("input[type=\"radio\"]:checked ~ span { background-color:#bfb; border-color: #4c4; }"), ## appearance when checked
                    fluidRow(column(8, offset = 2, uiOutput("buttongrid"))),
                    plotOutput('plot')
        )
    server <- function(input, output) {
        pal <- c("#92C5DE", "#808080", "#F4A582")
        outcome_levels <- c("-", "0", "+")
        output$buttongrid <- renderUI({
            nodelab <- node.labels(sim$edges)
            temp <- lapply(seq_along(nodelab),
                           function(z) fluidRow(column(4, tags$span(nodelab[z])),
                                                column(4, radioButtons(inputId = paste0("perturb@", z), label = NULL, choices = c("-", "0", "+"), selected = "0", inline = TRUE)),
                                                column(4, radioButtons(inputId = paste0("monitor@", z), label = NULL, choices = c("-", "0", "+", "?"), selected = "?", inline = TRUE))))
            do.call(tagList, c(list(fluidRow(column(4, tags$strong("Node")), column(4, tags$strong("Perturb")), column(4, tags$strong("Monitor")))), temp))
        })

        list2namednum <- function(xl, labs) {
            xl <- sapply(xl, function(z) if (is.null(z)) NA_character_ else z) ## list to char vector, replacing NULLs with NAs
            names(xl) <- labs
            xl <- gsub("?", "99", gsub("-", "-1", gsub("+", "1", xl, fixed = TRUE), fixed = TRUE), fixed = TRUE) ## from "-" to -1, "+" to +1, and "?" to "99"
            mode(xl) <- "numeric"
            xl
        }


        output$plot <- renderPlot({
            nodelab <- node.labels(sim$edges)
            pert <- lapply(seq_along(nodelab), function(z) input[[paste0("perturb@", z)]])
            pert <- list2namednum(pert, labs = nodelab)
            if (all(is.na(pert) | pert == 0)) return(NULL)
            pert <- pert[pert != 0]

            mon <- lapply(seq_along(nodelab), function(z) input[[paste0("monitor@", z)]])
            mon <- list2namednum(mon, labs = nodelab)
            mon <- mon[mon != 99]

            if (FALSE) {
                ## base graphics barplot
                imres <- do_impact(sim = sim, perturb = pert, monitor = mon, as_df = FALSE)##, epsilon = input$epsilon)
                barplot(t(imres), horiz = TRUE, las = 1, border = FALSE, col = pal, xlab = "Simulations", main = main, cex.axis = cex.axis)
            } else {
                ## use ggplot
                imres <- do_impact(sim = sim, perturb = pert, monitor = mon)##, epsilon = input$epsilon)
                ## convert to long format
                imres <- do.call(rbind, lapply(seq_len(ncol(imres)), function(z) data.frame(node = rownames(imres), outcome = colnames(imres)[z], n = imres[, z], stringsAsFactors = FALSE)))
                imres$outcome <- factor(imres$outcome, levels = outcome_levels)
                ggplot(imres, aes_string(x = "node", y = "n", group = "node", fill = "outcome")) + geom_col() +
                    coord_flip() +
                    scale_fill_manual(values = setNames(pal, outcome_levels), name = "Outcome") +
                    theme_bw() + labs(y = "Simulations", x = "")
            }
        })
    }

    ## return a Shiny app object
    shinyApp(ui = ui, server = server)
}

do_impact <- function(sim, perturb = 0, monitor = NA, epsilon = 1.0E-5, as_df = TRUE) {
    As <- sim$A
    nodes <- node.labels(sim$edges)
    results <- matrix(0, length(nodes), 3)

    perturb <- extend.vector(perturb, nodes, 0)
    monitor <- extend.vector(monitor, nodes, NA)

    for(i in seq_along(As)) {
        impact <- signum(drop(As[[i]]%*%perturb), epsilon = epsilon)
        if(all(monitor == impact, na.rm = TRUE)) {
            results <- results + outer(impact, -1:1, '==')
        }
    }
    rownames(results) <- nodes
    if (as_df) {
        results <- as.data.frame(results)
        colnames(results) <- c("-", "0", "+")
    }
    results
}

#' Shiny app to display weights of valid and invalid matrices as a density plots
#'
#' This control constructs density plots that show the distribution
#' of selected edge weights for the cases that meet the selected
#' validation criteria (blue), and those that do not (red), following
#' a given perturbation.
#'
#' The slider controls the level of smoothing of the densities.
#' Edges are labelled by pairs of integers for compactness, where the
#' integer codes correspond to the ordering of the node labels.
#'
#' \code{weight.density0} is a non-interactive variant for
#' programmatic use.
#'
#' @title Shiny Weight Density Plots
#' @param sim the result from \code{system.simulate}
#' @param epsilon outomes below this in absolute magnitude are treated as zero.
#' @param main text for plot title
#' @param perturb a named vector that indicates which nodes were
#' perturbed and the relative magnitude of the perturbation.
#' @param monitor n named vector of signs (-1,0,1) or NA that indicates the outcome of the perturbation.
#' @param edges logical vector indicating which edges to plot.
#' @param smooth double in the range [0,1] controlling the level of smoothing applied.
#' @export
weight.density.shiny <- function(sim, epsilon = 1.0E-5, main = "") {
  edges <- sim$edges

  ui <- fluidPage(theme = shinytheme("spacelab"),
                  tags$style(".radio-inline input[type=\"radio\"] { display:none; }"), ## hide radio buttons
                  tags$style(".radio-inline { display:inline; padding: 0px; margin-left: 0px;} .radio-inline+.radio-inline { margin-left: 0px;}"), ## unselected appearance
                  tags$style(".radio-inline span { display:inline-block; background-color:#ddd; padding: 5px 10px; border: 2px solid #444; border-radius: 4px; }"), ## unselected appearance of label
                  tags$style("input[type=\"radio\"]:checked ~ span { background-color:#bfb; border-color: #4c4; }"), ## appearance when checked
                  tags$style(".checkbox-inline span {display:none; }"),
                  fluidRow(column(8, uiOutput("buttongrid")), column(4, uiOutput("edgegrid"))),
                  plotOutput('plot')
                  )
  server <- function(input, output) {
      pal <- c("#92C5DE", "#808080", "#F4A582")
      outcome_levels <- c("-", "0", "+")
      output$edgegrid <- renderUI({
          nn <- length(node.labels(edges))
          A <- adjacency.matrix(edges)
          do.call(tags$table,
                  c(list(do.call(tags$tr, c(list(tags$th("")), lapply(seq_len(nn), function(ci) tags$th(ci))))), ## header row
                    lapply(seq_len(nn), function(ri) {
                        do.call(tags$tr, c(list(tags$th(ri)), lapply(seq_len(nn), function(ci) {
                            if (abs(A[ri, ci]) > 0) tags$td(checkboxInput(inputId = paste0("edge@", ri, "@", ci), label = NULL, width = "25px")) else tags$td()
                        })))
                    })
                    )
                  )
      })
      output$buttongrid <- renderUI({
          nodelab <- node.labels(sim$edges)
          temp <- lapply(seq_along(nodelab),
                         function(z) fluidRow(column(4, tags$span(nodelab[z])),
                                              column(4, radioButtons(inputId = paste0("perturb@", z), label = NULL, choices = c("-", "0", "+"), selected = "0", inline = TRUE)),
                                              column(4, radioButtons(inputId = paste0("monitor@", z), label = NULL, choices = c("-", "0", "+", "?"), selected = "?", inline = TRUE))))
          do.call(tagList, c(list(fluidRow(column(4, tags$strong("Node")), column(4, tags$strong("Perturb")), column(4, tags$strong("Monitor")))), temp))
      })

      list2namednum <- function(xl, labs) {
          xl <- sapply(xl, function(z) if (is.null(z)) NA_character_ else z) ## list to char vector, replacing NULLs with NAs
          names(xl) <- labs
          xl <- gsub("?", "99", gsub("-", "-1", gsub("+", "1", xl, fixed = TRUE), fixed = TRUE), fixed = TRUE) ## from "-" to -1, "+" to +1, and "?" to "99"
          mode(xl) <- "numeric"
          xl
      }

      output$plot <- renderPlot({
          nodelab <- node.labels(sim$edges)
          nn <- length(nodelab)
          pert <- lapply(seq_along(nodelab), function(z) input[[paste0("perturb@", z)]])
          pert <- list2namednum(pert, labs = nodelab)
          if (all(is.na(pert) | pert == 0)) return(NULL)
          pert <- pert[pert != 0]

          mon <- lapply(seq_along(nodelab), function(z) input[[paste0("monitor@", z)]])
          mon <- list2namednum(mon, labs = nodelab)
          mon <- mon[mon != 99]

          edg <- rep(FALSE, nrow(sim$edges))
          for (ri in seq_len(nn)) {
              for (ci in seq_len(nn)) {
                  if (!is.null(input[[paste0("edge@", ri, "@", ci)]]) && input[[paste0("edge@", ri, "@", ci)]]) {
                      edg[sim$edges$From == nodelab[ri] & sim$edges$To == nodelab[ci]] <- TRUE
                  }
              }
          }
          cat("edg:\n"); cat(str(edg))
          pal <- c("#0571B0", "#CA0020")
          imres <- do_weight_density(sim = sim, perturb = pert, monitor = mon, edges = edg, smooth = 1)
          if (!is.null(imres)) {
              nedg <- length(imres)
              n <- ceiling(sqrt(sum(nedg)))
              m <- ceiling(sum(nedg)/n)
              opar <- par(mfrow = c(m, n), mar = c(5, 4, 1, 1)+0.1)
              for (k in seq_along(imres)) {
                  d1 <- imres[[k]]$d1
                  d2 <- imres[[k]]$d2
                  plot(NULL, xlab = imres[[k]]$edgelab, ylab = "",
                       main = "", xlim = range(d1$x, d2$x), ylim =range(d1$y, d2$y))
                  lines(d1, col = pal[1])
                  lines(d2, col = pal[2])
                  title(main = main, outer = TRUE)
              }
              par(opar)
          } else {
              NULL
          }
      })
  }

  ## return a Shiny app object
  shinyApp(ui = ui, server = server)
}

do_weight_density <- function(sim, perturb, monitor, edges, smooth = 1, epsilon = 1.0E-5) {
    As <- sim$A
    ws <- sim$w
    nodes <- node.labels(sim$edges)

    perturb <- extend.vector(perturb,nodes,0)
    monitor <- extend.vector(monitor,nodes,NA)

    if (any(edges)) {
        keep <- rep(FALSE, nrow(ws))
        for (i in seq_along(As)) {
            impact <- signum(drop(As[[i]] %*% perturb), epsilon = epsilon)
            if(all(monitor == impact, na.rm = TRUE)) keep[i] <- TRUE
        }
        n <- ceiling(sqrt(sum(edges)))
        m <- ceiling(sum(edges)/n)

        lapply(which(edges), function(k) {
            list(edgelab = colnames(ws)[k],
                 d1 = if(sum(keep) > 10) density(ws[keep, k], adjust = smooth) else list(x = c(), y = c()),
                 d2 = if(sum(!keep) > 10) density(ws[!keep, k], adjust = smooth) else list(x = c(), y = c()))
        })
    } else {
        NULL
    }
}
