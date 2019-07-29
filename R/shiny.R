#' Display the impact of a perturbation as a barplot
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
#' @title Impact Barplot
#' @param sim the result from \code{system.simulate}
#' @param epsilon outomes below this in absolute magnitude are treated as zero.
#' @param main text for plot title
#' @param cex.axis character expansion factor for the edge labels
#' @param perturb a named vector that indicates which nodes were perturbed and the relative magnitude of the perturbation.
#' @param monitor n named vector of signs (-1,0,1) or NA that indicates the outcome of the perturbation.
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
    ui <- fluidPage(
        fluidRow(column(6, uiOutput("buttongrid")),
                 column(6, plotOutput('plot')))
    )
    server <- function(input, output) {
        pal <- c("#92C5DE", "#808080", "#F4A582")

        output$buttongrid <- renderUI({
            nodelab <- node.labels(sim$edges)
            temp <- lapply(seq_along(nodelab),
                           function(z) fluidRow(column(4, tags$span(nodelab[z])),
                                                column(4, radioButtons(inputId = paste0("perturb@", z), label = NULL, choices = c("-", "0", "+"), select = "0", inline = TRUE)),
                                                column(4, radioButtons(inputId = paste0("monitor@", z), label = NULL, choices = c("-", "0", "+"), select = "0", inline = TRUE))))
            do.call(tagList, temp) ## or verticalLayout
        })

        list2namednum <- function(xl, labs) {
            xl <- sapply(xl, function(z) if (is.null(z)) NA_character_ else z) ## list to char vector, replacing NULLs with NAs
            names(xl) <- labs
            xl <- gsub("-", "-1", gsub("+", "1", xl, fixed = TRUE), fixed = TRUE) ## from "-" to -1 and "+" to +1
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
            mon <- mon[mon != 0]

            imres <- do_impact(sim = sim, perturb = pert, monitor = mon)##, epsilon = input$epsilon)
            main <- ""
            cex.axis <- 1
            barplot(t(imres), horiz = TRUE, las = 1, border = FALSE, col = pal, xlab = "Simulations", main = main, cex.axis = cex.axis)
        })
    }

    ## return a Shiny app object
    shinyApp(ui = ui, server = server)
}

do_impact <- function(sim, perturb = 0, monitor = NA, epsilon = 1.0E-5) {
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
    results
}
