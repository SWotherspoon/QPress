# QPress

<!-- badges: start -->
  [![R-CMD-check](https://github.com/swotherspoon/QPress/workflows/R-CMD-check/badge.svg)](https://github.com/swotherspoon/QPress/actions)
<!-- badges: end -->

This package simulates qualitative press perturbation scenarios for network models
specified as signed directed graphs.

The package has four main functions.

* The package reads signed directed graphs from pre-specified formats
  and generates a community matrix.

* The package samples a community matrix to generate a set of outcomes
  which can explored interactively.

* The package provides a means to specify combinations of validation
  criteria and press perturbation scenarios, and to determine the
  proportion of simulated outcomes that meet these specifications.


## Installing

``` r
options(repos = c(SCAR = "https://scar.r-universe.dev",
                 CRAN = "https://cloud.r-project.org"))

install.packages("QPress")

```

Or install from GitHub, using the remotes package:

``` r
## install.packages("remotes") ## if you don't already have it
remotes::install_github("SWotherspoon/QPress")

```

The package documentation and vignettes can be [viewed online](https://swotherspoon.github.io/QPress/).

Alternatively, if you have pandoc on your system (which should be true if you are using RStudio) you can install the package and vignettes locally:


```{r}
remotes::install_github("SWotherspoon/QPress", build_vignettes = TRUE)

```

And the package vignettes can be viewed with:

```{r}
library(QPress)
vignette("Snowshoe")
```

and

```{r}
vignette("Mesocosm")
```

