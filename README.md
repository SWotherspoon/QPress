# QPress
[![Travis-CI Build Status](https://travis-ci.org/SWotherspoon/QPress.svg?branch=master)](https://travis-ci.org/SWotherspoon/QPress)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/SWotherspoon/QPress?branch=master&svg=true)](https://ci.appveyor.com/project/SWotherspoon/QPress)

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

First install the dependencies from CRAN:

```{r}
install.packages(c("tcltk2","XML","devtools"))
```

(This assumes that R has been compiled with Tcl/Tk support, which should be true for most distributions.)

Then install QPress itself from GitHub, using the devtools package. 

```R
devtools::install_github("SWotherspoon/QPress")
```

(QPress otherwise does not need devtools for normal use.)


