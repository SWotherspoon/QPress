# QPress

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

The package is easily built with RStudio

1. Install R

2. Install [RStudio](http://www.rstudio.com)

3. Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/) or equivalent for your platform

4. Install [devtools](http://cran.r-project.org/bin/windows/Rtools/) and [roxygen2](http://cran.r-project.org/web/packages/roxygen2/index.html) packages and dependencies in R

5. Install the [XML](http://cran.r-project.org/web/packages/XML/index.html) packages and dependencies.

6. Clone the repository

7. Create an Rstudio project in the folder containing this Readme file.

8. In the build tab, choose `More/Configure Build Tools...` and click
`Generate documentation with Roxygen`, select `Configure` and choose to generate `Rd files` and the `NAMESPACE file`, leaving the other options as they are.

9. Choose `More/Roxygenize` from the `Build` tab

10. Choose `Build & Reload` to make the package immediately available to R, or choose `More/Build source package` `More/Build binary package` from the `Build` tab to make packages.

