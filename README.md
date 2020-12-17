
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Smoking Initiation, Quit and Relapse Probabilities from Cross-sectional Survey Data <img src="logo.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## Motivation

The motivation for `smktrans` was to develop a set of functions to
estimate the transition probabilities among current, former and never
smoking states (i.e. initiation, quitting and relapse) from repeat
cross-sectional survey data.

`smktrans` was created as part of a programme of work on the health
economics of tobacco and alcohol at the School of Health and Related
Research (ScHARR), The University of Sheffield. This programme is based
around the construction of the Sheffield Tobacco and Alcohol Policy
Model (STAPM), which aims to use comparable methodologies to evaluate
the impacts of tobacco and alcohol policies, and investigate the
consequences of clustering and interactions between tobacco and alcohol
consumption behaviours.

## Usage

The **inputs** are:

1.  Several years of annual cross-sectional survey data on smoking.  
2.  Estimates of long-term smoking relapse.  
3.  Estimates of smoking related disease risk and mortality rates.

The **processes** applied by the functions in `smktrans` implement
calculations in the accompanying research paper. They estimate and
forecast the transition probabilities to get them into a form that can
be used in a model that simulated the population dynamics of smoking.

The **output** of these processes are the probabilities of smoking
initiation, quitting and relapse, stratified by age, period, sex and
quintiles of the Index of Multiple Deprivation. These data can be saved
so that you don’t need to run the estimation processes in `smktrans`
each time you want to run a model that uses the estimated transition
probabilities.

## Installation

`smktrans` is currently available only to members of the project team
(but please contact Duncan Gillespie <duncan.gillespie@sheffield.ac.uk>
to discuss). To access you need to [sign-up for a GitLab
account](https://gitlab.com/). You will then need to be added to the
STAPM project team to gain access.

Once that is sorted, you can the latest or a specified version from
GitLab with:

``` r
#install.packages("devtools")
#install.packages("getPass")

devtools::install_git(
  "https://gitlab.com/stapm/r-packages/smktrans.git", 
  credentials = git2r::cred_user_pass("uname", getPass::getPass()),
  ref = "x.x.x",
  build_vignettes = TRUE
)

# Where uname is your Gitlab user name.
# ref is the version you want to install - remove for the latest version
# this should make a box pop up where you enter your GitLab password
```

Then load the package, and some other packages that are useful. Note
that the code within `smktrans` uses the `data.table::data.table()`
syntax.

``` r
# Load the package
library(smktrans)

# Other useful packages
library(ggplot2) # for plotting
```

## Citation

Please cite the latest version of the package using:  
“Duncan Gillespie, Laura Webster and Alan Brennan (2020). smktrans:
Smoking Initiation and Quit Probabilities from Cross-sectional Survey
Data. R package version x.x.x.
<https://stapm.gitlab.io/r-packages/smktrans/>.”
