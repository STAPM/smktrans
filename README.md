
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Smoking Initiation, Quit and Relapse Probabilities from Cross-sectional Survey Data <img src="logo.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

## Motivation

The motivation for `smktrans` was to develop a set of functions to
estimate the transition probabilities among current, former and never
smoking states (i.e. initiation, quitting and relapse) from repeat
cross-sectional survey data.

`smktrans` was created as part of a programme of work to estimate the
health economic effects of tobacco trends, policies and interventions at
the School of Health and Related Research (ScHARR), The University of
Sheffield.

## Usage

The **inputs** are:

1.  Several years of annual cross-sectional survey data on smoking.  
2.  Estimates of long-term smoking relapse.  
3.  Estimates of smoking related disease risk and mortality rates.

The **processes** applied by the functions in `smktrans` implement
calculations to estimate and forecast the transition probabilities to
get them into a form that can be used in a model that simulated the
population dynamics of smoking.

The **output** of these processes are the probabilities of smoking
initiation, quitting and relapse, stratified by age, period, sex and
quintiles of the Index of Multiple Deprivation. These data can be saved
so that you don’t need to run the estimation processes in `smktrans`
each time you want to run a model that uses the estimated transition
probabilities.

## Installation

`smktrans` is currently available only to members of the project team -
we are developing plans to make the code open access. To access you need
to [sign-up for a GitLab account](https://gitlab.com/). You will then
need to be added to the STAPM Gitlab project to gain access.

Once that is sorted, you can the latest or a specified version from
GitLab with:

``` r
#install.packages("devtools")
#install.packages("getPass")
#install.packages("git2r")

devtools::install_git(
  "https://gitlab.com/stapm/r-packages/smktrans.git", 
  credentials = git2r::cred_user_pass("uname", getPass::getPass()),
  ref = "x.x.x",
  build_vignettes = FALSE # vignettes are better viewed on the website
)

# uname is your Gitlab user name

# ref = "x.x.x" is the version you want to install - change to the version you want e.g. "1.2.3"
# or leave blank for the latest version

# running this code should make a box pop up where you enter your GitLab password
```

Or clone the package repo locally and use the ‘install and restart’
button in the Build tab of RStudio. This option is more convenient when
testing development versions.

Then load the package, and some other packages that are useful. Note
that the code within `smktrans` uses the `data.table::data.table()`
syntax.

``` r
# Load the package
library(smktrans)
```
