# Ulysses

<!-- badges: start -->


<!-- badges: end -->

Ulysses is part of [HADES](https://ohdsi.github.io/Hades/)


# Introduction

Ulysses is an R package that automates setup of an OHDSI study and provides functions to assist with its maintenance and organization.

# System Requirements

Requires R (version 4.1 or higher)

# Installation

1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including RTools and Java.

2. In R, use the following commands to download and install Ulysses:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/Ulysses")
  ```

# Introduction

The goal of Capr, pronounced 'kay-pr' like the edible flower, is to provide a language for expressing OHDSI Cohort definitions in R code. OHDSI defines a cohort as "a set of persons who satisfy one or more inclusion criteria for a duration of time" and provides a standardized approach for defining them (Circe-be). Capr exposes the standardized approach to cohort building through a programmatic interface in R which is particularly helpful when creating a large number of similar cohorts. Capr version 2 introduces a new user interface designed for readability with the goal that Capr code being a human readable description of a cohort while also being executable on an OMOP Common Data Model.

Learn more about the OHDSI approach to cohort building in the [cohorts chapter of the Book of OHDSI.](https://ohdsi.github.io/TheBookOfOhdsi/Cohorts.html)

# Installation

Capr can be installed via:

``` r
# install.packages("Capr")
```

Users can install the current development version of Capr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ohdsi/Capr")
```

# User Documentation

Documentation can be found on the package website (ADD).


# Support

-   Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
-   We use the <a href="https://github.com/OHDSI/Capr/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

# Contributing

Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

# License

Ulysses is licensed under Apache License 2.0

# Development

Ulysses is being developed in R Studio.

