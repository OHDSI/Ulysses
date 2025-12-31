# Ulysses <img src="man/figures/ulyssesLogo.png" width=150 alt="logo" align="right"/>

<!-- badges: start -->

<!-- badges: end -->


# Introduction

Ulysses is an OHDSI R package serving as a workflow tool for the setup and organization of an RWE study. Ulysses draws inspiration from the R package [`usethis`](https://usethis.r-lib.org/), which is a workflow tool used for the development of R packages, providing functions to automate the initialization of an R package and helpers for its maintenance. Similarly, RWE studies have a lot of documentation, code and results that need to be maintained and versioned. Organization of these components can be complicated without a strict organizational standard. The Ulysses study workflow tool is not specific to OMOP/OHDSI and can be used for other setups; it does provide automation focused on OHDSI/OMOP application. 

Part of the Ulysses philosophy is contextualizing an RWE study/pipeline as a unit of software. As observational studies have become more and more popular given the access to longitudinal observational databases (such as claims and EHR) there is a lack of consistent standards on how code is maintained and executed. RWE studies have become more like software projects requiring a encapsulated root project with standard folders to source elements of the RWE pipeline such as inputs, functions, output and documentation to support these elements. A standard directory structure makes things more organized allowing for collaborative development and consistent execution based on a known structure. 

The basic functions of Ulysses include:

- Implementation of standard directory structure - see [Ulysses Stucture](https://raw.githubusercontent.com/OHDSI/Ulysses/main/extras/pdf_vignette/ulysses_structure.pdf)
- Launch of Ulysses style code repository - see [Get Started](https://raw.githubusercontent.com/OHDSI/Ulysses/main/extras/pdf_vignette/get_started.pdf)
- Input Manifest helper functions
- Helpers to pull cohorts and concept sets from WebApi
- Git Helpers to initialize and sync repo

# Technical Setup

## System Requirements

- Requires R (version 4.1 or higher)
- Quarto (version 1.6 or higher)

## Installation

1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including RTools and Java.

2. In R, use the following commands to download and install Ulysses:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/Ulysses")
  ```
3. Install [quarto](https://quarto.org/docs/get-started/index.html)
