
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DTRAC: A Classifier for Difficult-to-Treat Rheumatoid Arthritis

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of **DTRAC** (**D**ifficult-to-**T**reat **R**heumatoid
**A**rthritis **C**lassifier) is to identify rheumatoid arthritis (RA)
patients who are likely to follow a difficult-to-treat disease course.
This package implements a rule-based classification algorithm. It is
**not** a pre-trained statistical model. The classification is
determined by a series of logical conditions and matrix manipulations
applied to commonly available clinical data.

## Installation

You can install the development version of DTRAC from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("driossantini/DTRAC")
```

## Usage and Status ⚠️

This package is currently in an **experimental** stage.
