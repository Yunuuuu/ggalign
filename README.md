
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggalign <a href="https://yunuuuu.github.io/ggalign/"><img src="man/figures/logo.png" align="right" height="139" alt="ggalign website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Yunuuuu/ggalign/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Yunuuuu/ggalign/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Yunuuuu/ggalign/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Yunuuuu/ggalign?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggalign)](https://CRAN.R-project.org/package=ggalign)
[![](https://cranlogs.r-pkg.org/badges/ggalign)](https://cran.r-project.org/package=ggalign)
<!-- badges: end -->

This package extends ggplot2 by providing advanced tools for aligning
and organizing multiple plots, particularly those that automatically
reorder observations, such as dendrogram. It offers fine control over
layout adjustment and plot annotations, enabling you to create complex,
publication-quality visualizations while still using the familiar
grammar of ggplot2.

## Why use `ggalign`?

`ggalign` focuses on aligning observations across multiple plots. It
leverages the `"number of observations"` in the
[vctrs](https://vctrs.r-lib.org/reference/vec_size.html) package or
`NROW()` function to maintain consistency in plot organization.

If you’ve ever struggled with aligning plots with self-contained
ordering (like dendrogram), or applying consistent grouping or ordering
across multiple plots (e.g., with k-means clustering), `ggalign` is
designed to make this easier. The package integrates seamlessly with
ggplot2, providing the flexibility to use its geoms, scales, and other
components for complex visualizations.

## Installation

You can install `ggalign` from `CRAN` using:

``` r
install.packages("ggalign")
```

Alternatively, install the development version from
[r-universe](https://yunuuuu.r-universe.dev/ggalign) with:

``` r
install.packages("ggalign",
    repos = c("https://yunuuuu.r-universe.dev", "https://cloud.r-project.org")
)
```

or from [GitHub](https://github.com/Yunuuuu/ggalign) with:

``` r
# install.packages("remotes")
remotes::install_github("Yunuuuu/ggalign")
```

## Learning ggalign

1.  The complete tutorial is available at:
    <https://yunuuuu.github.io/ggalign-book/>

2.  For the full reference documentation, visit:
    <https://yunuuuu.github.io/ggalign/>

## Compare with other similar tools

|                                     | `ggalign`                                           | `ComplexHeatmap`                                     | `marsilea`               | `ggheatmap`                                          |
|-------------------------------------|-----------------------------------------------------|------------------------------------------------------|--------------------------|------------------------------------------------------|
| **Language**                        | R                                                   | R                                                    | Python                   | R                                                    |
| **User Interface**                  | Declarative                                         | Functional                                           | Declarative              | Functional                                           |
| **Plot System**                     | ggplot2 (Advanced plot system built on grid system) | grid                                                 | seaborn                  | ggplot2                                              |
| **Focus**                           | Composable Visualization                            | Heatmap                                              | Composable Visualization | Heatmap                                              |
| **Heatmap Layout**                  | Yes                                                 | Yes                                                  | Yes                      | Yes                                                  |
| **Marginal Layout**                 | Yes                                                 | No                                                   | Yes                      | No                                                   |
| **Stack Layout**                    | Yes                                                 | Yes                                                  | Yes                      | No                                                   |
| **Cross Layout**                    | Yes                                                 | No                                                   | No                       | No                                                   |
| **Group or Reorder Heatmap**        | Yes                                                 | Yes                                                  | Yes                      | Yes                                                  |
| **Group or Reorder Stack Only**     | Yes                                                 | No                                                   | Yes                      | No                                                   |
| **Separate Group into Slices**      | Yes                                                 | Yes                                                  | Yes                      | No                                                   |
| **Data input**                      | Various, and can be easily extended                 | fixed                                                | fixed                    | fixed                                                |
| **Visualization Type**              | Wide variety, provided by ggplot2                   | Many, but limited                                    | Many, but limited        | Limited                                              |
| **Customization**                   | Extensive                                           | Limited                                              | Extensive                | Limited                                              |
| **Legends Creation**                | Automatic                                           | Limited automatic, requires manual add               | Manual                   | Automatic                                            |
| **Legends Position**                | Anywhere, can be controlled for a single plot       | Four sides, can only be placed on one side at a time | ?                        | Four sides, can only be placed on one side at a time |
| **Advanced Statistical Plots**      | Yes, with ggplot2 extension                         | No                                                   | No                       | No                                                   |
| **Scientific Color Palettes**       | Many                                                | Limited                                              | Limited                  | Many                                                 |
| **Tanglegram**                      | Yes                                                 | No                                                   | Yes                      | No                                                   |
| **Dendrogram Position**             | Anywhere                                            | Heatmap only                                         | Anywhere                 | Heatmap only                                         |
| **3D Heatmap**                      | Yes                                                 | Yes                                                  | No                       | No                                                   |
| **Oncoplot**                        | Yes                                                 | Yes                                                  | Yes                      | No                                                   |
| **Fully Compatible with ggplot2\*** | Yes                                                 | No                                                   | No                       | No                                                   |
| **Interoperability with Python**    | Limited                                             | Limited                                              | Excellent                | Limited                                              |
| **Ease of Use**                     | Easy for ggplot2 users                              | Easy for for grid user\*                             | Easy for python user     | Easy                                                 |
| **Interactive**                     | No                                                  | Yes                                                  | No                       | No                                                   |

- `Fully Compatible with ggplot2*`: It can be considered a benefit, as
  the ggplot2 ecosystem is highly enriched.
- `Easy for for grid user*`: the direct population using grid should be
  smaller than ggplot2
