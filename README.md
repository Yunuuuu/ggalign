
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggalign <a href="https://yunuuuu.github.io/ggalign/"><img src="man/figures/logo.png" align="right" height="139" alt="ggalign website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Yunuuuu/ggalign/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Yunuuuu/ggalign/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Yunuuuu/ggalign/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Yunuuuu/ggalign?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggalign)](https://CRAN.R-project.org/package=ggalign)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://cranlogs.r-pkg.org/badges/ggalign)](https://cran.r-project.org/package=ggalign)
[![DOI](https://zenodo.org/badge/828195668.svg)](https://doi.org/10.5281/zenodo.14825328)
<!-- badges: end -->

The `ggalign` package provides an integrative framework for composable
visualization, enabling the construction of complex multi-plot
layouts—including insets, circular arrangements, and multi-panel
compositions. Built on the grammar of graphics, it introduces intuitive
tools to align, stack, and nest plots, making it easy to link related
views, overlay clustering results, or highlight shared patterns.
Designed for high-dimensional data contexts such as genomics,
transcriptomics, and microbiome studies, it simplifies the creation of
richly annotated, publication-ready figures from diverse visual
components.

## Why use `ggalign`?

`ggalign` focuses on aligning observations across multiple plots. If
you’ve ever struggled with aligning plots with self-contained ordering
(like dendrogram), or applying consistent grouping or ordering across
multiple plots (e.g., with k-means clustering), `ggalign` is designed to
make this easier. The package integrates seamlessly with ggplot2,
providing the flexibility to use its geoms, scales, and other components
for complex visualizations.

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

1.  For the full reference documentation, visit:
    <https://yunuuuu.github.io/ggalign/>
2.  The complete tutorial is available at:
    <https://yunuuuu.github.io/ggalign-book/>

## Design features

![](man/figures/design.png)

## Examples

![](man/figures/gallery.png)

## Comparison with other composable visualization tools

<table class="table"><thead>
    <tr>
        <th colspan="2"></th>
        <th> <code>ggalign</code> </th>
        <th> <code>marsilea</code> </th>
        <th> <code>ComplexHeatmap</code> </th>
    </tr></thead>
<tbody>
    <tr>
        <td colspan="2"> <strong>Language</strong> </td>
        <td>R</td>
        <td>Python</td>
        <td>R</td>
    </tr>
    <tr>
        <td colspan="2"> <strong>User Interface</strong> </td>
        <td>Declarative</td>
        <td>Declarative</td>
        <td>Functional</td>
    </tr>
    <tr>
        <td colspan="2"> <strong>Plot System</strong> </td>
        <td>ggplot2 (Advanced plot system built on grid system)</td>
        <td>Matplotlib</td>
        <td>grid</td>
    </tr>
    <tr>
        <td colspan="2"> <strong>Focus</strong> </td>
        <td>General-purpose composable visualization</td>
        <td>Grid-based composable visualization</td>
        <td>Heatmap</td>
    </tr>
    <tr>
        <td colspan="2"> <strong>StackLayout</strong> </td>
        <td>✅</td>
        <td>✅</td>
        <td>✅</td>
    </tr>
    <tr>
        <td colspan="2"> <strong>QuadLayout</strong> </td>
        <td>✅</td>
        <td>✅</td>
        <td>Heatmap Only (discrete variables)</td>
    </tr>
    <tr>
        <td colspan="2"> <strong>CircleLayout</strong> </td>
        <td>✅</td>
        <td>❌</td>
        <td>❌</td>
    </tr>
    <tr>
        <td rowspan="4"> <strong>Relationship</strong> </td>
        <td> <strong>One-to-One</strong> </td>
        <td>✅</td>
        <td>✅</td>
        <td>✅</td>
    </tr>
    <tr>
        <td> <strong>One-to-Many</strong>/<strong>Many-to-One</strong> </td>
        <td>✅</td>
        <td>❌</td>
        <td>❌</td>
    </tr>
    <tr>
        <td> <strong>Many-to-Many</strong> </td>
        <td>✅</td>
        <td>❌</td>
        <td>❌</td>
    </tr>
    <tr>
        <td> <strong>Crosswise</strong> </td>
        <td>✅</td>
        <td>❌</td>
        <td>❌</td>
    </tr>
    <tr>
        <td colspan="2"> <strong>Annotate observations</strong> </td>
        <td>✅</td>
        <td>❌</td>
        <td>✅</td>
    </tr>
    <tr>
        <td colspan="2"> <strong>Fully Compatible with ggplot2</strong> </td>
        <td>✅</td>
        <td>❌</td>
        <td>❌</td>
    </tr>
</tbody></table>

## Feature and specification comparison

| Specification                                | `ggalign`                                             | `marsilea`    | `ComplexHeatmap`                                      |
|----------------------------------------------|-------------------------------------------------------|---------------|-------------------------------------------------------|
| **Reorder observations**                     | ✅                                                    | ✅            | Heatmap Only                                          |
| **Group observations into different panels** | ✅                                                    | ✅            | Heatmap Only                                          |
| **Clustering algorithm**                     | Kmeans,Hierarchical Clustering and arbitary algorithm | ❌            | Kmeans,Hierarchical Clustering and arbitary algorithm |
| **Legends Creation**                         | Automatic                                             | Automatic     | Limited automatic, requires manual add                |
| **Legends Position**                         | Anywhere; independently controllable per plot         | Anywhere      | Fixed to one of four sides                            |
| **Dendrogram**                               | Tree from both `hclust` or `ape`                      | `hclust` only | `hclust` only                                         |
| **Tanglegram**                               | ✅                                                    | ❌            | ❌                                                    |
| **3D Heatmap**                               | ✅                                                    | ❌            | ✅                                                    |
| **Oncoplot**                                 | ✅                                                    | ✅            | ✅                                                    |
| **UpSet plot**                               | ✅                                                    | ✅            | ✅                                                    |

## Acknowledgements

I would like to express my sincere gratitude to the contributors of the
`ggplot2` project for providing a powerful and flexible framework for
data visualization in R. Their work laid the foundation for the
functionality and design of this package. I would also like to thank the
`patchwork` project, from which the core coding for the plot composer
was adapted. The `patchwork` library provided a useful mechanism for
combining and aligning plots, which was modified to suit the needs of
this package. Without the contributions of these open-source projects,
this package would not have been possible.

Additionally, I would like to extend my heartfelt thanks to
`@teunbrand`, who has fulfilled my numerous feature requests, and
assisted with the integration of new functions into ggplot2.
