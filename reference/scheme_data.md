# Plot data Specifications

**\[experimental\]**

Transforms the plot data. Many functions in this package require a
specific data format to align observations, `scheme_data()` helps
reformat data frames as needed.

## Usage

``` r
scheme_data(data = NULL, inherit = FALSE)
```

## Arguments

- data:

  A function to transform the plot data before rendering. Acceptable
  values include:

  - `NULL`: No action taken.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html):
    Inherits from the parent layout.

  - A `function` or purrr-style `formula`: Used to transform the plot
    data, which should accept a data frame and return a data frame. You
    can apply this after the parent layout `scheme_data` function, using
    the `inherit` argument.

  Use this hook to modify the data for all `geoms` after the layout is
  created (for matrix data, it has been melted to a long format data
  frame) but before rendering by `ggplot2`. The returned data must be a
  data frame for ggplot.

- inherit:

  A single boolean value indicates whether to apply the parent
  `scheme_data` first and then apply the specified `scheme_data` for the
  plot. Defaults to `FALSE`.

## Details

Defaults will attempt to inherit from the parent layout if the actual
data is inherited from the parent layout, with one exception:
[`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md),
which will not inherit the `scheme_data` by default.
