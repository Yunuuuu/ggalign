# Reset layout ordering and panel group

Reset layout ordering and panel group

## Usage

``` r
cross_none(
  data = waiver(),
  ...,
  inherit_index = NULL,
  inherit_panel = NULL,
  inherit_nobs = NULL
)
```

## Arguments

- data:

  The dataset to use for the layout. By default,
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
  will convert the data to a matrix. This argument allows you to change
  the layout data. If not specified, the original data will be used.

- ...:

  \<[dyn-dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md).

- inherit_index:

  A boolean value indicating whether to inherit the ordering index. If
  `TRUE`, will match the layout ordering index with the data names.

- inherit_panel:

  A boolean value indicating whether to inherit the panel group. If
  `TRUE`, will match the layout panel with the data names.

- inherit_nobs:

  A boolean value indicating whether to inherit the number of
  observations (nobs). If `TRUE`, the `data` input must be compatible
  with the layout data.
