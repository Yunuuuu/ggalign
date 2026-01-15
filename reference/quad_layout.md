# Arrange plots in the quad-side of a main plot

**\[stable\]**

This function arranges plots around the quad-sides of a main plot,
aligning both horizontal and vertical axes, and can handle either
discrete or continuous variables.

- If `xlim` is provided, a continuous variable will be required and
  aligned in the vertical direction. Otherwise, a discrete variable will
  be required and aligned.

- If `ylim` is provided, a continuous variable will be required and
  aligned in the horizontal direction. Otherwise, a discrete variable
  will be required and aligned.

The `quad_discrete` is a special case where both `xlim` and `ylim` are
not provided.

The `quad_continuous` is a special case where both `xlim` and `ylim` are
provided.

For historical reasons, the following aliases are available:

- `quad_alignh`: Align discrete variables in the horizontal direction
  and continuous variables in vertical direction.

- `quad_alignv`: Align discrete variables in the vertical direction and
  continuous variables in horizontal direction.

- `quad_alignb` is an alias for `quad_discrete`.

- `quad_free` is an alias for `quad_continuous`.

## Usage

``` r
quad_layout(
  data = waiver(),
  mapping = aes(),
  xlim = waiver(),
  ylim = waiver(),
  ...,
  theme = NULL,
  active = NULL,
  width = NA,
  height = NA
)

quad_alignh(..., ylim = waiver())

quad_alignv(..., xlim = waiver())

quad_discrete(
  data = waiver(),
  mapping = aes(),
  ...,
  theme = NULL,
  active = NULL,
  width = NA,
  height = NA
)

quad_continuous(
  data = waiver(),
  mapping = aes(),
  xlim = NULL,
  ylim = NULL,
  ...,
  theme = NULL,
  active = NULL,
  width = NA,
  height = NA
)
```

## Arguments

- data:

  Default dataset to use for the layout. If not specified, it must be
  supplied in each plot added to the layout. By default, this will
  attempt to inherit from the parent layout.

  If both `xlim` and `ylim` are provided, a `data frame` is required,
  and
  [`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)
  will be used to convert the data to a data frame. When inherited by an
  annotation stack, no transposition will be applied.

  Otherwise, a `matrix` is required, and
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
  will be used to convert the data to a matrix. When inherited by the
  column annotation stack, the data will be transposed.

- mapping:

  Default list of aesthetic mappings to use for main plot in the layout.
  If not specified, must be supplied in each layer added to the main
  plot.

- xlim, ylim:

  A
  [`continuous_limits()`](https://yunuuuu.github.io/ggalign/reference/continuous_limits.md)
  object specifying the left/lower limit and the right/upper limit of
  the scale. Used to align the continuous axis.

- ...:

  Additional arguments passed to
  [`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)
  or
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md).

- theme:

  A [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  object used to customize various elements of the layout, including
  `guides`, `title`, `subtitle`, `caption`, `margins`, `panel.border`,
  and `background`. By default, the theme will inherit from the parent
  `layout`. It also controls the panel spacing for all plots in the
  layout.

- active:

  A [`active()`](https://yunuuuu.github.io/ggalign/reference/active.md)
  object that defines the context settings when added to a layout.

- width, height:

  The relative width/height of the main plot, can be a
  [`unit`](https://rdrr.io/r/grid/unit.html) object.

## Value

A `QuadLayout` object.

## ggplot2 specification

If either `xlim` or `ylim` is not provided, the data input will be
converted to a matrix using
[`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md),
and the data in the underlying main plot will contain the following
columns:

- `.panel_x` and `.panel_y`: the column and row panel groups.

- `.x` and `.y`: an integer index of `x` and `y` coordinates

- `.discrete_x` and `.discrete_y`: a factor of the data labels (only
  applicable when `.row_names` and `.column_names` exists).

- `.row_names` and `.column_names`: A character of the row and column
  names of the original matrix (only applicable when names exist).

- `.row_index` and `.column_index`: the row and column index of the
  original matrix.

- `value`: the actual matrix value.

Otherwise, the data input will be used for the main plot.
