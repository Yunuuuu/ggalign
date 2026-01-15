# Create a New `CraftBox` Object with `CraftAlign` craftsman

**\[stable\]**

An `CraftAlign` object interacts with the `Layout` object to reorder or
split observations and, in some cases, add plot components to the
`Layout`.

## Usage

``` r
align(
  align,
  data = NULL,
  ...,
  plot = NULL,
  size = NULL,
  schemes = NULL,
  active = NULL,
  no_axes = deprecated(),
  call = caller_call()
)
```

## Arguments

- align:

  An `CraftAlign` object.

- data:

  The following options can be used:

  - `NULL`: No data is set.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html):
    Inherits the data from the layout matrix.

  - A `function` (including purrr-like lambda syntax): Applied to the
    layout matrix to transform the data before use. To transform the
    final plot data, please use
    [`scheme_data()`](https://yunuuuu.github.io/ggalign/reference/scheme_data.md).

  - A `matrix`, `data.frame`, or atomic vector.

- ...:

  Additional fields passed to the `align` object.

- plot:

  A ggplot object.

- size:

  The relative size of the plot, can be specified as a
  [`unit()`](https://rdrr.io/r/grid/unit.html). Note that for
  [`circle_layout()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md),
  all size values will be interpreted as relative sizes, as this layout
  type adjusts based on the available space in the circular arrangement.

- schemes:

  Options for `schemes`:

  - `NULL`: Used when `align` do not add a plot.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html):
    Try to infer `schemes` based on `data`.

- active:

  A [`active()`](https://yunuuuu.github.io/ggalign/reference/active.md)
  object that defines the context settings when added to a layout.

- no_axes:

  **\[deprecated\]** Please add
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  directly to the ggplot instead.

- call:

  The `call` used to construct the `align` object, for reporting
  messages.

## Value

A new `CraftBox` object.

## Details

Each of the `Align*` objects is just a
[`ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.html)
object, descended from the top-level `CraftAlign`, and each implements
various methods and fields.

To create a new type of `Align*` object, you typically will want to
override one or more of the following:

- `setup_params`: Prepare parameter or check parameters used by this
  plot.

- `setup_data`: Prepare data used by this plot.

- `compute`: A method used to compute statistics.

- `align`: A method used to group observations into panel or reorder
  observations.

- `draw`: A method used to draw the plot. Must return a `ggplot` object.

## Discrete Axis Alignment

It is important to note that we consider rows as observations, meaning
`vec_size(data)`/`NROW(data)` must match the number of observations
along the axis used for alignment (x-axis for a vertical stack layout,
y-axis for a horizontal stack layout).

## Examples

``` r
align_dendro()
#> <Class: AlignDendro AlignHclust CraftAlign Craftsman>
#>      plot: yes
#>   reorder: yes
#>     split: no 
```
