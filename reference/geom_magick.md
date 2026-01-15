# Draw images as point shapes using magick

Reads an image with **magick**, applies optional processing, and uses
the result as the graphical shape for points in a plot.

This is useful when you want to replace the usual point symbols with
arbitrary images while keeping full control over their placement, size,
and interpolation.

## Usage

``` r
geom_magick(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  magick = NULL,
  magick_params = list(),
  interpolate = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    `stat_count()`, give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    `position_jitter()`. This method allows for passing extra arguments
    to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use `position_jitter()`, give the position as
    `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  These dots are for future extensions and must be empty.

- magick:

  A function (purrr-style formula is accepted) that takes an
  [`image_read()`](https://docs.ropensci.org/magick/reference/editing.html)
  object as input and returns an object compatible with
  [`as.raster()`](https://rdrr.io/r/grDevices/as.raster.html). You can
  use any of the `image_*()` functions from the **magick** package to
  process the raster image.

- magick_params:

  Additional arguments passed on to `magick`

- interpolate:

  A logical value indicating whether to linearly interpolate the image
  (the alternative is to use nearest-neighbour interpolation, which
  gives a more blocky result).

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Aesthetics

`geom_magick()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                                |                 |
|-----|--------------------------------------------------------------------------------|-----------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/reference/aes_position.html)**           |                 |
| •   | **[`y`](https://ggplot2.tidyverse.org/reference/aes_position.html)**           |                 |
| •   | **`image`**                                                                    |                 |
| •   | [`alpha`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)  | → `NA`          |
| •   | `angle`                                                                        | → `0`           |
| •   | [`fill`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)   | → `NA`          |
| •   | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.html)        | → inferred      |
| •   | `hjust`                                                                        | → `0.5`         |
| •   | [`size`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via `theme()` |
| •   | `vjust`                                                                        | → `0.5`         |

Learn more about setting these aesthetics in
`vignette("ggplot2-specs")`.

## Examples

``` r
set.seed(123)
d <- data.frame(
    x = rnorm(10),
    y = rnorm(10),
    image = "https://jeroenooms.github.io/images/frink.png",
    fill = sample(c("A", "B", "C", "D"), 10, replace = TRUE),
    alpha = rnorm(10, mean = 0.5, sd = 0.1)
)
d$alpha <- pmax(pmin(d$alpha, 1), 0)
ggplot(d, aes(x, y)) +
    geom_magick(aes(image = image, fill = fill, alpha = alpha))
```
