# Scale for `gshape` aesthetic

**\[questioning\]**

`geom_gshape` depends on the new aesthetics `gshape` (shape with grid
functions), which should always be provided with
`scale_gshape_manual()`, in which, we can provide a list of grobs or
functions that define how each value should be drawn. Any ggplot2
aesthetics can be used as the arguments.

## Usage

``` r
scale_gshape_manual(..., values, breaks = waiver(), na.value = NA)
```

## Arguments

- ...:

  Arguments passed on to
  [`ggplot2::discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)

  `name`

  :   The name of the scale. Used as the axis or legend title. If
      `waiver()`, the default, the name of the scale is taken from the
      first mapping used for that aesthetic. If `NULL`, the legend title
      will be omitted.

  `minor_breaks`

  :   One of:

      - `NULL` for no minor breaks

      - `waiver()` for the default breaks (none for discrete, one minor
        break between each major break for continuous)

      - A numeric vector of positions

      - A function that given the limits returns a vector of minor
        breaks. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. When the function has two arguments, it will
        be given the limits and major break positions.

  `labels`

  :   One of the options below. Please note that when `labels` is a
      vector, it is highly recommended to also set the `breaks` argument
      as a vector to protect against unintended mismatches.

      - `NULL` for no labels

      - `waiver()` for the default labels computed by the transformation
        object

      - A character vector giving labels (must be same length as
        `breaks`)

      - An expression vector (must be the same length as breaks). See
        ?plotmath for details.

      - A function that takes the breaks as input and returns labels as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `limits`

  :   One of:

      - `NULL` to use the default scale values

      - A character vector that defines possible values of the scale and
        their order

      - A function that accepts the existing (automatic) values and
        returns new ones. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `na.translate`

  :   Unlike continuous scales, discrete scales can easily show missing
      values, and do so by default. If you want to remove missing values
      from a discrete scale, specify `na.translate = FALSE`.

  `drop`

  :   Should unused factor levels be omitted from the scale? The
      default, `TRUE`, uses the levels that appear in the data; `FALSE`
      includes the levels in the factor. Please note that to display
      every level in a legend, the layer should use
      `show.legend = TRUE`.

  `guide`

  :   A function used to create a guide or its name. See
      [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html)
      for more information.

  `call`

  :   The `call` used to construct the scale for reporting messages.

  `super`

  :   The super class to use for the constructed scale

- values:

  A list of grobs or functions (including purrr-like lambda syntax) that
  define how each cell's grob (graphical object) should be drawn.

- breaks:

  One of:

  - `NULL` for no breaks

  - `waiver()` for the default breaks (the scale limits)

  - A character vector of breaks

  - A function that takes the limits as input and returns breaks as
    output

- na.value:

  The aesthetic value to use for missing (`NA`) values

## Life cycle

We're unsure whether this function is truly necessary, which is why it
is marked as questioning. So far, we've found that
[`geom_subrect()`](https://yunuuuu.github.io/ggalign/reference/geom_subrect.md)
and
[`geom_subtile()`](https://yunuuuu.github.io/ggalign/reference/geom_subrect.md)
handle most use cases effectively.

## Examples

``` r
library(grid)
ggplot(data.frame(value = letters[seq_len(5)], y = seq_len(5))) +
    geom_gshape(aes(x = 1, y = y, gshape = value, fill = value)) +
    scale_gshape_manual(values = list(
        a = function(x, y, width, height, fill) {
            rectGrob(x, y,
                width = width, height = height,
                gp = gpar(fill = fill),
                default.units = "native"
            )
        },
        b = function(x, y, width, height, fill) {
            rectGrob(x, y,
                width = width, height = height,
                gp = gpar(fill = fill),
                default.units = "native"
            )
        },
        c = function(x, y, width, height, fill) {
            rectGrob(x, y,
                width = width, height = height,
                gp = gpar(fill = fill),
                default.units = "native"
            )
        },
        d = function(x, y, width, height, shape) {
            gList(
                pointsGrob(x, y, pch = shape),
                # To ensure the rectangle color is shown in the legends, you
                # must explicitly provide a color argument and include it in
                # the `gpar()` of the graphical object
                rectGrob(x, y, width, height,
                    gp = gpar(col = "black", fill = NA)
                )
            )
        },
        e = function(xmin, xmax, ymin, ymax) {
            segmentsGrob(
                xmin, ymin,
                xmax, ymax,
                gp = gpar(lwd = 2)
            )
        }
    )) +
    scale_fill_brewer(palette = "Dark2") +
    theme_void()

```
