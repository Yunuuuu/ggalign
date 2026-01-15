# Wrap Arbitrary Graphics to ggplot

The `ggwrap()` function allows non-ggplot2 elements to be converted into
a compliant representation for use with
[`align_plots()`](https://yunuuuu.github.io/ggalign/reference/alignpatches.md).
This is useful for adding any graphics that can be converted into a
[`grob`](https://rdrr.io/r/grid/grid-defunct.html) with the
[`as_grob()`](https://yunuuuu.github.io/ggalign/reference/as_grob.md)
method.

## Usage

``` r
ggwrap(plot, ..., align = "panel", on_top = FALSE, clip = TRUE, vp = NULL)
```

## Arguments

- plot:

  Any graphic that can be converted into a
  [`grob`](https://rdrr.io/r/grid/grid-defunct.html) using
  [`as_grob()`](https://yunuuuu.github.io/ggalign/reference/as_grob.md).

- ...:

  Additional arguments passed to the
  [`as_grob()`](https://yunuuuu.github.io/ggalign/reference/as_grob.md)
  method.

- align:

  A string specifying the area to place the plot: `"full"` for the full
  area, `"plot"` for the full plotting area (including the axis label),
  or `"panel"` for only the actual area where data is drawn.

- on_top:

  A single boolean value indicates whether the graphic plot should be
  put frontmost. Note: the graphic plot will always put above the
  background.

- clip:

  A single boolean value indicating whether the grob should be clipped
  if they expand outside their designated area.

- vp:

  A [`viewport`](https://rdrr.io/r/grid/viewport.html) object, you can
  use this to define the plot area.

## Value

A `wrapped_plot` object that can be directly placed into
[`align_plots()`](https://yunuuuu.github.io/ggalign/reference/alignpatches.md).

## Examples

``` r
library(grid)
ggwrap(rectGrob(gp = gpar(fill = "goldenrod")), align = "full") +
    inset(rectGrob(gp = gpar(fill = "steelblue")), align = "panel") +
    inset(textGrob("Here are some text", gp = gpar(color = "black")),
        align = "panel"
    )

p1 <- ggplot(mtcars) +
    geom_point(aes(mpg, disp)) +
    ggtitle("Plot 1")
align_plots(p1, ggwrap(
    ~ plot(mtcars$mpg, mtcars$disp),
    mar = c(0, 2, 0, 0), bg = NA
))

```
