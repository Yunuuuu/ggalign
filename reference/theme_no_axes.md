# Remove axis elements

Remove axis elements

## Usage

``` r
theme_no_axes(
  axes = "xy",
  text = TRUE,
  ticks = TRUE,
  title = TRUE,
  line = FALSE
)
```

## Arguments

- axes:

  Which axes elements should be removed? A string containing one or more
  of `"t"`, `"l"`, `"b"`, `"r"`, `"x"`, and `"y"`.

- text:

  If `TRUE`, will remove the axis labels.

- ticks:

  If `TRUE`, will remove the axis ticks.

- title:

  If `TRUE`, will remove the axis title.

- line:

  If `TRUE`, will remove the axis line.

## Value

A [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
object.

## Examples

``` r
p <- ggplot() +
    geom_point(aes(x = wt, y = qsec), data = mtcars)
p + theme_no_axes()

p + theme_no_axes("b")

p + theme_no_axes("l")
```
