# Change the layer adding order

This function allows you to change the order in which layers are added
to a ggplot.

## Usage

``` r
layer_order(layer, order = 0)
```

## Arguments

- layer:

  A
  [`layer geometry`](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
  object to be added.

- order:

  An integer indicating the position at which the layer should be added.
  If `<= 0`, the layer will be added at the beginning. If greater than
  the number of plot layers, it will be added at the end.

## Value

A `layer_order` object.

## Examples

``` r
ggplot(faithfuld, aes(waiting, eruptions)) +
    geom_raster(aes(fill = density)) +
    geom_point(color = "red", size = 1)

ggplot(faithfuld, aes(waiting, eruptions)) +
    geom_raster(aes(fill = density)) +
    layer_order(geom_point(color = "red", size = 1))
```
