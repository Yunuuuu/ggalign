# Rasterize the ggplot layers

The function rasterizes input graphical objects (e.g., grob, layer,
ggplot) and optionally processes the resulting raster using magick, a
powerful image manipulation library. This allows for advanced graphical
transformations directly within the plotting pipeline.

## Usage

``` r
raster_magick(x, ...)
```

## Arguments

- x:

  An object to rasterize, can be a
  [`grob()`](https://rdrr.io/r/grid/grid-defunct.html),
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html),
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html), or
  a list of such objects.

- ...:

  Additional arguments passed on to
  [`magickGrob()`](https://yunuuuu.github.io/ggalign/reference/magickGrob.md).

## Value

An object with the same class of the input.

## See also

[`magickGrob()`](https://yunuuuu.github.io/ggalign/reference/magickGrob.md)

## Examples

``` r
# Currently, `magick` package require R >= 4.1.0
if (requireNamespace("magick")) {
    # data generated code was copied from `ComplexHeatmap`
    set.seed(123)
    small_mat <- matrix(rnorm(56), nrow = 7)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
    ggheatmap(small_mat, aes(.x, .y), filling = NULL) +
        raster_magick(geom_tile(aes(fill = value)), res = 20)

    ggheatmap(small_mat, aes(.x, .y), filling = NULL) +
        # Use `magick::filter_types()` to check available `filter` arguments
        raster_magick(
            geom_tile(aes(fill = value)),
            magick = function(image) {
                magick::image_resize(image,
                    geometry = "50%x", filter = "Lanczos"
                )
            }
        )
}
```
