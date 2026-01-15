# Convert Object into a Grob

Convert Object into a Grob

## Usage

``` r
# S3 method for class 'formula'
as_grob(x, ..., device = NULL, name = NULL)

# S3 method for class '`function`'
as_grob(x, ..., device = NULL, name = NULL)
```

## Arguments

- x:

  An object to be converted into a
  [`grob`](https://rdrr.io/r/grid/grid-defunct.html).

- ...:

  Graphical Parameters passed on to
  [par()](https://rdrr.io/r/graphics/par.html).

- device:

  A function that opens a graphics device for `grid.echo()` to work on.
  By default this is an off-screen, in-memory device based on the `pdf`
  device. This default device may not be satisfactory when using custom
  fonts.

- name:

  A character identifier.

## Value

A [`grob`](https://rdrr.io/r/grid/grid-defunct.html) object.

## `as_grob` method collections

- [`as_grob.grob()`](https://yunuuuu.github.io/ggalign/reference/as_grob.grob.md)

- [`as_grob.gList()`](https://yunuuuu.github.io/ggalign/reference/as_grob.gList.md)

- [`as_grob.patch_ggplot()`](https://yunuuuu.github.io/ggalign/reference/as_grob.patch_ggplot.md)

- [`as_grob.ggplot()`](https://yunuuuu.github.io/ggalign/reference/as_grob.ggplot.md)

- [`as_grob.ggalign::alignpatches()`](https://yunuuuu.github.io/ggalign/reference/as_grob.ggalign-colon-colon-alignpatches.md)

- [`as_grob.patchwork()`](https://yunuuuu.github.io/ggalign/reference/as_grob.patchwork.md)

- [`as_grob.patch()`](https://yunuuuu.github.io/ggalign/reference/as_grob.patch.md)

- `as_grob.formula()`

- [`as_grob.recordedplot()`](https://yunuuuu.github.io/ggalign/reference/as_grob.recordedplot.md)

- [`as_grob.trellis()`](https://yunuuuu.github.io/ggalign/reference/as_grob.trellis.md)

- [`as_grob.Heatmap()`](https://yunuuuu.github.io/ggalign/reference/as_grob.Heatmap.md)

- [`as_grob.pheatmap()`](https://yunuuuu.github.io/ggalign/reference/as_grob.pheatmap.md)

## See also

[`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Other as_grob:
[`as_grob.Heatmap()`](https://yunuuuu.github.io/ggalign/reference/as_grob.Heatmap.md),
[`as_grob.gList()`](https://yunuuuu.github.io/ggalign/reference/as_grob.gList.md),
[`as_grob.ggalign::alignpatches()`](https://yunuuuu.github.io/ggalign/reference/as_grob.ggalign-colon-colon-alignpatches.md),
[`as_grob.ggplot()`](https://yunuuuu.github.io/ggalign/reference/as_grob.ggplot.md),
[`as_grob.grob()`](https://yunuuuu.github.io/ggalign/reference/as_grob.grob.md),
[`as_grob.patch()`](https://yunuuuu.github.io/ggalign/reference/as_grob.patch.md),
[`as_grob.patch_ggplot()`](https://yunuuuu.github.io/ggalign/reference/as_grob.patch_ggplot.md),
[`as_grob.patchwork()`](https://yunuuuu.github.io/ggalign/reference/as_grob.patchwork.md),
[`as_grob.pheatmap()`](https://yunuuuu.github.io/ggalign/reference/as_grob.pheatmap.md),
[`as_grob.recordedplot()`](https://yunuuuu.github.io/ggalign/reference/as_grob.recordedplot.md),
[`as_grob.trellis()`](https://yunuuuu.github.io/ggalign/reference/as_grob.trellis.md)
