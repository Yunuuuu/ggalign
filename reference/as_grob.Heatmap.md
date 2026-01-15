# Convert Object into a Grob

Convert Object into a Grob

## Usage

``` r
# S3 method for class 'Heatmap'
as_grob(x, ..., device = NULL)

# S3 method for class 'HeatmapList'
as_grob(x, ..., device = NULL)

# S3 method for class 'HeatmapAnnotation'
as_grob(x, ..., device = NULL)
```

## Arguments

- x:

  An object to be converted into a
  [`grob`](https://rdrr.io/r/grid/grid-defunct.html).

- ...:

  Additional arguments passed to draw().

- device:

  A function that opens a graphics device for temporary rendering. By
  default this is an off-screen, in-memory device based on the `pdf`
  device, but this default device may not be satisfactory when using
  custom fonts.

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

- [`as_grob.formula()`](https://yunuuuu.github.io/ggalign/reference/as_grob.formula.md)

- [`as_grob.recordedplot()`](https://yunuuuu.github.io/ggalign/reference/as_grob.recordedplot.md)

- [`as_grob.trellis()`](https://yunuuuu.github.io/ggalign/reference/as_grob.trellis.md)

- `as_grob.Heatmap()`

- [`as_grob.pheatmap()`](https://yunuuuu.github.io/ggalign/reference/as_grob.pheatmap.md)

## See also

- [`Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)

- [`HeatmapAnnotation()`](https://rdrr.io/pkg/ComplexHeatmap/man/HeatmapAnnotation.html)

Other as_grob:
[`as_grob.formula()`](https://yunuuuu.github.io/ggalign/reference/as_grob.formula.md),
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
