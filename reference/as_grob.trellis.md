# Convert Object into a Grob

Convert Object into a Grob

## Usage

``` r
# S3 method for class 'trellis'
as_grob(x, ..., device = NULL)
```

## Arguments

- x:

  An object to be converted into a
  [`grob`](https://rdrr.io/r/grid/grid-defunct.html).

- ...:

  Arguments passed on to
  [`grid::grid.grabExpr`](https://rdrr.io/r/grid/grid.grab.html)

  `warn`

  :   An integer specifying the amount of warnings to emit. 0 means no
      warnings, 1 means warn when it is certain that the grab will not
      faithfully represent the original scene. 2 means warn if there's
      any possibility that the grab will not faithfully represent the
      original scene.

  `wrap`

  :   A logical indicating how the output should be captured. If `TRUE`,
      each non-grob element on the display list is captured by wrapping
      it in a grob.

  `wrap.grobs`

  :   A logical indicating whether, if we are wrapping elements
      (`wrap=TRUE`), we should wrap grobs (or just wrap viewports).

  `width,height`

  :   Size of the device used for temporary rendering.

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

- `as_grob.trellis()`

- [`as_grob.Heatmap()`](https://yunuuuu.github.io/ggalign/reference/as_grob.Heatmap.md)

- [`as_grob.pheatmap()`](https://yunuuuu.github.io/ggalign/reference/as_grob.pheatmap.md)

## See also

[`trellis`](https://rdrr.io/pkg/lattice/man/trellis.object.html)

Other as_grob:
[`as_grob.Heatmap()`](https://yunuuuu.github.io/ggalign/reference/as_grob.Heatmap.md),
[`as_grob.formula()`](https://yunuuuu.github.io/ggalign/reference/as_grob.formula.md),
[`as_grob.gList()`](https://yunuuuu.github.io/ggalign/reference/as_grob.gList.md),
[`as_grob.ggalign::alignpatches()`](https://yunuuuu.github.io/ggalign/reference/as_grob.ggalign-colon-colon-alignpatches.md),
[`as_grob.ggplot()`](https://yunuuuu.github.io/ggalign/reference/as_grob.ggplot.md),
[`as_grob.grob()`](https://yunuuuu.github.io/ggalign/reference/as_grob.grob.md),
[`as_grob.patch()`](https://yunuuuu.github.io/ggalign/reference/as_grob.patch.md),
[`as_grob.patch_ggplot()`](https://yunuuuu.github.io/ggalign/reference/as_grob.patch_ggplot.md),
[`as_grob.patchwork()`](https://yunuuuu.github.io/ggalign/reference/as_grob.patchwork.md),
[`as_grob.pheatmap()`](https://yunuuuu.github.io/ggalign/reference/as_grob.pheatmap.md),
[`as_grob.recordedplot()`](https://yunuuuu.github.io/ggalign/reference/as_grob.recordedplot.md)
