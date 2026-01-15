# Determine the Active Context of Quad-Layout

**\[stable\]**

`quad_switch()` integrates
[`quad_active()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)
and
[`quad_anno()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)
into one function for ease of use. This function allows you to quickly
change the active context of the
[`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
and its annotations.

`hmanno` is an alias for `quad_switch`, with additional arguments for
backward compatibility

## Usage

``` r
quad_switch(
  position = NULL,
  size = NULL,
  width = NULL,
  height = NULL,
  free_guides = waiver(),
  initialize = NULL,
  what = waiver()
)

hmanno(
  position = NULL,
  size = NULL,
  width = NULL,
  height = NULL,
  free_guides = waiver(),
  initialize = NULL,
  what = waiver()
)
```

## Arguments

- position:

  A string of `"top"`, `"left"`, `"bottom"`, or `"right"` indicates
  which annotation stack should be activated. If `NULL`, it sets the
  active context to the
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)/[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  itself.

- size:

  A numeric value or an [`unit`](https://rdrr.io/r/grid/unit.html)
  object to set the total `height`/`width` of the annotation stack.

  - If `position` is `"top"` or `"bottom"`, `size` sets the total height
    of the annotation.

  - If `position` is `"left"` or `"right"`, `size` sets the total width
    of the annotation.

- width, height:

  The relative width/height of the main plot, can be a
  [`unit`](https://rdrr.io/r/grid/unit.html) object.

- free_guides:

  Override the `guides` collection behavior specified in the
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)/[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  for the annotation stack.

- initialize:

  A boolean indicating whether the annotation stack should be
  initialized if it is not already. By default, the annotation stack
  layout will attempt to initialize when the data is compatible. If set
  to `TRUE`, and the data in
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)/[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  is incompatible with the annotation stack, no data will be used in the
  stack.

- what:

  What should get activated in the annotation stack? A single number or
  string of the plot elements in the layout. If `NULL`, will remove any
  active context.

## Value

An object that can be added to
[`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)/[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md).

## See also

[`quad_active()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)/[`quad_anno()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)

## Examples

``` r
ggheatmap(matrix(rnorm(81), nrow = 9)) +
    anno_top() +
    align_dendro()
#> → heatmap built with `geom_tile()`
```
