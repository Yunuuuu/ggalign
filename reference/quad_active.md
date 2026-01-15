# Determine the Active Context of Quad-Layout

**\[stable\]**

- `quad_active`: Sets the active context to the
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)/[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  itself.

- `quad_anno`: Sets the active context to the specified annotation stack
  based on the `position` argument.

- `anno_top`: A special case of `quad_anno` with `position = "top"`.

- `anno_left`: A special case of `quad_anno` with `position = "left"`.

- `anno_bottom`: A special case of `quad_anno` with
  `position = "bottom"`.

- `anno_right`: A special case of `quad_anno` with `position = "right"`.

## Usage

``` r
quad_active(width = NULL, height = NULL)

quad_anno(
  position,
  size = NULL,
  free_guides = waiver(),
  initialize = NULL,
  what = waiver()
)

anno_top(
  size = NULL,
  free_guides = waiver(),
  initialize = NULL,
  what = waiver()
)

anno_left(
  size = NULL,
  free_guides = waiver(),
  initialize = NULL,
  what = waiver()
)

anno_bottom(
  size = NULL,
  free_guides = waiver(),
  initialize = NULL,
  what = waiver()
)

anno_right(
  size = NULL,
  free_guides = waiver(),
  initialize = NULL,
  what = waiver()
)
```

## Arguments

- width, height:

  The relative width/height of the main plot, can be a
  [`unit`](https://rdrr.io/r/grid/unit.html) object.

- position:

  A string of `"top"`, `"left"`, `"bottom"`, or `"right"` indicates
  which annotation stack should be activated.

- size:

  A numeric value or an [`unit`](https://rdrr.io/r/grid/unit.html)
  object to set the total `height`/`width` of the annotation stack.

  - If `position` is `"top"` or `"bottom"`, `size` sets the total height
    of the annotation.

  - If `position` is `"left"` or `"right"`, `size` sets the total width
    of the annotation.

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

## Details

By default, `quad_anno()` attempts to initialize the annotation stack
layout using data from
[`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)/[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md).
However, in situations where you want to use different data for the
annotation stack, you can set `initialize = FALSE` and then provide a
custom
[`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md).

## See also

[`quad_switch()`](https://yunuuuu.github.io/ggalign/reference/quad_switch.md)
