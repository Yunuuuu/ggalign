# Plot Adding Context Settings

**\[experimental\]**

These settings control the behavior of the plot when added to a layout,
as well as the arrangement of individual plot areas within the layout.

## Usage

``` r
active(order = NA_integer_, use = NA, name = NA_character_)
```

## Arguments

- order:

  An integer specifying the order of the plot area within the layout.

- use:

  A logical (`TRUE`/`FALSE`) indicating whether to set the active
  context to the current plot when added to a layout. If `TRUE`, any
  subsequent `ggplot` elements will be applied to this plot.

- name:

  A string specifying the plot's name, useful for switching active
  contexts through the `what` argument in functions like
  [`quad_anno()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)/[`stack_switch()`](https://yunuuuu.github.io/ggalign/reference/stack_switch.md).

## Details

By default, the active context is set only for functions that add plot
areas. This allows other `ggplot2` elements-such as `geoms`, `stats`,
`scales`, or `themes`- to be seamlessly added to the current plot area.

The default ordering of the plot areas is from top to bottom or from
left to right, depending on the layout orientation. However, users can
customize this order using the `order` argument.
