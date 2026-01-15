# Get Patch representation

[`ggalign::Patch`](https://yunuuuu.github.io/ggalign/reference/Patch-ggproto.md)
represents the layout manager for a single subplot within a composite
plot. The `Patch` object provides the interface for aligning the
subplot, managing panel sizes, and handling guide legends.

## Usage

``` r
patch(x)
```

## Arguments

- x:

  Any objects has a
  [Patch](https://yunuuuu.github.io/ggalign/reference/Patch-ggproto.md)
  representation

## Value

A
[`ggalign::Patch`](https://yunuuuu.github.io/ggalign/reference/Patch-ggproto.md)
object.

## See also

[`Patch`](https://yunuuuu.github.io/ggalign/reference/Patch-ggproto.md)

## Examples

``` r
patch(ggplot())
#> <ggproto object: Class PatchGgplot, ggalign::Patch, gg>
#>     adjust_panel_height: function
#>     adjust_panel_width: function
#>     align_border: function
#>     border_sizes: function
#>     decompose_bg: function
#>     decompose_guides: function
#>     get_option: function
#>     gtable: function
#>     is_alignpatches: function
#>     options: NULL
#>     panel_sizes: function
#>     place: function
#>     place_bg: function
#>     place_gt: function
#>     plot: ggplot2::ggplot, ggplot, ggplot2::gg, S7_object, gg
#>     respect_panel: function
#>     set_option: function
#>     set_options: function
#>     setup: function
#>     tag: function
#>     super:  <ggproto object: Class PatchGgplot, ggalign::Patch, gg>
```
