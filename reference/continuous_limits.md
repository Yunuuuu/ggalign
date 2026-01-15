# Set continuous limits for the layout

To align continuous axes, it is important to keep the limits consistent
across all plots in the layout. You can set the limits by passing a
function directly to the `limits` or `xlim`/`ylim` argument, using `...`
only. Alternatively, you can add a `ContinuousDomain` object to the
layout. For the
[`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
function, you must specify `x`/`y` arguments. For other layouts, you
should pass the limits using `...` directly.

## Usage

``` r
continuous_limits(...)
```

## Arguments

- ...:

  A list of two numeric values, specifying the left/lower limit and the
  right/upper limit of the scale.
