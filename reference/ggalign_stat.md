# Get the statistics from the layout

Get the statistics from the layout

## Usage

``` r
ggalign_stat(x, ...)

# S3 method for class '`ggalign::QuadLayout`'
ggalign_stat(x, position, ...)

# S3 method for class '`ggalign::StackLayout`'
ggalign_stat(x, what, ...)
```

## Arguments

- x:

  A
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)/[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  or
  [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  object.

- ...:

  Arguments passed to methods.

- position:

  A string of `"top"`, `"left"`, `"bottom"`, or `"right"`.

- what:

  A single number or string of the plot elements in the stack layout.

## Value

The statistics
