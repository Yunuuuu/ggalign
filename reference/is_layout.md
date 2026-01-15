# Reports whether `x` is layout object

Reports whether `x` is layout object

## Usage

``` r
is_layout(x)

is_quad_layout(x)

is_stack_layout(x)

is_stack_cross(x)

is_circle_layout(x)

is_heatmap_layout(x)

is_ggheatmap(x)
```

## Arguments

- x:

  An object to test.

## Value

A single boolean value.

## Examples

``` r
is_layout(ggheatmap(1:10))
#> [1] TRUE

# for quad_layout()
is_quad_layout(quad_alignb(1:10))
#> [1] TRUE
is_quad_layout(quad_alignh(1:10))
#> [1] TRUE
is_quad_layout(quad_alignv(1:10))
#> [1] TRUE
is_quad_layout(quad_free(mtcars))
#> [1] TRUE

# for stack_layout()
is_stack_layout(stack_discrete("h", 1:10))
#> [1] TRUE
is_stack_layout(stack_continuous("h", 1:10))
#> [1] TRUE

# for heatmap_layout()
is_heatmap_layout(ggheatmap(1:10))
#> [1] TRUE
is_ggheatmap(ggheatmap(1:10))
#> [1] TRUE
```
