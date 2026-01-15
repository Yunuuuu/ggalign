# Sort matrix for better visualization

Helper function used to order the Oncoplot samples. Typically, you would
use this in combination with
[`align_order2()`](https://yunuuuu.github.io/ggalign/reference/align_order2.md),
e.g., `align_order2(memo_order)`.

## Usage

``` r
memo_order(x)
```

## Arguments

- x:

  A matrix, where `NA` values will be treated as empty.

## Value

A vector of ordering weights.
