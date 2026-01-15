# Build a data frame

**\[stable\]**

This function converts various objects to a data frame.

## Usage

``` r
# S3 method for class 'GRanges'
fortify_data_frame(data, ..., data_arg = NULL, call = NULL)
```

## Arguments

- data:

  An object to be converted to a data frame.

- ...:

  Arguments passed to methods.

- data_arg:

  The argument name for `data`. Developers can use it to improve
  messages. Not used by the user.

- call:

  The execution environment where `data` and other arguments for the
  method are collected. Developers can use it to improve messages. Not
  used by the user.

## Value

A data frame with at least following columns:

- `seqnames`: The sequence (e.g., chromosome) names.

- `start`: The start positions of the ranges.

- `end`: The end positions of the ranges.

- `width`: The width of each range.

## See also

Other
[`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)
methods:
[`fortify_data_frame.character()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.character.md),
[`fortify_data_frame.default()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.default.md),
[`fortify_data_frame.dendrogram()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.dendrogram.md),
[`fortify_data_frame.matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.matrix.md),
[`fortify_data_frame.phylo()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.phylo.md)
