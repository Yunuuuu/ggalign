# Build a data frame

**\[stable\]**

This function converts various objects to a data frame.

## Usage

``` r
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

A data frame.

## `fortify_data_frame` method collections

- [`fortify_data_frame.default()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.default.md)

- [`fortify_data_frame.character()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.character.md)

- [`fortify_data_frame.GRanges()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.GRanges.md)

- [`fortify_data_frame.matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.matrix.md)

- [`fortify_data_frame.dendrogram()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.dendrogram.md)

- [`fortify_data_frame.phylo()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.phylo.md)
