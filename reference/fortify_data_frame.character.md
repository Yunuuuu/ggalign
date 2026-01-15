# Build a data frame

**\[stable\]**

This function converts various objects to a data frame.

## Usage

``` r
# S3 method for class 'character'
fortify_data_frame(data, ..., data_arg = NULL, call = NULL)

# S3 method for class 'factor'
fortify_data_frame(data, ..., data_arg = NULL, call = NULL)

# S3 method for class 'numeric'
fortify_data_frame(data, ..., data_arg = NULL, call = NULL)

# S3 method for class 'logical'
fortify_data_frame(data, ..., data_arg = NULL, call = NULL)

# S3 method for class 'complex'
fortify_data_frame(data, ..., data_arg = NULL, call = NULL)
```

## Arguments

- data:

  An object to be converted to a data frame.

- ...:

  These dots are for future extensions and must be empty.

- data_arg:

  The argument name for `data`. Developers can use it to improve
  messages. Not used by the user.

- call:

  The execution environment where `data` and other arguments for the
  method are collected. Developers can use it to improve messages. Not
  used by the user.

## Value

A data frame with following columns:

- `.names`: the names for the vector (only applicable if names exist).

- `value`: the actual value of the vector.

## See also

Other
[`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)
methods:
[`fortify_data_frame.GRanges()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.GRanges.md),
[`fortify_data_frame.default()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.default.md),
[`fortify_data_frame.dendrogram()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.dendrogram.md),
[`fortify_data_frame.matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.matrix.md),
[`fortify_data_frame.phylo()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.phylo.md)
