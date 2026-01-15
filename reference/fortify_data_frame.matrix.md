# Build a data frame

**\[stable\]**

This function converts various objects to a data frame.

## Usage

``` r
# S3 method for class 'matrix'
fortify_data_frame(data, lvls = NULL, ..., data_arg = NULL, call = NULL)

# S3 method for class 'DelayedMatrix'
fortify_data_frame(data, ...)

# S3 method for class 'Matrix'
fortify_data_frame(data, ...)
```

## Arguments

- data:

  A matrix-like object.

- lvls:

  A logical value indicating whether to restore factor levels using
  those stored in
  [`ggalign_lvls()`](https://yunuuuu.github.io/ggalign/reference/ggalign_attr.md),
  or a character vector specifying custom levels for the `value` column.
  If levels are provided or restored, the `value` column will be
  returned as a factor.

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

Matrix will be transformed into a long-form data frame, where each row
represents a unique combination of matrix indices and their
corresponding values. The resulting data frame will contain the
following columns:

- `.row_names` and `.row_index`: the row names (only applicable when
  names exist) and an integer representing the row index of the original
  matrix.

- `.column_names` and `.column_index`: the column names (only applicable
  when names exist) and column index of the original matrix.

- `value`: the matrix value, returned as a factor if levels are
  specified or restored.

## See also

Other
[`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)
methods:
[`fortify_data_frame.GRanges()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.GRanges.md),
[`fortify_data_frame.character()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.character.md),
[`fortify_data_frame.default()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.default.md),
[`fortify_data_frame.dendrogram()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.dendrogram.md),
[`fortify_data_frame.phylo()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.phylo.md)
