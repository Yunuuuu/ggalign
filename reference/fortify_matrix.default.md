# Build a Matrix

By default, it calls [`as.matrix()`](https://rdrr.io/r/base/matrix.html)
to build a matrix.

## Usage

``` r
# Default S3 method
fortify_matrix(data, ..., data_arg = NULL, call = NULL)
```

## Arguments

- data:

  An object to be converted into a matrix.

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

A matrix.

## See also

Other
[`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
methods:
[`fortify_matrix.GISTIC()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.GISTIC.md),
[`fortify_matrix.MAF()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.MAF.md),
[`fortify_matrix.list_upset()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.list_upset.md),
[`fortify_matrix.matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix.md),
[`fortify_matrix.matrix_oncoplot()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix_oncoplot.md),
[`fortify_matrix.matrix_upset()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix_upset.md)
