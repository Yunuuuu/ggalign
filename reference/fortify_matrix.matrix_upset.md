# Build a Matrix for UpSet plot

Converts a matrix suitable for creating an UpSet plot.
[`tune.matrix()`](https://yunuuuu.github.io/ggalign/reference/tune.matrix.md)
helps convert `matrix` object to a `matrix_upset` object.

## Usage

``` r
# S3 method for class 'matrix_upset'
fortify_matrix(data, ..., data_arg = NULL, call = NULL)
```

## Arguments

- data:

  A matrix where each row represents an element, and each column defines
  a set. The values in the matrix indicate whether the element is part
  of the set. Any non-missing value signifies that the element exists in
  the set.

- ...:

  Arguments passed on to
  [`fortify_matrix.list_upset`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.list_upset.md)

  `mode`

  :   A string of `"distinct"`, `"intersect"`, or `"union"` indicates
      the mode to define the set intersections. Check
      <https://jokergoo.github.io/ComplexHeatmap-reference/book/upset-plot.html#upset-mode>
      for details.

- data_arg:

  The argument name for `data`. Developers can use it to improve
  messages. Not used by the user.

- call:

  The execution environment where `data` and other arguments for the
  method are collected. Developers can use it to improve messages. Not
  used by the user.

## ggalign attributes

- `intersection_sizes`: An integer vector indicating the size of each
  intersection.

- `set_sizes`: An integer vector indicating the size of each set.

## See also

[`tune.matrix()`](https://yunuuuu.github.io/ggalign/reference/tune.matrix.md)

Other
[`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
methods:
[`fortify_matrix.GISTIC()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.GISTIC.md),
[`fortify_matrix.MAF()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.MAF.md),
[`fortify_matrix.default()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.default.md),
[`fortify_matrix.list_upset()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.list_upset.md),
[`fortify_matrix.matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix.md),
[`fortify_matrix.matrix_oncoplot()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix_oncoplot.md)
