# Build a Matrix for UpSet plot

**\[experimental\]**

This function converts a list into a matrix format suitable for creating
an UpSet plot. It always returns a matrix for a `horizontal` UpSet plot.

## Usage

``` r
# S3 method for class 'list_upset'
fortify_matrix(data, mode = "distinct", ..., data_arg = NULL, call = NULL)
```

## Arguments

- data:

  A list of sets.

- mode:

  A string of `"distinct"`, `"intersect"`, or `"union"` indicates the
  mode to define the set intersections. Check
  <https://jokergoo.github.io/ComplexHeatmap-reference/book/upset-plot.html#upset-mode>
  for details.

- ...:

  These dots are for future extensions and must be empty.

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

[`tune.list()`](https://yunuuuu.github.io/ggalign/reference/tune.list.md)

Other
[`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
methods:
[`fortify_matrix.GISTIC()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.GISTIC.md),
[`fortify_matrix.MAF()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.MAF.md),
[`fortify_matrix.default()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.default.md),
[`fortify_matrix.matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix.md),
[`fortify_matrix.matrix_oncoplot()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix_oncoplot.md),
[`fortify_matrix.matrix_upset()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix_upset.md)
