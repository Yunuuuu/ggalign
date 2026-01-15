# Build a Matrix for OncoPrint

Converts a matrix suitable for creating an OncoPrint.
[`tune.matrix()`](https://yunuuuu.github.io/ggalign/reference/tune.matrix.md)
helps convert `matrix` object to a `matrix_oncoplot` object.

## Usage

``` r
# S3 method for class 'matrix_oncoplot'
fortify_matrix(
  data,
  ...,
  genes = NULL,
  n_top = NULL,
  remove_empty_genes = TRUE,
  remove_empty_samples = TRUE,
  missing_genes = "error",
  data_arg = NULL,
  call = NULL
)
```

## Arguments

- data:

  A matrix where each row represents an genes, and each column
  represents samples. The values in the matrix indicate whether the
  element is part of the set.

- ...:

  These dots are for future extensions and must be empty.

- genes:

  An atomic character defines the genes to draw.

- n_top:

  A single number indicates how many top genes to be drawn.

- remove_empty_genes:

  A single boolean value indicats whether to drop genes without any
  genomic alterations.

- remove_empty_samples:

  A single boolean value indicats whether to drop samples without any
  genomic alterations.

- missing_genes:

  A string, either `"error"` or `"remove"`, specifying the action for
  handling missing genes.

- data_arg:

  The argument name for `data`. Developers can use it to improve
  messages. Not used by the user.

- call:

  The execution environment where `data` and other arguments for the
  method are collected. Developers can use it to improve messages. Not
  used by the user.

## ggalign attributes

- `gene_summary`: An integer vector of the altered samples for each
  gene.

- `sample_summary`: An integer vector of the altered genes for each
  sample.

- `n_genes`: Total number of genes.

- `n_samples`: Total number of samples.

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
[`fortify_matrix.matrix_upset()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix_upset.md)
