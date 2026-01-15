# Build a matrix from a maftools object

Build a matrix from a maftools object

## Usage

``` r
# S3 method for class 'GISTIC'
fortify_matrix(
  data,
  ...,
  n_top = NULL,
  bands = NULL,
  ignored_bands = NULL,
  sample_anno = NULL,
  remove_empty_samples = TRUE,
  data_arg = NULL,
  call = NULL
)
```

## Arguments

- data:

  A [`GISTIC`](https://rdrr.io/pkg/maftools/man/readGistic.html) object.

- ...:

  These dots are for future extensions and must be empty.

- n_top:

  A single number indicates how many top bands to be drawn.

- bands:

  An atomic character defines the bands to draw.

- ignored_bands:

  An atomic character defines the bands to be ignored.

- sample_anno:

  A data frame of sample clinical features to be added.

- remove_empty_samples:

  A single boolean value indicating whether to drop samples without any
  genomic alterations.

- data_arg:

  The argument name for `data`. Developers can use it to improve
  messages. Not used by the user.

- call:

  The execution environment where `data` and other arguments for the
  method are collected. Developers can use it to improve messages. Not
  used by the user.

## ggalign attributes

- `sample_anno`: sample clinical informations provided in `sample_anno`.

- `sample_summary`: sample copy number summary informations. See
  `data@cnv.summary` for details.

- `cytoband_summary`: cytoband summary informations. See
  `data@cytoband.summary` for details.

- `gene_summary`: gene summary informations. See `data@gene.summary` for
  details.

- `summary`: A data frame of summary information. See `data@summary` for
  details.

## See also

Other
[`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
methods:
[`fortify_matrix.MAF()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.MAF.md),
[`fortify_matrix.default()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.default.md),
[`fortify_matrix.list_upset()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.list_upset.md),
[`fortify_matrix.matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix.md),
[`fortify_matrix.matrix_oncoplot()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix_oncoplot.md),
[`fortify_matrix.matrix_upset()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix_upset.md)
