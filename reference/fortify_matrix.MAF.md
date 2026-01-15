# Build a Matrix for OncoPrint

Convert `MAF` object to a matrix:

- `fortify_matrix.MAF`: Extract genomic alterations for genes.

- `fortify_matrix.MAF_pathways`: Extract genomic alterations for
  pathways.
  [`tune.MAF()`](https://yunuuuu.github.io/ggalign/reference/tune.MAF.md)
  helps convert `MAF` object to a `MAF_pathways` object.

## Usage

``` r
# S3 method for class 'MAF'
fortify_matrix(
  data,
  ...,
  genes = NULL,
  n_top = NULL,
  remove_empty_genes = TRUE,
  remove_empty_samples = TRUE,
  collapse_vars = TRUE,
  use_syn = TRUE,
  missing_genes = "error",
  data_arg = NULL,
  call = NULL
)

# S3 method for class 'MAF_pathways'
fortify_matrix(
  data,
  ...,
  pathdb = "smgbp",
  remove_empty_pathways = TRUE,
  remove_empty_samples = TRUE,
  data_arg = NULL,
  call = NULL
)
```

## Arguments

- data:

  A [`MAF`](https://rdrr.io/pkg/maftools/man/read.maf.html) object.

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

- collapse_vars:

  A single boolean value indicating whether to collapse multiple
  alterations in the same sample and gene into a single value
  `"Multi_Hit"`. Alternatively, you can provide a single string
  indicates the collapsed values.

- use_syn:

  A single boolean value indicates whether to include synonymous
  variants when Classifies SNPs into transitions and transversions.

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

- pathdb:

  A string of `"smgbp"` or `"sigpw"`, or a named list of genes to define
  the pathways.

- remove_empty_pathways:

  A single boolean value indicats whether to drop pathways without any
  genomic alterations.

## ggalign attributes

For `fortify_matrix.MAF`:

- `gene_summary`: A data frame of gene summary informations. See
  [`maftools::getGeneSummary()`](https://rdrr.io/pkg/maftools/man/getGeneSummary.html)
  for details.

- `sample_summary`: A data frame of sample summary informations. See
  [`maftools::getSampleSummary()`](https://rdrr.io/pkg/maftools/man/getSampleSummary.html)
  for details.

- `sample_anno`: A data frame of sample clinical informations. See
  [`maftools::getClinicalData()`](https://rdrr.io/pkg/maftools/man/getClinicalData.html)
  for details.

- `variant_weights`: A data frame of variant weights. Each gene in a
  sample is assigned a total weight of `1`. When multiple variants occur
  in the same gene-sample pair, the weight for each variant reflects its
  proportion of the total.

- `n_genes`: Total number of genes.

- `n_samples`: Total number of samples.

- `titv`: A list of data frame with Transitions and Transversions
  summary. See
  [`maftools::titv()`](https://rdrr.io/pkg/maftools/man/titv.html) for
  details.

The levels of `Variant_Classification` will be stored in
[`ggalign_lvls()`](https://yunuuuu.github.io/ggalign/reference/ggalign_attr.md).
If they do not exist, alphabetical ordering will be used.

For `fortify_matrix.MAF_pathways`:

- `gene_list`: the pathway contents.

- `pathway_summary`: pathway summary informations. See
  [`maftools::pathways()`](https://rdrr.io/pkg/maftools/man/pathways.html)
  for details.

- `sample_summary`: sample summary informations. See
  [`maftools::getSampleSummary()`](https://rdrr.io/pkg/maftools/man/getSampleSummary.html)
  for details.

- `sample_anno`: sample clinical informations. See
  [`maftools::getClinicalData()`](https://rdrr.io/pkg/maftools/man/getClinicalData.html)
  for details.

## See also

Other
[`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
methods:
[`fortify_matrix.GISTIC()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.GISTIC.md),
[`fortify_matrix.default()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.default.md),
[`fortify_matrix.list_upset()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.list_upset.md),
[`fortify_matrix.matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix.md),
[`fortify_matrix.matrix_oncoplot()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix_oncoplot.md),
[`fortify_matrix.matrix_upset()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.matrix_upset.md)
