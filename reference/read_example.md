# Read Example Data

This function reads example data from the file. If no file is specified,
it returns a list of available example files.

## Usage

``` r
read_example(file = NULL)
```

## Arguments

- file:

  A string representing the name of the example file to be read. If
  `NULL`, the function will return a list of available example file
  names.

## Value

If `file` is `NULL`, returns a character vector of available example
file names. Otherwise, returns the contents of the specified example
file, read as an R object.

## Examples

``` r
read_example()
#> [1] "gene_expression.rds"   "measles.rds"           "ref_cytoband_hg19.rds"
#> [4] "ref_cytoband_hg38.rds"
```
