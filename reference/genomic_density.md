# Calculate Genomic Region Density

Computes the density or count of genomic regions in sliding or fixed
windows across the genome. The density can be reported as the percentage
of uncovered bases or the number of overlapping regions within each
window.

## Usage

``` r
genomic_density(
  region,
  window_size = 1e+07,
  n_window = NULL,
  overlap = TRUE,
  mode = c("coverage", "count"),
  seqlengths = NULL
)
```

## Arguments

- region:

  A data frame with at least 3 columns: chromosome, start, and end.

  - Column 1: character or factor, chromosome name.

  - Column 2: numeric, start position (must be \<= end).

  - Column 3: numeric, end position.

- window_size:

  Numeric, the width of each window (default is `1e+07`). Ignored if
  `n_window` is specified.

- n_window:

  Integer, the number of windows per chromosome. If provided, overrides
  `window_size` and evenly splits the chromosome into `n_window`
  (non-overlapping) or `2*n_window - 1` (overlapping) windows.

- overlap:

  Logical, whether to use overlapping windows (default `TRUE`).
  Overlapping windows are spaced by half the window size.

- mode:

  Character, either `"coverage"` or `"count"`:

  - `"count"`: reports the number of regions overlapping each window.

  - `"coverage"`: reports the fraction of each window covered by
    regions.

- seqlengths:

  Optional named vector of chromosome lengths. If missing, the maximum
  `end` value in the input is used as the chromosome length.

## Value

A data frame containing the first three columns from `region`, plus a
fourth column `density`, which represents either the region count or the
coverage percentage, depending on `mode`.

## Details

This function splits the input by chromosome and tiles the genomic space
into windows, optionally overlapping. For each window, it calculates:

- the number of regions that overlap it (if `mode = "count"`), or

- the fraction of bases covered by any region (if `mode = "percent"`).

## Examples

``` r
region <- data.frame(
    chr = rep("chr1", 3),
    start = c(100, 5000000, 15000000),
    end = c(2000000, 7000000, 17000000)
)
genomic_density(region, window_size = 1e7, mode = "count")
#>    chr   start      end density
#> 1 chr1       1 10000000       2
#> 2 chr1 5000001 15000000       2
genomic_density(region, n_window = 3, overlap = FALSE, mode = "coverage")
#>    chr    start      end   density
#> 1 chr1        1  5666667 0.4705710
#> 2 chr1  5666667 11333334 0.2352942
#> 3 chr1 11333334 17000001 0.3529413
```
