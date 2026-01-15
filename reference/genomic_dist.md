# Calculate inter-region distances for genomic rainfall plots

This function computes distances between adjacent genomic regions,
grouped by chromosome. Useful for visualizing clustering or dispersion
of genomic features.

## Usage

``` r
genomic_dist(region, mode = NULL)
```

## Arguments

- region:

  A data frame with at least 3 columns: chromosome, start, and end.

- mode:

  How to assign distance for intermediate regions: one of `"min"`,
  `"max"`, `"mean"`, `"left"`, or `"right"`.

## Value

A data frame with an additional `dist` column.

## Details

The distance between two adjacent regions is calculated as the number of
bases between the **end position of the upstream region** and the
**start position of the downstream region**. If two regions overlap or
are adjacent (\<=1 bp apart), the distance is set to `0`. The resulting
distance is assigned to each region according to the selected `mode`:

- `"left"`: assign the distance to the upstream region

- `"right"`: assign to the downstream region

- `"min"` / `"max"` / `"mean"`: for intermediate regions, calculate the
  minimum, maximum, or average of the distances to neighboring regions
