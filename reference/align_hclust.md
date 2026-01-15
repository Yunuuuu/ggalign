# Reorder or Group observations based on hierarchical clustering

**\[stable\]**

This function aligns observations within the layout according to a
hierarchical clustering tree, enabling reordering or grouping of
elements based on clustering results.

## Usage

``` r
align_hclust(
  distance = "euclidean",
  method = "complete",
  use_missing = "pairwise.complete.obs",
  reorder_dendrogram = FALSE,
  reorder_group = FALSE,
  k = NULL,
  h = NULL,
  cutree = NULL,
  data = NULL,
  active = NULL
)
```

## Arguments

- distance:

  A string of distance measure to be used. This must be one of
  `"euclidean"`, `"maximum"`, `"manhattan"`, `"canberra"`, `"binary"` or
  `"minkowski"`. Correlation coefficient can be also used, including
  `"pearson"`, `"spearman"` or `"kendall"`. In this way, `1 - cor` will
  be used as the distance. In addition, you can also provide a
  [`dist`](https://rdrr.io/r/stats/dist.html) object directly or a
  function return a [`dist`](https://rdrr.io/r/stats/dist.html) object.
  Use `NULL`, if you don't want to calculate the distance.

- method:

  A string of the agglomeration method to be used. This should be (an
  unambiguous abbreviation of) one of `"ward.D"`, `"ward.D2"`,
  `"single"`, `"complete"`, `"average"` (= UPGMA), `"mcquitty"` (=
  WPGMA), `"median"` (= WPGMC) or `"centroid"` (= UPGMC). You can also
  provide a function which accepts the calculated distance (or the input
  matrix if `distance` is `NULL`) and returns a
  [`hclust`](https://rdrr.io/r/stats/hclust.html) object. Alternative,
  you can supply an object which can be coerced to
  [`hclust`](https://rdrr.io/r/stats/hclust.html).

- use_missing:

  An optional character string giving a method for computing covariances
  in the presence of missing values. This must be (an abbreviation of)
  one of the strings `"everything"`, `"all.obs"`, `"complete.obs"`,
  `"na.or.complete"`, or `"pairwise.complete.obs"`. Only used when
  `distance` is a correlation coefficient string.

- reorder_dendrogram:

  A single boolean value indicating whether to reorder the dendrogram
  based on the means. Alternatively, you can provide a custom function
  that accepts an [`hclust`](https://rdrr.io/r/stats/hclust.html) object
  and the data used to generate the tree, returning either an
  [`hclust`](https://rdrr.io/r/stats/hclust.html) or
  [`dendrogram`](https://rdrr.io/r/stats/dendrogram.html) object.
  Default is `FALSE`.

- reorder_group:

  A single boolean value, indicates whether we should do Hierarchical
  Clustering between groups, only used when previous groups have been
  established. Default: `FALSE`.

- k:

  An integer scalar indicates the desired number of groups.

- h:

  A numeric scalar indicates heights where the tree should be cut.

- cutree:

  A function used to cut the
  [`hclust`](https://rdrr.io/r/stats/hclust.html) tree. It should accept
  four arguments: the [`hclust`](https://rdrr.io/r/stats/hclust.html)
  tree object, `distance` (only applicable when `method` is a string or
  a function for performing hierarchical clustering), `k` (the number of
  clusters), and `h` (the height at which to cut the tree). By default,
  [`cutree()`](https://rdrr.io/r/stats/cutree.html) is used.

- data:

  A matrix-like object. By default, it inherits from the layout
  `matrix`.

- active:

  A [`active()`](https://yunuuuu.github.io/ggalign/reference/active.md)
  object that defines the context settings when added to a layout.

## Discrete Axis Alignment

It is important to note that we consider rows as observations, meaning
`vec_size(data)`/`NROW(data)` must match the number of observations
along the axis used for alignment (x-axis for a vertical stack layout,
y-axis for a horizontal stack layout).

## See also

[`hclust2()`](https://yunuuuu.github.io/ggalign/reference/hclust2.md)

## Examples

``` r
# align_hclust won't add a dendrogram
ggheatmap(matrix(rnorm(81), nrow = 9)) +
    anno_top() +
    align_hclust(k = 3L)
#> → heatmap built with `geom_tile()`
```
