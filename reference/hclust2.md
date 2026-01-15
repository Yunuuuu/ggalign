# Generate Tree Structures with Hierarchical Clustering

Generate Tree Structures with Hierarchical Clustering

## Usage

``` r
hclust2(
  matrix,
  distance = "euclidean",
  method = "complete",
  use_missing = "pairwise.complete.obs"
)
```

## Arguments

- matrix:

  A numeric matrix, or data frame.

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

## Value

A [hclust](https://rdrr.io/r/stats/hclust.html) object.

## See also

- [cor()](https://rdrr.io/r/stats/cor.html)

- [dist()](https://rdrr.io/r/stats/dist.html)

- [hclust()](https://rdrr.io/r/stats/hclust.html)

## Examples

``` r
hclust2(dist(USArrests), method = "ward.D")
#> 
#> Call:
#> stats::hclust(d = d, method = method)
#> 
#> Cluster method   : ward.D 
#> Distance         : euclidean 
#> Number of objects: 50 
#> 
```
