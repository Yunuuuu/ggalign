# Build a data frame

**\[stable\]**

This function converts various objects to a data frame.

## Usage

``` r
# S3 method for class 'dendrogram'
fortify_data_frame(
  data,
  ...,
  priority = "right",
  center = FALSE,
  type = "rectangle",
  leaf_pos = NULL,
  leaf_braches = NULL,
  reorder_branches = TRUE,
  branch_gap = NULL,
  root = NULL,
  double = TRUE,
  data_arg = NULL,
  call = NULL
)

# S3 method for class 'hclust'
fortify_data_frame(data, ...)
```

## Arguments

- data:

  A [`hclust`](https://rdrr.io/r/stats/hclust.html) or a
  [`dendrogram`](https://rdrr.io/r/stats/dendrogram.html) object.

- ...:

  Additional arguments passed to `dendrogram` method.

- priority:

  A string of "left" or "right". if we draw from `right` to `left`, the
  left will override the right, so we take the `"left"` as the priority.
  If we draw from `left` to `right`, the right will override the left,
  so we take the `"right"` as priority. This is used by
  [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md)
  to provide support of facet operation in ggplot2.

- center:

  A boolean value. if `TRUE`, nodes are plotted centered with respect to
  all leaves/tips in the branch. Otherwise (default), plot them in the
  middle of the direct child nodes.

- type:

  A string indicates the plot type, `"rectangle"` or `"triangle"`.

- leaf_pos:

  The x-coordinates of the leaf node. Must be the same length of the
  number of observations in `data`.

- leaf_braches:

  Branches of the leaf node. Must be the same length of the number of
  observations in `data`. Usually come from
  [`cutree`](https://rdrr.io/r/stats/cutree.html).

- reorder_branches:

  A single boolean value, indicates whether reorder the provided
  `leaf_braches` based on the actual index.

- branch_gap:

  A single numeric value indicates the gap between different branches.

- root:

  A length one string or numeric indicates the root branch.

- double:

  A single logical value indicating whether horizontal lines should be
  doubled when segments span multiple branches. If `TRUE`, the
  horizontal lines will be repeated for each branch that the segment
  spans. If `FALSE`, only one horizontal line will be drawn. This is
  used by
  [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md)
  to provide support of facet operation in ggplot2.

- data_arg:

  The argument name for `data`. Developers can use it to improve
  messages. Not used by the user.

- call:

  The execution environment where `data` and other arguments for the
  method are collected. Developers can use it to improve messages. Not
  used by the user.

## Value

A `data frame` with the node coordinates:

- `.panel`: Similar with `panel` column, but always give the correct
  panel for usage of the ggplot facet.

- `.index`: the original index in the tree for the the node

- `label`: node label text

- `x` and `y`: x-axis and y-axis coordinates for the node

- `branch`: which branch the node is. You can use this column to color
  different groups.

- `panel`: which panel the node is, if we split the plot into panel
  using
  [facet_grid](https://ggplot2.tidyverse.org/reference/facet_grid.html),
  this column will show which panel the node is from. Note: some nodes
  may fall outside panel (between two panels), so there are possible
  `NA` values in this column.

- `leaf`: A logical value indicates whether the node is a leaf.

## ggalign attributes

`edge`: A `data frame` for edge coordinates:

- `.panel`: Similar with `panel` column, but always give the correct
  panel for usage of the ggplot facet.

- `x` and `y`: x-axis and y-axis coordinates for the start node of the
  edge.

- `xend` and `yend`: the x-axis and y-axis coordinates of the terminal
  node for edge.

- `branch`: which panel the edge is. You can use this column to color
  different groups.

- `panel1` and `panel2`: The panel1 and panel2 columns have the same
  functionality as `panel`, but they are specifically for the `edge`
  data and correspond to both nodes of each edge.

## See also

Other
[`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)
methods:
[`fortify_data_frame.GRanges()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.GRanges.md),
[`fortify_data_frame.character()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.character.md),
[`fortify_data_frame.default()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.default.md),
[`fortify_data_frame.matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.matrix.md),
[`fortify_data_frame.phylo()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.phylo.md)

## Examples

``` r
fortify_data_frame(hclust(dist(USArrests), "ave"))
#>    .index          label         x         y branch  leaf panel .panel
#> 1       9        Florida  1.000000  0.000000   root  TRUE  root   root
#> 2      33 North Carolina  2.000000  0.000000   root  TRUE  root   root
#> 3      NA           <NA>  1.500000 38.527912   root FALSE  root   root
#> 4       5     California  3.000000  0.000000   root  TRUE  root   root
#> 5      20       Maryland  4.000000  0.000000   root  TRUE  root   root
#> 6       3        Arizona  5.000000  0.000000   root  TRUE  root   root
#> 7      31     New Mexico  6.000000  0.000000   root  TRUE  root   root
#> 8      NA           <NA>  5.500000 13.896043   root FALSE  root   root
#> 9      NA           <NA>  4.750000 15.453120   root FALSE  root   root
#> 10     NA           <NA>  3.875000 28.012211   root FALSE  root   root
#> 11      8       Delaware  7.000000  0.000000   root  TRUE  root   root
#> 12      1        Alabama  8.000000  0.000000   root  TRUE  root   root
#> 13     18      Louisiana  9.000000  0.000000   root  TRUE  root   root
#> 14     NA           <NA>  8.500000 15.454449   root FALSE  root   root
#> 15     NA           <NA>  7.750000 16.891499   root FALSE  root   root
#> 16     13       Illinois 10.000000  0.000000   root  TRUE  root   root
#> 17     32       New York 11.000000  0.000000   root  TRUE  root   root
#> 18     NA           <NA> 10.500000  6.236986   root FALSE  root   root
#> 19     22       Michigan 12.000000  0.000000   root  TRUE  root   root
#> 20     28         Nevada 13.000000  0.000000   root  TRUE  root   root
#> 21     NA           <NA> 12.500000 13.297368   root FALSE  root   root
#> 22     NA           <NA> 11.500000 18.417331   root FALSE  root   root
#> 23     NA           <NA>  9.625000 26.363428   root FALSE  root   root
#> 24      2         Alaska 14.000000  0.000000   root  TRUE  root   root
#> 25     24    Mississippi 15.000000  0.000000   root  TRUE  root   root
#> 26     40 South Carolina 16.000000  0.000000   root  TRUE  root   root
#> 27     NA           <NA> 15.500000 21.167192   root FALSE  root   root
#> 28     NA           <NA> 14.750000 28.095803   root FALSE  root   root
#> 29     NA           <NA> 12.187500 39.394633   root FALSE  root   root
#> 30     NA           <NA>  8.031250 44.283922   root FALSE  root   root
#> 31     NA           <NA>  4.765625 77.605024   root FALSE  root   root
#> 32     47     Washington 17.000000  0.000000   root  TRUE  root   root
#> 33     37         Oregon 18.000000  0.000000   root  TRUE  root   root
#> 34     50        Wyoming 19.000000  0.000000   root  TRUE  root   root
#> 35     36       Oklahoma 20.000000  0.000000   root  TRUE  root   root
#> 36     46       Virginia 21.000000  0.000000   root  TRUE  root   root
#> 37     NA           <NA> 20.500000  7.355270   root FALSE  root   root
#> 38     NA           <NA> 19.750000 10.736739   root FALSE  root   root
#> 39     NA           <NA> 18.875000 12.878100   root FALSE  root   root
#> 40     NA           <NA> 17.937500 16.425489   root FALSE  root   root
#> 41     39   Rhode Island 22.000000  0.000000   root  TRUE  root   root
#> 42     21  Massachusetts 23.000000  0.000000   root  TRUE  root   root
#> 43     30     New Jersey 24.000000  0.000000   root  TRUE  root   root
#> 44     NA           <NA> 23.500000 11.456439   root FALSE  root   root
#> 45     NA           <NA> 22.750000 22.595978   root FALSE  root   root
#> 46     NA           <NA> 20.343750 26.713777   root FALSE  root   root
#> 47     25       Missouri 25.000000  0.000000   root  TRUE  root   root
#> 48      4       Arkansas 26.000000  0.000000   root  TRUE  root   root
#> 49     42      Tennessee 27.000000  0.000000   root  TRUE  root   root
#> 50     NA           <NA> 26.500000 12.614278   root FALSE  root   root
#> 51     NA           <NA> 25.750000 20.198479   root FALSE  root   root
#> 52     10        Georgia 28.000000  0.000000   root  TRUE  root   root
#> 53      6       Colorado 29.000000  0.000000   root  TRUE  root   root
#> 54     43          Texas 30.000000  0.000000   root  TRUE  root   root
#> 55     NA           <NA> 29.500000 14.501034   root FALSE  root   root
#> 56     NA           <NA> 28.750000 23.972143   root FALSE  root   root
#> 57     NA           <NA> 27.250000 29.054195   root FALSE  root   root
#> 58     NA           <NA> 23.796875 44.837933   root FALSE  root   root
#> 59     12          Idaho 31.000000  0.000000   root  TRUE  root   root
#> 60     27       Nebraska 32.000000  0.000000   root  TRUE  root   root
#> 61     17       Kentucky 33.000000  0.000000   root  TRUE  root   root
#> 62     26        Montana 34.000000  0.000000   root  TRUE  root   root
#> 63     NA           <NA> 33.500000  3.834058   root FALSE  root   root
#> 64     NA           <NA> 32.750000 12.438692   root FALSE  root   root
#> 65     NA           <NA> 31.875000 15.026107   root FALSE  root   root
#> 66     35           Ohio 35.000000  0.000000   root  TRUE  root   root
#> 67     44           Utah 36.000000  0.000000   root  TRUE  root   root
#> 68     NA           <NA> 35.500000  6.637771   root FALSE  root   root
#> 69     14        Indiana 37.000000  0.000000   root  TRUE  root   root
#> 70     16         Kansas 38.000000  0.000000   root  TRUE  root   root
#> 71     NA           <NA> 37.500000  3.929377   root FALSE  root   root
#> 72      7    Connecticut 39.000000  0.000000   root  TRUE  root   root
#> 73     38   Pennsylvania 40.000000  0.000000   root  TRUE  root   root
#> 74     NA           <NA> 39.500000  8.027453   root FALSE  root   root
#> 75     NA           <NA> 38.500000 13.352260   root FALSE  root   root
#> 76     NA           <NA> 37.000000 15.122897   root FALSE  root   root
#> 77     NA           <NA> 34.437500 20.598507   root FALSE  root   root
#> 78     11         Hawaii 41.000000  0.000000   root  TRUE  root   root
#> 79     48  West Virginia 42.000000  0.000000   root  TRUE  root   root
#> 80     19          Maine 43.000000  0.000000   root  TRUE  root   root
#> 81     41   South Dakota 44.000000  0.000000   root  TRUE  root   root
#> 82     NA           <NA> 43.500000  8.537564   root FALSE  root   root
#> 83     NA           <NA> 42.750000 10.771175   root FALSE  root   root
#> 84     34   North Dakota 45.000000  0.000000   root  TRUE  root   root
#> 85     45        Vermont 46.000000  0.000000   root  TRUE  root   root
#> 86     NA           <NA> 45.500000 13.044922   root FALSE  root   root
#> 87     23      Minnesota 47.000000  0.000000   root  TRUE  root   root
#> 88     49      Wisconsin 48.000000  0.000000   root  TRUE  root   root
#> 89     15           Iowa 49.000000  0.000000   root  TRUE  root   root
#> 90     29  New Hampshire 50.000000  0.000000   root  TRUE  root   root
#> 91     NA           <NA> 49.500000  2.291288   root FALSE  root   root
#> 92     NA           <NA> 48.750000 10.184218   root FALSE  root   root
#> 93     NA           <NA> 47.875000 18.993398   root FALSE  root   root
#> 94     NA           <NA> 46.687500 27.779904   root FALSE  root   root
#> 95     NA           <NA> 44.718750 33.117815   root FALSE  root   root
#> 96     NA           <NA> 42.859375 41.094765   root FALSE  root   root
#> 97     NA           <NA> 38.648438 54.746831   root FALSE  root   root
#> 98     NA           <NA> 31.222656 89.232093   root FALSE  root   root
```
