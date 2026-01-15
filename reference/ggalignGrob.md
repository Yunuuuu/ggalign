# Generate a plot grob.

Generate a plot grob.

## Usage

``` r
ggalignGrob(x)
```

## Arguments

- x:

  An object to be converted into a
  [grob](https://rdrr.io/r/grid/grid-defunct.html).

## Value

A [`grob()`](https://rdrr.io/r/grid/grid-defunct.html) object.

## See also

- [`ggalign_build()`](https://yunuuuu.github.io/ggalign/reference/ggalign_build.md)
  for the process of building the object.

- [`ggalign_gtable()`](https://yunuuuu.github.io/ggalign/reference/ggalign_gtable.md)
  for converting the object into a table format.

## Examples

``` r
ggalignGrob(ggplot())
#> TableGrob (20 x 17) "layout": 26 grobs
#>     z         cells               name                                   grob
#> 1   0 ( 1-20, 1-17)         background       rect[plot.background..rect.4486]
#> 2   5 ( 9- 9, 7- 7)             spacer                         zeroGrob[NULL]
#> 3   7 (11-11, 7- 7)             axis-l                         zeroGrob[NULL]
#> 4   3 (13-13, 7- 7)             spacer                         zeroGrob[NULL]
#> 5   6 ( 9- 9, 9- 9)             axis-t                         zeroGrob[NULL]
#> 6   1 (11-11, 9- 9)              panel              gTree[panel-1.gTree.4481]
#> 7   9 (13-13, 9- 9)             axis-b                         zeroGrob[NULL]
#> 8   4 ( 9- 9,11-11)             spacer                         zeroGrob[NULL]
#> 9   8 (11-11,11-11)             axis-r                         zeroGrob[NULL]
#> 10  2 (13-13,11-11)             spacer                         zeroGrob[NULL]
#> 11 10 ( 8- 8, 9- 9)             xlab-t                         zeroGrob[NULL]
#> 12 11 (14-14, 9- 9)             xlab-b                         zeroGrob[NULL]
#> 13 12 (11-11, 6- 6)             ylab-l                         zeroGrob[NULL]
#> 14 13 (11-11,12-12)             ylab-r                         zeroGrob[NULL]
#> 15 14 (11-11,15-15)    guide-box-right                         zeroGrob[NULL]
#> 16 15 (11-11, 3- 3)     guide-box-left                         zeroGrob[NULL]
#> 17 16 (17-17, 9- 9)   guide-box-bottom                         zeroGrob[NULL]
#> 18 17 ( 5- 5, 9- 9)      guide-box-top                         zeroGrob[NULL]
#> 19 18 (11-11, 9- 9)   guide-box-inside                         zeroGrob[NULL]
#> 20 19 ( 4- 4, 9- 9)           subtitle zeroGrob[plot.subtitle..zeroGrob.4483]
#> 21 20 ( 3- 3, 9- 9)              title    zeroGrob[plot.title..zeroGrob.4482]
#> 22 21 (18-18, 9- 9)            caption  zeroGrob[plot.caption..zeroGrob.4484]
#> 23 22 ( 7- 7, 9- 9)    patch-title-top                         zeroGrob[NULL]
#> 24 23 (11-11, 5- 5)   patch-title-left                         zeroGrob[NULL]
#> 25 24 (15-15, 9- 9) patch-title-bottom                         zeroGrob[NULL]
#> 26 25 (11-11,13-13)  patch-title-right                         zeroGrob[NULL]
```
