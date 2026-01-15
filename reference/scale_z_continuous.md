# z scales

z scales

## Usage

``` r
scale_z_continuous(name = waiver(), ..., range = c(0.1, 1), guide = "none")

scale_z_binned(name = waiver(), ..., range = c(0.1, 1), guide = "none")

scale_z_discrete(...)

scale_z_ordinal(name = waiver(), ..., range = c(0.1, 1), guide = "none")
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  `waiver()`, the default, the name of the scale is taken from the first
  mapping used for that aesthetic. If `NULL`, the legend title will be
  omitted.

- ...:

  Other arguments passed on to
  [`continuous_scale()`](https://ggplot2.tidyverse.org/reference/continuous_scale.html),
  [`binned_scale()`](https://ggplot2.tidyverse.org/reference/binned_scale.html),
  or
  [`discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)
  as appropriate, to control name, limits, breaks, labels and so forth.

- range:

  Output range of z values. Must larger than 0.

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html) for
  more information.

## See also

[`geom_tile3d()`](https://yunuuuu.github.io/ggalign/reference/geom_rect3d.md)/[`geom_rect3d()`](https://yunuuuu.github.io/ggalign/reference/geom_rect3d.md)

## Examples

``` r
set.seed(7)
mat <- matrix(runif(100), 10)
rownames(mat) <- LETTERS[1:10]
colnames(mat) <- letters[1:10]
ggheatmap(mat,
    filling = FALSE,
    theme = theme(
        legend.box.spacing = unit(10, "mm"),
        plot.margin = margin(t = 15, unit = "mm")
    )
) +
    geom_tile3d(aes(fill = value, z = value, width = 0.8, height = 0.8)) +
    scale_z_continuous(range = c(0.2, 1)) +
    coord_cartesian(clip = "off")
```
