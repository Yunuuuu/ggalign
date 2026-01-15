# Polar coordinates with Facet support

Draw each panel in a sector of the polar coordinate system. If
`facet_sector()` is used in a ggplot, the coordinate system must be
created with
[`coord_circle()`](https://yunuuuu.github.io/ggalign/reference/coord_circle.md)
or
[`coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html).

## Usage

``` r
facet_sector(
  facets,
  sector_spacing = pi/180,
  drop = TRUE,
  radial = deprecated(),
  spacing_theta = deprecated()
)
```

## Arguments

- facets:

  A set of variables or expressions quoted by
  [`vars()`](https://ggplot2.tidyverse.org/reference/vars.html) and
  defining faceting groups on the rows or columns dimension. The
  variables can be named (the names are passed to `labeller`).

  For compatibility with the classic interface, can also be a formula or
  character vector. Use either a one sided formula, `~a + b`, or a
  character vector, `c("a", "b")`.

- sector_spacing:

  The size of spacing between different panel. A numeric of the radians
  or a [`rel()`](https://ggplot2.tidyverse.org/reference/element.html)
  object.

- drop:

  If `TRUE`, the default, all factor levels not used in the data will
  automatically be dropped. If `FALSE`, all factor levels will be shown,
  regardless of whether or not they appear in the data.

- radial:

  **\[deprecated\]** Please add the coordinate system directly to the
  ggplot instead.

- spacing_theta:

  **\[deprecated\]** Please use `sector_spacing` instead.

## Examples

``` r
ggplot(mtcars, aes(disp, mpg)) +
    geom_point() +
    facet_sector(vars(cyl)) +
    coord_circle(
        start = -0.4 * pi, end = 0.4 * pi, inner.radius = 0.3,
        outer.radius = 0.8, expand = TRUE
    )
```
