# Create a Circular Layout for Genomic Data

`circle_genomic()` constructs a circular layout specifically for genomic
data. It is a specialized variant of
[`circle_continuous()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md)
that applies default axis limits and coerces the first column of each
plot's data to use chromosome (`seqname`) identifiers-matching those in
the layout data-as factor levels.

## Usage

``` r
circle_genomic(
  data,
  ...,
  radial = NULL,
  direction = "outward",
  sector_spacing = NULL,
  theme = NULL
)
```

## Arguments

- data:

  The input data, which can be:

  - A `character` string ("hg19" or "hg38") to load a predefined
    cytoband reference.

  - A `data.frame` with at least three columns: `chromosome`, `start`,
    and `end` positions.

  - A genomic object convertible via
    [`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md).

- ...:

  Additional arguments passed to specific methods or
  [`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md).

- radial:

  A
  [`coord_circle()`](https://yunuuuu.github.io/ggalign/reference/coord_circle.md)/[`coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
  object that defines the global parameters for coordinate across all
  plots in the layout. The parameters `start`, `end`, `direction`, and
  `expand` will be inherited and applied uniformly to all plots within
  the layout. The parameters `theta` and `r.axis.inside` will always be
  ignored and will be set to `"x"` and `TRUE`, respectively, for all
  plots.

- direction:

  A single string of `"inward"` or `"outward"`, indicating the direction
  in which the plot is added.

  - `outward`: The plot is added from the inner to the outer.

  - `inward`: The plot is added from the outer to the inner.

- sector_spacing:

  The size of spacing between different panel. A numeric of the radians
  or a [`rel()`](https://ggplot2.tidyverse.org/reference/element.html)
  object.

- theme:

  A [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  object used to customize various elements of the layout, including
  `guides`, `title`, `subtitle`, `caption`, `margins`, `panel.border`,
  and `background`. By default, the theme will inherit from the parent
  `layout`. It also controls the panel spacing for all plots in the
  layout.

## Value

A `circle_layout` object representing the genomic layout.
