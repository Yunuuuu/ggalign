# Create a stack Layout for Genomic Data

`stack_genomic()` constructs a stack layout specifically for genomic
data. It is a specialized variant of
[`stack_continuous()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
that applies default axis limits and coerces the first column of each
plot's data to use chromosome (`seqname`) identifiers-matching those in
the layout data-as factor levels.

## Usage

``` r
stack_genomic(direction, data = NULL, ..., theme = NULL, sizes = NA)

stack_genomicv(data = NULL, ...)

stack_genomich(data = NULL, ...)
```

## Arguments

- direction:

  A string indicating the direction of the stack layout, either
  `"h"`(`horizontal`) or `"v"`(`vertical`).

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

- theme:

  A [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  object used to customize various elements of the layout, including
  `guides`, `title`, `subtitle`, `caption`, `margins`, `panel.border`,
  and `background`. By default, the theme will inherit from the parent
  `layout`. It also controls the panel spacing for all plots in the
  layout.

- sizes:

  A numeric value or a [`unit`](https://rdrr.io/r/grid/unit.html)
  object. When used for the
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
  annotation, it must be of length `1`. When used in the
  [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  with a nested
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md),
  it should be of length `3`, specifying the relative heights (for
  `direction = "h"`) or widths (for `direction = "v"`) to be applied to
  the layout.

## Value

A `stack_layout` object representing the genomic layout.
