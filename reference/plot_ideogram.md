# Add an aligned cytoband ideogram plot

Creates a cytoband ideogram-typically representing chromosome banding
patterns-and aligns it within a genomic layout.

Cytoband features (`gieStain`) are mapped to fill colors following
standard cytogenetic conventions (e.g., gpos, gneg, acen, stalk).
Optionally, chromosome names can be displayed as labels.

## Usage

``` r
plot_ideogram(
  mapping = aes(),
  ...,
  seqnames = NULL,
  size = NULL,
  active = NULL
)
```

## Arguments

- mapping:

  Default list of aesthetic mappings to use for plot. If not specified,
  must be supplied in each layer added to the plot.

- ...:

  Arguments passed on to
  [`ggplot2::geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html)

  `stat`

  :   The statistical transformation to use on the data for this layer.
      When using a `geom_*()` function to construct a layer, the `stat`
      argument can be used to override the default coupling between
      geoms and stats. The `stat` argument accepts the following:

      - A `Stat` ggproto subclass, for example `StatCount`.

      - A string naming the stat. To give the stat as a string, strip
        the function name of the `stat_` prefix. For example, to use
        `stat_count()`, give the stat as `"count"`.

      - For more information and other ways to specify the stat, see the
        [layer
        stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
        documentation.

  `position`

  :   A position adjustment to use on the data for this layer. This can
      be used in various ways, including to prevent overplotting and
      improving the display. The `position` argument accepts the
      following:

      - The result of calling a position function, such as
        `position_jitter()`. This method allows for passing extra
        arguments to the position.

      - A string naming the position adjustment. To give the position as
        a string, strip the function name of the `position_` prefix. For
        example, to use `position_jitter()`, give the position as
        `"jitter"`.

      - For more information and other ways to specify the position, see
        the [layer
        position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
        documentation.

  `parse`

  :   If `TRUE`, the labels will be parsed into expressions and
      displayed as described in
      [`?plotmath`](https://rdrr.io/r/grDevices/plotmath.html).

  `size.unit`

  :   How the `size` aesthetic is interpreted: as millimetres (`"mm"`,
      default), points (`"pt"`), centimetres (`"cm"`), inches (`"in"`),
      or picas (`"pc"`).

  `na.rm`

  :   If `FALSE`, the default, missing values are removed with a
      warning. If `TRUE`, missing values are silently removed.

  `show.legend`

  :   logical. Should this layer be included in the legends? `NA`, the
      default, includes if any aesthetics are mapped. `FALSE` never
      includes, and `TRUE` always includes. It can also be a named
      logical vector to finely select the aesthetics to display. To
      include legend keys for all levels, even when no data exists, use
      `TRUE`. If `NA`, all levels are shown in legend, but unobserved
      levels are omitted.

  `inherit.aes`

  :   If `FALSE`, overrides the default aesthetics, rather than
      combining with them. This is most useful for helper functions that
      define both data and aesthetics and shouldn't inherit behaviour
      from the default plot specification, e.g.
      [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

  `check_overlap`

  :   If `TRUE`, text that overlaps previous text in the same layer will
      not be plotted. `check_overlap` happens at draw time and in the
      order of the data. Therefore data should be arranged by the label
      column before calling `geom_text()`. Note that this argument is
      not supported by `geom_label()`.

- seqnames:

  A single logical or numeric value controlling chromosome label
  display. Defaults to `TRUE`.

  - **Logical (`TRUE`/`FALSE`)**:

    - `TRUE`: display labels at the default offset:

      - `1` above the ideogram (vertical layout)

      - `-1` below the ideogram (horizontal layout)

    - `FALSE`: do not display labels.

  - **Numeric**: Specifies the vertical position of labels relative to
    the ideogram’s y-axis:

    - Positive: above the ideogram (offset from the upper border)

    - Negative: below the ideogram (offset from the lower border)

    - `0`: centered.

  Note: The cytoband vertical range spans from `0` to `1`.

- size:

  The relative size of the plot, can be specified as a
  [`unit()`](https://rdrr.io/r/grid/unit.html). Note that for
  [`circle_layout()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md),
  all size values will be interpreted as relative sizes, as this layout
  type adjusts based on the available space in the circular arrangement.

- active:

  A [`active()`](https://yunuuuu.github.io/ggalign/reference/active.md)
  object that defines the context settings when added to a layout.
