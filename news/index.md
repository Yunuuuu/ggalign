# Changelog

## ggalign (development version)

## ggalign 1.2.0

CRAN release: 2025-10-18

- Fixed alignment of absolute panel dimensions when using
  `theme(panel.widths, panel.heights)` in ggplot2 v4.0.0.

- ’alignpatch-\*’ scripts have been fully refactored with S7 classes,
  with more informative and consistent naming.

- Improved
  [`align_plots()`](https://yunuuuu.github.io/ggalign/reference/alignpatches.md)
  to properly handle strips with different placements.

- Renamed the original `alignpatch()` to
  [`patch()`](https://yunuuuu.github.io/ggalign/reference/patch.md) for
  clearer and more consistent naming.

- The `Patch` object is now exported for external use.

- Refactored `patch_titles` to the new S7-based `patch_title` class
  (soft-deprecated the old version).

- Renamed the original
  [`patch()`](https://yunuuuu.github.io/ggalign/reference/patch.md) to
  [`as_grob()`](https://yunuuuu.github.io/ggalign/reference/as_grob.md)
  for clearer.

## ggalign 1.1.0

CRAN release: 2025-09-11

The internal structure has been fully refactored, with more informative
and consistent naming.

- remove
  [`element_polygon()`](https://ggplot2.tidyverse.org/reference/element.html)/`element_curve()`,
  to be implemented upstream in ggplot2

- new
  [`layout_theme()`](https://yunuuuu.github.io/ggalign/reference/layout_theme.md)
  to control layout theming.

- Deprecated the `theme` argument in
  [`layout_annotation()`](https://yunuuuu.github.io/ggalign/reference/layout_annotation.md).

- Migrated all internal objects to the S7 class system.

- replace
  [`with_quad()`](https://yunuuuu.github.io/ggalign/reference/with_quad.md)
  with
  [`quad_scope()`](https://yunuuuu.github.io/ggalign/reference/quad_scope.md)
  and deprecate old name.

- new
  [`layout_tags()`](https://yunuuuu.github.io/ggalign/reference/layout_tags.md):
  add support for tagging plots and nested layouts.

- `&` operator now can work for
  [`align_plots()`](https://yunuuuu.github.io/ggalign/reference/alignpatches.md).

- new
  [`geom_magick()`](https://yunuuuu.github.io/ggalign/reference/geom_magick.md)
  object to draw images as point shapes using magick

- The `sector_spacing` argument in
  [`facet_sector()`](https://yunuuuu.github.io/ggalign/reference/facet_sector.md)
  can be a multi-value numeric
  ([\#73](https://github.com/Yunuuuu/ggalign/issues/73))

- new argument `split` in
  [`align_phylo()`](https://yunuuuu.github.io/ggalign/reference/align_phylo.md)
  to support splitting phylo objects into multiple trees
  ([\#74](https://github.com/Yunuuuu/ggalign/issues/74))

- The
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
  method for `phylo` object is removed

- new
  [`plot_ideogram()`](https://yunuuuu.github.io/ggalign/reference/plot_ideogram.md)
  function to add an aligned cytoband ideogram plot

## ggalign 1.0.2

CRAN release: 2025-05-14

- [`align_plots()`](https://yunuuuu.github.io/ggalign/reference/alignpatches.md):
  Always ensure that plots placed in a border collect their guides, if
  any guides are to be collected in that border. This prevents overlap,
  unless the guides will be collected by the parent layout.

- new scale for z aesthetic: scale_z_continuous, scale_z_binned,
  scale_z_discrete, scale_z_ordinal, scale_z_datetime, scale_z_date

- [`geom_subrect()`](https://yunuuuu.github.io/ggalign/reference/geom_subrect.md)
  and
  [`geom_subtile()`](https://yunuuuu.github.io/ggalign/reference/geom_subrect.md)
  gain `nrow` and `ncol` arguments to control the layout, and deprecate
  `direction` argument

- [`ggoncoplot()`](https://yunuuuu.github.io/ggalign/reference/ggoncoplot.md)
  gains a new argument `remove_duplicates` to control whether to remove
  duplicated variants within the same cell.

- [`fortify_data_frame.matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.matrix.md)
  gains a new argument `lvls`, allowing matrix values to be converted
  into a factor.

- [`circle_layout()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md):
  fix wrong `sector_spacing` due to the operator precedence when
  building.

- [`circle_layout()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md):
  rename `spacing_theta` argument to `sector_spacing` argument, and
  deprecate `spacing_theta` argument.

- `circle_continous`: gains `spacing_theta` argument to control the
  sector spacing for all plots.

- [`facet_sector()`](https://yunuuuu.github.io/ggalign/reference/facet_sector.md):
  deprecate `radial` argument, user should add the
  [`coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
  or
  [`coord_circle()`](https://yunuuuu.github.io/ggalign/reference/coord_circle.md)
  directly to the plot.

- new
  [`magickGrob()`](https://yunuuuu.github.io/ggalign/reference/magickGrob.md)
  to construct grob with `magick` processing.

## ggalign 1.0.1

CRAN release: 2025-04-10

- fix R CMD check error for the updated version of ggplot2 3.5.2.

- new grob `channelGrob()` used to create grobs across different facet.

- fix `patch.gList` method.

## ggalign 1.0.0

CRAN release: 2025-03-15

This is a major release. While not many new features have been added,
this marks the first release where the entire layout system has been
completed.

### Breaking changes

- align_reorder() has been renamed to align_order2().

- geom_draw2() has been renamed to geom_gshape().

### New features

- Now
  [`circle_layout()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md)
  support multiple facets.

- new
  [`facet_sector()`](https://yunuuuu.github.io/ggalign/reference/facet_sector.md)
  function to add Facet support for polar coordinates

- new
  [`coord_circle()`](https://yunuuuu.github.io/ggalign/reference/coord_circle.md)
  function to provide additional customization options for
  coord_radial()

- new [`tune()`](https://yunuuuu.github.io/ggalign/reference/tune.md)
  method shape (“oncoplot”) for matrix

## ggalign 0.1.0

CRAN release: 2025-02-06

### Breaking changes

- Now, all `*_free()` layouts have been removed, all layout has been
  splitted into two type: align discrete or continous vairables.

### New features

- new helper function
  [`memo_order()`](https://yunuuuu.github.io/ggalign/reference/memo_order.md)
  to reorder the oncoplot samples.

- new
  [`geom_subrect()`](https://yunuuuu.github.io/ggalign/reference/geom_subrect.md)
  and
  [`geom_subtile()`](https://yunuuuu.github.io/ggalign/reference/geom_subrect.md)
  to subdivide rectangles with shared borders into a grid.

- new `cross_link` function to reset the layout ordering index or layout
  panel group, and add plot to connect selected observations.

- new `cross_mark` function to reset the layout ordering index or layout
  panel group, and add plot to annotate observations.

- new
  [`pair_links()`](https://yunuuuu.github.io/ggalign/reference/pair_links.md)
  function to create pairs of observation groups.

- new
  [`mark_draw()`](https://yunuuuu.github.io/ggalign/reference/mark_draw.md),
  [`mark_line()`](https://yunuuuu.github.io/ggalign/reference/mark_line.md),
  [`mark_tetragon()`](https://yunuuuu.github.io/ggalign/reference/mark_tetragon.md)
  to define the links to connect the marked observations.

- new
  [`link_draw()`](https://yunuuuu.github.io/ggalign/reference/link_draw.md),
  [`link_line()`](https://yunuuuu.github.io/ggalign/reference/link_line.md),
  [`link_tetragon()`](https://yunuuuu.github.io/ggalign/reference/link_tetragon.md)
  to define the links to connect a pair of observations

- new
  [`ggmark()`](https://yunuuuu.github.io/ggalign/reference/ggmark.md) to
  add a plot to annotate selected observations.

- new
  [`element_vec()`](https://yunuuuu.github.io/ggalign/reference/element_vec.md)
  functions to apply function to the vectorized fields of the theme
  element object.

- new
  [`no_expansion()`](https://yunuuuu.github.io/ggalign/reference/no_expansion.md)
  function to remove scale expansion.

- new layout
  [`circle_layout()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md)
  to arrange plot in a circular.

- new
  [`raster_magick()`](https://yunuuuu.github.io/ggalign/reference/raster_magick.md)
  function to post-processing the image raster with `magick` package was
  added.

- new
  [`fortify_matrix.GISTIC()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.GISTIC.md)
  method for `GISTIC` object from `maftools` package
  ([\#24](https://github.com/Yunuuuu/ggalign/issues/24)).

- `+` operator now will respect
  [`with_quad()`](https://yunuuuu.github.io/ggalign/reference/with_quad.md).

- new `ggalign_attr_set()` and `ggalign_attr_get()` for basic operations
  of attached attribute.

- new
  [`geom_draw()`](https://yunuuuu.github.io/ggalign/reference/geom_draw.md)
  function to draw ggplot2 layer with customized draw function.

### Bug fixes

- fix breaks and labels not work well in discrete scale
  ([\#30](https://github.com/Yunuuuu/ggalign/issues/30),
  [\#32](https://github.com/Yunuuuu/ggalign/issues/32))

- fix wrong results of
  [`fortify_matrix.MAF()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.MAF.md)
  method

### Improvements

- Now,
  [`with_quad()`](https://yunuuuu.github.io/ggalign/reference/with_quad.md)
  wraps the object into a list with the class `with_quad`. This approach
  offers greater flexibility in defining the actions associated with
  `with_quad`, allowing for more customizable behavior
  ([\#26](https://github.com/Yunuuuu/ggalign/issues/26),
  [@Yunuuuu](https://github.com/Yunuuuu)).

- Now, we always set as.table = FALSE when using ggplot2 `facet_*()`
  functions, we don’t need to reorder the `panel` when drawing.

- `free_gg`, `align` and the new `cross_link` objects have been
  collapsed to one class `ggalign_plot`

## ggalign 0.0.5

CRAN release: 2024-11-14

### Breaking changes

- `direction` argument in
  [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)/`ggstack()`
  is soft-deprecated now, user must provide it manually.

- `set_context`, `order`, and `name` arguments in `align_*()` and
  [`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  are soft-deprecated, which is advised to use `active` argument.

- `guides`, `free_guides`, `free_spaces`, `free_labs`, and `plot_data`
  arguments in `align_*()` and
  [`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  are deprecated, which is advised to use `scheme_align` function
  instead.

- `theme` argument in `align_*()` and
  [`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  is deprecated, which is advised to use `plot_theme` function.

- `what` argument in
  [`stack_active()`](https://yunuuuu.github.io/ggalign/reference/stack_switch.md)
  is soft-deprecated, user is advised to use
  [`stack_switch()`](https://yunuuuu.github.io/ggalign/reference/stack_switch.md)
  to change the active plot with `what`.

- [`align_panel()`](https://yunuuuu.github.io/ggalign/reference/align_panel.md)/[`ggpanel()`](https://yunuuuu.github.io/ggalign/reference/align_panel.md)
  is deprecated, user is advised to use `ggalign(data = NULL)` instead.

### New features

- new
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
  to create layout allowing free from alignment.

- new
  [`quad_alignv()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md),
  [`quad_alignh()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md),
  [`quad_free()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)/[`ggside()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md),
  [`quad_alignb()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
  alias for
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md).

- [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  now gains a `type` argument to control whether the layout should align
  observations.

- new
  [`ggoncoplot()`](https://yunuuuu.github.io/ggalign/reference/ggoncoplot.md)
  function to draw oncoprint.

- new
  [`ggfree()`](https://yunuuuu.github.io/ggalign/reference/ggfree.md)
  function to add ggplot to the layout.

- new
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
  function to convert any objects to a matrix, and add
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
  method for `MAF` object.

- new
  [`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)
  function to convert any objects to a data frame.

- now, a special attribute `ggalign` can be used to pass additional
  informations across the building process, and a new function
  [`ggalign_attr()`](https://yunuuuu.github.io/ggalign/reference/ggalign_attr.md)
  can be used to extract the data from the attribute.

- new `plot_theme()` function to control the default theme of the plot
  in layout.

- new `plot_data()` function to control the data transformation of the
  plot in layout.

- new
  [`scheme_align()`](https://yunuuuu.github.io/ggalign/reference/scheme_align.md)
  function to control specifications of the plot in layout.

- new
  [`with_quad()`](https://yunuuuu.github.io/ggalign/reference/with_quad.md)
  function to control the `-` operator context.

- new
  [`quad_init()`](https://yunuuuu.github.io/ggalign/reference/quad_init.md)
  function to initialize the annotation with self-provided data.

- new
  [`stack_switch()`](https://yunuuuu.github.io/ggalign/reference/stack_switch.md)
  and
  [`quad_switch()`](https://yunuuuu.github.io/ggalign/reference/quad_switch.md)
  to switch active plots for
  [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  and
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md).

- new
  [`anno_top()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md),
  [`anno_left()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md),
  [`anno_bottom()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)
  and
  [`anno_right()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)
  alias for add annotation in
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md).

- [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md)
  gain a new argument `cutree` to customize the process of tree cutting.

- `reorder_dendrogram` argument in
  [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md)
  function now can accept a function to reorder the tree.

- new
  [`layer_order()`](https://yunuuuu.github.io/ggalign/reference/layer_order.md)
  function to change the ggplot2 layer adding order.

- new
  [`theme_no_axes()`](https://yunuuuu.github.io/ggalign/reference/theme_no_axes.md)
  function to remove axis elements.

- new
  [`geom_pie()`](https://yunuuuu.github.io/ggalign/reference/geom_pie.md)
  function to draw pie charts.

### Bug fixes

- fix bug when apply `free_border` in `alignpatches` object.

- fix bug when provide a function in `distance` argument of
  [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md)

- fix bug when provide `hclust` or `dendrogram` in method argument of
  [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md)

- fix bug of `coord_ggalign()` for discrete scale

### Improvements

- `merge` function has been implemented with `vctrs` for performance

- for large matrix (\> 20000 cells), automatically use
  [`geom_raster()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)

- {data.table} is now removed from the dependency

## ggalign 0.0.4

CRAN release: 2024-10-12

- `layout_heatmap()`/[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  arguments width/height have been renamed `.width`/`.height` to helps
  avoid conflicts with arguments from
  [`geom_tile()`](https://ggplot2.tidyverse.org/reference/geom_tile.html).

- the nestet alignpatches theme by default will inherit from the parent
  alignpatches

- new
  [`layout_title()`](https://yunuuuu.github.io/ggalign/reference/layout_title.md)
  function, now
  [`layout_annotation()`](https://yunuuuu.github.io/ggalign/reference/layout_annotation.md)
  only control the layout theme

- add
  [`ggrastr::rasterize`](https://rdrr.io/pkg/ggrastr/man/rasterize.html)
  method for both
  [`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  and `ggstack()`

- `set_context` argument in
  [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md)
  now depends on `plot_dendrogram` by default

- [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md)
  gain a new argument `reorder_dendrogram` to control whether to reorder
  the generated dendrogram based on the mean values.

- [`align_order2()`](https://yunuuuu.github.io/ggalign/reference/align_order2.md)
  now splitted into two function
  [`align_order()`](https://yunuuuu.github.io/ggalign/reference/align_order.md)
  and
  [`align_order2()`](https://yunuuuu.github.io/ggalign/reference/align_order2.md)

- `dendrogram_data()` gain a new argument `reorder_branches` to control
  whether to reorder the input `leaf_braches` based on the tree.

- [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md)
  gain new argument `merge_dendrogram` to control whether the
  dendrograms in multiple groups should be merged

- dendrogram height axis will be automatically reversed in bottom and
  left annotation stack

- heatmap fill color scale now default use
  `scale_fill_gradient2(low = "blue", high = "red")` for continuous
  values.

- `Coord` is used to set limits instead of `Scale`

- all `free_*` arguments now accept the same argument of the
  corresponding `free_*` functions.

- new `theme_ggalign()` function for the default theme of ggalign
  package

- export option `ggalign.default_theme` allow user to change the global
  default theme

- new
  [`free_guide()`](https://yunuuuu.github.io/ggalign/reference/free.md)
  function, to override the layout guides argument for single plot

- [`geom_draw()`](https://yunuuuu.github.io/ggalign/reference/geom_draw.md)
  now utilize
  [`patch()`](https://yunuuuu.github.io/ggalign/reference/patch.md)
  function to convert objects into grob

- internal changes: all vectors have been operated with `vctrs` package

- [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md):
  gain `sizes` argument to control the relative sizes of the stack

- `align_()*`: now `order` argument only control the plot area.

- [`align_order2()`](https://yunuuuu.github.io/ggalign/reference/align_order2.md)
  now can accept an ordering character names.

- export
  [`order2()`](https://yunuuuu.github.io/ggalign/reference/order2.md)
  function

- [`align_order2()`](https://yunuuuu.github.io/ggalign/reference/align_order2.md):
  fun argument has been renamed to `order` argument, and it can now
  accept the ordering integer index directly

- `fun` in
  [`align_order2()`](https://yunuuuu.github.io/ggalign/reference/align_order2.md)
  now can return any statistics which can be handled by
  [`order2()`](https://yunuuuu.github.io/ggalign/reference/order2.md)

- fix
  [`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  order not work when put in a stack layout

- `method` in
  [`hclust2()`](https://yunuuuu.github.io/ggalign/reference/hclust2.md)
  can now return any objects which can be coerced into hclust, or you
  can provide such object directly in `method`.

- [`hclust2()`](https://yunuuuu.github.io/ggalign/reference/hclust2.md)
  now can accept `distance = NULL`

- [`hmanno()`](https://yunuuuu.github.io/ggalign/reference/quad_switch.md),
  [`stack_active()`](https://yunuuuu.github.io/ggalign/reference/stack_switch.md),
  and `align_*()` functions gain a new argument `theme` to control the
  theme for the plot.

- new
  [`layout_annotation()`](https://yunuuuu.github.io/ggalign/reference/layout_annotation.md)
  and
  [`layout_theme()`](https://yunuuuu.github.io/ggalign/reference/layout_theme.md)
  to modify the layout components

- `borders` argument in
  [`free_space()`](https://yunuuuu.github.io/ggalign/reference/free.md)
  has been renamed to `spaces`

- {tibble} is now removed from the dependency

- [`align_plots()`](https://yunuuuu.github.io/ggalign/reference/alignpatches.md)
  now can accept `gList`, `function`, `recordedplot`, `trellis`,
  `pheatmap`, `Heatmap`, `HeatmapAnnotation`, and `HeatmapList`, we have
  added `alignpatch` method for these objects.

- add `patch` method for `gList`, `function`, `recordedplot`, `trellis`,
  `pheatmap`, `Heatmap`, `HeatmapAnnotation`, and `HeatmapList`

- fix a bug in `wrap()` function where duplicated names in gtable cause
  some grobs missing

- new [`free_vp()`](https://yunuuuu.github.io/ggalign/reference/free.md)
  function to customize the viewport when aligning.

- `StackLayout` and `HeatmapLayout` methods have been collapsed into one
  method for `Layout` class

- New `Patch` class, now we can extend the alignment process of any
  object and customize the building process with `ggproto`.

- fix missing variable `labs` in the `free_space` method of
  `PatchAlignpatches`

## ggalign 0.0.3

CRAN release: 2024-09-15

- Add package logo

- new [`inset()`](https://yunuuuu.github.io/ggalign/reference/inset.md)
  function

- add package quotes in DESCRIPTION

- add return value docs for exported function or methods

## ggalign 0.0.2

- omit the redudnant description and add single quote for package name

## ggalign 0.0.1

### Features

`ggalign` pacakge provides two layout to arrange ggplot objects:

- [`heatmap_layout()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)/[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md):
  Arrange ggplot into a Heatmap layout. See `vignette("heatmap-layout")`
  for details.

- [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)/`ggstack()`:
  Arrange ggplot vertically or horizontally. See
  `vignette("stack-layout")` for details.

To further customize these layouts, we offer following functions:

- [`align_group()`](https://yunuuuu.github.io/ggalign/reference/align_group.md):
  Group layout axis into panel with a group variable.
- [`align_kmeans()`](https://yunuuuu.github.io/ggalign/reference/align_kmeans.md):
  Group layout axis into panel by kmeans
- [`align_order2()`](https://yunuuuu.github.io/ggalign/reference/align_order2.md):
  Reorders layout observations based on weights or summary statistics.
- [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md):
  Reorder or Group layout based on hierarchical clustering

For more detailed instructions on customizing layouts, see the vignette:
`vignette("layout-customize")`.

Additionally, plots can be added in the layout with following functions:

- `align_gg()`/[`ggalign()`](https://yunuuuu.github.io/ggalign/reference/ggalign.md):
  Create ggplot object with a customized data.
- [`align_panel()`](https://yunuuuu.github.io/ggalign/reference/align_panel.md)/[`ggpanel()`](https://yunuuuu.github.io/ggalign/reference/align_panel.md):
  Create ggplot object with the layout panel data.

For more information on adding plots, refer to the vignette:
`vignette("layout-plot")`.
