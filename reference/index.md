# Package index

## Initialize Layout

A `Layout` object defines how to place the plots.

- [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  [`stack_horizontal()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  [`stack_vertical()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  [`stack_discrete()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  [`stack_discretev()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  [`stack_discreteh()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  [`stack_continuous()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  [`stack_continuousv()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  [`stack_continuoush()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  **\[stable\]** : Arrange plots horizontally or vertically
- [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
  [`quad_alignh()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
  [`quad_alignv()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
  [`quad_discrete()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
  [`quad_continuous()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
  **\[stable\]** : Arrange plots in the quad-side of a main plot
- [`circle_layout()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md)
  [`circle_discrete()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md)
  [`circle_continuous()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md)
  **\[experimental\]** : Arrange plots in a circular layout
- [`stack_cross()`](https://yunuuuu.github.io/ggalign/reference/stack_cross.md)
  [`stack_crossv()`](https://yunuuuu.github.io/ggalign/reference/stack_cross.md)
  [`stack_crossh()`](https://yunuuuu.github.io/ggalign/reference/stack_cross.md)
  **\[experimental\]** : Arrange plots crosswise horizontally or
  vertically
- [`heatmap_layout()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  **\[stable\]** : Create a heatmap
- [`ggoncoplot()`](https://yunuuuu.github.io/ggalign/reference/ggoncoplot.md)
  **\[stable\]** : Create an OncoPrint
- [`ggupset()`](https://yunuuuu.github.io/ggalign/reference/ggupset.md)
  **\[experimental\]** : Create an UpSet plot
- [`circle_genomic()`](https://yunuuuu.github.io/ggalign/reference/circle_genomic.md)
  : Create a Circular Layout for Genomic Data
- [`stack_genomic()`](https://yunuuuu.github.io/ggalign/reference/stack_genomic.md)
  [`stack_genomicv()`](https://yunuuuu.github.io/ggalign/reference/stack_genomic.md)
  [`stack_genomich()`](https://yunuuuu.github.io/ggalign/reference/stack_genomic.md)
  : Create a stack Layout for Genomic Data

## Build Layout

All plots begin with a call to
[`circle_layout()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md),
[`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md),
or
[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)/[`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md),
supplying default data. You then reorder the observations, or split the
observations into groups, and add plots, layers, scales, coords with
`+`. Use
[`quad_switch()`](https://yunuuuu.github.io/ggalign/reference/quad_switch.md)/[`hmanno()`](https://yunuuuu.github.io/ggalign/reference/quad_switch.md)
or
[`stack_switch()`](https://yunuuuu.github.io/ggalign/reference/stack_switch.md)
to switch the active context. To save a plot to disk, use `ggsave()`.

- [`quad_switch()`](https://yunuuuu.github.io/ggalign/reference/quad_switch.md)
  [`hmanno()`](https://yunuuuu.github.io/ggalign/reference/quad_switch.md)
  **\[stable\]** : Determine the Active Context of Quad-Layout

- [`quad_active()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)
  [`quad_anno()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)
  [`anno_top()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)
  [`anno_left()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)
  [`anno_bottom()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)
  [`anno_right()`](https://yunuuuu.github.io/ggalign/reference/quad_active.md)
  **\[stable\]** : Determine the Active Context of Quad-Layout

- [`quad_scope()`](https://yunuuuu.github.io/ggalign/reference/quad_scope.md)
  **\[experimental\]** :

  Modify operated Context in
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)

- [`stack_switch()`](https://yunuuuu.github.io/ggalign/reference/stack_switch.md)
  [`stack_active()`](https://yunuuuu.github.io/ggalign/reference/stack_switch.md)
  **\[stable\]** : Determine the active context of stack layout

- [`circle_switch()`](https://yunuuuu.github.io/ggalign/reference/circle_switch.md)
  **\[stable\]** : Determine the active context of circle layout

- [`active()`](https://yunuuuu.github.io/ggalign/reference/active.md)
  **\[experimental\]** : Plot Adding Context Settings

- [`ggalign_attr()`](https://yunuuuu.github.io/ggalign/reference/ggalign_attr.md)
  [`ggalign_lvls()`](https://yunuuuu.github.io/ggalign/reference/ggalign_attr.md)
  : Get Data from the Attribute Attached by ggalign

- [`continuous_limits()`](https://yunuuuu.github.io/ggalign/reference/continuous_limits.md)
  : Set continuous limits for the layout

- [`layout-operator`](https://yunuuuu.github.io/ggalign/reference/layout-operator.md)
  **\[experimental\]** : Layout operator

## Customize Layout

Fine-tune the arrangement of plots by reordering or splitting axes into
separate panels. These functions allow for more detailed customization
of plot layouts.

- [`align_hclust()`](https://yunuuuu.github.io/ggalign/reference/align_hclust.md)
  **\[stable\]** : Reorder or Group observations based on hierarchical
  clustering
- [`align_group()`](https://yunuuuu.github.io/ggalign/reference/align_group.md)
  **\[stable\]** : Group and align observations based on a group vector
- [`align_kmeans()`](https://yunuuuu.github.io/ggalign/reference/align_kmeans.md)
  **\[stable\]** : Split observations by k-means clustering groups.
- [`align_order()`](https://yunuuuu.github.io/ggalign/reference/align_order.md)
  **\[stable\]** : Order observations based on weights
- [`align_order2()`](https://yunuuuu.github.io/ggalign/reference/align_order2.md)
  : Reorders layout observations based on specific statistics.
- [`cross_none()`](https://yunuuuu.github.io/ggalign/reference/cross_none.md)
  : Reset layout ordering and panel group
- [`memo_order()`](https://yunuuuu.github.io/ggalign/reference/memo_order.md)
  : Sort matrix for better visualization

## Initialize Plot

This section covers essential functions to add individual plots to the
layout, ensuring they align and interact correctly within the defined
structure.

- [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md)
  : Plot dendrogram tree
- [`align_phylo()`](https://yunuuuu.github.io/ggalign/reference/align_phylo.md)
  : Plot Phylogenetics tree
- [`ggfree()`](https://yunuuuu.github.io/ggalign/reference/ggfree.md)
  **\[experimental\]** : Add ggplot to layout without alignment
- [`ggalign()`](https://yunuuuu.github.io/ggalign/reference/ggalign.md)
  **\[stable\]** : Add ggplot by Aligning discrete or continuous
  variable
- [`ggmark()`](https://yunuuuu.github.io/ggalign/reference/ggmark.md) :
  Add a plot to annotate selected observations
- [`ggcross()`](https://yunuuuu.github.io/ggalign/reference/ggcross.md)
  : Connect two layout crosswise
- [`cross_mark()`](https://yunuuuu.github.io/ggalign/reference/cross_mark.md)
  : Add a plot to annotate observations
- [`cross_link()`](https://yunuuuu.github.io/ggalign/reference/cross_link.md)
  : Add a plot to connect selected observations
- [`plot_ideogram()`](https://yunuuuu.github.io/ggalign/reference/plot_ideogram.md)
  : Add an aligned cytoband ideogram plot

## Style Links

Functions to control the appearance and geometry of visual links between
elements.

- [`mark_line()`](https://yunuuuu.github.io/ggalign/reference/mark_line.md)
  : Link the observations and the panel with a line
- [`mark_tetragon()`](https://yunuuuu.github.io/ggalign/reference/mark_tetragon.md)
  : Link the observations and the panel with a quadrilateral
- [`mark_triangle()`](https://yunuuuu.github.io/ggalign/reference/mark_triangle.md)
  : Link the observations and the panel with a triangle
- [`mark_draw()`](https://yunuuuu.github.io/ggalign/reference/mark_draw.md)
  : Define the links to connect the marked observations
- [`link_line()`](https://yunuuuu.github.io/ggalign/reference/link_line.md)
  : Link the paired observations with a line
- [`link_tetragon()`](https://yunuuuu.github.io/ggalign/reference/link_tetragon.md)
  : Link the paired observations with a quadrilateral
- [`link_draw()`](https://yunuuuu.github.io/ggalign/reference/link_draw.md)
  : Define the links to connect a pair of observations
- [`pair_links()`](https://yunuuuu.github.io/ggalign/reference/pair_links.md)
  [`range_link()`](https://yunuuuu.github.io/ggalign/reference/pair_links.md)
  : Helper function to create pairs of observation groups

## Schemes

Schemes control the specific behaviour of plots in the layout and can be
applied either globally to the layout or individually to specific plots.

- [`scheme_align()`](https://yunuuuu.github.io/ggalign/reference/scheme_align.md)
  **\[experimental\]** : Align Specifications in the Layout
- [`scheme_data()`](https://yunuuuu.github.io/ggalign/reference/scheme_data.md)
  **\[experimental\]** : Plot data Specifications
- [`scheme_theme()`](https://yunuuuu.github.io/ggalign/reference/scheme_theme.md)
  **\[experimental\]** : Plot default theme

## Data-Free Composition

This section outlines essential functions for composing plots into a
grid, intended for internal use. The core code is derived from the
`patchwork` package, with the hope of eventually integrating and merging
these functions into the `patchwork` project.

- [`alignpatches()`](https://yunuuuu.github.io/ggalign/reference/alignpatches.md)
  [`align_plots()`](https://yunuuuu.github.io/ggalign/reference/alignpatches.md)
  : Arrange multiple plots into a grid
- [`free_align()`](https://yunuuuu.github.io/ggalign/reference/free.md)
  [`free_border()`](https://yunuuuu.github.io/ggalign/reference/free.md)
  [`free_guide()`](https://yunuuuu.github.io/ggalign/reference/free.md)
  [`free_lab()`](https://yunuuuu.github.io/ggalign/reference/free.md)
  [`free_space()`](https://yunuuuu.github.io/ggalign/reference/free.md)
  [`free_vp()`](https://yunuuuu.github.io/ggalign/reference/free.md) :
  Free plots from alignment constraints
- [`area()`](https://yunuuuu.github.io/ggalign/reference/area.md) :
  Define the plotting areas
- [`inset()`](https://yunuuuu.github.io/ggalign/reference/inset.md) :
  Create a ggplot inset
- [`ggwrap()`](https://yunuuuu.github.io/ggalign/reference/ggwrap.md) :
  Wrap Arbitrary Graphics to ggplot
- [`layout_design()`](https://yunuuuu.github.io/ggalign/reference/layout_design.md)
  : Define the grid to compose plots in
- [`layout_title()`](https://yunuuuu.github.io/ggalign/reference/layout_title.md)
  : Annotate the whole layout
- [`layout_theme()`](https://yunuuuu.github.io/ggalign/reference/layout_theme.md)
  : Modify theme of the layout
- [`layout_tags()`](https://yunuuuu.github.io/ggalign/reference/layout_tags.md)
  : Control Plot Tagging in Layouts
- [`patch()`](https://yunuuuu.github.io/ggalign/reference/patch.md) :
  Get Patch representation
- [`Patch-ggproto`](https://yunuuuu.github.io/ggalign/reference/Patch-ggproto.md)
  [`Patch`](https://yunuuuu.github.io/ggalign/reference/Patch-ggproto.md)
  : Patch object
- [`as_grob()`](https://yunuuuu.github.io/ggalign/reference/as_grob.md)
  : Convert Object into a Grob

## Extending ggalign

Functions to enhance and customize the capabilities of ggalign.

- [`ggalign_init()`](https://yunuuuu.github.io/ggalign/reference/ggalign_init.md)
  : Initialize the Default Properties of an Object

- [`ggalign_update()`](https://yunuuuu.github.io/ggalign/reference/ggalign_update.md)
  : Update the Properties of an Object

- [`ggalign_inherit()`](https://yunuuuu.github.io/ggalign/reference/ggalign_inherit.md)
  : Inherit Properties from Another Object

- [`ggalign_build()`](https://yunuuuu.github.io/ggalign/reference/ggalign_build.md)
  : Build a Graphical Object for Rendering

- [`ggalign_gtable()`](https://yunuuuu.github.io/ggalign/reference/ggalign_gtable.md)
  : Convert a Built Object into a gtable

- [`.mark_draw()`](https://yunuuuu.github.io/ggalign/reference/dot-mark_draw.md)
  : Define the links to connect the marked observations

- [`.link_draw()`](https://yunuuuu.github.io/ggalign/reference/dot-link_draw.md)
  : Define the links to connect a pair of observations

- [`align()`](https://yunuuuu.github.io/ggalign/reference/align.md)
  **\[stable\]** :

  Create a New `CraftBox` Object with `CraftAlign` craftsman

- [`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)
  **\[stable\]** : Build a data frame

- [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
  **\[stable\]** : Build a Matrix

- [`tune()`](https://yunuuuu.github.io/ggalign/reference/tune.md) :
  Change the shape of the input object

- [`new_tune()`](https://yunuuuu.github.io/ggalign/reference/new_tune.md)
  [`tune_data()`](https://yunuuuu.github.io/ggalign/reference/new_tune.md)
  : Change the shape of the input object

- [`ggalign_data_set()`](https://yunuuuu.github.io/ggalign/reference/ggalign_data_set.md)
  : Attach supplementary data and levels for ggalign

- [`order2()`](https://yunuuuu.github.io/ggalign/reference/order2.md) :
  Ordering Permutation

## Extending ggplot2

Extending ggplot2 with additional geoms, scales, and other
functionalities.

- [`patch_title()`](https://yunuuuu.github.io/ggalign/reference/patch_title.md)
  : Add patch titles to plot borders

- [`geom_pie()`](https://yunuuuu.github.io/ggalign/reference/geom_pie.md)
  : Pie charts

- [`geom_subrect()`](https://yunuuuu.github.io/ggalign/reference/geom_subrect.md)
  [`geom_subtile()`](https://yunuuuu.github.io/ggalign/reference/geom_subrect.md)
  : Subdivide Rectangles

- [`geom_rect3d()`](https://yunuuuu.github.io/ggalign/reference/geom_rect3d.md)
  [`geom_tile3d()`](https://yunuuuu.github.io/ggalign/reference/geom_rect3d.md)
  : Add z-aesthetic for geom_tile

- [`geom_magick()`](https://yunuuuu.github.io/ggalign/reference/geom_magick.md)
  : Draw images as point shapes using magick

- [`scale_z_continuous()`](https://yunuuuu.github.io/ggalign/reference/scale_z_continuous.md)
  [`scale_z_binned()`](https://yunuuuu.github.io/ggalign/reference/scale_z_continuous.md)
  [`scale_z_discrete()`](https://yunuuuu.github.io/ggalign/reference/scale_z_continuous.md)
  [`scale_z_ordinal()`](https://yunuuuu.github.io/ggalign/reference/scale_z_continuous.md)
  : z scales

- [`theme_no_axes()`](https://yunuuuu.github.io/ggalign/reference/theme_no_axes.md)
  : Remove axis elements

- [`no_expansion()`](https://yunuuuu.github.io/ggalign/reference/no_expansion.md)
  : Remove scale expansion

- [`layer_order()`](https://yunuuuu.github.io/ggalign/reference/layer_order.md)
  : Change the layer adding order

- [`raster_magick()`](https://yunuuuu.github.io/ggalign/reference/raster_magick.md)
  : Rasterize the ggplot layers

- [`geom_draw()`](https://yunuuuu.github.io/ggalign/reference/geom_draw.md)
  : Layer with Grid or Function

- [`geom_gshape()`](https://yunuuuu.github.io/ggalign/reference/geom_gshape.md)
  **\[questioning\]** : Layer with a customized shape graphic using grid
  functions.

- [`coord_circle()`](https://yunuuuu.github.io/ggalign/reference/coord_circle.md)
  : Polar Coordinates with Enhanced Controls

- [`facet_sector()`](https://yunuuuu.github.io/ggalign/reference/facet_sector.md)
  : Polar coordinates with Facet support

- [`scale_gshape_manual()`](https://yunuuuu.github.io/ggalign/reference/scale_gshape_manual.md)
  **\[questioning\]** :

  Scale for `gshape` aesthetic

- [`draw_key_gshape()`](https://yunuuuu.github.io/ggalign/reference/draw_key_gshape.md)
  : Key glyphs for legends

- [`element_vec_fields()`](https://yunuuuu.github.io/ggalign/reference/element_vec.md)
  [`element_vec()`](https://yunuuuu.github.io/ggalign/reference/element_vec.md)
  [`element_rep()`](https://yunuuuu.github.io/ggalign/reference/element_vec.md)
  [`element_rep_len()`](https://yunuuuu.github.io/ggalign/reference/element_vec.md)
  [`element_vec_recycle()`](https://yunuuuu.github.io/ggalign/reference/element_vec.md)
  [`element_vec_rep()`](https://yunuuuu.github.io/ggalign/reference/element_vec.md)
  [`element_vec_rep_each()`](https://yunuuuu.github.io/ggalign/reference/element_vec.md)
  [`element_vec_slice()`](https://yunuuuu.github.io/ggalign/reference/element_vec.md)
  : Apply a function to the fields of an element object

## helpers

A collection of utility functions to enhance usability and
functionality.

- [`ggalignGrob()`](https://yunuuuu.github.io/ggalign/reference/ggalignGrob.md)
  : Generate a plot grob.

- [`ggalign_stat()`](https://yunuuuu.github.io/ggalign/reference/ggalign_stat.md)
  : Get the statistics from the layout

- [`magickGrob()`](https://yunuuuu.github.io/ggalign/reference/magickGrob.md)
  : Rasterize a grob object with magick

- [`is_layout()`](https://yunuuuu.github.io/ggalign/reference/is_layout.md)
  [`is_quad_layout()`](https://yunuuuu.github.io/ggalign/reference/is_layout.md)
  [`is_stack_layout()`](https://yunuuuu.github.io/ggalign/reference/is_layout.md)
  [`is_stack_cross()`](https://yunuuuu.github.io/ggalign/reference/is_layout.md)
  [`is_circle_layout()`](https://yunuuuu.github.io/ggalign/reference/is_layout.md)
  [`is_heatmap_layout()`](https://yunuuuu.github.io/ggalign/reference/is_layout.md)
  [`is_ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/is_layout.md)
  :

  Reports whether `x` is layout object

- [`hclust2()`](https://yunuuuu.github.io/ggalign/reference/hclust2.md)
  : Generate Tree Structures with Hierarchical Clustering

- [`genomic_dist()`](https://yunuuuu.github.io/ggalign/reference/genomic_dist.md)
  : Calculate inter-region distances for genomic rainfall plots

- [`genomic_density()`](https://yunuuuu.github.io/ggalign/reference/genomic_density.md)
  : Calculate Genomic Region Density

- [`read_example()`](https://yunuuuu.github.io/ggalign/reference/read_example.md)
  : Read Example Data
