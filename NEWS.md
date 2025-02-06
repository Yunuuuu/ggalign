# ggalign (development version)

# ggalign 0.1.0

## Breaking changes

* Now, all `*_free()` layouts have been removed, all layout has been splitted into two type: align 
  discrete or continous vairables.

## New features

* new helper function `memo_order()` to reorder the oncoplot samples.

* new `geom_subrect()` and `geom_subtile()` to subdivide rectangles with shared borders into a grid.

* new `cross_link` function to reset the layout ordering index or layout panel group, and
  add plot to connect selected observations.

* new `cross_mark` function to reset the layout ordering index or layout panel group, and
  add plot to annotate observations.

* new `pair_links()` function to create pairs of observation groups.

* new `mark_draw()`, `mark_line()`, `mark_tetragon()` to define the links to
  connect the marked observations.

* new `link_draw()`, `link_line()`, `link_tetragon()` to define the links to
  connect a pair of observations

* new `ggmark()` to add a plot to annotate selected observations.

* new `element_vec()` functions to apply function to the vectorized fields of
  the theme element object.

* new `no_expansion()` function to remove scale expansion.

* new layout `circle_layout()` to arrange plot in a circular.

* new `raster_magick()` function to post-processing the image raster with `magick` package was added.

* new `fortify_matrix.GISTIC()` method for `GISTIC` object from `maftools` package (#24).

* `+` operator now will respect `with_quad()`.

* new `ggalign_attr_set()` and `ggalign_attr_get()` for basic operations of attached attribute.

* new `geom_draw()` function to draw ggplot2 layer with customized draw function.

## Bug fixes

* fix breaks and labels not work well in discrete scale (#30, #32)

* fix wrong results of `fortify_matrix.MAF()` method

## Improvements

* Now, `with_quad()` wraps the object into a list with the class `with_quad`. This approach offers greater flexibility in defining the actions associated with `with_quad`, allowing for more customizable behavior (#26, @Yunuuuu).

* Now, we always set as.table = FALSE when using ggplot2 `facet_*()` functions,
  we don't need to reorder the `panel` when drawing.

* `free_gg`, `align` and the new `cross_link` objects have been collapsed to one class `ggalign_plot`

# ggalign 0.0.5

## Breaking changes

* `direction` argument in `stack_layout()`/`ggstack()` is soft-deprecated now, user must provide it manually.

* `set_context`, `order`, and `name` arguments in `align_*()` and `ggheatmap()` are soft-deprecated, which is advised to use `active` argument.

* `guides`, `free_guides`, `free_spaces`, `free_labs`, and `plot_data` arguments in `align_*()` and `ggheatmap()` are deprecated, which is advised to use `scheme_align` function instead.

* `theme` argument in `align_*()` and `ggheatmap()` is deprecated, which is advised to use `plot_theme` function. 

* `what` argument in `stack_active()` is soft-deprecated, user is advised to use `stack_switch()` to change the active plot with `what`.

* `align_panel()`/`ggpanel()` is deprecated, user is advised to use `ggalign(data = NULL)` instead.

## New features

* new `quad_layout()` to create layout allowing free from alignment.

* new `quad_alignv()`, `quad_alignh()`, `quad_free()`/`ggside()`, `quad_alignb()` alias for `quad_layout()`.

* `stack_layout()` now gains a `type` argument to control whether the layout should align observations.

* new `ggoncoplot()` function to draw oncoprint.

* new `ggfree()` function to add ggplot to the layout.

* new `fortify_matrix()` function to convert any objects to a matrix, and add `fortify_matrix()` method for `MAF` object.

* new `fortify_data_frame()` function to convert any objects to a data frame.

* now, a special attribute `ggalign` can be used to pass additional informations across the building process, and a new function `ggalign_attr()` can be used to extract the data from the attribute.

* new `plot_theme()` function to control the default theme of the plot in layout.

* new `plot_data()` function to control the data transformation of the plot in layout.

* new `scheme_align()` function to control specifications of the plot in layout.

* new `with_quad()` function to control the `-` operator context.

* new `quad_init()` function to initialize the annotation with self-provided data.

* new `stack_switch()` and `quad_switch()` to switch active plots for `stack_layout()` and `quad_layout()`.

* new `anno_top()`, `anno_left()`, `anno_bottom()` and `anno_right()` alias for add annotation in `quad_layout()`.

* `align_dendro()` gain a new argument `cutree` to customize the process of tree cutting.

* `reorder_dendrogram` argument in `align_dendro()` function now can accept a function to reorder the tree.

* new `layer_order()` function to change the ggplot2 layer adding order.

* new `theme_no_axes()` function to remove axis elements.

* new `geom_pie()` function to draw pie charts.

## Bug fixes

* fix bug when apply `free_border` in `alignpatches` object.

* fix bug when provide a function in `distance` argument of `align_dendro()`

* fix bug when provide `hclust` or `dendrogram` in method argument of `align_dendro()`

* fix bug of `coord_ggalign()` for discrete scale

## Improvements

* `merge` function has been implemented with `vctrs` for performance

* for large matrix (> 20000 cells), automatically use `geom_raster()`

* {data.table} is now removed from the dependency

# ggalign 0.0.4

* `layout_heatmap()`/`ggheatmap()` arguments width/height have been renamed `.width`/`.height` to helps avoid conflicts with arguments from `geom_tile()`. 

* the nestet alignpatches theme by default will inherit from the parent alignpatches

* new `layout_title()` function, now `layout_annotation()` only control the layout theme

* add `ggrastr::rasterize` method for both `ggheatmap()` and `ggstack()`

* `set_context` argument in `align_dendro()` now depends on `plot_dendrogram` by default

* `align_dendro()` gain a new argument `reorder_dendrogram` to control whether to reorder the generated dendrogram based on the mean values.

* `align_reorder()` now splitted into two function `align_order()` and `align_reorder()`

* `dendrogram_data()` gain a new argument `reorder_branches` to control whether to reorder the input `leaf_braches` based on the tree.

* `align_dendro()` gain new argument `merge_dendrogram` to control whether the dendrograms in multiple groups should be merged

* dendrogram height axis will be automatically reversed in bottom and left annotation stack

* heatmap fill color scale now default use `scale_fill_gradient2(low = "blue", high = "red")` for continuous values.

* `Coord` is used to set limits instead of `Scale`

* all `free_*` arguments now accept the same argument of the corresponding `free_*` functions.

* new `theme_ggalign()` function for the default theme of ggalign package

* export option `ggalign.default_theme` allow user to change the global default theme

* new `free_guide()` function, to override the layout guides argument for single plot

* `geom_draw()` now utilize `patch()` function to convert objects into grob

* internal changes: all vectors have been operated with `vctrs` package

* `stack_layout()`: gain `sizes` argument to control the relative sizes of the stack

* `align_()*`: now `order` argument only control the plot area.

* `align_reorder()` now can accept an ordering character names.

* export `order2()` function

* `align_reorder()`: fun argument has been renamed to `order` argument, and it can now accept the ordering integer index directly

* `fun` in `align_reorder()` now can return any statistics which can be handled by `order2()`

* fix `ggheatmap()` order not work when put in a stack layout

* `method` in `hclust2()` can now return any objects which can be coerced into hclust, or you can provide such object directly in `method`.

* `hclust2()` now can accept `distance = NULL`

* `hmanno()`, `stack_active()`, and `align_*()` functions gain a new argument `theme` to control the theme for the plot.

* new `layout_annotation()` and `layout_theme()` to modify the layout components

* `borders` argument in `free_space()` has been renamed to `spaces`

* {tibble} is now removed from the dependency

* `align_plots()` now can accept `gList`, `function`, `recordedplot`, `trellis`, `pheatmap`, `Heatmap`, `HeatmapAnnotation`, and `HeatmapList`, we have added `alignpatch` method for these objects.

* add `patch` method for `gList`, `function`, `recordedplot`, `trellis`, `pheatmap`, `Heatmap`, `HeatmapAnnotation`, and `HeatmapList`

* fix a bug in `wrap()` function where duplicated names in gtable cause some grobs missing

* new `free_vp()` function to customize the viewport when aligning.

* `StackLayout` and `HeatmapLayout` methods have been collapsed into one method for `Layout` class

* New `Patch` class, now we can extend the alignment process of any object and customize the building process with `ggproto`.

* fix missing variable `labs` in the `free_space` method of `PatchAlignpatches`

# ggalign 0.0.3

* Add package logo

* new `inset()` function

* add package quotes in DESCRIPTION

* add return value docs for exported function or methods

# ggalign 0.0.2
  
* omit the redudnant description and add single quote for package name

# ggalign 0.0.1

## Features

`ggalign` pacakge provides two layout to arrange ggplot objects:

  - `heatmap_layout()`/`ggheatmap()`: Arrange ggplot into a Heatmap layout. See
  `vignette("heatmap-layout")` for details.

  - `stack_layout()`/`ggstack()`: Arrange ggplot vertically or horizontally. See
  `vignette("stack-layout")` for details.

To further customize these layouts, we offer following functions:

  - `align_group()`: Group layout axis into panel with a group variable.
  - `align_kmeans()`: Group layout axis into panel by kmeans
  - `align_reorder()`: Reorders layout observations based on weights or summary
  statistics. 
  - `align_dendro()`: Reorder or Group layout based on hierarchical clustering

For more detailed instructions on customizing layouts, see the vignette:
`vignette("layout-customize")`. 

Additionally, plots can be added in the layout with following functions: 

  - `align_gg()`/`ggalign()`: Create ggplot object with a customized data.
  - `align_panel()`/`ggpanel()`: Create ggplot object with the layout panel
    data.

For more information on adding plots, refer to the vignette:
`vignette("layout-plot")`.
