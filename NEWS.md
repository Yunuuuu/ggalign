# ggalign 0.0.4

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

* Heatmap layout: Arrange ggplot into a Heatmap.
* Stack layout: Arrange ggplot vertically or horizontally.
* Customize layouts or add plots:
  * `align_group()`: Group layout axis into panel
  * `align_kmeans()`: Group layout observations by kmeans
  * `align_reorder()`: Reorder layout observations
  * `align_dendro()`: Reorder or Group layout based on Hierarchical Clustering
  * `align_gg()`/`ggalign()`: Create ggplot object in the layout
  * `align_panel()`/`ggpanel()`: Create ggplot object based on the layout panel data.
