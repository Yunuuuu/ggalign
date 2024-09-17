# ggalign 0.0.4

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