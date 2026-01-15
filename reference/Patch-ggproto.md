# Patch object

In
[`alignpatches()`](https://yunuuuu.github.io/ggalign/reference/alignpatches.md),
each subplot require a `Patch` object for proper alignment and layout
operations. `Patch` object is a
[`ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.html)
object that provides the core methods for arranging and aligning the
plots.

## Fields

- `options`:

  A
  [`patch_options`](https://yunuuuu.github.io/ggalign/reference/patch_options.md)
  object used to align the plot.

- `set_options`:

  **Description**

  Sets multiple options at once in the `options` field. This method
  allows you to provide one or more key-value pairs, where each key
  represents an option name and its corresponding value is assigned to
  that option.

  **Arguments**

  - `...`: Key-value pairs to set specific properties of the `options`
    object. Each argument should be of the form `name = value`, where
    `name` is the option name and `value` is the value to assign to it.

- `set_option`:

  **Description**

  Sets a specific option in the `options` field. This method allows you
  to modify individual properties of the `options` object by providing a
  key-value pair.

  **Arguments**

  - `name`: The name of the option to be set (as a character string).

  - `value`: The value to assign to the specified option.

  - `check`: Logical, if `TRUE` (default), checks the validity of the
    option before assigning it. Set to `FALSE` to bypass the check.

- `get_option`:

  **Description**

  Retrieves a specific option from the `options` field.

  **Arguments**

  - `option`: The name of the option to retrieve.

  **Value** The value of the specified option.

- `setup`:

  **Description**

  (Optional method) Sets up the `options` and `data` for the plot.

  **Arguments**

  - `options`: A
    [`patch_options`](https://yunuuuu.github.io/ggalign/reference/patch_options.md)
    object that contains various layout options.

- `gtable`:

  **Description**

  (Required method) Constructs a
  [`standardized gtable`](https://yunuuuu.github.io/ggalign/reference/standardized_gtable.md)
  object.

  **Value** A standardized
  [`gtable`](https://gtable.r-lib.org/reference/gtable.html) object, or
  a simple [`grob`](https://rdrr.io/r/grid/grid-defunct.html) object.

  The returned object represents the graphical structure, which can
  either be a full table-based layout (`gtable`) for more complex
  arrangements or a simpler graphical object (`grob`) when only basic
  plot elements are involved.

- `decompose_guides`:

  **Description**

  (Optional method) Collects guide legends and optionally removes the
  space they occupy.

  This method extracts guide legends based on the sides specified in the
  `guides` argument. After collecting the guides, the corresponding
  space in the `gt` is removed to free up space, except for guides
  placed `inside` the panel.

  **Arguments**

  - `gt`: A [`gtable`](https://gtable.r-lib.org/reference/gtable.html)
    object, usually returned by `self$gtable()`.

  **Value** A list with:

  - `gt`: The updated gtable with guide legends removed (if applicable).

  - `guides`: A named list of collected guide grobs corresponding to the
    sides specified in `guides` (or `NULL` if absent).

- `tag`:

  Add a Tag to a `gtable`

  **Description**: This function adds a tag to a given `gtable` at a
  specified position. The tag is added only if the `gtable` is
  standardized (i.e., it has the expected number of rows and columns)..

  **Arguments**:

  - `gt`: A `gtable` object to which the tag will be added.

  - `label`: A string representing the label or content for the tag.

  - `t`, `l`, `b`, `r`: Numeric values representing the top, left,
    bottom, and right margins (coordinates) for placing the tag on the
    `gtable`.

  - `z`: A numeric value representing the z-order, controlling the
    stacking order of the tag relative to other elements in the
    `gtable`.

  **Value**: The modified `gtable` with the tag added. If the `gtable`
  is not standardized (i.e., it doesn't meet the row/column
  requirements), the function returns the original `gtable` without
  modification.

- `panel_sizes`:

  **Description**

  This method retrieves the panel sizes (widths and heights) of a
  `gtable` or `grob` object.

  **Arguments**

  - `gt`: A [`gtable`](https://gtable.r-lib.org/reference/gtable.html)
    object, usually returned by `self$decompose_guides()`.

  **Value** A list with components:

  - `widths`: The panel widths as a unit object

  - `heights`: The panel heights as a unit object

- `adjust_panel_width/adjust_panel_height`:

  **Description**

  (Optional method) In most cases, the panel sizes do not need to be
  manually adjusted when aligning, as long as their border sizes are
  consistent. However, for
  [`gtable`](https://gtable.r-lib.org/reference/gtable.html) objects
  with absolute panel sizes, the panel sizes must be directly set to
  ensure that all plot panels fill in. This is particularly important
  when the specified and desired panel sizes are in absolute units. The
  method is only called when the plot does not span across multiple
  areas.

  **Arguments**

  - `panel_widths`/`panel_heights`: Unit objects representing the panel
    widths or heights of the underlying
    [`gtable`](https://gtable.r-lib.org/reference/gtable.html) object.
    These values correspond to the `widths`/`heights` fields returned by
    the `$panel_sizes()` method.

  - `panel_width`/`panel_height`: Unit objects representing the desired
    panel size. If the internal numeric value is `NA`, the size will be
    computed based on `panel_widths`/`panel_heights`.

  **Value** The panel width/height as a unit object

- `respect_panel`:

  **Description**

  (Optional method) In most cases, panel sizes do not need to be
  manually adjusted when aligning panels, as long as their border sizes
  are consistent. However, for `gtables` with a fixed panel aspect
  ratio, if the internal *numeric value* of the input panel sizes
  (either `panel_width` or `panel_height` is `NA`), that dimension will
  be inferred while maintaining the aspect ratio, particularly for
  single-panel layouts.

  **Arguments**

  - `gt`: A [`gtable`](https://gtable.r-lib.org/reference/gtable.html)
    object, usually returned by `self$decompose_guides()`.

  - `panel_width`/`panel_height`: Unit objects specifying the desired
    panel size. If the internal numeric value of either is `NA`, the
    size is computed from the gtable (`gt`).

  **Value** A list with components:

  - `width`: The panel width as a unit object

  - `height`: The panel height as a unit object

  - `respect`: If `TRUE`, the aspect ratio was enforced

- `border_sizes`:

  **Description**

  (Optional method) retrieve the border sizes of a gtable.

  **Arguments**

  - `gt`: A [`gtable`](https://gtable.r-lib.org/reference/gtable.html)
    object, usually returned by `self$decompose_guides()`.

  **Value** A list with components:

  - `top`: `unit` values for the top borders or `NULL`.

  - `left`: `unit` values for the left borders or `NULL`.

  - `bottom`: `unit` values for the bottom borders or `NULL`.

  - `right`: `unit` values for the right borders or `NULL`.

- `align_border`:

  **Description**

  (Optional method) This method modifies the top, left, bottom, and
  right border sizes of the underlying gtable (`gt`) by replacing
  corresponding entries in its `heights` and `widths` vectors..

  **Arguments**

  - `gt`: A [`gtable`](https://gtable.r-lib.org/reference/gtable.html)
    object, usually returned by `self$decompose_guides()`.

  - `t`, `l`, `b`, `r`: Optional numeric vectors specifying new sizes
    for the top, left, bottom, and right borders, respectively. Each
    vector replaces the corresponding entries in `gt$heights` or
    `gt$widths`.

  **Value** A modified
  [`gtable`](https://gtable.r-lib.org/reference/gtable.html) object.

- `place`:

  **Description** (Optional method) Inserts the patch's gtable
  (including optional background) into the target canvas gtable.

  This method places the patch's gtable into a specified location of
  another gtable, preserving the background and plot layers separately
  if a background exists. The `t`, `l`, `b`, `r` arguments specify the
  position in the target gtable, and `bg_z` / `plot_z` define the
  stacking order (z-order) for background and plot.

  **Arguments**

  - `gtable`: the target canvas gtable into which the patch will be
    inserted.

  - `gt`: A [`gtable`](https://gtable.r-lib.org/reference/gtable.html)
    object, usually returned by `self$align_border()`.

  - `t`, `l`, `b`, `r`: Integer positions (top, left, bottom, right)
    specifying where to insert the patch in the target gtable.

  - `i`: Index of the current patch, used to generate unique grob names.

  - `bg_z`: Z-order for the background grob (default `1L`).

  - `plot_z`: Z-order for the plot grob (default `2L`).

  - `options`: A
    [`patch_options`](https://yunuuuu.github.io/ggalign/reference/patch_options.md)
    object containing various layout options. Typically, this is the
    value returned by the subplot's `self$setup()` method.

  **Details**

  - If the patch includes a grob named `"background"`, it is separated
    from the main plot and inserted independently from the plot grob.

  - If no background is present, the entire gtable is inserted as the
    plot grob.

  **Value** The modified target canvas gtable with the patch's gtable
  added.

- `decompose_bg`:

  **Description** Separates the background grob (if present) from the
  main gtable.

  **Value** A list with:

  - `bg`: The background grob (or `NULL` if absent)

  - `gt`: The gtable with background removed

- `place_bg`:

  **Description** Adds the background grob into the target gtable.

- `place_gt`:

  **Description** Adds the main plot gtable into the target gtable.

- `is_alignpatches`:

  **Description**

  Checks whether the object inherits from the
  [`alignpatches()`](https://yunuuuu.github.io/ggalign/reference/alignpatches.md)
  `Patch` representation.

  If `TRUE`, the `self$alignpatches` will include `patches`, `gt_list`,
  and `borders_list`. For an example of how these are used, refer to the
  `patch.ggalign_free_lab` function in the `alignpatch-free-lab.R`
  script.

  **Value** Logical value (`TRUE` or `FALSE`) indicating whether `self`
  is a `PatchAlignpatches` object.
