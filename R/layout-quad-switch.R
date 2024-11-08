#' Determine the Active Context of Quad-Layout
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' - `quad_active`: Sets the active context to the `r rd_quad()` itself.
#' - `quad_anno`: Sets the active context to the specified annotation stack
#'   based on the `position` argument.
#' - `anno_top`: A special case of `quad_anno` with `position = "top"`.
#' - `anno_left`: A special case of `quad_anno` with `position = "left"`.
#' - `anno_bottom`: A special case of `quad_anno` with `position = "bottom"`.
#' - `anno_right`: A special case of `quad_anno` with `position = "right"`.
#'
#' @inheritParams quad_layout
#' @return An object that can be added to `r rd_quad()`.
#' @export
#' @rdname quad_active
quad_active <- function(width = NULL, height = NULL) {
    if (!is.null(width)) width <- check_size(width)
    if (!is.null(height)) height <- check_size(height)
    structure(
        list(width = width, height = height),
        class = c("quad_active", "quad_switch")
    )
}

#' @details
#' By default, `quad_anno()` will try to initialize the annotation stack layout
#' using data from `r rd_quad()`. However, there are situations where the
#' annotation stack cannot be initialized due to incompatible data formats
#' between [`quad_layout()`] and the required format for the annotation stack.
#' This often occurs in [`quad_alignh()`] and [`quad_alignv()`], where the
#' layout data is a matrix, but top and bottom annotations (in
#' [`quad_alignh()`]) or left and right annotations (in [`quad_alignv()`])
#' require a data frame. In such cases, you must use [`anno_init()`] to manually
#' initialize the annotation stack.
#'
#' @param position `r rd_quad_position("activated")`.
#' @param size A numeric value or an [`unit`][grid::unit] object to set the
#' total `height`/`width` of the annotation stack.
#'  - If `position` is `"top"` or `"bottom"`, `size` sets the total height of
#' the annotation.
#'  - If `position` is `"left"` or `"right"`, `size` sets the total width of the
#' annotation.
#' @param free_guides Override the `guides` collection behavior specified in the
#' `r rd_quad()` for the annotation stack.
#' @param what What should get activated in the annotation stack?
#' `r rd_stack_what()`.
#' @seealso
#' - [`quad_switch()`]
#' - [`anno_init()`]
#' @export
#' @rdname quad_active
quad_anno <- function(position, size = NULL, free_guides = waiver(),
                      what = waiver()) {
    if (is.null(position)) {
        cli::cli_abort(c(
            paste(
                "{.arg position} must be a single string of",
                "{oxford_or(.TLBR)}, not `NULL`"
            ),
            i = "Do you want to set the active context to the `quad_layout()` with {.fn quad_active}?"
        ))
    }
    position <- match.arg(position, .TLBR)
    quad_switch_anno(
        size = size,
        free_guides = free_guides, what = what,
        position = position
    )
}

#' @export
#' @rdname quad_active
anno_top <- function(size = NULL, free_guides = waiver(),
                     what = waiver()) {
    quad_switch_anno(
        size = size, free_guides = free_guides, what = what,
        position = "top"
    )
}

#' @export
#' @rdname quad_active
anno_left <- function(size = NULL, free_guides = waiver(),
                      what = waiver()) {
    quad_switch_anno(
        size = size, free_guides = free_guides, what = what,
        position = "left"
    )
}

#' @export
#' @rdname quad_active
anno_bottom <- function(size = NULL, free_guides = waiver(),
                        what = waiver()) {
    quad_switch_anno(
        size = size, free_guides = free_guides, what = what,
        position = "bottom"
    )
}

#' @export
#' @rdname quad_active
anno_right <- function(size = NULL, free_guides = waiver(),
                       what = waiver()) {
    quad_switch_anno(
        size = size, free_guides = free_guides, what = what,
        position = "right"
    )
}

quad_switch_anno <- function(position, size, free_guides, what,
                             call = caller_call()) {
    if (!is.null(size)) size <- check_size(size, call = call)
    assert_layout_position(free_guides, call = call)
    if (!is.waive(what)) what <- check_stack_context(what, call = call)
    structure(
        list(
            position = position, size = size,
            free_guides = free_guides, what = what
        ),
        class = c("quad_anno", "quad_switch")
    )
}

#' Initialize Quad-Layout Annotation
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Initializes an annotation stack with a user-specified data.
#'
#' @param position `r rd_quad_position("initialized")`.
#' @param data Default dataset to use for the annotation stack. If not
#' specified, a dataset must be provided for each plot added to the layout.
#' Possible values:
#'  - [`waiver()`]: try to inherit from the [quad_layout()].
#'  - `NULL`: no data for the annotation stack.
#'  - Any data which can be coerced by
#'    [`fortify_matrix()`]/[`fortify_data_frame()`]
#'
#' Data conversion depends on whether the annotation stack will align the
#' observations:
#'   - If aligned, [`fortify_matrix()`] will be applied to convert the data
#'     into a matrix.
#'   - If not aligned, [`fortify_data_frame()`] will be used to convert the
#'     data into a data frame.
#' @inheritParams quad_free
#' @export
anno_init <- function(position, data = waiver(), ...) {
    if (is.null(position)) {
        cli::cli_abort(paste(
            "{.arg position} must be a single string of",
            "{oxford_or(.TLBR)}, not `NULL`"
        ))
    }
    position <- match.arg(position, .TLBR)
    data <- allow_lambda(data)
    structure(
        list(data = data, position = position, params = rlang::list2(...)),
        class = c("anno_init", "quad_anno", "quad_switch")
    )
}

#' @inherit quad_active title return
#' @description
#' `r lifecycle::badge('stable')`
#'
#' `quad_switch()` integrates [`quad_active()`] and [`quad_anno()`] into one
#' function for ease of use. This function allows you to quickly change the
#' active context of the [quad_layout()] and its annotations.
#'
#' `hmanno` is an alias for `quad_switch`, with additional arguments for
#' backward compatibility
#'
#' @param position `r rd_quad_position("activated")`. If `NULL`, it sets the
#'  active context to the `r rd_quad()` itself.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quad_active
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_dendro()
#' @importFrom ggplot2 waiver
#' @seealso
#' - [`quad_active()`]/[`quad_anno()`]
#' - [`anno_init()`]
#' @export
quad_switch <- function(position = NULL, size = NULL,
                        width = NULL, height = NULL, free_guides = waiver(),
                        what = waiver()) {
    if (is.null(position)) {
        quad_active(width = width, height = height)
    } else {
        position <- match.arg(position, .TLBR)
        quad_switch_anno(
            size = size,
            free_guides = free_guides, what = what,
            position = position
        )
    }
}

#' @inheritParams align_gg
#' @inheritParams heatmap_layout
#' @export
#' @rdname quad_switch
hmanno <- function(position = NULL, size = NULL,
                   width = NULL, height = NULL, free_guides = waiver(),
                   what = waiver(), ...,
                   # following parameters have replaced with `action`
                   # argument
                   guides = deprecated(),
                   free_spaces = deprecated(), plot_data = deprecated(),
                   theme = deprecated(), free_labs = deprecated()) {
    rlang::check_dots_empty()
    deprecate_action(
        "hmanno",
        guides = guides,
        free_spaces = free_spaces,
        plot_data = plot_data,
        theme = theme,
        free_labs = free_labs
    )
    ans <- quad_switch(
        position = position, size = size,
        width = width, height = height, free_guides = free_guides,
        what = what
    )
    ans
}
