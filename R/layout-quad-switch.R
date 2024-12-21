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
#' By default, `quad_anno()` attempts to initialize the annotation stack layout
#' using data from `r rd_quad()`. However, in situations where you want to use
#' different data for the annotation stack, you can set `initialize = FALSE`
#' and then provide a custom `stack_layout()`.
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
#' @param initialize A boolean indicating whether the annotation stack should be
#' initialized if it is not already. By default, the annotation stack layout
#' will attempt to initialize when the data is compatible. If set to `TRUE`, and
#' the data in `r rd_quad()` is incompatible with the annotation stack, no
#' data will be used in the stack.
#' @param what What should get activated in the annotation stack?
#' `r rd_stack_what()`.
#' @seealso [`quad_switch()`]
#' @export
#' @rdname quad_active
quad_anno <- function(position, size = NULL, free_guides = waiver(),
                      initialize = NULL, what = waiver()) {
    if (is.null(position)) {
        cli_abort(c(
            paste(
                "{.arg position} must be a single string of",
                "{oxford_or(.TLBR)}, not `NULL`"
            ),
            i = "Do you want to set the active context to the `quad_layout()` with {.fn quad_active}?"
        ))
    }
    position <- match.arg(position, .TLBR)
    quad_switch_anno(
        size = size, free_guides = free_guides,
        initialize = initialize, what = what,
        position = position
    )
}

#' @export
#' @rdname quad_active
anno_top <- function(size = NULL, free_guides = waiver(), initialize = NULL,
                     what = waiver()) {
    quad_switch_anno(
        size = size, free_guides = free_guides,
        initialize = initialize, what = what,
        position = "top"
    )
}

#' @export
#' @rdname quad_active
anno_left <- function(size = NULL, free_guides = waiver(), initialize = NULL,
                      what = waiver()) {
    quad_switch_anno(
        size = size, free_guides = free_guides,
        initialize = initialize, what = what,
        position = "left"
    )
}

#' @export
#' @rdname quad_active
anno_bottom <- function(size = NULL, free_guides = waiver(), initialize = NULL,
                        what = waiver()) {
    quad_switch_anno(
        size = size, free_guides = free_guides,
        initialize = initialize, what = what,
        position = "bottom"
    )
}

#' @export
#' @rdname quad_active
anno_right <- function(size = NULL, free_guides = waiver(), initialize = NULL,
                       what = waiver()) {
    quad_switch_anno(
        size = size, free_guides = free_guides,
        initialize = initialize, what = what,
        position = "right"
    )
}

quad_switch_anno <- function(position, size, free_guides, initialize, what,
                             call = caller_call()) {
    if (!is.null(size)) size <- check_size(size, call = call)
    assert_layout_position(free_guides, call = call)
    if (!is.waive(what)) what <- check_stack_context(what, call = call)
    assert_bool(initialize, allow_null = TRUE)
    structure(
        list(
            position = position, size = size,
            free_guides = free_guides, what = what,
            initialize = initialize
        ),
        class = c("quad_anno", "quad_switch")
    )
}

#' Initialize Quad-Layout Annotation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated, you can add `stack_layout()` directly.
#'
#' @export
#' @keywords internal
quad_init <- function(position, data = waiver(), ...) {
    lifecycle::deprecate_stop("0.0.6", "quad_init()", "stack_layout()")
}

#' @inherit quad_active title return
#' @description
#' `r lifecycle::badge('stable')`
#'
#' `quad_switch()` integrates [`quad_active()`] and [`quad_anno()`] into one
#' function for ease of use. This function allows you to quickly change the
#' active context of the [`quad_layout()`] and its annotations.
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
#' @seealso [`quad_active()`]/[`quad_anno()`]
#' @export
quad_switch <- function(position = NULL, size = NULL,
                        width = NULL, height = NULL, free_guides = waiver(),
                        initialize = NULL, what = waiver()) {
    if (is.null(position)) {
        quad_active(width = width, height = height)
    } else {
        position <- match.arg(position, .TLBR)
        quad_switch_anno(
            size = size,
            free_guides = free_guides, what = what,
            initialize = initialize,
            position = position
        )
    }
}

#' @inheritParams heatmap_layout
#' @export
#' @rdname quad_switch
hmanno <- quad_switch
