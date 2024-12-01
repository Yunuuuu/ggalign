#' Modify operated Context in `quad_layout()`
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The `with_quad()` function modifies the application context of elements in
#' `ggheatmap()`/`quad_layout()`. It controls how objects like themes, scales,
#' or other plot modifications apply to specific annotation stacks or the main
#' plot without altering the currently active layout or plot.
#'
#' @param x An object which can be added to the ggplot, including
#' **plot options** (see `vignette("plot-options")` for details).
#' @param position A string specifying one or more positions-
#' `r oxford_and(.tlbr)`- to indicate the annotation stack context for `x`. If
#' `NULL`, will change the operated context to the `quad_layout()` itself. For
#' default behaivours, see `details` section.
#' @param main A single boolean value indicating whether `x` should apply to the
#' main plot, used only when `position` is not `NULL`. By default, if `position`
#' is `waiver()` and the active context of `quad_layout()` is an annotation
#' stack or the active context of `stack_layout()` is itself, `main` will be set
#' to `TRUE`; otherwise, it defaults to `FALSE`.
#' @return The original object with an added attribute that sets the specified
#' context.
#' @details
#' Default Behavior when adding object wrapped with `with_quad()`:
#'
#' For `quad_layout()` object:
#'
#'  - When `ggheatmap()`/`quad_layout()` has no active annotation stack, objects
#'  added via `+` or `-` operate normally without `with_quad()`.
#'  - When the active annotation stack is set, `with_quad()` ensures the applied
#'  object also modifies:
#'    * The main plot (by default).
#'    * Opposite annotation stacks when using `-`.
#'
#' For `stack_layout()` object:
#'
#'   - When the active layout is the `stack_layout()` itself:
#'     * `-` operator will apply changes to all plots along the
#'         `stack_layout()`, which means if the stack layout is in `horizontal`,
#'         `-` operator will also add the element to the `left` and `right`
#'         annotation, if the stack layout is in `vertical`, `-` operator will
#'         also add element to the `top` and `bottom` annotation.
#'     * `+` operator won't do anything special.
#'   - When the active layout is the nested `ggheatmap()`/`quad_layout()`, the
#'   `+`/`-` operator applies the elements to this nested layout, following the
#'   same principles as for `ggheatmap()`/`quad_layout()`.
#'
#' @examples
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 7)
#'
#' # By wrapping object with `with_quad()`, the `+` operator will apply the
#' # object not only to the active plot in the annotation stack, but also to
#' # the main plot unless specified by `main` argument otherwise.
#' ggheatmap(small_mat) +
#'     # initialize the left annotation
#'     anno_left(size = 0.2) +
#'     align_dendro() +
#'     # apply the object not only to the active plot in the annotation stack,
#'     # but also to the main plot
#'     with_quad(theme(plot.background = element_rect(fill = "red")))
#'
#' # the `-` operator will apply changes not only to the active annotation
#' # stack but also to the opposite one (i.e., bottom if top is active, and
#' # vice versa). The same principle applies to the left and right annotation.
#' ggheatmap(small_mat) +
#'     anno_left(size = 0.2) +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     # Change the active layout to the left annotation
#'     anno_top(size = 0.2) +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     anno_bottom(size = 0.2) +
#'     align_dendro(aes(color = branch), k = 3L) -
#'     # Modify the color scale of all plots in the bottom and the opposite
#'     # annotation, in this way, the `main` argument by default would be `TRUE`
#'     with_quad(scale_color_brewer(palette = "Dark2", name = "Top and bottom"))
#'
#' # When the `position` argument is manually set, the
#' # default value of the `main` argument will be `FALSE`.
#' ggheatmap(small_mat) +
#'     anno_left(size = 0.2) +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     anno_top(size = 0.2) +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     anno_bottom(size = 0.2) +
#'     align_dendro(aes(color = branch), k = 3L) -
#'     # Modify the background of all plots in the left and top annotation
#'     with_quad(theme(plot.background = element_rect(fill = "red")), "tl")
#' @export
with_quad <- function(x, position = waiver(), main = NULL) {
    UseMethod("with_quad")
}

#' @export
with_quad.default <- function(x, position = waiver(), main = NULL) {
    assert_layout_position(position)
    assert_bool(main, allow_null = TRUE)
    structure(
        list(
            object = x,
            object_name = deparse(substitute(x)),
            position = position, main = main
        ),
        class = "ggalign_with_quad"
    )
}

#' @export
with_quad.ggalign_plot <- function(x, position = waiver(), main = NULL) {
    cli_abort(sprintf("Cannot used with %s", object_name(x)))
}

#' @export
with_quad.layout_title <- function(x, position = waiver(), main = NULL) {
    cli_abort("Cannot used with {.obj_type_friendly {x}}")
}

#' @export
with_quad.quad_active <- with_quad.layout_title

#' @export
with_quad.quad_anno <- with_quad.layout_title

#' @export
with_quad.stack_switch <- with_quad.layout_title

quad_operated_context <- function(with, active, operator) {
    if (is.waive(ans <- .subset2(with, "position"))) {
        if (operator == "-") {
            # if wrap with `with_quad`
            # we determine the `context` from current actual active position
            if (is.null(active)) {
                ans <- NULL
            } else {
                ans <- c(active, opposite_pos(active))
                if (is.null(main <- .subset2(with, "main")) || main) {
                    ans <- c(ans, list(NULL))
                }
            }
        } else if (operator == "+") {
            ans <- active
            if (!is.null(ans)) {
                if (is.null(main <- .subset2(with, "main")) || main) {
                    ans <- c(ans, list(NULL))
                }
            }
        } else {
            cli_abort("Not implement for {operator}")
        }
    } else if (!is.null(ans)) { # if set manually
        ans <- setup_pos(ans)
        if (!is.null(main <- .subset2(with, "main")) && main) {
            ans <- c(ans, list(NULL))
        }
    }
    ans
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggalign_with_quad <- function(object, plot, object_name) {
    object <- .subset2(object, "object")
    object_name <- .subset2(object, "object_name")
    ggplot_add(object, plot, object_name)
}
