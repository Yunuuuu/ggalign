#' Modify operated Context in `quad_layout()`
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The `quad_scope()` function controls how plot elements (e.g., themes, scales,
#' or other ggplot objects) are applied within a `ggheatmap()` or
#' `quad_layout()` context. It allows you to direct modifications to specific
#' annotation positions or the main plot without altering the currently active
#' layout or nesting state.
#'
#' @param object An object which can be added to the ggplot.
#' @param position A string or character vector specifying one or more positions
#' (`r oxford_and(c(.tlbr, "i"))`) indicating where `x` should be applied.
#' Use `'i'` to refer to the quad body (i.e., the main plot). If `NULL`, the
#' active annotation context is cleared, behaving as if no annotation is active.
#' See the **Details** section for more information.
#' @return The original object with an added attribute that sets the specified
#' context.
#' @details
#' Default behavior when adding objects wrapped with `quad_scope()`:
#'
#' - **When no annotation stack is active**:
#'   Modifications are applied normally without needing `quad_scope()`.
#'
#' - **When an annotation stack is active**:
#'   `quad_scope()` ensures the object is also applied to:
#'     - The active annotation stack
#'     - The main plot
#'
#' **When `position` is manually specified**:
#'   - If `NULL`, it behaves as if no annotation is active
#'   - If a string, the object is applied only to the
#'     specified positions
#'     (to include the main plot, explicitly add `"i"` to `position`)
#'
#' @examples
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 7)
#'
#' # By wrapping object with `quad_scope()`, the `+` operator will apply the
#' # object not only to the active plot in the annotation stack, but also to
#' # the main plot unless specified by `main` argument otherwise.
#' ggheatmap(small_mat) +
#'     # initialize the left annotation
#'     anno_left(size = 0.2) +
#'     align_dendro() +
#'     # apply the object not only to the active plot in the annotation stack,
#'     # but also to the main plot
#'     quad_scope(theme(plot.background = element_rect(fill = "red")))
#'
#' # When the `position` argument is manually set, the
#' # we must explicitly include `"i"` in `position` to apply it to the main plot
#' ggheatmap(small_mat) +
#'     anno_left(size = 0.2) +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     anno_top(size = 0.2) +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     anno_bottom(size = 0.2) +
#'     align_dendro(aes(color = branch), k = 3L) -
#'     # Modify the background of all plots in the left and top annotation
#'     quad_scope(theme(plot.background = element_rect(fill = "red")), "tl")
#' @importFrom S7 S7_dispatch S7_inherits
#' @export
quad_scope <- S7::new_class(
    "quad_scope",
    properties = list(
        object = S7::new_property(
            S7::class_any,
            validator = function(value) {
                invalid <- c(
                    "layout_title", "layout_theme", "layout_annotation",
                    "quad_active", "quad_anno", "stack_switch"
                )
                if (S7_inherits(value, CraftBox) || inherits(value, invalid)) {
                    cli_abort("Scope cannot be set for {.obj_type_friendly {value}}")
                }
                NULL
            }
        ),
        position = S7::new_property(
            S7::class_any,
            validator = function(value) {
                if (is.waive(value) || is.null(value)) {
                    return(NULL)
                }
                if (is_string(value) && !grepl("[^tlbri]", value)) {
                    return(NULL)
                }
                sprintf(
                    "must be a single string containing only the characters %s",
                    oxford_and(c(.tlbr, "i"))
                )
            },
            default = quote(waiver())
        )
    )
)

#' Create ggplot object with layout panel data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated, please use `quad_scope()` instead.
#' @export
#' @keywords internal
with_quad <- function(x, position = waiver(), main = deprecated(), ...) {
    lifecycle::deprecate_soft(
        "1.1.0",
        "with_quad()",
        details = "quad_scope()"
    )
    if (lifecycle::is_present(main)) {
        lifecycle::deprecate_soft(
            "1.1.0",
            "with_quad(main = )",
            details = "with_quad(position = \"i\")"
        )
        if (isTRUE(main)) {
            if (is.null(position) || is.waive(position)) {
                position <- "i"
            } else if (is_string(position)) {
                position <- paste0(position, "i")
            }
        }
    }
    quad_scope(x, position, ...)
}

quad_scope_contexts <- function(scope, current) {
    contexts <- prop(scope, "position")
    if (is.null(contexts)) return(contexts) # styler: off
    if (is.waive(contexts)) {
        # have active annotation, we by default add quad body
        if (is.null(current)) current else list(current, NULL)
    } else {
        .subset(
            list(t = "top", l = "left", b = "bottom", r = "right", i = NULL),
            split_position(contexts)
        )
    }
}

#' @importFrom ggplot2 update_ggplot
S7::method(update_ggplot, list(quad_scope, ggplot2::class_ggplot)) <-
    function(object, plot, objectname, ...) {
        object <- prop(object, "object")
        update_ggplot(object, plot, objectname, ...)
    }
