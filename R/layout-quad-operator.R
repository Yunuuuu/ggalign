###############################################################
#' @keywords internal
quad_layout_subtract <- function(object, quad, object_name) {
    UseMethod("quad_layout_subtract")
}

#' Modify `-` Operator Context in `quad_layout()`
#'
#' @description
#' `r lifecycle::badge('experimental')` By default, the `-` operator acts on all
#' plots at the same position as the current active annotation in
#' [`quad_layout()`]. If there is no active annotation stack, it will act on all
#' plots in the `quad_layout()`. Wrapping objects with `with_position` allows
#' you to change the context of the subtraction `-` operator.
#'
#' @param x Objects to add to the layout using the `-` operator.
#' @param position A string containing one or more of `r oxford_and(.tlbr)`
#' indicating which annotation stack should be used as the context. If `NULL`,
#' the context will default to the [`quad_layout()`] itself. By default,
#' `waiver()` is used, which triggers the following behavior: If the current
#' active context in [`quad_layout()`] is `top` or `bottom`, the operator will
#' also act on the corresponding `bottom` or `top` annotation. If the context is
#' `left` or `right`, the operator will also act on the `right` or `left`
#' annotation, respectively. If there is no active annotation stack, it defaults
#' to `NULL`.
#' @return The input object with an additional attribute that specifies the
#' selected context.
#' @export
with_position <- function(x, position = waiver()) {
    assert_layout_position(position)
    attr(x, sprintf("__%s.quad_active_position__", pkg_nm())) <- list(position)
    x
}

quad_active_context <- function(quad, object) {
    context <- attr(object,
        sprintf("__%s.quad_active_position__", pkg_nm()),
        exact = TRUE
    )
    position <- quad@active
    if (is.null(context)) { # if not set
        context <- position
        # if wrap with `with_position`
    } else if (is.waive(context <- .subset2(context, 1L))) {
        if (is.null(position)) {
            context <- NULL
        } else {
            context <- c(position, opposite_pos(position))
        }
    } else if (!is.null(context)) {
        context <- setup_pos(context)
    }
    context
}

#' @importFrom rlang is_string
#' @export
quad_layout_subtract.default <- function(object, quad, object_name) {
    context <- quad_active_context(quad, object)
    if (is.null(context)) {
        quad <- quad_body_add(object, quad, object_name)
        context <- .TLBR
    }
    for (position in context) {
        if (!is.null(slot(quad, position))) {
            slot(quad, position) <- stack_layout_subtract(
                object, slot(quad, position), object_name
            )
        }
    }
    quad
}

# for objects can inherit from layout
#' @export
quad_layout_subtract.theme <- function(object, quad, object_name) {
    context <- quad_active_context(quad, object)
    if (is.null(context)) {
        quad <- update_layout_option_theme(quad, object, object_name)
    } else {
        for (position in context) {
            if (!is.null(slot(quad, position))) {
                slot(quad, position) <- update_layout_option_theme(
                    slot(quad, position), object, object_name
                )
            }
        }
    }
    quad
}

#' @export
quad_layout_subtract.ggalign_controls <- function(object, quad, object_name) {
    context <- quad_active_context(quad, object)
    if (is.null(context)) {
        quad <- update_layout_option(quad, object, object_name)
    } else {
        for (position in context) {
            if (!is.null(slot(quad, position))) {
                slot(quad, position) <- update_layout_option(
                    slot(quad, position), object, object_name
                )
            }
        }
    }
    quad
}

# for objects should only be added with `+`
#' @export
quad_layout_subtract.ggplot <- function(object, quad, object_name) {
    cli::cli_abort(c(
        "Cannot use {.code -} to add {.obj_type_friendly {object}}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
quad_layout_subtract.layout_title <- quad_layout_subtract.ggplot

#' @export
quad_layout_subtract.free_gg <- quad_layout_subtract.ggplot

#' @export
quad_layout_subtract.Align <- function(object, quad, object_name) {
    cli::cli_abort(c(
        "Cannot use {.code -} to add {.fn {snake_class(object)}}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
quad_layout_subtract.layout_annotation <- quad_layout_subtract.ggplot

###############################################################
#' @keywords internal
quad_layout_and_add <- function(object, quad, object_name) {
    UseMethod("quad_layout_and_add")
}

#' @export
quad_layout_and_add.default <- function(object, quad, object_name) {
    quad <- quad_body_add(object, quad, object_name)
    for (position in .TLBR) {
        stack <- slot(quad, position)
        if (is.null(stack)) next
        slot(quad, position) <- stack_layout_and_add(
            object, stack, object_name
        )
    }
    quad
}

#' @export
quad_layout_and_add.theme <- function(object, quad, object_name) {
    ans <- NextMethod()
    # to align with `patchwork`, we also modify the layout theme
    # when using `&` to add the theme object.
    ans@theme <- ans@theme + object
    ans
}

#' @export
quad_layout_and_add.layout_title <- function(object, quad, object_name) {
    cli::cli_abort(c(
        "Cannot use {.code &} to add {.obj_type_friendly {object}}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
quad_layout_and_add.ggplot <- quad_layout_and_add.layout_title

#' @export
quad_layout_and_add.free_gg <- quad_layout_and_add.layout_title

#' @export
quad_layout_and_add.Align <- function(object, quad, object_name) {
    cli::cli_abort(c(
        "Cannot use {.code &} to add {.fn {snake_class(object)}}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
quad_layout_and_add.layout_annotation <- quad_layout_and_add.layout_title
