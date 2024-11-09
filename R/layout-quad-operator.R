#' Modify Context for the `-` Operator in `quad_layout()`
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The `with_quad` function allows you to adjust the context in which the
#' subtraction `-` operator is applied within `quad_layout()`. This function
#' wraps objects to specify their target layout contexts when using `-` in
#' `quad_layout()`.
#'
#' @param x The object to be added to the layout using the `-` operator.
#' @param position A string specifying one or more positions—
#' `r oxford_and(.tlbr)`— to indicate the annotation stack context for the
#' operator [`-`][layout-operator]. By default, `waiver()` is used, which sets
#' the behavior as follows: if the active context in `quad_layout()` is `top` or
#' `bottom`, the operator will also apply to the corresponding `bottom` or `top`
#' annotation, respectively. Similarly, if the context is `left` or `right`, the
#' operator applies to the `right` or `left` annotation. When no annotation
#' stack is active, the context defaults to `NULL`.
#' @param main A single boolean value indicating whether `x` should apply to the
#' main plot, used only when `position` is not `NULL`. By default, if `position`
#' is `waiver()` and the active context of `quad_layout()` is an annotation
#' stack, `main` will be set to `TRUE`; otherwise, it defaults to `FALSE`.
#' @return The original object with an added attribute that sets the specified
#' layout context.
#' @export
with_quad <- function(x, position = waiver(), main = NULL) {
    assert_layout_position(position)
    assert_bool(main, allow_null = TRUE)
    attr(x, sprintf("__%s.quad_active_context__", pkg_nm())) <- list(
        position = position, main = main
    )
    x
}

quad_active_context <- function(x) {
    attr(x, sprintf("__%s.quad_active_context__", pkg_nm()), exact = TRUE)
}

quad_subtract_actives <- function(context, position) {
    if (is.null(context)) { # if not set, use the actual active position
        ans <- position
    } else if (is.waive(ans <- .subset2(context, "position"))) {
        # if wrap with `with_quad`
        # we determine the `actives` from current actual layout active
        if (is.null(position)) {
            ans <- NULL
        } else {
            ans <- c(position, opposite_pos(position))
            if (is.null(main <- .subset2(context, "main")) || main) {
                ans <- c(ans, list(NULL))
            }
        }
    } else if (!is.null(ans)) { # if set manually
        ans <- setup_pos(ans)
        if (!is.null(main <- .subset2(context, "main")) && main) {
            ans <- c(ans, list(NULL))
        }
    }
    ans
}

###############################################################
#' @keywords internal
quad_layout_subtract <- function(object, quad, object_name) {
    UseMethod("quad_layout_subtract")
}

#' @export
quad_layout_subtract.default <- function(object, quad, object_name) {
    context <- quad_active_context(object)
    actives <- quad_subtract_actives(context, quad@active)
    if (is.null(actives)) actives <- c(.TLBR, list(NULL))
    for (act in actives) {
        if (is.null(act)) {
            quad <- quad_body_add(object, quad, object_name)
        } else if (!is.null(slot(quad, act))) {
            slot(quad, act) <- stack_layout_subtract(
                object, slot(quad, act), object_name
            )
        }
    }
    quad
}

# for objects can inherit from layout
#' @export
quad_layout_subtract.ggalign_option <- function(object, quad, object_name) {
    context <- quad_active_context(object)
    actives <- quad_subtract_actives(context, quad@active)
    if (is.null(actives)) {
        quad <- update_layout_option(object, quad, object_name)
    } else {
        for (act in actives) {
            if (is.null(act)) {
                quad <- quad_body_add(object, quad, object_name)
            } else if (!is.null(slot(quad, act))) {
                slot(quad, act) <- update_layout_option(
                    object, slot(quad, act), object_name
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
quad_layout_subtract.quad_active <- quad_layout_subtract.ggplot

#' @export
quad_layout_subtract.quad_anno <- quad_layout_subtract.ggplot

#' @export
quad_layout_subtract.anno_init <- quad_layout_subtract.ggplot

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
