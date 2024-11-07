#' Modify `-` Operator Context in `quad_layout()`
#'
#' @description
#' `r lifecycle::badge('experimental')` Wrapping objects with `with_quad`
#' allows you to change the context of the subtraction `-` operator in
#' `quad_layout()`.
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
#' @param main A single boolean value indicates whether to apply `x` for the
#' main plot.
#' @return The input object with an additional attribute that specifies the
#' selected context.
#' @export
with_quad <- function(x, position = waiver(), main = NULL) {
    assert_layout_position(position)
    assert_bool(main, null_ok = TRUE)
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
