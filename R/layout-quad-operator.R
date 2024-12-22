# `subtract` operates at layout-level
#' @keywords internal
quad_layout_subtract <- function(object, quad, object_name) {
    UseMethod("quad_layout_subtract")
}

#' @export
quad_layout_subtract.default <- function(object, quad, object_name) {
    if (is.null(context <- quad@active)) context <- c(.TLBR, list(NULL))
    for (active in context) {
        if (is.null(active)) {
            quad <- quad_body_add(object, quad, object_name)
        } else if (!is.null(slot(quad, active))) {
            slot(quad, active) <- chain_layout_subtract(
                object, slot(quad, active), object_name
            )
        }
    }
    quad
}

# for object can set at layout level
#' @export
quad_layout_subtract.ggalign_scheme <- function(object, quad, object_name) {
    if (is.null(context <- quad@active)) {
        quad <- update_layout_scheme(object, quad, object_name)
    } else {
        slot(quad, context) <- update_layout_scheme(
            object, slot(quad, context), object_name
        )
    }
    quad
}

#' @export
quad_layout_subtract.ggalign_with_quad <- function(object, quad, object_name) {
    old <- quad@active
    context <- quad_operated_context(object, old, "-")
    object <- .subset2(object, "object")
    object_name <- .subset2(object, "object_name")
    # `subtract` operates at layout-level
    if (is.null(context)) {
        quad@active <- context
        quad <- quad_layout_subtract(object, quad, object_name)
    } else {
        for (active in context) {
            if (is.null(active)) {
                quad <- quad_body_add(object, quad, object_name)
            } else if (!is.null(slot(quad, active))) {
                slot(quad, active) <- chain_layout_subtract(
                    object, slot(quad, active), object_name
                )
            }
        }
    }
    quad@active <- old
    quad
}

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
        slot(quad, position) <- chain_layout_and_add(
            object, stack, object_name
        )
    }
    quad
}

#' @export
quad_layout_and_add.ggalign_with_quad <- function(object, quad, object_name) {
    object <- .subset2(object, "object")
    object_name <- .subset2(object, "object_name")
    NextMethod()
}

#' @export
quad_layout_and_add.theme <- function(object, quad, object_name) {
    ans <- NextMethod()
    # to align with `patchwork`, we also modify the layout theme
    # when using `&` to add the theme object.
    ans@theme <- ans@theme + object
    ans
}
