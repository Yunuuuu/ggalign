# `subtract` operates at layout-level
#' @keywords internal
quad_layout_subtract <- function(object, quad, object_name) {
    UseMethod("quad_layout_subtract")
}

#' @export
quad_layout_subtract.default <- function(object, quad, object_name) {
    if (is.null(context <- quad@current)) context <- c(.TLBR, list(NULL))
    for (active in context) {
        if (is.null(active)) {
            quad <- quad_body_add(object, quad, object_name)
        } else if (!is.null(prop(quad, active))) {
            prop(quad, active) <- chain_layout_subtract(
                object, prop(quad, active), object_name
            )
        }
    }
    quad
}

# for object can set at layout level
#' @export
`quad_layout_subtract.ggalign::Scheme` <- function(object, quad, object_name) {
    if (is.null(context <- quad@current)) {
        quad <- update_layout_schemes(object, quad, object_name)
    } else {
        prop(quad, context) <- update_layout_schemes(
            object, prop(quad, context), object_name
        )
    }
    quad
}

#' @export
quad_layout_subtract.ggalign_with_quad <- function(object, quad, object_name) {
    old <- quad@current
    context <- quad_operated_context(object, old, "-")
    object_name <- .subset2(object, "object_name")
    object <- .subset2(object, "object")
    # `subtract` operates at layout-level
    if (is.null(context)) {
        quad@current <- context
        quad <- quad_layout_subtract(object, quad, object_name)
    } else {
        for (active in context) {
            if (is.null(active)) {
                quad <- quad_body_add(object, quad, object_name)
            } else if (!is.null(prop(quad, active))) {
                prop(quad, active) <- chain_layout_subtract(
                    object, prop(quad, active), object_name
                )
            }
        }
    }
    quad@current <- old
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
        stack <- prop(quad, position)
        if (is.null(stack)) next
        prop(quad, position) <- chain_layout_and_add(
            object, stack, object_name
        )
    }
    quad
}

#' @export
quad_layout_and_add.ggalign_with_quad <- function(object, quad, object_name) {
    object <- .subset2(object, "object")
    object_name <- .subset2(object, "object_name")
    quad_layout_and_add(object, quad, object_name)
}

#' @export
quad_layout_and_add.theme <- function(object, quad, object_name) {
    ans <- NextMethod()
    # to align with `patchwork`, we also modify the layout theme
    # when using `&` to add the theme object.
    ans@theme <- ggfun("add_theme")(ans@theme, object)
    ans
}
