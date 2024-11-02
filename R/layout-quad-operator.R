###############################################################
#' @keywords internal
quad_layout_subtract <- function(object, quad, object_name) {
    UseMethod("quad_layout_subtract")
}

#' @export
quad_layout_subtract.default <- function(object, quad, object_name) {
    if (is.null(position <- quad@active)) {
        quad <- quad_body_add(object, quad, object_name)
        return(quad)
    }
    slot(quad, position) <- stack_layout_subtract(
        object, slot(quad, position), object_name
    )
    quad
}

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
        "Cannot use {.code -} to add a {.fn {snake_class(object)}}",
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
        "Cannot use {.code &} to add a {.fn {snake_class(object)}}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
quad_layout_and_add.layout_annotation <- quad_layout_and_add.layout_title
