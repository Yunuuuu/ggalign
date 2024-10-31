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
quad_layout_and_add.ggplot <- function(object, quad, object_name) {
    cli::cli_abort(c(
        "Cannot add {.var {object_name}} into the quad layout",
        i = "try to use {.fn ggalign} to initialize the {.cls ggplot}"
    ))
}
