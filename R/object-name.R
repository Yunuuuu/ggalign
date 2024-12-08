format_object_name <- function(name, format = NULL) {
    if (is.null(format)) {
        name
    } else {
        sprintf("{.%s %s}", format, name)
    }
}

object_name <- function(object, format) UseMethod("object_name")

#' @export
object_name.StackLayout <- function(object, format = "fn") {
    ans <- format_object_name(object@name, format)
    if (!is.null(position <- .subset2(object@heatmap, "position"))) {
        ans <- sprintf("the %s annotation %s", position, ans)
    } else {
    }
    ans
}

#' @export
object_name.QuadLayout <- function(object, format = "fn") {
    format_object_name(object@name, format)
}

#' @export
object_name.ggalign_align_plot <- function(object, format = "fn") {
    object_name(object@align, format)
}

#' @export
object_name.ggalign_free_plot <- function(object, format = "cls") {
    format_object_name(.subset(class(object), 1L), format)
}

#' @export
object_name.ggalign_free_gg <- function(object, format = "fn") {
    format_object_name("ggfree", format)
}

#' @export
object_name.AlignProto <- function(object, format = "fn") {
    format_object_name(snake_class(object), format)
}
