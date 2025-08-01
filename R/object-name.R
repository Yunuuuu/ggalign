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
        ans <- sprintf("the %s %s", object@direction, ans)
    }
    ans
}

#' @export
object_name.CircleLayout <- function(object, format = "fn") {
    format_object_name(object@name, format)
}

#' @export
object_name.QuadLayout <- function(object, format = "fn") {
    format_object_name(object@name, format)
}

#' @export
`object_name.ggalign::CraftBox` <- function(object, format = "fn") {
    object_name(prop(object, "craftsman"), format)
}

#' @export
object_name.Craftsman <- function(object, format = "fn") {
    format_object_name(snake_class(object), format)
}

#' @export
object_name.AlignGg <- function(object, format = "fn") {
    format_object_name("ggalign", format)
}

#' @export
object_name.CrossGg <- function(object, format = "fn") {
    format_object_name("ggcross", format)
}

#' @export
object_name.FreeGg <- function(object, format = "fn") {
    format_object_name("ggfree", format)
}

#' @export
object_name.MarkGg <- function(object, format = "fn") {
    format_object_name("ggmark", format)
}
