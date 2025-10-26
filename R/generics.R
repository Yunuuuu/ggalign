#' Initialize the Default Properties of an Object
#'
#' This helper function is used to initialize an object before use by setting
#' its properties or internal elements to their default values. It is
#' particularly useful when the object's internal defaults differ from those
#' required for proper usage, such as when internal defaults indicate values not
#' set by the user. The function ensures that the object is properly configured
#' for subsequent operations.
#'
#' @param x An object that needs initialization.
#' @return The initialized object, ready for use.
#' @importFrom S7 S7_dispatch
#' @export
ggalign_init <- S7::new_generic("ggalign_init", "x", function(x) S7_dispatch())

#' Update the Properties of an Object
#'
#' This generic function allows for updating the properties or state of an
#' object by applying changes from another object. It ensures that the input
#' object is modified according to the specifications of the provided `object`.
#'
#' @param x The object whose properties are being updated.
#' @param object The object from which the properties will be applied to `x`.
#' @export
ggalign_update <- S7::new_generic("ggalign_update", c("x", "object"))

#' Inherit Properties from Another Object
#'
#' This generic function allows one object to inherit properties or
#' configurations from another object. It is used when an object needs to adopt
#' the state of another object without directly modifying it.
#'
#' @param x The object that will inherit properties.
#' @param object The object from which `x` will inherit properties.
#'
#' @return The `x` object with properties inherited from `object`.
#' @export
ggalign_inherit <- S7::new_generic("ggalign_inherit", c("x", "object"))

#' Generate a plot grob.
#'
#' @param x An object to be converted into a [grob][grid::grob].
#' @return A [`grob()`][grid::grob] object.
#' @seealso
#' - [`ggalign_build()`] for the process of building the object.
#' - [`ggalign_gtable()`] for converting the object into a table format.
#'
#' @examples
#' ggalignGrob(ggplot())
#' @export
ggalignGrob <- function(x) ggalign_gtable(ggalign_build(x))

#' Build a Graphical Object for Rendering
#'
#' This generic function takes an object (typically a plot) and performs all
#' necessary steps to transform it into a graphical object that can be rendered
#' in a grid-based layout.
#'
#' @param x The object to be processed and built into a renderable format.
#' @return A processed object that can be used for rendering or further
#' manipulation.
#' @importFrom S7 S7_dispatch
#' @export
ggalign_build <- S7::new_generic(
    "ggalign_build", "x",
    function(x) S7_dispatch()
)

#' Convert a Built Object into a gtable
#'
#' This generic function takes a built graphical object and converts it into a
#' [`gtable`][gtable::gtable].
#'
#' @param x The object to be converted into a gtable.
#' @return A [`gtable`][gtable::gtable] object.
#' @importFrom S7 S7_dispatch
#' @export
ggalign_gtable <- S7::new_generic(
    "ggalign_gtable", "x",
    function(x) S7_dispatch()
)

S7::method(ggalign_gtable, S7::new_S3_class("grob")) <- function(x) x

#' @importFrom S7 S7_dispatch
layout_build <- S7::new_generic(
    "layout_build", "layout",
    function(layout, context = NULL, orientation = NULL) S7_dispatch()
)

#' Get the statistics from the layout
#'
#' @param x A `r rd_layout()`.
#' @inheritParams rlang::args_dots_used
#' @return The statistics
#' @export
ggalign_stat <- S7::new_generic("ggalign_stat", "x")

S7::method(ggalign_stat, S7::class_any) <- function(x, ...) {
    cli_abort("no statistics found for {.obj_type_friendly {x}}")
}
