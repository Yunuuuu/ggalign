#' A container for multiple layout schemes
#'
#' `Schemes` is a container class that holds a list of `Scheme` objects, each
#' uniquely identified by their `key` property (usually derived from the class
#' name). It is used internally by the `ggalign` system to manage sets of layout
#' or rendering configurations that can be inherited, updated, or applied during
#' plot composition.
#'
#' @param ... <[dyn-dots][rlang::dyn-dots]> A list of [`Scheme`] object.
#' @section Keys and Validation:
#' - Each `Scheme` in the container must have a unique key.
#' - The key is used to reference and update individual schemes.
#'
#' @section Accessors:
#' - `value`: The underlying list of `Scheme` objects.
#' - `entries`: A named list of schemes, where names are derived from their
#'   keys.
#' - `keys`: A character vector of all scheme keys.
#'
#' @seealso [`Scheme`]
#' @keywords internal
#' @importFrom S7 S7_inherits
#' @export
Schemes <- S7::new_class("Schemes",
    properties = list(
        value = S7::new_property(
            S7::class_list,
            validator = function(value) {
                if (!all(vapply(value, S7_inherits, logical(1L), Scheme,
                                USE.NAMES = FALSE))) { # styler: off
                    return("'must be a list of `scheme` objects")
                }
                keys <- vapply(
                    value,
                    function(entry) prop(entry, "key"), character(1L),
                    USE.NAMES = FALSE
                )
                if (vec_duplicate_any(keys)) {
                    return("must not contain duplicate `key` values")
                }
                NULL
            }
        ),
        entries = S7::new_property(
            getter = function(self) {
                out <- prop(self, "value")
                names(out) <- prop(self, "keys")
                out
            }
        ),
        keys = S7::new_property(
            getter = function(self) {
                vapply(
                    prop(self, "value"),
                    function(entry) prop(entry, "key"), character(1L),
                    USE.NAMES = FALSE
                )
            }
        )
    ),
    constructor = function(...) new_object(S7_object(), value = list2(...))
)

prop_schemes <- function(property, ...) {
    force(property)
    S7::new_property(
        Schemes,
        setter = function(self, value) {
            prop(self, property) <- value
            self
        },
        ...
    )
}

#' @importFrom utils str
#' @importFrom S7 props
local(S7::method(str, Schemes) <- function(object, ..., nest.lev = 0) {
    cat(if (nest.lev > 0) " ")
    cat(paste0("<", main_class(object), ">\n"))
    str_nest(.subset(props(object), "value"), "@", ..., nest.lev = nest.lev)
})

#' @importFrom rlang list2
schemes_set <- function(schemes, ..., check = TRUE) {
    entries <- prop(schemes, "entries")
    for (entry in list2(...)) {
        entries[[prop(entry, "key")]] <- entry
    }
    prop(schemes, "value", check = check) <- entries
    schemes
}

schemes_get <- function(schemes, scheme) {
    entries <- prop(schemes, "entries")
    .subset2(entries, sprintf("ggalign::%s", scheme))
}

#' @importFrom ggplot2 theme waiver
default_schemes <- function(data = NULL, th = theme()) {
    if (!is.waive(data)) data <- NULL
    Schemes(
        scheme_data(data),
        scheme_theme(th),
        scheme_align(waiver(), waiver(), waiver())
    )
}

#' Abstract Scheme Class
#'
#' `scheme` is an abstract base class that represents a configurable scheme with
#' a unique `key` (the first class name). Developers should create subclasses of
#' `scheme` to define specific schemes used in layouts or plotting contexts.
#'
#' Developers should subclass `Scheme` to implement specific behaviors (e.g.,
#' theme adjustments, alignment guides, layout spacings) and define how those
#' schemes are initialized, combined, and applied to plots.
#'
#'
#' @section Developer Guide:
#' When creating a new subclass of `Scheme`, you may optionally override the
#' following methods to customize its behavior:
#'
#' - [`scheme_init(scheme)`][scheme_init] *(optional)*: Initializes the scheme,
#'   often by assigning default values or computing derived properties.
#'
#'   **Default behavior**: Returns the scheme unchanged.
#'
#' - [`scheme_update(e1, e2)`][scheme_update] *(optional)*: Defines how to
#'   update a scheme by merging it with another of the same key (e.g., during
#'   user overrides).
#'
#'   **Default behavior**: Replaces `e1` entirely with `e2`.
#'
#' - [`scheme_inherit(e1, e2)`][scheme_inherit] *(optional)*: Defines how a
#'   scheme inherits from a parent scheme (e.g., a layout template), typically
#'   merging instead of replacing.
#'
#'   **Default behavior**: Inheritance is ignored; `e2` is returned unchanged.
#'
#' - [`plot_add_scheme(plot, scheme, ...)`][plot_add_scheme]: Applies the scheme
#'   to a plot object (usually a `ggplot`) by modifying the plot components,
#'   theming, or annotations.
#' @keywords internal
#' @export
Scheme <- S7::new_class(
    "Scheme",
    properties = list(
        # Usually the S7 class will automatically add the package name
        # so the key will always be unique
        key = S7::new_property(getter = function(self) .subset(class(self), 1L))
    ),
    abstract = TRUE
)

#' Initialize a scheme object
#'
#' `scheme_init()` is a developer-facing generic used to define the initial
#' state of a [`Scheme`] object. It is typically called during rendering to
#' initialize layout schemes, allowing plots to inherit layout behavior from the
#' scheme.
#'
#' @param scheme A [`Scheme`] object to initialize.
#' @return The initialized [`Scheme`] object.
#' @keywords internal
#' @export
scheme_init <- S7::new_generic("scheme_init", "scheme", function(scheme) {
    S7_dispatch()
})

#' @importFrom S7 S7_inherits prop prop<-
S7::method(scheme_init, Schemes) <- function(scheme) {
    schemes <- lapply(prop(scheme, "value"), function(s) {
        if (!S7_inherits(s <- scheme_init(s), Scheme)) {
            cli_abort("{.fn scheme_update} method must return a {.cls scheme}")
        }
        s
    })
    prop(scheme, "value", check = FALSE) <- schemes
    scheme
}

S7::method(scheme_init, Scheme) <- function(scheme) scheme

#' Update the scheme
#'
#' `scheme_update()` is used by developers to define how two [`Scheme`] objects
#' with the same key should be merged. This typically happens when adding or
#' updating a [`Scheme`] in a collection.
#'
#' @param e1 The original [`Scheme`] object.
#' @param e2 The new [`Scheme`] object. Usually should have the same `key` as
#' `e1`.
#' @param ... Additional arguments passed to methods.
#' @return A new [`Scheme`] object, resulting from merging `e1` and `e2`.
#' @keywords internal
#' @export
scheme_update <- S7::new_generic("scheme_update", c("e1", "e2"))

S7::method(scheme_update, list(Schemes, Schemes)) <- function(e1, e2, e2name) {
    for (entry in prop(e2, "value")) {
        e1 <- scheme_update(e1, entry, e2name)
    }
    e1
}

#' @importFrom S7 S7_inherits prop
S7::method(scheme_update, list(Schemes, Scheme)) <- function(e1, e2, e2name) {
    # By default, we remove the original value and set the new one
    entries <- prop(e1, "entries")
    new_entry <- scheme_update(.subset2(entries, prop(e2, "key")), e2, e2name)
    if (!S7_inherits(new_entry, Scheme)) {
        cli_abort("{.fn scheme_update} method must return a {.cls scheme}")
    }
    entries[[prop(new_entry, "key")]] <- new_entry
    attr(e1, "value") <- entries
    e1
}

S7::method(scheme_update, list(Scheme, S7::class_any)) <-
    function(e1, e2, ...) {
        if (is.null(e2)) return(e1) # styler: off
        cli_abort("No {.fn scheme_update} method for {.obj_type_friendly {e2}}")
    }

# No parent scheme
S7::method(scheme_update, list(S7::class_any, Scheme)) <-
    function(e1, e2, ...) {
        if (is.null(e1)) return(e2) # styler: off
        cli_abort("No {.fn scheme_update} method for {.obj_type_friendly {e1}}")
    }

S7::method(scheme_update, list(Scheme, Scheme)) <- function(e1, e2, ...) e2

#' Inherit a scheme from a parent
#'
#' This generic is used by developers to define how one [`Scheme`] object
#' inherits from another (typically the scheme defined in the layout). This is
#' called when adding a new [`Scheme`] via inheritance.
#'
#' @param e1 The parent [`Scheme`] object.
#' @param e2 The child [`Scheme`] object. Usually should have the same `key` as
#' `e1`.
#' @return A new [`Scheme`] object.
#' @keywords internal
#' @export
scheme_inherit <- S7::new_generic(
    "scheme_inherit", c("e1", "e2"),
    function(e1, e2) S7_dispatch()
)

#' @importFrom S7 prop
#' @keywords internal
S7::method(scheme_inherit, list(Schemes, Schemes)) <- function(e1, e2) {
    for (entry in prop(e2, "value")) {
        e1 <- scheme_inherit(e1, entry)
    }
    e1
}

#' @importFrom S7 S7_inherits prop
#' @keywords internal
S7::method(scheme_inherit, list(Schemes, Scheme)) <- function(e1, e2) {
    entries <- prop(e1, "entries")
    new_entry <- scheme_inherit(.subset2(entries, prop(e2, "key")), e2)
    if (!S7_inherits(new_entry, Scheme)) {
        cli_abort("{.fn scheme_inherit} method must return a {.cls scheme}")
    }
    entries[[prop(new_entry, "key")]] <- new_entry
    attr(e1, "value") <- entries
    e1
}

S7::method(scheme_inherit, list(Scheme, S7::class_any)) <- function(e1, e2) {
    if (is.null(e2)) return(e1) # styler: off
    cli_abort("No {.fn scheme_inherit} method for {.obj_type_friendly {e2}}")
}

# No parent scheme
S7::method(scheme_inherit, list(S7::class_any, Scheme)) <- function(e1, e2) {
    if (is.null(e1)) return(e2) # styler: off
    cli_abort("No {.fn scheme_inherit} method for {.obj_type_friendly {e1}}")
}

S7::method(scheme_inherit, list(Scheme, Scheme)) <- function(e1, e2) e2

#' Apply a Scheme to a plot
#'
#' `plot_add_scheme()` is a generic used to apply a [`Scheme`] (or a [`Schemes`]
#' container) to a plot object. This allows schemes to update or modify
#' the plot's appearance or behavior according to their configuration.
#'
#' By default When a [`Schemes`] object is passed, each individual [`Scheme`] is
#' applied in sequence via its respective `plot_add_scheme()` method.
#'
#' @param plot A plot object, typically a ggplot.
#' @param scheme A [`Scheme`] or [`Schemes`] object to apply.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return The modified `plot` object.
#' @keywords internal
#' @export
plot_add_scheme <- S7::new_generic("plot_add_scheme", c("plot", "scheme"))

S7::method(plot_add_scheme, list(S7::class_any, Schemes)) <-
    function(plot, scheme, ...) {
        entries <- prop(scheme, "entries")
        for (entry in entries) {
            plot <- plot_add_scheme(plot, entry, ...)
        }
        plot
    }

#' @include utils-ggplot.R
S7::method(plot_add_scheme, list(S3_class_ggplot, Scheme)) <-
    function(plot, scheme, ...) {
        cli_abort(
            "No {.fn plot_add_scheme} method for {.obj_type_friendly {plot}} and {.obj_type_friendly {scheme}}"
        )
    }

update_layout_schemes <- function(object, layout, object_name) {
    layout@schemes <- scheme_update(layout@schemes, object, object_name)
    layout
}
