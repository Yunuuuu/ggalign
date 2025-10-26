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
#' @importFrom rlang list2
#' @importFrom S7 S7_inherits
Schemes <- S7::new_class("Schemes",
    properties = list(
        keys = S7::new_property(
            getter = function(self) {
                vapply(
                    prop(self, "sets"),
                    function(entry) prop(entry, "key"), character(1L),
                    USE.NAMES = FALSE
                )
            }
        ),
        sets = S7::new_property(
            S7::class_list,
            validator = function(value) {
                if (!all(vapply(value, S7_inherits, logical(1L), Scheme,
                                USE.NAMES = FALSE))) { # styler: off
                    return("'must be a list of `scheme` objects")
                }
                keys <- vapply(
                    value,
                    function(scheme) prop(scheme, "key"), character(1L),
                    USE.NAMES = FALSE
                )
                if (vec_duplicate_any(keys)) {
                    return("must not contain duplicate `key` values")
                }
            },
            setter = function(self, value) {
                names(value) <- NULL
                prop(self, "sets") <- value
                self
            }
        ),
        entries = S7::new_property(
            getter = function(self) {
                out <- prop(self, "sets")
                names(out) <- prop(self, "keys")
                out
            }
        )
    ),
    constructor = function(...) new_object(S7_object(), sets = list2(...))
)

#' @importFrom utils str
#' @importFrom S7 prop
local(S7::method(str, Schemes) <- function(object, ..., nest.lev = 0) {
    cat(if (nest.lev > 0) " ")
    cat(paste0("<", main_class(object), ">\n"))
    str_nest(props(object, "sets"), "@", ..., nest.lev = nest.lev)
})

#' @importFrom rlang list2
#' @importFrom S7 prop prop<-
schemes_set <- function(schemes, ..., check = TRUE) {
    entries <- prop(schemes, "entries")
    for (entry in list2(...)) {
        entries[[prop(entry, "key")]] <- entry
    }
    prop(schemes, "sets", check = check) <- entries
    schemes
}

#' @importFrom S7 prop
schemes_get <- function(schemes, scheme, pkg) {
    entries <- prop(schemes, "entries")
    if (missing(pkg)) pkg <- pkg_nm()
    if (is.null(pkg)) key <- scheme else key <- sprintf("%s::%s", pkg, scheme)
    .subset2(entries, key)
}

#' Complete Plot Schemes
#'
#' This method completes the set of schemes for the given input object by
#' ensuring that the necessary scheme components are included. It checks if
#' the input object contains keys for `scheme_data`, `scheme_theme`, and
#' `scheme_align`, and if any of these are missing.
#'
#' The function ensures that the input object has all the required schemes
#' before it can be used for further processing or visualization.
#' @noRd
schemes_complete <- function(schemes) {
    new_sets <- list()
    keys <- prop(schemes, "keys")
    if (!any(keys == "ggalign::scheme_data")) {
        new_sets <- c(new_sets, scheme_data())
    }
    if (!any(keys == "ggalign::scheme_theme")) {
        new_sets <- c(new_sets, scheme_theme())
    }
    if (!any(keys == "ggalign::scheme_align")) {
        new_sets <- c(new_sets, scheme_align(waiver(), waiver(), waiver()))
    }
    schemes_set(schemes, !!!new_sets, check = FALSE)
}

#' @importFrom ggplot2 theme waiver
default_schemes <- function(data = NULL, th = theme()) {
    if (!is_waiver(data)) data <- NULL
    Schemes(
        scheme_data(data), scheme_theme(th),
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
#' - [`ggalign_init()`][ggalign_init] *(optional)*: Initializes the
#'   scheme, often by assigning default values or computing derived properties.
#'
#'   **Default behavior**: Returns the scheme unchanged.
#'
#' - [`ggalign_update(x, object, ...)`][ggalign_update] *(optional)*: Defines
#'   how to update a scheme by merging it with another of the same key.
#'
#'   **Default behavior**: Replaces `x` entirely with `object`.
#'
#' - [`ggalign_inherit(x, object)`][ggalign_inherit] *(optional)*: Defines how a
#'   scheme inherits from a parent scheme (e.g., a layout template), typically
#'   merging instead of replacing.
#'
#'   **Default behavior**: Inheritance is ignored; `x` is returned unchanged.
#'
#' - [`ggalign_update(x, object, ...)`][ggalign_update]: Applies the scheme
#'   (`object`) to a plot (`x`) (usually a `ggplot`) by modifying the `x`
#'   components, theming, or annotations.
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

S7::method(ggalign_init, Scheme) <- function(x) x

S7::method(ggalign_update, list(Scheme, S7::class_any)) <-
    function(x, object, ...) {
        if (is.null(object)) return(x) # styler: off
        cli_abort(sprintf(
            "No {.fn ggalign_update} method for %s and %s",
            "{.obj_type_friendly {x}}",
            "{.obj_type_friendly {object}}"
        ))
    }

S7::method(ggalign_update, list(Scheme, Scheme)) <- function(x, object, ...) {
    object
}

S7::method(ggalign_inherit, list(Scheme, Scheme)) <- function(x, object) x

#' Apply a Scheme to a plot
#'
#' `plot_add_scheme()` is a generic used to apply a [`Scheme`] (or a [`Schemes`]
#' container) to a plot object. This allows schemes to update or modify
#' the plot's appearance or behavior according to their configuration.
#'
#' By default When a [`Schemes`] object is passed, each individual [`Scheme`] is
#' applied in sequence via its respective `plot_add_scheme()` method.
#'
#' @inheritParams ggalign_update
#' @keywords internal
#' @export
plot_add_scheme <- ggalign_update

S7::method(ggalign_update, list(ggplot2::class_ggplot, Scheme)) <-
    function(x, object, ...) {
        cli_abort(
            "No {.fn ggalign_update} method for {.obj_type_friendly {x}} and {.obj_type_friendly {object}}"
        )
    }

###########################################################################
#' @importFrom S7 prop prop<-
S7::method(ggalign_init, Schemes) <- function(x) {
    prop(x, "sets") <- lapply(prop(x, "sets"), ggalign_init)
    x
}

#' @importFrom S7 prop
#' @keywords internal
S7::method(ggalign_update, list(Schemes, Schemes)) <- function(x, object, ...) {
    for (scheme in prop(object, "sets")) {
        x <- ggalign_update(x, scheme, ...)
    }
    x
}

#' @importFrom S7 prop prop<-
S7::method(ggalign_update, list(Schemes, Scheme)) <- function(x, object, ...) {
    entries <- prop(x, "entries")
    if (is.null(entry <- .subset2(entries, prop(object, "key")))) {
        entry <- object
    } else {
        entry <- ggalign_update(entry, object, ...)
    }
    entries[[prop(entry, "key")]] <- entry
    prop(x, "sets") <- entries
    x
}

#' @importFrom S7 prop
#' @keywords internal
S7::method(ggalign_inherit, list(Schemes, Schemes)) <- function(x, object, ...) {
    for (scheme in prop(object, "sets")) {
        x <- ggalign_inherit(x, scheme, ...)
    }
    x
}

#' @importFrom S7 prop
#' @keywords internal
S7::method(ggalign_inherit, list(Schemes, Scheme)) <- function(x, object, ...) {
    entries <- prop(x, "entries")
    if (is.null(entry <- .subset2(entries, prop(object, "key")))) {
        entry <- object
    } else {
        entry <- ggalign_inherit(entry, object, ...)
    }
    entries[[prop(entry, "key")]] <- entry
    prop(x, "sets") <- entries
    x
}

S7::method(ggalign_update, list(S7::class_any, Schemes)) <-
    function(x, object, ...) {
        for (entry in prop(object, "sets")) {
            x <- ggalign_update(x, entry, ...)
        }
        x
    }
