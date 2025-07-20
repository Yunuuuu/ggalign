#' @importFrom S7 S7_inherits
#' @keywords internal
schemes <- S7::new_class("schemes",
    properties = list(
        entries = S7::new_property(
            S7::class_list,
            setter = function(self, value) {
                if (!all(vapply(value, S7_inherits, logical(1L), "scheme",
                                USE.NAMES = FALSE))) { # styler: off
                    cli_abort("'@entries' must be a list of `scheme` object")
                }
                for (entry in value) {
                    self <- self + entry
                }
                self
            }
        ),
        keys = S7::new_property(
            getter = function(self) {
                vapply(self,
                    function(entry) entry@key, character(1L),
                    USE.NAMES = FALSE
                )
            }
        )
    )
)

#' @importFrom ggplot2 theme
new_schemes <- function(data = NULL, th = theme()) {
    if (!is.waive(data)) data <- NULL
    schemes(list(new_scheme_data(data), new_scheme_theme(th)))
}

#' Abstract scheme Class
#'
#' `scheme` is an abstract base class that represents a configurable scheme with
#' a unique `key` (the first class name). Developers should create subclasses of
#' `scheme` to define specific schemes used in layouts or plotting contexts.
#'
#' @section Developer Guide:
#' When defining a new `scheme` subclass, developers **must implement** the
#' following methods for their class:
#'
#' - [`update_scheme(e1, e2)`][update_scheme]: Defines how to merge two schemes
#'   with the same key when updating or replacing schemes.
#' - [`inherit_scheme(e1, e2)`][inherit_scheme]: Defines how a scheme inherits
#'   from a parent scheme, typically merging configurations rather than
#'   replacing.
#' - [`update_ggplot(e1, e2)`][ggplot2::update_ggplot]: Defines how the scheme
#'   modifies a ggplot object during plot updates.
#'
#' @export
scheme <- S7::new_class(
    "scheme",
    properties = list(
        key = S7::new_property(getter = function(self) .subset(class(self), 1L))
    ),
    abstract = TRUE
)

#' Update the scheme
#'
#' `update_scheme()` is used by developers to define how two `scheme` objects
#' with the same key should be merged. This typically happens when adding or
#' updating a `scheme` in a collection.
#'
#' @param e1 The original `scheme` object.
#' @param e2 The new `scheme` object. Must have the same `key` as `e1`.
#' @return A new `scheme` object, resulting from merging `e1` and `e2`.
#' @export
update_scheme <- S7::new_generic("update_scheme", c("e1", "e2"))

S7::method(update_scheme, list(schemes, schemes)) <- function(e1, e2) {
    for (entry in e2@entries) {
        e1 <- update_scheme(e1, entry)
    }
    e1
}

#' @importFrom S7 S7_inherits
S7::method(update_scheme, list(schemes, scheme)) <- function(e1, e2) {
    # By default, we remove the original value and set the new one
    entries <- e1@entries
    if (!is.null(old <- .subset2(entries, e2@key))) {
        new_entry <- update_scheme(old, e2)
        if (!S7_inherits(new_entry, "scheme") || new_entry@key != e2@key) {
            cli_abort(
                "{.fn update_scheme} method must return a {.cls scheme} with the same key"
            )
        }
    } else {
        new_entry <- e2
    }
    entries[[new_entry@key]] <- new_entry
    attr(e1, "entries") <- entries
    e1
}

S7::method(update_scheme, list(scheme, scheme)) <- function(e1, e2, ...) e2

#' Inherit a scheme from a parent
#'
#' This generic is used by developers to define how one `scheme` object inherits
#' from another (typically the scheme defined in the layout). This is called
#' when adding a new `scheme` via inheritance (not replacement).
#'
#' @param e1 The parent `scheme` object.
#' @param e2 The child `scheme` object. Must have the same `key` as `e1`.
#'
#' @return A new `scheme` object.
#' @export
inherit_scheme <- S7::new_generic("inherit_scheme", c("e1", "e2"))

S7::method(inherit_scheme, list(schemes, schemes)) <- function(e1, e2) {
    for (entry in e2@entries) {
        e1 <- inherit_scheme(e1, entry)
    }
    e1
}

#' @importFrom S7 S7_inherits
S7::method(inherit_scheme, list(schemes, scheme)) <- function(e1, e2) {
    entries <- e1@entries
    if (!is.null(old <- .subset2(entries, e2@key))) {
        new_entry <- inherit_scheme(old, e2)
        if (!S7_inherits(new_entry, "scheme") || new_entry@key != e2@key) {
            cli_abort(
                "{.fn inherit_scheme} method must return a {.cls scheme} with the same key"
            )
        }
    } else {
        new_entry <- e2
    }
    entries[[new_entry@key]] <- new_entry
    attr(e1, "entries") <- entries
    e1
}

S7::method(inherit_scheme, list(scheme, scheme)) <-
    function(e1, e2, ...) {
        cli_abort("No {.fn inherit_scheme} method for {.obj_type_friendly {e1}}")
    }

#' @importFrom ggplot2 update_ggplot class_ggplot
S7::method(update_ggplot, list(schemes, class_ggplot)) <-
    function(object, plot, ...) {
        entries <- object@entries
        for (entry in entries) {
            plot <- update_ggplot(entry, plot, ...)
        }
        plot
    }

#' @importFrom ggplot2 update_ggplot class_ggplot
S7::method(update_ggplot, list(scheme, class_ggplot)) <-
    function(object, plot, ...) {
        cli_abort(
            "No {.fn update_ggplot} method for {.obj_type_friendly {object}}"
        )
    }
