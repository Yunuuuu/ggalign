#' Plot Adding Context Settings
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' These settings control the behavior of the plot when added to a layout, as
#' well as the arrangement of individual plot areas within the layout.
#'
#' @details
#' By default, the active context is set only for functions that add plot areas.
#' This allows other `ggplot2` elements-such as `geoms`, `stats`, `scales`, or
#' `themes`- to be seamlessly added to the current plot area.
#'
#' The default ordering of the plot areas is from top to bottom or from left to
#' right, depending on the layout orientation. However, users can customize this
#' order using the `order` argument.
#'
#' @param order An integer specifying the order of the plot area within the
#'   layout.
#' @param use A logical (`TRUE`/`FALSE`) indicating whether to set the
#'   active context to the current plot when added to a layout. If `TRUE`,
#'   any subsequent `ggplot` elements will be applied to this plot.
#' @param name A string specifying the plot's name, useful for switching active
#'   contexts through the `what` argument in functions like
#'   [`quad_anno()`]/[`stack_switch()`].
#' @importFrom S7 prop prop<-
#' @export
active <- S7::new_class("active",
    properties = list(
        order = S7::new_property(
            S7::class_integer,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single integer number")
                }
            },
            setter = function(self, value) {
                if (identical(value, NA) || identical(value, NA_integer_)) {
                    prop(self, "order", check = FALSE) <- NA_integer_
                } else if (.rlang_check_number(value, FALSE) == 0L) {
                    prop(self, "order", check = FALSE) <- as.integer(value)
                } else {
                    prop(self, "order") <- value
                }
                self
            },
            default = NA_integer_
        ),
        use = S7::new_property(
            S7::class_logical,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single boolean value")
                }
            },
            default = NA
        ),
        name = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single character string")
                }
            },
            setter = function(self, value) {
                if (identical(value, NA)) value <- NA_character_
                prop(self, "name") <- value
                self
            },
            default = NA_character_
        )
    )
)

#' @importFrom S7 S7_inherits
is_active <- function(x) S7_inherits(x, active)

#' @importFrom S7 props props<-
local(S7::method(`+`, list(active, active)) <- function(e1, e2) {
    prop_list <- props(e2)
    # it's safe to use `is.na` directly, since all properties are scalar.
    props(e1) <- prop_list[
        !vapply(prop_list, is.na, logical(1L), USE.NAMES = FALSE)
    ]
    e1
})

local(S7::method(`+`, list(active, S7::class_any)) <- function(e1, e2) {
    if (is.null(e2)) {
        return(e1)
    }
    stop_incompatible_op("+", e1, e2)
})

local(S7::method(`+`, list(S7::class_any, active)) <- function(e1, e2) {
    if (is.null(e1)) {
        return(e2)
    }
    stop_incompatible_op("+", e1, e2)
})

###############################################################
plot_control <- S7::new_class(
    "plot_control",
    properties = list(
        size = S7::new_property(
            S7::new_union(NULL, GridUnit),
            validator = function(value) {
                if (is.null(value)) return(NULL) # styler: off
                l <- length(value)
                if (l != 1L && l != 2L) {
                    return(sprintf(
                        "must be of length `1` or `2`, not length %d", l
                    ))
                }
            },
            setter = function(self, value) {
                if (!is.null(value) && !S7_inherits(value, GridUnit)) {
                    # we by default use null unit
                    if (!is.unit(value)) value <- unit(value, "null")
                    value <- convert(value, GridUnit)
                }
                prop(self, "size") <- value
                self
            },
            default = NULL
        ),
        order = S7::new_property(
            S7::new_union(NULL, S7::class_integer),
            validator = function(value) {
                if (is.null(value)) return(NULL) # styler: off
                if (length(value) != 1L) {
                    return("must be a single integer number")
                }
            },
            setter = function(self, value) {
                if (identical(value, NA) || identical(value, NA_integer_)) {
                    prop(self, "order", check = FALSE) <- NA_integer_
                } else if (.rlang_check_number(value, FALSE) == 0L) {
                    prop(self, "order", check = FALSE) <- as.integer(value)
                } else {
                    prop(self, "order") <- value
                }
                self
            },
            default = NULL
        ),
        use = S7::new_property(
            S7::class_logical,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single boolean value")
                }
            },
            default = NA
        )
    )
)

local(S7::method(`+`, list(plot_control, S7::class_missing)) <-
    function(e1, e2) {
        cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    })

#' @importFrom S7 props props<-
local(S7::method(`+`, list(plot_control, S7::class_any)) <- function(e1, e2) {
    e2name <- deparse(substitute(e2, env = caller_env(2L)))
    ggalign_update(e1, e2, e2name)
})

S7::method(ggalign_init, plot_control) <- function(x) {
    if (is.null(prop(x, "size"))) {
        prop(x, "size", check = FALSE) <- unit(NA, "null")
    }
    if (is.null(prop(x, "order"))) {
        prop(x, "order", check = FALSE) <- NA_integer_
    }
    if (is.na(prop(x, "use"))) {
        prop(x, "use", check = FALSE) <- TRUE
    }
    x
}

S7::method(ggalign_update, list(plot_control, plot_control)) <-
    function(x, object, ...) {
        if (!is.null(prop(object, "order"))) {
            prop(x, "order", check = FALSE) <- prop(object, "order")
        }
        if (!is.null(prop(object, "size"))) {
            prop(x, "size", check = FALSE) <- prop(object, "size")
        }
        if (!is.na(prop(object, "use"))) {
            prop(x, "use", check = FALSE) <- prop(object, "use")
        }
        x
    }

S7::method(ggalign_update, list(plot_control, active)) <-
    function(x, object, ...) {
        prop(x, "order") <- prop(object, "order")
        if (!is.na(prop(object, "use"))) {
            prop(x, "use") <- prop(object, "use")
        }
        x
    }

S7::method(ggalign_update, list(plot_control, S3_unit)) <-
    function(x, object, ...) {
        prop(x, "size") <- object
        x
    }

###############################################################
#' A class representing a graph object that encapsulates a `ggplot` object
#' and other properties used to extend and customize the `ggplot`.
#'
#' This class combines a **ggplot2** plot with additional properties, such as
#' schemes, which can be used to extend or modify the plot's appearance or
#' behavior.
#'
#' @importFrom S7 new_object S7_object
#' @include scheme-.R
#' @noRd
Graph <- S7::new_class(
    "Graph",
    properties = list(
        plot = S7::new_property(
            ggplot2::class_ggplot,
            default = quote(ggplot())
        ),
        schemes = Schemes,
        control = plot_control
    )
)

#' @importFrom ggplot2 update_ggplot
#' @importFrom S7 prop prop<-
S7::method(ggalign_init, Graph) <- function(x) {
    prop(x, "schemes", check = FALSE) <- schemes_complete(prop(x, "schemes"))
    prop(x, "control", check = FALSE) <- ggalign_init(prop(x, "control"))
    x
}

#' @importFrom grid grid.draw
local(S7::method(plot, Graph) <- S7::method(grid.draw, Graph) <-
    function(x, ...) {
        cli_abort("Direct plotting of {.obj_type_friendly {x}} is not supported")
    })

local(S7::method(`+`, list(Graph, S7::class_missing)) <-
    function(e1, e2) {
        cli_abort(c(
            "The {.code +} operator cannot be used with a single argument.",
            "i" = "Did you accidentally place {.code +} on a new line without a second argument?"
        ))
    })

local(S7::method(`+`, list(Graph, S7::class_any)) <- function(e1, e2) {
    e2name <- deparse(substitute(e2, env = caller_env(2L)))
    ggalign_update(e1, e2, e2name)
})

#' @importFrom ggplot2 update_ggplot
#' @importFrom S7 prop prop<-
S7::method(ggalign_update, list(Graph, S7::class_any)) <-
    function(x, object, ...) {
        if (is.null(object)) return(x) # styler: off
        prop(x, "plot", check = FALSE) <- update_ggplot(
            object, ggfun("plot_clone")(prop(x, "plot")), ...
        )
        x
    }

#' @importFrom S7 prop prop<-
S7::method(ggalign_update, list(Graph, S7::new_union(Schemes, Scheme))) <-
    function(x, object, ...) {
        prop(x, "schemes", check = FALSE) <- ggalign_update(
            prop(x, "schemes"), object, ...
        )
        x
    }

#' @importFrom S7 prop prop<-
S7::method(
    ggalign_update,
    list(Graph, S7::new_union(plot_control, active, S3_unit))
) <-
    function(x, object, ...) {
        prop(x, "control", check = FALSE) <- ggalign_update(
            prop(x, "control"), object, ...
        )
        x
    }
