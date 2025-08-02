new_craftbox <- function(craftsman = NULL, ...,
                         plot = NULL, active = NULL, size = NULL,
                         schemes = NULL, call = caller_call()) {
    assert_active(active, allow_null = FALSE, call = call)
    # `call`: used to provide error message
    CraftBox(
        craftsman = ggproto(NULL, craftsman %||% Craftsman, ..., call = call),
        schemes = schemes %||% default_schemes(),
        plot = plot, active = active, size = size
    )
}

#' @importFrom ggplot2 is.ggplot
#' @include scheme-.R
#' @include grid-utils.R
CraftBox <- S7::new_class("CraftBox",
    properties = list(
        craftsman = S7::new_property(
            S7::class_any,
            validator = function(value) {
                if (!is_craftsman(value)) {
                    return("must be a 'Craftsman' object")
                }
            },
            setter = function(self, value) {
                if (!is.null(prop(self, "craftsman"))) {
                    cli_abort("'@craftsman' is read-only")
                }
                prop(self, "craftsman") <- value
                self
            }
        ),
        plot = S7::new_property(
            S7::new_union(S7::class_any),
            validator = function(value) {
                if (!is.null(value) && !is_ggplot(value)) {
                    return("must be a 'ggplot' object")
                }
            },
            setter = function(self, value) {
                if (!is.null(prop(self, "plot"))) {
                    cli_abort("'@plot' is read-only")
                }
                prop(self, "plot") <- value
                self
            },
            default = NULL
        ),
        active = S7::new_property(
            active,
            setter = function(self, value) {
                if (!is.null(prop(self, "active"))) {
                    cli_abort("'@active' is read-only; use the '+' operator to update it.")
                }
                prop(self, "active") <- value
                self
            }
        ),
        size = S7::new_property( # plot size
            GridUnit,
            setter = function(self, value) {
                # make sure it is a `unit` object
                if (is.null(value)) {
                    value <- unit(NA, "null")
                } else {
                    value <- check_size(value, arg = "@size")
                }
                value <- convert(value, GridUnit)
                prop(self, "size", check = FALSE) <- value
                self
            }
        ),
        schemes = S7::new_property(
            Schemes,
            setter = function(self, value) {
                if (!is.null(prop(self, "schemes"))) {
                    cli_abort("'@schemes' is read-only; use the '+' operator to update it.")
                }
                prop(self, "schemes") <- value
                self
            }
        )
    )
)

#' @importFrom S7 S7_inherits
is_craftbox <- function(x) S7_inherits(x, CraftBox)

#' @importFrom S7 prop
is_cross_craftbox <- function(x) {
    is_craftbox(x) && is_cross_craftsman(prop(x, "craftsman"))
}

#' @importFrom S7 prop
is_align_craftbox <- function(x) {
    is_craftbox(x) && is_align_craftsman(prop(x, "craftsman"))
}

is_cross_craftsman <- function(x) inherits(x, "CraftCross")

is_align_craftsman <- function(x) inherits(x, "CraftAlign")

#' @importFrom utils str
local(S7::method(str, CraftBox) <- function(object, ...) {
    cat(prop(object, "craftsman")$summary(prop(object, "plot")), sep = "\n")
    invisible(object)
})

#' @importFrom grid grid.draw
local(S7::method(plot, CraftBox) <- S7::method(grid.draw, CraftBox) <-
    function(x, ...) {
        cli_abort(sprintf("Cannot plot %s object directly", object_name(x)))
    })

local(S7::method(`+`, list(CraftBox, S7::class_missing)) <-
    function(e1, e2) {
        cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    })

local(S7::method(`+`, list(S7::class_missing, CraftBox)) <-
    function(e1, e2) {
        cli_abort(c(
            "Cannot apply {.code +} to a single {.cls CraftBox} object.",
            "i" = "You might have used {.code +} at the end of the previous line"
        ))
    })

local(S7::method(`+`, list(CraftBox, S7::class_any)) <- function(e1, e2) {
    e2name <- deparse(substitute(e2, env = caller_env(2L)))
    craftbox_add(e2, e1, e2name)
})

local(S7::method(`+`, list(S7::class_any, CraftBox)) <- function(e1, e2) {
    e1name <- deparse(substitute(e1, env = caller_env(2L)))
    craftbox_add(e1, e2, e1name)
})

#' @importFrom S7 S7_dispatch
craftbox_add <- S7::new_generic(
    "craftbox_add", "object",
    function(object, box, objectname) S7_dispatch()
)

#' @importFrom ggplot2 ggplot_add
S7::method(craftbox_add, S7::class_any) <- function(object, box, objectname) {
    if (is.null(object)) return(box) # styler: off
    if (is.null(prop(box, "plot"))) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {objectname}} to %s",
                object_name(box)
            ),
            i = sprintf("no plot found for %s", object_name(box))
        ))
    }
    attr(box, "plot") <- ggplot_add(
        object, ggfun("plot_clone")(prop(box, "plot")), objectname
    )
    box
}

# Bypass S7 setter validation: update internal property via attr() directly
S7::method(craftbox_add, Schemes) <- function(object, box, objectname) {
    attr(box, "schemes") <- scheme_update(prop(box, "schemes"), object)
    box
}

S7::method(craftbox_add, Scheme) <- function(object, box, objectname) {
    attr(box, "schemes") <- scheme_update(prop(box, "schemes"), object)
    box
}

S7::method(craftbox_add, active) <- function(object, box, objectname) {
    attr(box, "active") <- active_update(prop(box, "active"), object)
    box
}

S7::method(craftbox_add, S3_unit) <- function(object, box, objectname) {
    prop(box, "size", check = FALSE) <- object
    box
}

######################################################################
plot_build <- function(align, ..., schemes, theme) {
    plot <- align$build_plot(plot@plot, ...)
    align$finish_plot(plot, schemes, theme)
}
