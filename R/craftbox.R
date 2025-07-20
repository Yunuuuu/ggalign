#' @importFrom ggplot2 is.ggplot
#' @include schemes.R
#' @include scheme-.R
#' @include grid-utils.R
CraftBox <- S7::new_class("CraftBox",
    properties = list(
        craftsman = S7::new_property(
            S7::class_environment,
            validator = function(value) {
                if (is_craftsman(value)) {
                    return("must be a 'Craftsman' object")
                }
            }
        ),
        plot = S7::new_property(
            S7::class_any,
            validator = function(value) {
                if (!is.null(value) && !is.ggplot(value)) {
                    return("must be a 'ggplot' object")
                }
            },
            default = NULL
        ),
        active = active,
        size = S7::new_property( # plot size
            GridUnit,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be of length 1")
                }
            }
        ),
        schemes = schemes
    )
)

#' @importFrom S7 S7_inherits
is_craftbox <- function(x) S7_inherits(x, "CraftBox")

is_cross_craftbox <- function(x) {
    is_craftbox(x) && is_cross_craftsman(x@craftsman)
}

is_align_craftbox <- function(x) {
    is_craftbox(x) && is_align_craftsman(x@craftsman)
}

is_cross_craftsman <- function(x) inherits(x, "CraftCross")

is_align_craftsman <- function(x) inherits(x, "CraftAlign")

S7::method(print, CraftBox) <- function(x, ...) {
    cat(x@craftsman$summary(x@plot), sep = "\n")
    invisible(x)
}

#' @importFrom grid grid.draw
S7::method(plot, CraftBox) <- S7::method(grid.draw, CraftBox) <-
    function(x, ...) {
        cli_abort(sprintf("Cannot plot %s object directly", object_name(x)))
    }

S7::method(`+`, list(CraftBox, S7::class_missing)) <-
    function(e1, e2) {
        cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }

S7::method(`+`, list(S7::class_missing, CraftBox)) <-
    function(e1, e2) {
        cli_abort(c(
            "Cannot apply {.code +} to a single {.cls CraftBox} object.",
            "i" = "You might have used {.code +} at the end of the previous line"
        ))
    }

S7::method(`+`, list(CraftBox, S7::class_any)) <- function(e1, e2) {
    e2name <- deparse(substitute(e2, env = caller_env(2)))
    craftbox_add(e2, e1, e2name)
}

S7::method(`+`, list(S7::class_any, CraftBox)) <- function(e1, e2) {
    e1name <- deparse(substitute(e1, env = caller_env(2)))
    craftbox_add(e1, e2, e1name)
}

#' @importFrom S7 S7_dispatch
craftbox_add <- S7::new_generic(
    "craftbox_add", "object",
    function(object, box, object_name) S7_dispatch()
)

#' @importFrom ggplot2 ggplot_add
S7::method(craftbox_add, S7::class_any) <- function(object, box, object_name) {
    if (is.null(object)) return(box) # styler: off
    if (is.null(box@plot)) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(box)
            ),
            i = sprintf("no plot found for %s", object_name(box))
        ))
    }
    box@plot <- ggplot_add(object, ggfun("plot_clone")(box@plot), object_name)
    box
}

S7::method(craftbox_add, schemes) <- function(object, box, object_name) {
    box@schemes <- update_scheme(box@schemes, object)
    box
}

S7::method(craftbox_add, scheme) <- function(object, box, object_name) {
    box@schemes <- update_scheme(box@schemes, object)
    box
}

S7::method(craftbox_add, active) <- function(object, box, object_name) {
    box@active <- box@active + object
    box
}

S7::method(craftbox_add, S3_unit) <- function(object, box, object_name) {
    box@size <- convert(object, GridUnit)
    box
}

######################################################################
plot_build <- function(align, ..., schemes, theme) {
    plot <- align$build_plot(plot@plot, ...)
    align$finish_plot(plot, schemes, theme)
}
