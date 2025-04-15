# Use S4 to override the double dispatch problem of ggplot2
# And it's easy to convert a S4 Class to a S7 Class
methods::setClass(
    "CraftBox",
    list(
        plot = "ANY", # To avoid modify in place, we put plot in a slot
        active = "ANY",
        size = "ANY",
        schemes = "ANY",
        craftsman = "ANY" # `Craftsman` object
    )
)

#' Show `CraftBox` information
#' @param object A `CraftBox` object.
#' @return The input invisiblely.
#' @keywords internal
methods::setMethod("show", "CraftBox", function(object) {
    print(object)
})

#' @importFrom methods new
new_craftbox <- function(craftsman = NULL, ...,
                         plot = NULL, active = NULL, size = NULL,
                         schemes = NULL, call = caller_call()) {
    assert_active(active, allow_null = FALSE, call = call)
    if (is.null(size)) {
        size <- unit(NA, "null")
    } else {
        size <- check_size(size, call = call)
    }
    new(
        "CraftBox",
        # `call`: used to provide error message
        craftsman = ggproto(NULL, craftsman %||% Craftsman, ..., call = call),
        schemes = schemes %||% default_schemes(),
        plot = plot, active = active, size = size
    )
}

#' @export
print.CraftBox <- function(x, ...) {
    cat(x@craftsman$summary(x@plot), sep = "\n")
    invisible(x)
}

#' @export
plot.CraftBox <- function(x, ...) {
    cli_abort(sprintf("Cannot plot %s object directly", object_name(x)))
}

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.CraftBox <- plot.CraftBox

#' Add custom objects to ggalign plot
#' @keywords internal
methods::setMethod("+", c("CraftBox", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli_abort(c(
            "Cannot use {.code {.Generic}} with a single argument.",
            "i" = "Did you accidentally put {.code {.Generic}} on a new line?"
        ))
    }

    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- paste(deparse(substitute(e2)), collapse = " ")
    switch(.Generic, # nolint
        `+` = plot_add(e2, e1, e2name),
        stop_incompatible_op(.Generic, e1, e2)
    )
})

#' @importFrom methods is
is_craftbox <- function(x) is(x, "CraftBox")

is_cross_plot <- function(x) is_craftbox(x) && is_cross(x@craftsman)

is_cross <- function(x) inherits(x, "CraftCross")

#######################################################
#' @importFrom ggplot2 ggproto
Craftsman <- ggproto("Craftsman",
    call = NULL,

    # following fields will be added when added to the layout
    in_linear = NULL,
    layout_name = NULL,
    direction = NULL,
    position = NULL, # for stack_layout() in quad_layout()
    labels = NULL,

    # A single boolean value indicates whether we should set facet and coord
    free_facet = FALSE,
    free_coord = FALSE,
    free_limits = FALSE,

    # we always prevent user from modifying the object in `$build_plot()` and
    # `$finish_plot()` methods
    locked = TRUE,
    lock = function(self) {
        assign("locked", value = TRUE, envir = self)
    },
    unlock = function(self) {
        assign("locked", value = FALSE, envir = self)
    },

    ############################################################
    # when added to the `Layout` object, will call following methods

    # we usually, define the `nobs` in `interact_layout`, since we can
    # act with the layout data in `interact_layout` method
    interact_layout = function(self, layout) layout,

    # we define the `panel` and `index` method in `setup_design` method
    setup_design = function(self, design) design,
    setup_plot = function(self, plot) plot,

    ##############################################################
    # Don't change the facet and coord in following methods
    build_plot = function(self, plot, design, extra_design = NULL,
                          previous_design = NULL) {
        plot
    },
    finish_plot = function(self, plot, schemes, theme) {
        plot <- plot_add_schemes(plot, schemes)
        ggremove_margin(plot, self$direction) + theme_recycle()
    },

    # utils method to print the object, should return a character vector
    summary = function(self, plot) {
        cls <- class(self)
        cls <- cls[seq_len(which(cls == "Craftsman"))]
        sprintf("<Class: %s>", paste(cls, collapse = " "))
    }
)

# Used to lock the `Craftsman` object
#' @export
`$<-.Craftsman` <- function(x, name, value) {
    if (x$locked) {
        cli_abort(c(
            sprintf("Cannot modify %s", object_name(x)),
            i = sprintf("%s is locked", object_name(x))
        ), call = x$call)
    }
    NextMethod()
}

#################################################################
plot_add <- function(object, plot, object_name) {
    if (is.null(plot@plot)) {
        cli_abort(c(
            sprintf("Cannot add {.var {object_name}} to %s", object_name(plot)),
            i = sprintf("no plot found for %s", object_name(plot))
        ))
    }
    UseMethod("plot_add")
}

#' @importFrom ggplot2 ggplot_add
#' @export
plot_add.default <- function(object, plot, object_name) {
    plot@plot <- ggplot_add(object, ggfun("plot_clone")(plot@plot), object_name)
    plot
}

#' @export
plot_add.ggalign_scheme <- function(object, plot, object_name) {
    name <- ggalign_scheme_name(object)
    plot@schemes[name] <- list(update_scheme(
        object, .subset2(plot@schemes, name), object_name
    ))
    plot
}

######################################################################
plot_build <- function(align, ..., schemes, theme) {
    plot <- align$build_plot(plot@plot, ...)
    align$finish_plot(plot, schemes, theme)
}
