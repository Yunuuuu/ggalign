# Use S4 to override the double dispatch problem of ggplot2
# And it's easy to convert a S4 Class to a S7 Class
methods::setClass(
    "ggalign_plot",
    list(
        plot = "ANY", # To avoid modify in place, we put plot in a slot
        active = "ANY",
        size = "ANY",
        schemes = "ANY",
        align = "ANY" # `AlignProto` object
    )
)

#' Show `ggalign_plot` information
#' @param object A `ggalign_plot` object.
#' @return The input invisiblely.
#' @keywords internal
methods::setMethod("show", "ggalign_plot", function(object) {
    print(object)
})

#' @export
print.ggalign_plot <- function(x, ...) {
    oo <- summary(x)
    cli::cli_inform(c(
        sprintf("%s object:", object_name(x)),
        " " = sprintf(
            "  {.field plot}: %s",
            if (oo[1L]) "yes" else "no"
        ),
        " " = sprintf(
            "  {.field reorder}: %s",
            if (oo[2L]) "yes" else "no"
        ),
        " " = sprintf(
            "  {.field split}: %s",
            if (oo[3L]) "yes" else "no"
        )
    ))
    invisible(x)
}

#' Summary the action of `ggalign_plot`
#'
#' @param object A `ggalign_plot` object
#' @return A logical vector of length 3, indicating:
#' - Whether the object adds a plot.
#' - Whether the object reorders the observations.
#' - Whether the object splits the observations into groups.
#' @export
#' @keywords internal
summary.ggalign_plot <- function(object, ...) {
    c(!is.null(object@plot), summary(object@align))
}

#' @importFrom methods new
new_ggalign_plot <- function(align = NULL, ...,
                             plot = NULL, active = NULL, size = NULL,
                             schemes = NULL, class = "ggalign_plot",
                             call = caller_call()) {
    assert_active(active, allow_null = FALSE, call = call)
    if (is.null(size)) {
        size <- unit(NA, "null")
    } else {
        size <- check_size(size, call = call)
    }
    new(
        class,
        # `call`: used to provide error message
        align = ggproto(NULL, align %||% AlignProto, ..., call = call),
        schemes = schemes %||% default_schemes(),
        plot = plot, active = active, size = size
    )
}

#' @export
plot.ggalign_plot <- function(x, ...) {
    cli_abort(sprintf("Cannot plot %s object directly", object_name(x)))
}

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.ggalign_plot <- plot.ggalign_plot

#' Add custom objects to ggalign plot
#' @keywords internal
methods::setMethod("+", c("ggalign_plot", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli_abort(c(
            "Cannot use {.code {.Generic}} with a single argument.",
            "i" = "Did you accidentally put {.code {.Generic}} on a new line?"
        ))
    }

    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    switch(.Generic, # nolint
        `+` = plot_add(e2, e1, e2name),
        stop_incompatible_op(.Generic, e1, e2)
    )
})

#' @importFrom methods is
is_ggalign_plot <- function(x) is(x, "ggalign_plot")

is_cross_plot <- function(x) is_ggalign_plot(x) && is_cross(x@align)

is_cross <- function(x) inherits(x, "Cross")

#######################################################
#' Summary the action of `AlignProto`
#'
#' @param object A `AlignProto` object
#' @return A logical vector of length 2, indicating:
#' - Whether the object reorders the observations.
#' - Whether the object splits the observations into groups.
#' @export
#' @keywords internal
summary.AlignProto <- function(object, ...) c(FALSE, FALSE)

#' @importFrom ggplot2 ggproto
AlignProto <- ggproto("AlignProto",
    # following fields will be added when added to the layout
    in_linear = NULL,
    object_name = NULL,
    layout_name = NULL,
    direction = NULL,
    position = NULL, # for stack_layout() in quad_layout()

    # A single boolean value indicates whether we should set facet and coord
    free_facet = FALSE,
    free_coord = FALSE,

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
    setup_layout = function(self, layout) layout,
    setup_design = function(self, data, design) design,
    setup_plot = function(self, plot) plot,

    ##############################################################
    build_plot = function(self, plot, design, extra_design = NULL,
                          previous_design = NULL, facet = NULL) {
        plot
    },
    finish_plot = function(self, plot, schemes, theme) {
        plot_add_schemes(plot, schemes) + theme_recycle()
    }
)

#' @importFrom rlang inject
align_inject <- function(method, params) {
    inject(method(
        !!!params[intersect(align_method_params(method), names(params))]
    ))
}

ggproto_formals <- function(x) formals(environment(x)$f)

align_method_params <- function(f, remove = character()) {
    vec_set_difference(names(ggproto_formals(f)), c("self", remove))
}

# Used to lock the `AlignProto` object
#' @export
`$<-.AlignProto` <- function(x, name, value) {
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
    plot@plot <- ggplot_add(object, plot@plot, object_name)
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
