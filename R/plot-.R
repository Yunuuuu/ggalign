# Use S4 to override the double dispatch problem of ggplot2
# And it's easy to convert a S4 Class to a S7 Class
methods::setClass(
    "ggalign_plot",
    list(
        plot = "ANY",
        active = "ANY",
        size = "ANY",
        schemes = "ANY"
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
    c(!is.null(object@plot), FALSE, FALSE)
}

#' @importFrom methods new
new_ggalign_plot <- function(..., plot = NULL, active = NULL, size = NULL,
                             schemes = NULL, class = "ggalign_plot",
                             call = caller_call()) {
    if (!inherits(active, "ggalign_active")) {
        cli_abort("{.arg active} must be created by {.fn active}", call = call)
    }
    if (is.null(size)) {
        size <- unit(NA, "null")
    } else {
        size <- check_size(size, call = call)
    }
    new(
        class,
        ...,
        schemes = schemes %||% new_schemes(),
        plot = plot,
        active = active,
        size = size
    )
}

#' @importFrom methods is
is_ggalign_plot <- function(x) is(x, "ggalign_plot")

#' @export
plot.ggalign_plot <- function(x, ...) {
    cli_abort(sprintf("Cannot plot %s object directly", object_name(x)))
}

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.ggalign_plot <- plot.ggalign_plot

plot_build <- function(plot, ...) UseMethod("plot_build")

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
        `+` = plot_add(e1, e2, e2name),
        stop_incompatible_op(.Generic, e1, e2)
    )
})
