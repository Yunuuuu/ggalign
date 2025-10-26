#' A class representing a graph object that encapsulates a `ggplot` object
#' and other properties used to extend and customize the `ggplot`.
#'
#' This class combines a **ggplot2** plot with additional properties, such as
#' schemes, which can be used to extend or modify the plot's appearance or
#' behavior.
#'
#' @importFrom S7 new_object S7_object
#' @include scheme-.R
#' @include plot-control.R
#' @keywords internal
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
