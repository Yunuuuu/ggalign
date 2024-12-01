ggfun <- local({
    ggplot2_namespace <- NULL
    function(x, mode = "any") {
        if (is.null(ggplot2_namespace)) {
            ggplot2_namespace <<- getNamespace("ggplot2")
        }
        get(x, envir = ggplot2_namespace, inherits = FALSE, mode = mode)
    }
})

allow_lambda <- function(x) {
    if (rlang::is_formula(x)) rlang::as_function(x) else x
}

is.waive <- function(x) inherits(x, "waiver")

`%|w|%` <- function(x, y) if (inherits(x, "waiver")) y else x

snake_class <- function(x) ggfun("snake_class")(x)

add_default_mapping <- function(mapping, default_mapping) {
    for (nm in names(mapping)) {
        default_mapping[[nm]] <- .subset2(mapping, nm)
    }
    default_mapping
}

identity_trans <- function(scale) {
    # for continuous scale, we don't allow the trans
    if (!scale$is_discrete() && !identical(scale$trans$name, "identity")) {
        cli_warn(sprintf(
            "{.arg trans} must be {.field identity} in {.code %s}",
            deparse(scale$call)
        ))
        scale$trans <- scales::as.transform("identity")
    }
    scale
}

default_expansion <- function(x = NULL, y = NULL) {
    structure(list(x = x, y = y), class = c("ggalign_default_expansion"))
}

#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @export
ggplot_add.ggalign_default_expansion <- function(object, plot, object_name) {
    if (is.null(.subset2(object, "x")) && is.null(.subset2(object, "y"))) {
        return(object)
    }
    ParentFacet <- .subset2(plot, "facet")
    plot$facet <- ggproto(
        NULL, ParentFacet,
        init_scales = function(self, layout, x_scale = NULL,
                               y_scale = NULL, params) {
            if (!is.null(x_scale) && !is.null(.subset2(object, "x"))) {
                x_scale$expand <- x_scale$expand %|w|% .subset2(object, "x")
            }
            if (!is.null(y_scale) && !is.null(.subset2(object, "y"))) {
                y_scale$expand <- y_scale$expand %|w|% .subset2(object, "y")
            }
            ggproto_parent(ParentFacet, self)$init_scales(
                layout = layout,
                x_scale = x_scale,
                y_scale = y_scale,
                params = params
            )
        }
    )
    plot
}
