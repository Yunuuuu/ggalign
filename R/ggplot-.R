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
