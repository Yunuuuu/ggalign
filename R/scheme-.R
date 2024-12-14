new_schemes <- function(...) {
    default <- list(
        new_scheme_data(),
        new_scheme_align(),
        new_scheme_theme()
    )
    names(default) <- vapply(default, ggalign_scheme_name,
        character(1L),
        USE.NAMES = FALSE
    )
    for (i in seq_len(...length())) {
        scheme <- ...elt(i)
        default[[ggalign_scheme_name(scheme)]] <- scheme
    }
    default
}

new_scheme <- function(name, data, ..., class = character()) {
    structure(data,
        `__ggalign.scheme_name__` = name, ...,
        class = c(class, "ggalign_scheme")
    )
}

ggalign_scheme_name <- function(x) {
    attr(x, "__ggalign.scheme_name__", exact = TRUE)
}

#' @importFrom ggplot2 theme
default_schemes <- function(data = NULL, th = theme()) {
    if (!is.waive(data)) data <- NULL
    new_schemes(
        new_scheme_data(data),
        new_scheme_theme(th)
    )
}

###############################################################
#' Used to update global data
#' @noRd
update_scheme <- function(new, old, object_name) {
    UseMethod("update_scheme", old)
}

#' @export
update_scheme.default <- function(new, old, object_name) new

update_layout_scheme <- function(object, layout, object_name) {
    name <- ggalign_scheme_name(object)
    layout@schemes[name] <- list(update_scheme(
        object, .subset2(layout@schemes, name), object_name
    ))
    layout
}

###############################################################
inherit_scheme <- function(scheme, pscheme) {
    UseMethod("inherit_scheme", pscheme)
}

# If no parent scheme, use child scheme directly
#' @export
inherit_scheme.NULL <- function(scheme, pscheme) scheme

inherit_schemes <- function(schemes, pschemes) {
    nms <- vapply(pschemes,
        ggalign_scheme_name, character(1L),
        USE.NAMES = FALSE
    )
    ans <- lapply(nms, function(opt) {
        inherit_scheme(.subset2(schemes, opt), .subset2(pschemes, opt))
    })
    names(ans) <- nms
    ans
}

###############################################################
plot_add_scheme <- function(plot, scheme) UseMethod("plot_add_scheme", scheme)

#' @export
plot_add_scheme.NULL <- function(plot, scheme) plot

plot_add_schemes <- function(plot, schemes) {
    for (i in seq_along(schemes)) {
        plot <- plot_add_scheme(plot, scheme = .subset2(schemes, i))
    }
    plot
}
