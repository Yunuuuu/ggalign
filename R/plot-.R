new_controls <- function(plot_data = new_plot_data(),
                         plot_align = new_plot_align(),
                         plot_theme = new_plot_theme()) {
    list(
        plot_data = plot_data,
        plot_theme = plot_theme,
        plot_align = plot_align
    )
}

new_option <- function(name, option, ..., class = character()) {
    new_vctr(option,
        `__ggalign.option_name__` = name, ...,
        class = c(class, "ggalign_option")
    )
}

ggalign_option_name <- function(x) {
    attr(x, sprintf("__%s.option_name__", pkg_nm()), exact = TRUE)
}

#' Used to update global data
#' @noRd
update_option <- function(new_option, old_option, object_name) {
    UseMethod("update_option")
}

#' @export
update_option.default <- function(new_option, old_option, object_name) {
    new_option
}

update_layout_option <- function(object, layout, object_name) {
    name <- ggalign_option_name(object)
    layout@controls[name] <- list(update_option(
        object, .subset2(layout@controls, name), object_name
    ))
    layout
}

# By default, we'll always initialize the default value when building the layout
# so parent has the right class, we dispatch method based on the parent option
inherit_option <- function(option, poption) UseMethod("inherit_option", poption)

plot_add <- function(option, plot) UseMethod("plot_add")

inherit_controls <- function(controls, pcontrols) {
    options <- vapply(pcontrols, ggalign_option_name,
        character(1L),
        USE.NAMES = FALSE
    )
    ans <- lapply(options, function(opt) {
        inherit_option(.subset2(controls, opt), .subset2(pcontrols, opt))
    })
    vec_set_names(ans, options)
}

plot_add_controls <- function(plot, controls) {
    for (i in seq_along(controls)) {
        plot <- plot_add(.subset2(controls, i), plot = plot)
    }
    plot
}
