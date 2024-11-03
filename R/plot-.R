new_option <- function(name, option, ..., class = character()) {
    new_vctr(option, name = name, ..., class = c(class, "ggalign_controls"))
}

new_controls <- function(data = new_plot_data(),
                         action = new_plot_action(),
                         theme = NULL) {
    list(data = data, theme = theme, action = action)
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

update_layout_option <- function(layout, object, object_name) {
    name <- attr(object, "name")
    layout@controls[name] <- list(update_option(
        object, .subset2(layout@controls, name), object_name
    ))
    layout
}

update_layout_option_theme <- function(layout, object, object_name) {
    layout@controls["theme"] <- list(update_option(
        object, .subset2(layout@controls, "theme"), object_name
    ))
    layout
}

# By default, we'll always initialize the default value when building the layout
# so parent has the right class, we dispatch method based on the parent option
# option sometimes may be NULL
inherit_option <- function(option, poption) UseMethod("inherit_option", poption)

plot_add <- function(option, plot) UseMethod("plot_add")

inherit_controls <- function(controls, pcontrols) {
    ans <- lapply(names(controls), function(nm) {
        inherit_option(.subset2(controls, nm), .subset2(pcontrols, nm))
    })
    vec_set_names(ans, names(controls))
}

plot_add_controls <- function(plot, controls) {
    for (i in seq_along(controls)) {
        plot <- plot_add(.subset2(controls, i), plot = plot)
    }
    plot
}
