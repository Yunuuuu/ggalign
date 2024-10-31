free_add <- function(object, free, object_name) UseMethod("free_add")

#' @importFrom ggplot2 ggplot_add
#' @export
free_add.default <- function(object, free, object_name) {
    free$plot <- ggplot_add(object, .subset2(free, "plot"), object_name)
    free
}

#' @export
free_add.plot_action <- function(object, free, object_name) {
    free$action <- update_action(free$action, object)
    free
}
