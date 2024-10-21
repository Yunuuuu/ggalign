#' Build data for the heatmap layout
#'
#' @param data Any objects to be plot with [ggheatmap()].
#' @inheritParams rlang::args_dots_empty
#' @export
fortify_heatmap <- function(data, ...) {
    rlang::check_dots_empty()
    UseMethod("fortify_heatmap")
}

#' @export
fortify_heatmap.matrix <- function(data, ...) data

#' @importFrom rlang try_fetch
#' @export
fortify_heatmap.default <- function(data, ...) {
    try_fetch(
        as.matrix(data),
        error = function(cnd) {
            cli::cli_abort(paste0(
                "{.arg data} must be a {.cls matrix}, ",
                "or an object coercible by {.fn fortify_heatmap}, or a valid ",
                "{.cls matrix}-like object coercible by {.fn as.matrix}"
            ), parent = cnd)
        }
    )
}
