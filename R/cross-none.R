#' Reset layout ordering and panel group
#'
#' @param data The dataset to use for the layout. By default,
#'   [`fortify_matrix()`] will convert the data to a matrix. This argument
#'   allows you to change the layout data. If not specified, the original data
#'   will be used.
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' [`fortify_matrix()`].
#' @param inherit_index A boolean value indicating whether to inherit the
#'   ordering index. If `TRUE`, will match the layout ordering index with the
#'   data names.
#' @param inherit_panel A boolean value indicating whether to inherit the
#'   panel group. If `TRUE`, will match the layout panel with the data names.
#' @param inherit_nobs A boolean value indicating whether to inherit the
#'   number of observations (nobs). If `TRUE`, the `data` input must be
#'   compatible with the layout data.
#' @export
cross_none <- function(data = waiver(), ...,
                       inherit_index = NULL,
                       inherit_panel = NULL,
                       inherit_nobs = NULL) {
    cross(CrossNone,
        data = data, data_params = list2(...), plot = NULL,
        active = new_active(use = FALSE),
        schemes = default_schemes(),
        inherit_index = inherit_index,
        inherit_panel = inherit_panel,
        inherit_nobs = inherit_nobs
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @include cross-.R
CrossNone <- ggproto("CrossNone", Cross)
