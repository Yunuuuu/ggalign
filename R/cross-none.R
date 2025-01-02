#' Reset layout ordering and panel group
#'
#' @param data The dataset to use for the layout. By default,
#'   [`fortify_matrix()`] will convert the data to a matrix. This argument
#'   allows you to change the layout data. If not specified, the original data
#'   will be used.
#' @param inherit_panel A boolean value indicating whether to inherit the
#'   panel group information.
#' @param inherit_nobs A boolean value indicating whether to inherit the
#'   number of observations.
#' @export
cross_none <- function(data = waiver(), inherit_panel = NULL,
                       inherit_nobs = NULL) {
    cross(CrossNone,
        data = data,
        plot = NULL,
        active = new_active(use = FALSE),
        schemes = default_schemes(),
        inherit_panel = inherit_panel,
        inherit_nobs = inherit_nobs
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @include cross-.R
CrossNone <- ggproto("CrossNone", Cross,
    interact_layout = function(self, layout) {
        # will define labels0 using CrossGg
        layout <- ggproto_parent(Cross, self)$interact_layout(layout)

        # setup data
        layout_data <- layout@data
        inherit_nobs <- self$inherit_nobs
        design <- layout@design
        if (is.waive(input_data <- self$input_data)) { # inherit from the layout
            data <- layout_data
            # 1. data is NULL, `reset_nobs` can be `TRUE` or `FALSE`
            # 2. data is not `NULL`, `reset_nobs` must be `FALSE`
            if (is.null(data) && isFALSE(inherit_nobs)) {
                self$reset_nobs <- TRUE
            } else { # for `TRUE` and `NULL`
                self$reset_nobs <- FALSE
            }
        } else {
            if (is.function(input_data)) {
                if (is.null(layout_data)) {
                    cli_abort(c(
                        sprintf(
                            "{.arg data} in %s cannot be a function",
                            object_name(self)
                        ),
                        i = sprintf("no data was found in %s", self$layout_name)
                    ))
                }
                data <- input_data(layout_data)
            } else {
                data <- input_data
            }
            data <- fortify_matrix(data) %|w|% NULL
            if (isTRUE(inherit_nobs)) { # we require inherit nobs
                # we check if the data match original data dimention
                if (!is.null(data) &&
                    !is.null(.subset2(design, "nobs")) &&
                    NROW(data) != .subset2(design, "nobs")) {
                    cli_abort(c(
                        sprintf(
                            "%s (nobs: %d) is not compatible with the %s (nobs: %d)",
                            object_name(self), NROW(data), layout_name, layout_nobs
                        ),
                        i = "try to set {.code inherit_nobs = FALSE}"
                    ))
                }
                self$reset_nobs <- FALSE
            } else { # for `FALSE` and `NULL`
                self$reset_nobs <- TRUE
            }
        }

        # determine if we should inherit panel
        # by default, `inherit_panel = FALSE`
        self$reset_panel <- !isTRUE(self$inherit_panel)

        # we keep the names from the layout data for usage
        self$labels <- vec_names(data)

        # reset layout data
        layout@data <- data # don't restore the attribute
        layout
    }
)
