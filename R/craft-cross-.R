cross <- function(cross = NULL, data = waiver(),
                  data_params = list(), ...,
                  inherit_index = NULL,
                  inherit_panel = NULL,
                  inherit_nobs = NULL,
                  plot = NULL, active = NULL, size = NULL, schemes = NULL,
                  data_arg = caller_arg(data),
                  call = caller_call()) {
    if (override_call(call)) {
        call <- current_call()
    }
    new_craftbox(
        craftsman = cross %||% CraftCross,
        data = allow_lambda(data), data_params = data_params,
        ...,
        inherit_nobs = inherit_nobs,
        inherit_panel = inherit_panel,
        inherit_index = inherit_index,
        plot = plot, active = active, size = size, schemes = schemes,
        data_arg = data_arg, call = call
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @include craftsman.R
CraftCross <- ggproto(
    "CraftCross", Craftsman,
    free_facet = TRUE,
    free_limits = TRUE,
    data_params = NULL,
    inherit_nobs = NULL,
    inherit_panel = NULL,
    inherit_index = NULL,
    interact_layout = function(self, layout) {
        #  1. check layout is `*_cross()`
        #  2. add `cross_points`
        #  3. add `odomain`
        #  4. define `labels`, we'll rename the `labels` to `labels0`
        layout <- ggproto_parent(CrossGg, self)$interact_layout(layout)

        # will define `labels0`
        self$labels0 <- self$labels

        # check the previous (between two `break_points`) define has been
        # initialized
        if (length(layout@break_points) &&
            is.na(prop(layout@domain, "nobs"))) {
            cli_abort(sprintf(
                "layout {.field nobs} for %s must be initialized before adding %s",
                self$layout_name, object_name(self)
            ))
        }

        # setup data
        layout_data <- layout@data
        domain <- layout@domain

        if (is.waive(input_data <- self$data)) { # inherit from the layout
            data <- layout_data
            # `data` is `NULL`, `inherit_nobs` can be `TRUE` or `FALSE`, we by
            # default regard `inherit_nobs` as `TRUE`
            if (is.null(data) && isFALSE(self$inherit_nobs)) {
                prop(domain) <- NA_integer_
            }

            # `data` is not `NULL`, the `nobs` will always be the same with
            # previous domain, nothing to do
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
            data <- inject(
                fortify_matrix(
                    data, !!!self$data_params,
                    data_arg = self$data_arg,
                    call = self$call
                )
            ) %|w|% NULL
            if (isTRUE(self$inherit_nobs)) { # we require inherit nobs
                # we check if the data match original data dimention
                if (!is.null(data) &&
                    !is.na(prop(domain, "nobs")) &&
                    NROW(data) != prop(domain, "nobs")) {
                    cli_abort(c(
                        sprintf(
                            "%s (nobs: %d) is not compatible with the %s (nobs: %d)",
                            object_name(self), NROW(data), layout_name, layout_nobs
                        ),
                        i = "try to set {.code inherit_nobs = FALSE}"
                    ))
                }
            } else { # for `FALSE` and `NULL`
                if (is.null(data)) {
                    prop(domain, "nobs") <- NA_integer_
                } else {
                    if (NROW(data) == 0L) {
                        cli_abort("{.arg data} cannot be empty",
                            call = self$call
                        )
                    }
                    prop(domain, "nobs") <- NROW(data)
                }
            }
        }

        # we keep the names from the layout data for usage
        self$labels <- vec_names(data)

        # determine if we should inherit panel
        # by default, `inherit_panel = FALSE`
        if (isTRUE(self$inherit_panel)) {
            if (is.null(self$labels0)) {
                cli_abort(c(
                    "Cannot inherit panel from the layout",
                    i = "No labels found in the layout data"
                ))
            }
            if (is.null(self$labels)) {
                cli_abort(c(
                    "Cannot inherit panel from the layout",
                    i = "No labels found in the current {.arg data}"
                ))
            }
            if (!all(self$labels %in% self$labels0)) {
                cli_abort(c(
                    "Cannot inherit panel from the layout",
                    i = "Some labels in the current data are not found in the previous layout data"
                ))
            }
            if (!is.null(panel <- prop(domain, "panel"))) {
                prop(domain, "panel") <- droplevels(
                    panel[match(self$labels, self$labels0)]
                )
            }
        } else {
            prop(domain, "panel") <- NULL
        }

        # determine if we should inherit panel
        # by default, `inherit_index = FALSE`
        if (isTRUE(self$inherit_index)) {
            if (is.null(self$labels0)) {
                cli_abort(c(
                    "Cannot inherit ordering index from the layout",
                    i = "No labels found in the previous layout data"
                ))
            }

            if (is.null(self$labels)) {
                cli_abort(c(
                    "Cannot inherit ordering index from the layout",
                    i = "No labels found in the current {.arg data}"
                ))
            }

            if (!all(self$labels %in% self$labels0)) {
                cli_abort(c(
                    "Cannot inherit ordering index from the layout",
                    i = "Some labels in the current data are not found in the previous layout data"
                ))
            }

            if (!is.null(index <- prop(domain, "index"))) {
                new_index <- order(match(
                    self$labels,
                    vec_slice(self$labels0, index)
                ))

                # we always make the index following the panel
                if (!is.null(panel <- prop(domain, "panel"))) {
                    new_index <- reorder_index(panel, new_index)
                }
                prop(domain, "index") <- new_index
            }
        } else {
            prop(domain, "index") <- NULL
        }

        # reset layout data
        layout@data <- data # don't restore the attribute

        # update the domain
        layout@domain <- domain

        # udpate break_points
        layout@break_points <- c(layout@break_points, length(layout@box_list))
        layout
    }
)
