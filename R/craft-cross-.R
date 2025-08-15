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
    data_params = NULL,
    inherit_nobs = NULL,
    inherit_panel = NULL,
    inherit_index = NULL,
    interact_layout = function(self, layout) {
        # Step 1: Preprocess the layout using CrossGg's method
        #   - Ensures layout is in a *_cross() form
        #   - Adds cross_points and odomain
        #   - Define labels -> labels0 for later reference
        layout <- ggproto_parent(CrossGg, self)$interact_layout(layout)

        # Keep a copy of the original labels from the layout
        self$labels0 <- self$labels

        # Step 2: Validate that nobs in the layout domain has been initialized
        if (length(layout@break_points) &&
            is.na(prop(layout@domain, "nobs"))) {
            cli_abort(sprintf(
                "layout {.field nobs} for %s must be initialized before adding %s",
                self$layout_name, object_name(self)
            ))
        }

        # Extract current layout data and domain
        layout_data <- layout@data
        domain <- layout@domain
        nobs <- prop(domain, "nobs")

        # Step 3: Handle the input data
        if (is.waive(input_data <- self$data)) { # inherit from the layout
            # No explicit data: inherit from layout
            data <- layout_data
            # If no data and inherit_nobs is explicitly FALSE, unset nobs
            if (is.null(data) && isFALSE(self$inherit_nobs)) {
                prop(domain, "nobs") <- NA_integer_
            }
        } else {
            # Data is provided explicitly
            if (is.function(input_data)) {
                if (is.null(layout_data)) {
                    cli_abort(c(
                        sprintf(
                            "{.arg data} in %s cannot be a function",
                            object_name(self)
                        ),
                        i = sprintf("No data was found in %s", self$layout_name)
                    ))
                }
                data <- input_data(layout_data)
            } else {
                data <- input_data
            }
            # Fortify the data into a consistent matrix-like format
            data <- inject(
                fortify_matrix(
                    data, !!!self$data_params,
                    data_arg = self$data_arg,
                    call = self$call
                )
            ) %|w|% NULL
            if (isTRUE(self$inherit_nobs)) {
                # Check that the number of observations matches the original
                if (!is.null(data) && !is.na(nobs) && NROW(data) != nobs) {
                    cli_abort(c(
                        sprintf(
                            "%s (nobs: %d) is not compatible with the %s (nobs: %d)",
                            object_name(self), NROW(data), layout_name, layout_nobs
                        ),
                        i = "Set {.code inherit_nobs = FALSE} if you want to allow a different number of observations."
                    ))
                }
            } else if (is.null(data)) { # inherit_nobs is `FALSE` or `NULL`
                prop(domain, "nobs") <- NA_integer_
            } else {
                if (NROW(data) == 0L) {
                    cli_abort("{.arg data} cannot be empty", call = self$call)
                }
                prop(domain, "nobs") <- NROW(data)
            }
        }

        # Store variable names from the data for later matching
        self$labels <- vec_names(data)

        # Step 4: Inherit panel if requested
        if (isTRUE(self$inherit_panel) &&
            !is.null(panel <- prop(domain, "panel"))) {
            if (identical(nobs, prop(domain, "nobs"))) { # If nobs matches
                # If labels match, inherit panel by labels
                # Otherwise, use original panel directly (no changes needed)
                if (!is.null(self$labels0) && !is.null(self$labels) &&
                    all(self$labels %in% self$labels0)) {
                    prop(domain, "panel") <- droplevels(
                        panel[match(self$labels, self$labels0)]
                    )
                }
            } else {
                # nobs differ, validate labels before inheriting `panel`
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
                        "Cannot inherit panel from the layout.",
                        i = "Some labels in the current data do not match any labels in the previous layout data."
                    ))
                }
                prop(domain, "panel") <- droplevels(
                    panel[match(self$labels, self$labels0)]
                )
            }
        } else {
            prop(domain, "panel") <- NULL
        }

        # Step 5: Inherit ordering index if requested
        if (isTRUE(self$inherit_index) &&
            !is.null(index <- prop(domain, "index"))) {
            if (identical(nobs, prop(domain, "nobs"))) { # If nobs matches
                # If labels match, inherit ordering by labels
                # Otherwise, use original index directly (no changes needed)
                if (!is.null(self$labels0) && !is.null(self$labels) &&
                    all(self$labels %in% self$labels0)) {
                    new_index <- order(match(
                        self$labels,
                        vec_slice(self$labels0, index)
                    ))

                    # Always reorder index according to panel if panel exists
                    if (!is.null(panel <- prop(domain, "panel"))) {
                        new_index <- reorder_index(panel, new_index)
                    }
                    prop(domain, "index") <- new_index
                }
            } else {
                # nobs differ, validate labels before inheriting `index`
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
    },
    setup_stack_facet = function(self, plot, ...) plot,
    setup_stack_plot = function(self, plot, ...) plot,
    setup_circle_facet = function(self, plot, ...) plot,
    setup_circle_plot = function(self, plot, ...) plot,
)
