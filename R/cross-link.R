#' Add a plot to connect selected observations
#'
#' @param link A [`link_draw()`] object that defines how to draw the links,
#'   such as [`link_line()`].
#' @param data The dataset to use for the layout. By default,
#'   [`fortify_matrix()`] will convert the data to a matrix. This argument
#'   allows you to change the layout data. If not specified, the original data
#'   will be used.
#' @param on_top A boolean value indicating whether to draw the link on top of
#'   the plot panel (`TRUE`) or below (`FALSE`).
#' @param inherit_panel A boolean value indicating whether to inherit the
#'   panel group information.
#' @param inherit_nobs A boolean value indicating whether to inherit the
#'   number of observations.
#' @inheritParams ggalign
#'
#' @section ggplot2 Specification:
#' The `cross_link` function initializes a `ggplot` object but does not
#' initialize any data. Use [`scheme_data()`] to change the internal data if
#' needed.
#'
#' @export
cross_link <- function(link, data = waiver(), on_top = TRUE,
                       inherit_panel = NULL, inherit_nobs = NULL,
                       size = NULL, active = NULL) {
    if (!inherits(link, "ggalign_link_draw")) {
        cli_abort("{.arg link} must be a {.fn link_draw} object")
    }
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    cross(CrossLink,
        link = link,
        plot = ggplot(), size = size,
        schemes = default_schemes(th = theme_no_panel()),
        active = active,
        on_top = on_top,
        input_data = allow_lambda(data),
        inherit_panel = inherit_panel,
        inherit_nobs = inherit_nobs
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @include cross-.R
CrossLink <- ggproto("CrossLink", Cross,
    interact_layout = function(self, layout) {
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
    },
    build_plot = function(self, plot, design, extra_design = NULL,
                          previous_design = NULL) {
        if (is.null(.subset2(previous_design, "nobs"))) {
            cli_abort(
                sprintf(
                    "layout {.field nobs} for %s before %s is not initialized ",
                    self$layout_name, object_name(self)
                )
            )
        }
        if (is.null(.subset2(design, "nobs"))) {
            cli_abort(
                sprintf(
                    "layout {.field nobs} for %s after %s is not initialized ",
                    self$layout_name, object_name(self)
                )
            )
        }

        direction <- self$direction
        position <- self$position

        # parse links --------------------------------------------
        link <- self$link
        design1 <- previous_design
        design2 <- design
        full_data1 <- split(
            seq_len(.subset2(design1, "nobs")),
            .subset2(design1, "panel")
        )
        full_data2 <- split(
            seq_len(.subset2(design2, "nobs")),
            .subset2(design2, "panel")
        )
        links <- .subset2(link, "links")
        link_index <- lapply(links, make_pair_link_data,
            design1 = design1, design2 = design2,
            labels1 = self$labels0, labels2 = self$labels
        )
        names(link_index) <- names(links)
        data_index <- lapply(link_index, function(index) {
            if (is.null(index)) {
                return(NULL)
            }
            link1 <- .subset2(index, "link1")
            link2 <- .subset2(index, "link2")
            list(
                link1 = .subset2(design1, "index")[link1],
                link2 = .subset2(design2, "index")[link2]
            )
        })
        plot$ggalign_link_data <- list(
            full_data1 = full_data1,
            full_data2 = full_data2,
            link_index = link_index,
            data_index = data_index,
            direction = direction,
            draw = .subset2(link, "draw")
        )
        plot
    },
    finish_plot = function(self, plot, schemes, theme) {
        plot <- plot_add_schemes(plot, schemes)

        # save spacing for usage
        spacing <- calc_element(
            switch_direction(
                self$direction,
                "panel.spacing.y",
                "panel.spacing.x"
            ),
            theme
        ) %||% unit(0, "mm")

        # setup the grob
        grob <- inject(grid::grob(
            !!!.subset2(plot, "ggalign_link_data"),
            spacing1 = spacing,
            spacing2 = spacing,
            cl = "ggalignLinkGrob"
        ))
        plot$ggalign_link_data <- NULL

        # insert the grob
        plot + inset(grob, on_top = self$on_top) + theme_recycle()
    }
)
