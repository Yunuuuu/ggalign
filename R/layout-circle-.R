#' Arrange Plots in a Circular Layout
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The `circle_discrete` function arranges plots by aligning discrete variables
#' in a circular layout.
#'
#' @inheritParams stack_discrete
#' @param radial A [`coord_radial()`][ggplot2::coord_radial] object that defines
#' the global parameters for `coord_radial` across all plots in the layout.
#' The parameters `start`, `end`, `direction`, and `expand` will be inherited
#' and applied uniformly to all plots within the layout. The parameters
#' `theta` and `r.axis.inside` will always be ignored and will be set to
#' `"x"` and `TRUE`, respectively, for all plots.
#' @examples
#' set.seed(123)
#' circle_discrete(matrix(rnorm(56), nrow = 7L),
#'     radial = coord_radial(inner.radius = 0.1)
#' ) +
#'     ggalign() +
#'     geom_tile(aes(y = .column_index, fill = value)) +
#'     scale_fill_viridis_c() +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     scale_color_brewer(palette = "Dark2")
#' 
#' @export
circle_discrete <- function(data = NULL, ..., radial = NULL, theme = NULL) {
    UseMethod("circle_discrete", data)
}

#' @export
circle_discrete.default <- function(data = NULL, ..., radial = NULL,
                                    theme = NULL) {
    # the observations are rows, we use matrix to easily
    # reshape it into a long formated data frame for ggplot,
    # and we can easily determine the number of observations
    # from matrix
    data <- data %|w|% NULL
    data <- fortify_matrix(data = data, ...)
    schemes <- default_schemes()
    if (!is.null(data) && !is.function(data)) {
        # if we have provided data, we initialize the `nobs`
        nobs <- vec_size(data)
    } else {
        nobs <- NULL
    }
    new_circle_layout(
        data = data,
        design = discrete_design(nobs = nobs),
        radial = radial,
        schemes = schemes, theme = theme
    )
}

#' @export
circle_discrete.function <- function(data = NULL, ...) {
    cli_abort(paste0(
        "{.arg data} must be a {.cls matrix}, ",
        "or an object coercible by {.fn fortify_matrix}, or a valid ",
        "{.cls matrix}-like object coercible by {.fn as.matrix}"
    ))
}

#' @export
circle_discrete.formula <- circle_discrete.function

################################################################
#' @inherit circle_discrete title
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The `circle_continuous` function arranges plots by aligning continuous
#' variables in a circular layout.
#'
#' @inheritParams circle_discrete
#' @inheritParams stack_continuous
#' @examples
#' circle_continuous(mpg, limits = continuous_limits(c(3, 5))) +
#'     ggalign(mapping = aes(displ, hwy, colour = class)) +
#'     geom_point(size = 2) +
#'     ggalign(mapping = aes(displ, hwy, colour = class)) +
#'     geom_point(size = 2) &
#'     scale_color_brewer(palette = "Dark2") &
#'     theme_bw()
#' @export
circle_continuous <- function(data = NULL, ..., radial = NULL,
                              limits = NULL, theme = NULL) {
    UseMethod("circle_continuous", data)
}

#' @export
circle_continuous.default <- function(data = NULL, ..., radial = NULL,
                                      limits = NULL, theme = NULL) {
    assert_limits(limits)
    data <- data %|w|% NULL
    data <- fortify_data_frame(data = data, ...)
    schemes <- default_schemes()
    new_circle_layout(
        data = data, design = limits, radial = radial,
        schemes = schemes, theme = theme
    )
}

#' @export
circle_continuous.function <- function(data = NULL, ...) {
    cli_abort(paste0(
        "{.arg data} must be a {.cls data.frame}, ",
        "or an object coercible by {.fn fortify_data_frame}, or a valid ",
        "{.cls data.frame}-like object coercible by {.fn as.data.frame}"
    ))
}

#' @export
circle_continuous.formula <- circle_continuous.function

#' @importFrom methods new
new_circle_layout <- function(data, design, radial, schemes = NULL,
                              theme = NULL, name = NULL, call = caller_call()) {
    if (!is.null(theme)) assert_s3_class(theme, "theme", call = call)
    assert_s3_class(radial, "CoordRadial", allow_null = TRUE)
    if (!is.null(radial) && abs(diff(radial$arc)) < pi / 2L) {
        cli_abort(
            "Cannot use circle of acute angle < 90 in {.arg radial}",
            call = call
        )
    }
    if (is.null(name)) {
        if (is_continuous_design(design)) {
            name <- "circle_continuous"
        } else {
            name <- "circle_discrete"
        }
    }
    new(
        "CircleLayout",
        name = name, data = data,
        schemes = schemes, # used by the layout
        design = design, theme = theme,
        radial = radial
    )
}

#' @inherit circle_discrete title
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function integrates the functionalities of `circle_discrete()` and
#' `circle_continuous()` into a single interface.
#'
#' @inheritParams stack_layout
#' @return A `CircleLayout` object.
#' @seealso
#'  - [`circle_discrete()`]
#'  - [`circle_continuous()`]
#' @examples
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 7L)
#'
#' # circle_discrete
#' circle_layout(matrix(rnorm(56), nrow = 7L)) +
#'     ggalign() +
#'     geom_tile(aes(y = .column_index, fill = value)) +
#'     scale_fill_viridis_c() +
#'     align_dendro(aes(color = branch), k = 3) +
#'     circle_switch(coord_radial(inner.radius = 0.5))
#'
#' # circle_continuous
#' circle_layout(mpg, limits = continuous_limits(c(3, 5))) +
#'     ggalign(mapping = aes(displ, hwy, colour = class)) +
#'     geom_point(size = 2) +
#'     ggalign(mapping = aes(displ, hwy, colour = class)) +
#'     geom_point(size = 2) &
#'     scale_color_brewer(palette = "Dark2") &
#'     theme_bw()
#' @export
circle_layout <- function(data = NULL, ..., limits = waiver()) {
    if (is.waive(limits)) {
        circle_discrete(data = data, ...)
    } else {
        circle_continuous(data = data, limits = limits, ...)
    }
}

############################################################
# Used to place multiple objects in one axis
#' @importFrom grid unit
#' @importFrom ggplot2 waiver
#' @keywords internal
#' @include layout-chain-.R
methods::setClass("CircleLayout",
    contains = "ChainLayout",
    list(radial = "ANY")
)
