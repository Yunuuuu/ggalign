#' Arrange plots in a circular layout
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' If `limits` is provided, a continuous variable will be required and aligned
#' in the direction specified (`circle_continuous`). Otherwise, a discrete
#' variable will be required and aligned (`circle_discrete`).
#'
#' @param radial A [`coord_circle()`]/[`coord_radial()`][ggplot2::coord_radial]
#' object that defines the global parameters for coordinate across all plots
#' in the layout. The parameters `start`, `end`, `direction`, and `expand` will
#' be inherited and applied uniformly to all plots within the layout. The
#' parameters `theta` and `r.axis.inside` will always be ignored and will be set
#' to `"x"` and `TRUE`, respectively, for all plots.
#' @param direction A single string of `r oxford_or(c("inward", "outward"))`,
#' indicating the direction in which the plot is added.
#' - `outward`: The plot is added from the inner to the outer.
#' - `inward`: The plot is added from the outer to the inner.
#' @inheritParams stack_layout
#' @return A `CircleLayout` object.
#' @examples
#' set.seed(123)
#'
#' small_mat <- matrix(rnorm(56), nrow = 7)
#' rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
#' colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
#'
#' # circle_layout
#' # same for circle_discrete()
#' circle_layout(small_mat) +
#'     ggalign() +
#'     geom_tile(aes(y = .column_index, fill = value)) +
#'     scale_fill_viridis_c() +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     scale_color_brewer(palette = "Dark2")
#'
#' # same for circle_continuous()
#' circle_layout(mpg, limits = continuous_limits(c(3, 5))) +
#'     ggalign(mapping = aes(displ, hwy, colour = class)) +
#'     geom_point(size = 2) +
#'     ggalign(mapping = aes(displ, hwy, colour = class)) +
#'     geom_point(size = 2) &
#'     scale_color_brewer(palette = "Dark2") &
#'     theme_bw()
#'
#' @export
circle_layout <- function(data = NULL, ..., radial = NULL,
                          direction = "outward", sector_spacing = NULL,
                          limits = waiver(), theme = NULL,
                          spacing_theta = deprecated()) {
    if (is.waive(limits)) {
        circle_discrete(
            data = data, ..., radial = radial,
            direction = direction, sector_spacing = sector_spacing,
            theme = theme, spacing_theta = spacing_theta
        )
    } else {
        circle_continuous(
            data = data, ..., radial = radial,
            direction = direction, theme = theme, limits = limits,
            spacing_theta = spacing_theta
        )
    }
}

############################################################
#' @inheritParams facet_sector
#' @examples
#' # circle_discrete()
#' # direction outward
#' circle_discrete(small_mat) +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     scale_color_brewer(palette = "Dark2") +
#'     ggalign() +
#'     geom_tile(aes(y = .column_index, fill = value)) +
#'     scale_fill_viridis_c()
#'
#' # direction inward
#' circle_discrete(small_mat, direction = "inward") +
#'     ggalign() +
#'     geom_tile(aes(y = .column_index, fill = value)) +
#'     scale_fill_viridis_c() +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     scale_color_brewer(palette = "Dark2")
#'
#' @export
#' @rdname circle_layout
circle_discrete <- function(data = NULL, ..., radial = NULL,
                            direction = "outward", sector_spacing = NULL,
                            theme = NULL, spacing_theta = deprecated()) {
    UseMethod("circle_discrete", data)
}

#' @export
circle_discrete.default <- function(data = NULL, ..., radial = NULL,
                                    direction = "outward",
                                    sector_spacing = NULL,
                                    theme = NULL,
                                    spacing_theta = deprecated()) {
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
        nobs <- NA_integer_
    }
    new_circle_layout(
        data = data,
        domain = DiscreteDomain(nobs = nobs),
        radial = radial, direction = direction, sector_spacing = sector_spacing,
        schemes = schemes, theme = theme, spacing_theta = spacing_theta
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
#' @examples
#' # circle_continuous()
#' circle_continuous(mpg, limits = continuous_limits(c(3, 5))) +
#'     ggalign(mapping = aes(displ, hwy, colour = class)) +
#'     geom_point(size = 2) +
#'     ggalign(mapping = aes(displ, hwy, colour = class)) +
#'     geom_point(size = 2) &
#'     scale_color_brewer(palette = "Dark2") &
#'     theme_bw()
#' @export
#' @rdname circle_layout
circle_continuous <- function(data = NULL, ..., radial = NULL,
                              direction = "outward", sector_spacing = NULL,
                              limits = NULL, theme = NULL,
                              spacing_theta = deprecated()) {
    UseMethod("circle_continuous", data)
}

#' @export
circle_continuous.default <- function(data = NULL, ..., radial = NULL,
                                      direction = "outward",
                                      sector_spacing = NULL,
                                      limits = NULL, theme = NULL,
                                      spacing_theta = deprecated()) {
    assert_limits(limits)
    data <- data %|w|% NULL
    data <- fortify_data_frame(data = data, ...)
    schemes <- default_schemes()
    new_circle_layout(
        data = data, domain = limits,
        radial = radial, direction = direction, sector_spacing = sector_spacing,
        schemes = schemes, theme = theme, spacing_theta = spacing_theta
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
new_circle_layout <- function(data, domain, radial, direction,
                              sector_spacing = NULL, schemes = NULL,
                              theme = NULL, name = NULL,
                              spacing_theta = deprecated()) {
    direction <- arg_match0(direction, c("inward", "outward"))
    if (is.null(name)) {
        if (is_discrete_domain(domain)) {
            name <- "circle_discrete"
        } else {
            name <- "circle_continuous"
        }
    }
    if (lifecycle::is_present(spacing_theta)) {
        lifecycle::deprecate_warn(
            "1.0.2",
            "facet_sector(spacing_theta = )",
            "facet_sector(sector_spacing = )"
        )
        if (is.null(sector_spacing)) sector_spacing <- spacing_theta
    }
    CircleLayout(
        name = name, data = data,
        schemes = schemes,
        domain = domain,
        sector_spacing = sector_spacing,
        theme = theme,
        radial = radial,
        direction = direction
    )
}

############################################################
#' Determine the active context of circle layout
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams circle_discrete
#' @param what What should get activated for the [`circle_layout()`]?
#' `r rd_chain_what()`.
#' @return A `circle_switch` object which can be added to [`circle_layout()`].
#' @examples
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 7)
#' rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
#' colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
#' circle_discrete(small_mat) +
#'     ggalign() +
#'     geom_tile(aes(y = .column_index, fill = value)) +
#'     scale_fill_viridis_c() +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     scale_color_brewer(palette = "Dark2")
#' @export
circle_switch <- function(radial = waiver(), direction = NULL,
                          what = waiver(), ...) {
    rlang::check_dots_empty()
    if (!is.waive(radial) && !is.null(radial)) {
        assert_s3_class(radial, "CoordRadial")
        if (abs(diff(radial$arc)) < pi / 2L) {
            cli_abort("Cannot use circle of acute angle < 90 in {.arg radial}")
        }
    }
    if (!is.null(direction)) {
        direction <- arg_match0(direction, c("inward", "outward"))
    }
    if (!is.waive(what)) what <- check_stack_context(what)
    structure(
        list(what = what, radial = radial, direction = direction),
        class = "circle_switch"
    )
}

S7::method(layout_add, list(CircleLayout, S7::new_S3_class("circle_switch"))) <-
    function(layout, object, objectname) {
        if (!is.waive(radial <- .subset2(object, "radial"))) {
            layout@radial <- radial
        }
        if (!is.null(direction <- .subset2(object, "direction"))) {
            layout@direction <- direction
        }
        layout <- switch_chain_plot(
            layout, .subset2(object, "what"),
            quote(circle_switch())
        )
        layout
    }
