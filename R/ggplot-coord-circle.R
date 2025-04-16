#' Polar Coordinates with Enhanced Controls
#'
#' An extended version of [`coord_radial()`][ggplot2::coord_radial], providing
#' additional customization options.
#'
#' @inheritParams ggplot2::coord_radial
#' @param thetalim,rlim Limits for the `theta` and `r` axes.
#' @param inner.radius A numeric in `[0, 1)` indicates the inner radius.
#' @param outer.radius A numeric in `(0, 1]` indicates the outer radius.
#' [`coord_radial()`][ggplot2::coord_radial] by default uses `0.8`.
#' @examples
#' ggplot(mtcars, aes(disp, mpg)) +
#'     geom_point() +
#'     coord_circle(
#'         start = -0.4 * pi, end = 0.4 * pi,
#'         inner.radius = 0.3, outer.radius = 1
#'     )
#' ggplot(mtcars, aes(disp, mpg)) +
#'     geom_point() +
#'     coord_circle(
#'         start = -0.4 * pi, end = 0.4 * pi,
#'         inner.radius = 0.3, outer.radius = 0.5
#'     )
#' @importFrom ggplot2 ggproto
#' @importFrom rlang arg_match0
#' @export
coord_circle <- function(theta = "x", start = 0, end = NULL,
                         thetalim = NULL, rlim = NULL, expand = FALSE,
                         direction = 1, clip = "off",
                         r.axis.inside = NULL, rotate.angle = FALSE,
                         inner.radius = 0, outer.radius = 0.95) {
    theta <- arg_match0(theta, c("x", "y"))
    assert_number_decimal(start, allow_infinite = FALSE)
    assert_number_decimal(end, allow_infinite = FALSE, allow_null = TRUE)
    if (packageVersion("ggplot2") <= "3.5.2") {
        assert_bool(expand)
    }
    clip <- arg_match0(clip, c("off", "on"))

    valid_inside_axis <- .rlang_check_bool(r.axis.inside,
        allow_null = TRUE
    ) ||
        .rlang_check_number(r.axis.inside,
            allow_decimal = TRUE, allow_infinite = FALSE
        ) == 0L
    if (!valid_inside_axis) {
        cli_abort(
            "{.arg r.axis.inside} must be a single boolean value or a number"
        )
    }

    assert_bool(rotate.angle)
    assert_number_decimal(inner.radius,
        min = 0, max = 1,
        allow_infinite = FALSE
    )
    assert_number_decimal(outer.radius,
        min = 0, max = 1,
        allow_infinite = FALSE
    )
    if (inner.radius >= outer.radius) {
        cli_abort(
            "{.arg outer.radius} must be larger than {.arg inner.radius}"
        )
    }
    r <- if (theta == "x") "y" else "x"

    arc <- c(start, end %||% (start + 2 * pi))
    if (arc[1] > arc[2]) {
        n_rotate <- ((arc[1] - arc[2]) %/% (2 * pi)) + 1
        arc[1] <- arc[1] - n_rotate * 2 * pi
    }
    r.axis.inside <- r.axis.inside %||% !(abs(arc[2] - arc[1]) >= 1.999 * pi)
    inner_radius <- c(inner.radius, outer.radius) / 2

    ggproto(NULL, CoordCircle,
        limits = list(theta = thetalim, r = rlim),
        theta = theta,
        r = r,
        arc = arc,
        expand = expand,
        direction = sign(direction),
        r_axis_inside = r.axis.inside,
        rotate_angle = rotate.angle,
        inner_radius = inner_radius,
        clip = clip
    )
}

#' @importFrom ggplot2 ggproto_parent
circle_panel_params <- function(self, scale_x, scale_y, params = list()) {
    out <- ggproto_parent(ggplot2::CoordRadial, self)$setup_panel_params(
        scale_x, scale_y, params
    )
    if (packageVersion("ggplot2") <= "3.5.2") {
        if (self$theta == "x") {
            xlimits <- self$limits$theta
            ylimits <- self$limits$r
        } else {
            xlimits <- self$limits$r
            ylimits <- self$limits$theta
        }
        new <- c(
            view_scales_polar(
                scale_x, self$theta, xlimits,
                expand = params$expand[c(4, 2)] %||% self$expand
            ),
            view_scales_polar(
                scale_y, self$theta, ylimits,
                expand = params$expand[c(3, 1)] %||% self$expand
            )
        )
        out[names(new)] <- new
    }
    out$bbox <- ggfun("polar_bbox")(
        self$arc, margin = c(0, 0, 0, 0),
        inner_radius = self$inner_radius
    )
    out
}

view_scales_polar <- function(scale, theta, coord_limits, expand = TRUE) {
    aesthetic <- scale$aesthetics[1]
    is_theta <- theta == aesthetic
    name <- if (is_theta) "theta" else "r"
    expansion <- ggfun("default_expansion")(scale, expand = expand)
    limits <- scale$get_limits()
    continuous_range <- ggfun("expand_limits_scale")(
        scale, expansion, limits, coord_limits = coord_limits
    )
    primary <- ggfun("view_scale_primary")(scale, limits, continuous_range)
    view_scales <- list(
        primary,
        sec = ggfun("view_scale_secondary")(scale, limits, continuous_range),
        major = primary$map(primary$get_breaks()),
        minor = primary$map(primary$get_breaks_minor()),
        range = continuous_range
    )
    names(view_scales) <- c(name, paste0(name, ".", names(view_scales)[-1]))
    view_scales
}

#' @importFrom ggplot2 ggproto
CoordCircle <- ggproto(
    "CoordCircle", ggplot2::CoordRadial,
    setup_panel_params = circle_panel_params
)
