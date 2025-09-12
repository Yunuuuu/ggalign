#' Draw images as point shapes using magick
#'
#' @description
#' Reads an image with **magick**, applies optional processing, and uses
#' the result as the graphical shape for points in a plot.
#'
#' This is useful when you want to replace the usual point symbols
#' with arbitrary images while keeping full control over their placement,
#' size, and interpolation.
#'
#' @inheritParams grid::rasterGrob
#' @inheritParams magickGrob
#' @inheritParams ggplot2::geom_point
#' @param magick_params Additional arguments passed on to `magick`
#' @aesthetics GeomMagick
#' @examples
#' set.seed(123)
#' d <- data.frame(
#'     x = rnorm(10),
#'     y = rnorm(10),
#'     image = "https://jeroenooms.github.io/images/frink.png",
#'     fill = sample(c("A", "B", "C", "D"), 10, replace = TRUE),
#'     alpha = rnorm(10, mean = 0.5, sd = 0.1)
#' )
#' d$alpha <- pmax(pmin(d$alpha, 1), 0)
#' ggplot(d, aes(x, y)) +
#'     geom_magick(aes(image = image, fill = fill, alpha = alpha))
#' @importFrom rlang list2
#' @export
geom_magick <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", ..., magick = NULL,
                        magick_params = list(), interpolate = TRUE,
                        na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE) {
    rlang::check_installed("magick", "to use `geom_magick()`")
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomMagick,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(
            na.rm = na.rm,
            magick = magick,
            magick_params = magick_params,
            interpolate = interpolate,
            ...
        )
    )
}

#' @importFrom ggplot2 ggproto aes
GeomMagick <- ggproto(
    "GeomMagick",
    ggplot2::Geom,
    default_aes = aes(
        # Optional fill and transparency
        fill = NA, alpha = NA,
        # Reuse text size default (in mm)
        !!!ggplot2::GeomText$default_aes["size"],
        # Rotation & justification defaults
        angle = 0, hjust = 0.5, vjust = 0.5
    ),
    required_aes = c("x", "y", "image"),
    non_missing_aes = c("size", "angle"),
    draw_panel = function(self, data, panel_params, coord, magick = NULL,
                          magick_params = list(),
                          interpolate = TRUE, na.rm = FALSE) {
        data <- coord$transform(data, panel_params)

        # Group data rows by unique 'image' to avoid re-reading same images
        # multiple times
        data_groups <- vec_split(data, .subset2(data, "image"))

        # read image only once for the same images to avoid download image
        # multiples times.
        images <- lapply(.subset2(data_groups, "key"), function(image) {
            magick::image_read(image)
        })

        raster_list <- .mapply(function(image, image_data) {
            # Remove 'image' column from data subset to avoid duplication
            image_data$image <- NULL
            # Process each row in image_data independently (multiple points can
            # use same image)
            lapply(vec_seq_along(image_data), function(row) {
                row_data <- vec_slice(data, row)

                # Extract aesthetics for this point
                x <- .subset2(row_data, "x")
                y <- .subset2(row_data, "y")
                size <- .subset2(row_data, "size")
                angle <- .subset2(row_data, "angle")
                hjust <- .subset2(row_data, "hjust")
                vjust <- .subset2(row_data, "vjust")
                alpha <- .subset2(row_data, "alpha")
                fill <- .subset2(row_data, "fill")

                # Apply user-supplied magick transformation function if provided
                if (!is.null(magick)) {
                    # Remove all known aesthetics to leave only extra params for
                    # magick
                    row_data$x <- NULL
                    row_data$y <- NULL
                    row_data$size <- NULL
                    row_data$angle <- NULL
                    row_data$hjust <- NULL
                    row_data$vjust <- NULL
                    row_data$alpha <- NULL
                    row_data$fill <- NULL
                    image <- rlang::inject(magick(
                        image, !!!row_data, !!!magick_params
                    ))
                }

                # If fill color specified, colorize the image with alpha
                # adjustment
                if (!is.na(fill)) {
                    if (is.na(alpha)) alpha <- 0
                    image <- magick::image_colorize(image,
                        opacity = (1 - alpha) * 100, color = fill
                    )
                }

                # Rotate image by specified angle if not zero
                if (angle != 0) image <- magick::image_rotate(image, angle)

                # Convert the processed magick image to a native raster object
                # for grid graphics
                raster <- grDevices::as.raster(image, native = TRUE)

                # ggplot2 uses 'mm'
                grid::rasterGrob(
                    raster,
                    x = x, y = y,
                    width = unit(size, "mm"),
                    default.units = "native",
                    hjust = hjust, vjust = vjust,
                    interpolate = interpolate
                )
            })
        }, list(images, .subset2(data_groups, "val")), NULL)

        # Flatten the nested list of grobs into a single list
        raster_list <- unlist(raster_list, recursive = FALSE, use.names = FALSE)

        # Combine all grobs into a gTree for grid drawing and return
        gTree(children = rlang::inject(gList(!!!raster_list)))
    }
)
