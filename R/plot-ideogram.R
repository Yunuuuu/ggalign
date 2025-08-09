#' Add an aligned cytoband ideogram plot
#'
#' @description
#' Creates a cytoband ideogram-typically representing chromosome banding
#' patterns-and aligns it within a genomic layout.
#'
#' Cytoband features (`gieStain`) are mapped to fill colors following standard
#' cytogenetic conventions (e.g., gpos, gneg, acen, stalk). Optionally,
#' chromosome names can be displayed as labels.
#'
#' @inheritDotParams ggplot2::geom_text -data -mapping
#' @param seqnames A single logical or numeric value controlling chromosome
#'   label display. Defaults to `TRUE`.
#'
#'   - **Logical (`TRUE`/`FALSE`)**:
#'     * `TRUE`: display labels at the default offset:
#'       + `1` above the ideogram (vertical layout)
#'       + `-1` below the ideogram (horizontal layout)
#'     * `FALSE`: do not display labels.
#'
#'   - **Numeric**: Specifies the vertical position of labels relative to the
#'     ideogram’s y-axis:
#'     * Positive: above the ideogram (offset from the upper border)
#'     * Negative: below the ideogram (offset from the lower border)
#'     * `0`: centered.
#'
#'   Note: The cytoband vertical range spans from `0` to `1`.
#' @inheritParams ggalign
#' @importFrom ggplot2 waiver
#' @export
plot_ideogram <- function(mapping = aes(), ..., seqnames = NULL, size = NULL,
                          active = NULL) {
    out <- ggalign(
        data = waiver(), mapping = mapping, size = size %||% 0.1,
        active = active
    ) +

        ggplot2::scale_fill_manual(
            values = c(
                gpos100 = grDevices::rgb(0, 0, 0, maxColorValue = 255),
                gpos = grDevices::rgb(0, 0, 0, maxColorValue = 255),
                gpos75 = grDevices::rgb(130, 130, 130, maxColorValue = 255),
                gpos66 = grDevices::rgb(160, 160, 160, maxColorValue = 255),
                gpos50 = grDevices::rgb(200, 200, 200, maxColorValue = 255),
                gpos33 = grDevices::rgb(210, 210, 210, maxColorValue = 255),
                gpos25 = grDevices::rgb(200, 200, 200, maxColorValue = 255),
                gvar = grDevices::rgb(220, 220, 220, maxColorValue = 255),
                gneg = grDevices::rgb(255, 255, 255, maxColorValue = 255),
                acen = grDevices::rgb(217, 47, 39, maxColorValue = 255),
                stalk = grDevices::rgb(100, 127, 164, maxColorValue = 255)
            ),
            guide = "none"
        )

    # Add chromosome labels
    dots <- list2(...) # nolint
    call <- current_call()
    force(seqnames)
    init_hook <- function(plot, direction) {
        if (is_horizontal(direction)) {
            plot <- plot + ggplot2::geom_rect(
                ggplot2::aes(
                    ymin = .data$start,
                    ymax = .data$end,
                    fill = .data$gieStain
                ),
                xmin = 0L, xmax = 1L
            ) +
                ggplot2::scale_x_continuous(
                    limits = c(0, 1),
                    oob = scales::oob_keep,
                    breaks = NULL
                )
        } else {
            plot <- plot + ggplot2::geom_rect(
                ggplot2::aes(
                    xmin = .data$start,
                    xmax = .data$end,
                    fill = .data$gieStain
                ),
                ymin = 0L, ymax = 1L
            ) +
                ggplot2::scale_y_continuous(
                    limits = c(0, 1),
                    oob = scales::oob_keep,
                    breaks = NULL
                )
        }
        seqnames <- seqnames %||% TRUE
        if (isTRUE(seqnames)) {
            seqnames <- switch_direction(direction, -1, 1)
        }
        if (is.numeric(seqnames)) {
            if (seqnames > 0) {
                seqnames <- seqnames + 1
                if (is_horizontal(direction)) {
                    if (is.null(dots$hjust)) dots$hjust <- 0
                } else {
                    if (is.null(dots$vjust)) dots$vjust <- 0
                }
            }
            if (seqnames == 0) seqnames <- 0.5
            if (seqnames < 0) {
                if (is_horizontal(direction)) {
                    if (is.null(dots$hjust)) dots$hjust <- 1
                } else {
                    if (is.null(dots$vjust)) dots$vjust <- 1
                }
            }
            if (is_horizontal(direction)) {
                if (is.null(dots$x)) dots$x <- seqnames
            } else {
                if (is.null(dots$y)) dots$y <- seqnames
            }
            if (is.null(input <- allow_lambda(dots$data)) || is.waive(input)) {
                dots$data <- function(d) {
                    data <- ggalign_attr(d, "ranges")
                    data$middle <- (data$start + data$end) / 2L
                    data
                }
            } else if (is.function(input)) {
                dots$data <- function(d) {
                    data <- ggalign_attr(d, "ranges")
                    data$middle <- (data$start + data$end) / 2L
                    input(data)
                }
            } else {
                data <- fortify_data_frame(input,
                    data_arg = "data", call = call
                )
                assert_genomic_data(data, arg = "data", call = call)
                data$middle <- (data$start + data$end) / 2L
                names(data)[1L] <- "seqnames"
                dots["data"] <- list(data)
            }
            if (is_horizontal(direction)) {
                plot <- plot + rlang::inject(ggplot2::geom_text(
                    mapping = aes(y = .data$middle, label = .data$seqnames),
                    !!!dots
                ))
            } else {
                plot <- plot + rlang::inject(ggplot2::geom_text(
                    mapping = aes(x = .data$middle, label = .data$seqnames),
                    !!!dots
                ))
            }
        }
        plot
    }
    out + init_hook
}
