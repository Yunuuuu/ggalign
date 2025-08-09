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
#' label display (Default is `TRUE`).
#'   - If `TRUE`, labels are added at a default offset of `1` above the
#'     ideogram.
#'   - If numeric, specifies the position of labels along the y-axis:
#'     * Positive values place labels above the ideogram (offset from the upper
#'       border).
#'     * Negative values place labels below the ideogram (offset from the lower
#'       border).
#'     * Zero centers labels vertically.
#'   Note: the cytoband vertical range is from 0 to 1.
#' @inheritParams ggalign
#' @importFrom ggplot2 waiver
#' @export
plot_ideogram <- function(mapping = aes(), ..., seqnames = NULL, size = NULL,
                          active = NULL) {
    out <- ggalign(
        data = waiver(), mapping = mapping, size = size %||% 0.1,
        active = active
    ) +
        ggplot2::geom_rect(
            ggplot2::aes(
                xmin = .data$start,
                xmax = .data$end,
                ymin = 0L, ymax = 1L,
                fill = .data$gieStain
            )
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
        ) +

        ggplot2::scale_y_continuous(limits = c(0, 1), oob = scales::oob_keep)

    # Add chromosome labels
    seqnames <- seqnames %||% TRUE
    if (isTRUE(seqnames)) seqnames <- 1
    if (is.numeric(seqnames)) {
        dots <- list2(...)
        if (seqnames > 0) {
            seqnames <- seqnames + 1
            if (is.null(dots$vjust)) dots$vjust <- 0
        }
        if (seqnames == 0) seqnames <- 0.5
        if (seqnames < 0) {
            if (is.null(dots$vjust)) dots$vjust <- 1
        }
        if (is.null(dots$y)) dots$y <- seqnames
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
            data <- fortify_data_frame(input, data_arg = "data")
            assert_genomic_data(data, arg = "data")
            data$middle <- (data$start + data$end) / 2L
            names(data)[1L] <- "seqnames"
            dots["data"] <- list(data)
        }
        out <- out + rlang::inject(ggplot2::geom_text(
            mapping = aes(.data$middle, label = .data$seqnames),
            !!!dots
        ))
    }
    out
}
