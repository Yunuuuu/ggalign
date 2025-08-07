plot_ideogram <- function(data = waiver(), ...,
                          seqnames = TRUE, seqnames_args = list()) {
    out <- ggalign(data = data, ...) +
        ggplot2::geom_rect(
            ggplot2::aes(
                xmin = .data$start, xmax = .data$end,
                ymin = 0, ymax = 1,
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
    if (isTRUE(seqnames)) seqnames <- 1
    if (is.numeric(seqnames)) {
        if (seqnames > 0) {
            seqnames <- seqnames + 1
            if (is.null(seqnames_args$vjust)) seqnames_args$vjust <- 0
        }
        if (seqnames == 0) seqnames <- 0.5
        if (seqnames < 0) {
            if (is.null(seqnames_args$vjust)) seqnames_args$vjust <- 1
        }
        if (is.null(seqnames_args$y)) seqnames_args$y <- seqnames
        seqnames_args$vjust <-
            out <- out + rlang::inject(ggplot2::geom_text(
                aes(.data$middle, label = .data[[1L]]),
                !!!seqnames_args,
                data = function(d) {
                    data <- ggalign_attr(d, "ranges")
                    data$middle <- (data$start + data$end) / 2
                    data
                }
            ))
    }
    out
}
