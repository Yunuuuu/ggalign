test_that("add ggplot elements works well", {
    expect_no_error(ggalign() + geom_point())
    expect_no_error(ggfree() + geom_point())
    expect_no_error(ggcross() + geom_point())
})

test_that("`plot_ideogram()` works well", {
    expect_doppelganger(
        "circle_genomic, plot_ideogram",
        circle_genomic(
            "hg19",
            radial = coord_radial(inner.radius = 0.2, rotate.angle = TRUE),
            direction = "inward",
            theme = theme(plot.margin = margin(t = 10, b = 10, l = 2, unit = "mm"))
        ) -
            # Remove radial axes for a cleaner circular look
            scheme_theme(
                axis.text.r = element_blank(),
                axis.ticks.r = element_blank()
            ) +

            # Cytoband (Ideogram) Layer ------------------
            plot_ideogram(seqnames = 0.1) +
            scale_x_continuous(
                breaks = scales::breaks_pretty(2),
                labels = scales::label_bytes()
            ) +
            guides(
                r = "none", r.sec = "axis",
                theta = guide_axis_theta(angle = 0)
            ) +
            theme(axis.text.theta = element_text(size = 6))
    )
})
