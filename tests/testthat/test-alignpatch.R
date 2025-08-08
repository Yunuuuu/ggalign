with_empty_dev <- function(plot, ...) {
    grDevices::pdf(NULL)
    print(plot, ...)
    dev.off()
}

test_that("print.alignpatches sets last plot and draws grid", {
    p <- align_plots()
    with_empty_dev(p)
    expect_identical(ggplot2::last_plot(), p)
})

test_that("print.alignpatches seeks or pushes viewport when vp is provided", {
    p <- align_plots()

    # Errors if vp is character and newpage=TRUE
    expect_snapshot_error(with_empty_dev(p, newpage = TRUE, vp = "some_vp"))

    # Errors if vp does not exist as a viewport
    expect_snapshot_error(with_empty_dev(p, newpage = FALSE, vp = "panel-1-1"))

    # Test with viewport object
    vp_obj <- viewport()
    expect_silent(with_empty_dev(p, newpage = TRUE, vp = vp_obj))
})

test_that("The grid can be controlled", {
    p1 <- ggplot(mtcars) +
        geom_point(aes(mpg, disp)) +
        ggtitle("Plot 1")

    p2 <- ggplot(mtcars) +
        geom_boxplot(aes(gear, disp, group = gear)) +
        ggtitle("Plot 2")

    p3 <- ggplot(mtcars) +
        geom_point(aes(hp, wt, colour = mpg)) +
        ggtitle("Plot 3")

    p4 <- ggplot(mtcars) +
        geom_bar(aes(gear)) +
        facet_wrap(~cyl) +
        ggtitle("Plot 4")
    expect_doppelganger("Setting ncol", {
        align_plots(p1, p2, p3, p4, ncol = 3L)
    })

    expect_doppelganger("Setting nrow", {
        align_plots(p1, p2, p3, p4, nrow = 3L)
    })

    expect_doppelganger("Setting widths", {
        align_plots(p1, p2, p3, p4, widths = c(1, 2))
    })

    expect_doppelganger("Setting heights", {
        align_plots(p1, p2, p3, p4, heights = c(1, 2))
    })

    expect_doppelganger("Setting widths as units", {
        align_plots(p1, p2, p3, p4, widths = grid::unit(3, "cm"))
    })

    expect_doppelganger("Setting heights as units", {
        align_plots(p1, p2, p3, p4, heights = grid::unit(3, "cm"))
    })

    expect_doppelganger("Setting title", {
        align_plots(p1, p2, p3, p4) +
            layout_title(
                title = "I'm title", subtitle = "I'm subtitle",
                caption = "I'm caption"
            )
    })

    expect_doppelganger("patch titles", {
        align_plots(
            p1 + patch_titles(top = "I'm top patch title"),
            p2 + patch_titles(left = "I'm left patch title"),
            p3 + patch_titles(bottom = "I'm bottom patch title"),
            p4 + patch_titles(right = "I'm right patch title")
        )
    })
    expect_doppelganger("patch titles theme", {
        align_plots(
            p1 + patch_titles(top = "I'm top patch title") +
                theme(plot.patch_title.top = element_text(face = "bold")),
            p2 + patch_titles(left = "I'm left patch title") +
                theme(plot.patch_title.left = element_text(face = "bold"))
        )
    })
    expect_doppelganger("background and panel border", {
        align_plots(
            p1 + theme(plot.background = element_blank()),
            p2 + theme(plot.background = element_blank()),
            align_plots(
                p3 + theme(plot.background = element_blank()),
                p4 + theme(plot.background = element_blank())
            ) +
                layout_theme(
                    panel.border = element_rect(colour = "blue"),
                    plot.background = element_rect(
                        fill = "yellow", color = "black",
                        linewidth = unit(1, "cm")
                    )
                )
        ) +
            layout_title(
                title = "I'm layout title",
                subtitle = "I'm layout subtitle",
                caption = "I'm layout caption"
            ) + layout_theme(
                panel.border = element_rect(colour = "red"),
                plot.background = element_rect(
                    fill = "green", color = "black",
                    linewidth = unit(1, "cm")
                )
            )
    })
})

test_that("accept patchwork", {
    testthat::skip_if_not_installed("patchwork")
    p1 <- ggplot(mtcars) +
        geom_point(aes(mpg, disp)) +
        ggtitle("Plot 1")

    p2 <- ggplot(mtcars) +
        geom_boxplot(aes(gear, disp, group = gear)) +
        ggtitle("Plot 2")

    p3 <- ggplot(mtcars) +
        geom_point(aes(hp, wt, colour = mpg)) +
        ggtitle("Plot 3")

    p4 <- ggplot(mtcars) +
        geom_bar(aes(gear)) +
        facet_wrap(~cyl) +
        ggtitle("Plot 4")
    expect_doppelganger("align_plots() add patchwork wrap_plots", {
        align_plots(patchwork::wrap_plots(p1, p2), p3, p4)
    })
    expect_doppelganger("align_plots() add patchwork spacer", {
        align_plots(patchwork::plot_spacer(), p2, p3, p4)
    })
    p1 <- ggplot(mtcars) +
        geom_bar(aes(y = factor(gear), fill = factor(gear))) +
        scale_y_discrete(
            "",
            labels = c(
                "3 gears are often enough",
                "But, you know, 4 is a nice number",
                "I would def go with 5 gears in a modern car"
            )
        )
    expect_doppelganger("align_plots() add patchwork free", {
        align_plots(patchwork::free(p1, side = "l"), p2, p3, p4)
    })
})

test_that("`ggsave()` works well", {
    p1 <- ggplot(mtcars) +
        geom_point(aes(mpg, disp)) +
        ggtitle("Plot 1")

    p2 <- ggplot(mtcars) +
        geom_boxplot(aes(gear, disp, group = gear)) +
        ggtitle("Plot 2")

    p3 <- ggplot(mtcars) +
        geom_point(aes(hp, wt, colour = mpg)) +
        ggtitle("Plot 3")

    p4 <- ggplot(mtcars) +
        geom_bar(aes(gear)) +
        facet_wrap(~cyl) +
        ggtitle("Plot 4")
    p <- align_plots(p1, p2, p3, p4, widths = c(1, 2))
    expect_no_error(ggplot2::ggsave(tempfile(fileext = ".png"), plot = p))
})

test_that("collect guides works well", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp)) +
        ggtitle("Plot without guide")
    p_color <- ggplot(mtcars) +
        geom_point(aes(hp, wt, colour = mpg)) +
        ggtitle("Plot with color")
    expect_doppelganger(
        "collect normal guides",
        align_plots(p, p_color, guides = "tlbr")
    )
    p_guide_null_unit <- p_color +
        scale_color_continuous(guide = guide_colorbar(
            theme = theme(legend.key.height = unit(1, "null"))
        ))
    expect_doppelganger(
        "collect guides with null unit",
        align_plots(p, p_guide_null_unit, guides = "tlbr")
    )
    expect_doppelganger(
        "collect guides from multiple plots with null unit",
        align_plots(
            p, p_guide_null_unit,
            p_guide_null_unit + labs(color = "another"),
            guides = "tlbr"
        )
    )
    expect_doppelganger(
        "collect inside guides",
        align_plots(
            p_color +
                labs(color = "Plot 1", title = "Plot 1") +
                theme(
                    legend.position = "inside",
                    legend.position.inside = c(1, 0),
                    legend.justification.inside = c(1, 0)
                ),
            p_color +
                labs(color = "Plot 2", title = "Plot 2") +
                theme(
                    legend.position = "inside",
                    legend.position.inside = c(0, 1),
                    legend.justification.inside = c(0, 1)
                ),
            guides = "tlbri"
        )
    )
})

test_that("Fixed aspect plots behave", {
    p1 <- ggplot(mtcars) +
        geom_point(aes(mpg, disp)) +
        ggtitle("Plot 1")

    p2 <- ggplot(mtcars) +
        geom_boxplot(aes(gear, disp, group = gear)) +
        ggtitle("Plot 2")

    p3 <- ggplot(mtcars) +
        geom_point(aes(hp, wt, colour = mpg)) +
        ggtitle("Plot 3")

    p4 <- ggplot(mtcars) +
        geom_bar(aes(gear)) +
        facet_wrap(~cyl) +
        ggtitle("Plot 4")

    p_f <- ggplot(mtcars) +
        geom_point(aes(hp, disp)) +
        coord_fixed() +
        ggtitle("Fixed Aspect")
    expect_doppelganger("FAR optimise space by default 1", {
        align_plots(p1, p_f, p3, p4)
    })
    expect_doppelganger("FAR optimise space by default 2", {
        align_plots(p1, p_f, p_f, p4)
    })
    expect_doppelganger("FAR optimise space by default 3", {
        align_plots(p_f, p_f, p3, p4)
    })
    expect_doppelganger("FAR optimise space by default 4", {
        align_plots(p1, p2, p_f, p4, widths = 1L)
    })
    expect_doppelganger("FAR optimise space by default 5", {
        align_plots(p1, p2, p_f, p4, heights = 1L)
    })
    expect_doppelganger("FAR space optimisation can be turned off", {
        align_plots(p1, p2, p_f, p4, widths = 1L, heights = 1L)
    })
    expect_doppelganger("FAR dimensions can be set with units:...", {
        align_plots(p1, p2, p_f,
            widths = unit(c(1, 3, NA), c("null", "cm", "null"))
        )
    })

    p_l1 <- ggplot(mtcars, aes(cyl, qsec, color = as.factor(vs))) +
        geom_point()
    p_l2 <- p_l1 + labs(color = "a very looooooong legend title")
    expect_doppelganger("FAR legend justification", {
        align_plots(
            p_l1 + theme(legend.justification = "left", aspect.ratio = 1),
            p_l2 + theme(legend.justification = "left", aspect.ratio = 1),
            ncol = 1L,
            widths = unit(c(1, 3, NA), c("null", "cm", "null"))
        )
    })
})

test_that("`free_align()` works well", {
    p1 <- ggplot(mtcars) +
        geom_point(aes(mpg, disp)) +
        ggtitle("Plot 1")

    p2 <- ggplot(mtcars) +
        geom_boxplot(aes(gear, disp, group = gear)) +
        ggtitle("Plot 2")

    p3 <- ggplot(mtcars) +
        geom_point(aes(hp, wt, colour = mpg)) +
        ggtitle("Plot 3")

    p4 <- ggplot(mtcars) +
        geom_bar(aes(gear)) +
        facet_wrap(~cyl) +
        ggtitle("Plot 4")
    p5 <- ggplot(mpg, aes(class)) +
        geom_bar() +
        ylim(0, 65) +
        coord_flip()
    p_f <- ggplot(mtcars) +
        geom_point(aes(hp, disp)) +
        coord_fixed() +
        ggtitle("Fixed Aspect")
    expect_doppelganger("free_align() with ggplot", {
        align_plots(free_align(p3, "l"), p5, ncol = 1L)
    })
    expect_doppelganger("free_align() with facet ggplot", {
        align_plots(p3, free_align(p4, "t"))
    })
    expect_doppelganger("free_align() with FAR", {
        align_plots(free_align(p_f, "b"), p4)
    })
    expect_doppelganger("free_align() with nested alignpatches", {
        align_plots(
            free_align(align_plots(p1, p2, p_f, p4, heights = 1L), "t"),
            p4
        )
    })
})

test_that("`free_border()` works well", {
    p1 <- ggplot(mtcars) +
        geom_point(aes(mpg, disp)) +
        ggtitle("Plot 1")

    p2 <- ggplot(mtcars) +
        geom_boxplot(aes(gear, disp, group = gear)) +
        ggtitle("Plot 2")

    p3 <- ggplot(mtcars) +
        geom_point(aes(hp, wt, colour = mpg)) +
        ggtitle("Plot 3")

    p4 <- ggplot(mtcars) +
        geom_bar(aes(gear)) +
        facet_wrap(~cyl) +
        ggtitle("Plot 4")
    p5 <- ggplot(mpg, aes(class)) +
        geom_bar() +
        ylim(0, 65) +
        coord_flip()
    p_f <- ggplot(mtcars) +
        geom_point(aes(hp, disp)) +
        coord_fixed() +
        ggtitle("Fixed Aspect")
    expect_doppelganger("free_border() with ggplot", {
        align_plots(free_border(p3, "l"), p5, ncol = 1L)
    })
    expect_doppelganger("free_border() with facet ggplot", {
        align_plots(free_border(p3, "t"), p4)
    })
    expect_doppelganger("free_border() with FAR", {
        align_plots(free_border(p_f, "t"), p4)
    })
    expect_doppelganger("free_border() with nested alignpatches", {
        align_plots(
            free_border(align_plots(p1, p2, free_border(p_f, "t"),
                p4,
                heights = 1L
            ), "l"),
            p5,
            ncol = 1L
        )
    })
    expect_doppelganger("free_border() mix with free_align", {
        align_plots(
            free_align(free_border(
                align_plots(p1, p2, p_f, p4, heights = 1L), "l"
            ), "t"), p4, p5, NULL,
            ncol = 2L
        )
    })
})

test_that("`free_space()` works well", {
    p1 <- ggplot(mtcars) +
        geom_point(aes(mpg, disp)) +
        ggtitle("Plot 1")

    p2 <- ggplot(mtcars) +
        geom_boxplot(aes(gear, disp, group = gear)) +
        ggtitle("Plot 2")

    p3 <- ggplot(mtcars) +
        geom_point(aes(hp, wt, colour = mpg)) +
        ggtitle("Plot 3")

    p4 <- ggplot(mtcars) +
        geom_bar(aes(gear)) +
        facet_wrap(~cyl) +
        ggtitle("Plot 4")
    p5 <- ggplot(mpg, aes(class)) +
        geom_bar() +
        ylim(0, 65) +
        coord_flip()
    p_f <- ggplot(mtcars) +
        geom_point(aes(hp, disp)) +
        coord_fixed() +
        ggtitle("Fixed Aspect")
    p1 <- ggplot(mtcars) +
        geom_bar(aes(y = factor(gear), fill = factor(gear))) +
        scale_y_discrete(
            "",
            labels = c(
                "3 gears are often enough",
                "But, you know, 4 is a nice number",
                "I would def go with 5 gears in a modern car"
            )
        )
    # When combined with other plots it ends up looking bad
    p2 <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    expect_doppelganger("free_space() with ggplot", {
        align_plots(NULL, free_space(p1, "l"), p2, p2)
    })
    expect_doppelganger("free_space() with alignpatches", {
        align_plots(NULL, free_space(align_plots(p1), "l"), p2, p2)
    })
})

test_that("`free_guide()` works well", {
    p_no_guide <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p_color <- ggplot(mtcars) +
        geom_point(aes(hp, wt, colour = mpg))
    p_top <- p_color +
        scale_color_continuous(
            name = "top",
            guide = guide_colorbar(position = "top")
        )
    p_left <- p_color +
        scale_color_continuous(
            name = "left",
            guide = guide_colorbar(position = "left")
        )
    p_bottom <- p_color +
        scale_color_continuous(
            name = "bottom",
            guide = guide_colorbar(position = "bottom")
        )
    p_right <- p_color +
        scale_color_continuous(
            name = "right",
            guide = guide_colorbar(position = "right")
        )
    p_guides <- align_plots(
        p_color + scale_color_continuous(
            name = "patches_top",
            guide = guide_colorbar(position = "top")
        ),
        p_color + scale_color_continuous(
            name = "patches_left",
            guide = guide_colorbar(position = "left")
        ),
        p_color + scale_color_continuous(
            name = "patches_bottom",
            guide = guide_colorbar(position = "bottom")
        ),
        p_color + scale_color_continuous(
            name = "patches_right",
            guide = guide_colorbar(position = "right")
        ),
        guides = "tlbr"
    )
    expect_doppelganger(
        "free_guide() with ggplot",
        align_plots(
            p_guides,
            free_guide(p_top, "l"),
            free_guide(p_left, "t"),
            free_guide(p_bottom, "l"),
            free_guide(p_right, "t"),
            guides = "tlbr"
        )
    )
    expect_doppelganger(
        "free_guide() with ggplot of NULL guides",
        align_plots(p_no_guide, free_guide(p_right, NULL), guides = "tlbr")
    )
    expect_doppelganger(
        "free_guide() with alignpatches",
        align_plots(
            p_guides,
            p_top, p_left, p_bottom, p_right,
            guides = "tlbr"
        )
    )
})

test_that("`layout_tags()` works well", {
    p1 <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p2 <- ggplot(mtcars) +
        geom_boxplot(aes(gear, disp, group = gear))
    p3 <- ggplot(mtcars) +
        geom_bar(aes(gear)) +
        facet_wrap(~cyl)

    # Add tags to plots
    expect_doppelganger(
        "layout_tags, inherits tags by nested layout",
        align_plots(p1, align_plots(p2, p3), ncol = 1) + layout_tags("A")
    )

    expect_doppelganger(
        "layout_tags, disable nested inner tagging",
        align_plots(p1, align_plots(p2, p3) + layout_tags(NULL), ncol = 1) +
            layout_tags("A")
    )

    # Add multilevel tagging to nested layouts
    expect_doppelganger(
        "layout_tags, nested layout with multilevel tagging",
        align_plots(
            p1,
            align_plots(p2, p3) + layout_tags(1),
            ncol = 1
        ) +
            layout_tags("A")
    )

    # Use a custom tag sequence (mixed with a standard one)
    expect_doppelganger(
        "layout_tags, custom tag sequence",
        align_plots(
            p1,
            align_plots(p2, p3) + layout_tags(1),
            ncol = 1
        ) +
            layout_tags(c("&", "%"))
    )

    expect_warning(
        with_empty_dev(align_plots(p1, p2, p3, nrow = 1) +
            layout_tags(c("&", "%")))
    )
    expect_doppelganger(
        "layout_tags, recycled the tags",
        align_plots(
            p1,
            align_plots(p2, p3) + layout_tags(1),
            ncol = 1
        ) +
            layout_tags("A")
    )
    expect_doppelganger(
        "layout_tags, location = plot",
        align_plots(
            p1,
            align_plots(p2, p3) + layout_tags(1),
            ncol = 1
        ) +
            layout_tags("A") +
            layout_theme(plot.tag.location = "plot")
    )
    expect_doppelganger(
        "layout_tags, location = panel",
        align_plots(
            p1,
            align_plots(p2, p3) + layout_tags(1),
            ncol = 1
        ) +
            layout_tags("A") +
            layout_theme(plot.tag.location = "panel")
    )
    expect_doppelganger(
        "layout_tags, location = margin",
        align_plots(
            p1,
            align_plots(p2, p3) + layout_tags(1),
            ncol = 1
        ) +
            layout_tags("A") +
            layout_theme(plot.tag.location = "margin")
    )
})
