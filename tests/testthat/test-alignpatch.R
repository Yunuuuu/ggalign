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

test_that("The grid can be controlled", {
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

    expect_doppelganger("Setting annotation", {
        align_plots(p1, p2, p3, p4,
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
})

test_that("collect guides works well", {
    expect_doppelganger(
        "collect normal guides",
        align_plots(p1, p3, guides = "tlbr")
    )
    p_guides <- p3 + scale_color_continuous(guide = guide_colorbar(
        theme = theme(legend.key.height = unit(1, "null"))
    ))
    expect_doppelganger(
        "collect guides with null unit",
        align_plots(p1, p_guides, guides = "tlbr")
    )
    expect_doppelganger(
        "collect guides from multiple plots with null unit",
        align_plots(p1, p_guides,
            p_guides + labs(color = "another"),
            guides = "tlbr"
        )
    )
})

test_that("Fixed aspect plots behave", {
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

testthat::test_that("`free_space()` works well", {
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
