test_that("ChainLayout class creation", {
    layout <- new("ChainLayout")
    expect_s4_class(layout, "ChainLayout")
    expect_true(is.list(layout@plot_list))
})

test_that("is_layout_discrete function", {
    expect_true(is_layout_discrete(stack_discrete("h")))
    expect_true(is_layout_discrete(stack_discrete("v")))
})

test_that("is_layout_continuous function", {
    expect_false(is_layout_continuous(stack_discrete("h")))
    expect_false(is_layout_continuous(stack_discrete("v")))
    expect_true(is_layout_continuous(stack_continuous("h")))
    expect_true(is_layout_continuous(stack_continuous("v")))
})

test_that("`chain_layout_add()` with layout_title", {
    layout <- new("ChainLayout")
    layout <- chain_layout_add(layout_title("my title"), layout, "title")
    expect_equal(layout@titles, list(title = "my title"))
})

test_that("`chain_layout_add()` function with NULL", {
    old <- new("ChainLayout", plot_list = list())
    new <- chain_layout_add(NULL, old, "NULL")
    expect_identical(old, new)
})

test_that("`chain_layout_add()` function with `ggalign_plot`", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    expect_snapshot_error(stack_continuous("h") + align_dendro())
    expect_snapshot_error(stack_continuous("v") + align_dendro())

    # stack_discrete("h") ----------------------------------------
    # no layout data, we must provide data in `align_*()`
    expect_snapshot_error(stack_discrete("h") + align_dendro())

    # layout `index` is updated correctly
    stack <- stack_discrete("h", small_mat) +
        align_dendro()
    expect_identical(
        stack@design$index,
        order2(ggalign_stat(stack, 1L))
    )
    # layout `panel` is updated correctly
    stack <- stack_discrete("h", small_mat) + align_dendro(k = 3L)
    expect_identical(
        stack@design$panel,
        .subset2(stack@plot_list, 1L)@align$panel
    )
    expect_identical(
        stack@design$index,
        reorder_index(
            .subset2(stack@plot_list, 1L)@align$panel,
            order2(ggalign_stat(stack, 1L))
        )
    )

    # stack_discrete("v") ----------------------------------------
    # no layout data, we must provide data in `align_*()`
    expect_snapshot_error(stack_discrete("v") + align_dendro())

    # layout `index` is updated correctly
    stack <- stack_discrete("v", small_mat) +
        align_dendro()
    expect_identical(
        stack@design$index,
        order2(ggalign_stat(stack, 1L))
    )
    # layout `panel` is updated correctly
    stack <- stack_discrete("v", small_mat) + align_dendro(k = 3L)
    expect_identical(
        stack@design$panel,
        .subset2(stack@plot_list, 1L)@align$panel
    )
    expect_identical(
        stack@design$index,
        reorder_index(
            .subset2(stack@plot_list, 1L)@align$panel,
            order2(ggalign_stat(stack, 1L))
        )
    )
})

###################################################################
testthat::test_that("`chain_layout_add()` function with QuadLayout", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    # stack with no data adds a quad_layout() with no data gave error
    expect_snapshot_error(stack_discrete("h") + quad_discrete())
    expect_snapshot_error(stack_discrete("h") + quad_alignv())
    expect_snapshot_error(stack_discrete("h") + quad_continuous())
    # incompatible design
    expect_snapshot_error(stack_discrete("h", small_mat) + quad_alignv())

    expect_snapshot_error(stack_discrete("v") + quad_discrete())
    expect_snapshot_error(stack_discrete("v") + quad_alignv())
    expect_snapshot_error(stack_discrete("v") + quad_continuous())
    # incompatible design
    expect_snapshot_error(stack_discrete("v", small_mat) + quad_alignh())

    # stack_continuous()
    expect_snapshot_error(stack_continuous("h") + quad_alignh())
    expect_snapshot_error(stack_continuous("h") + quad_alignv())
    expect_snapshot_error(stack_continuous("h", mtcars) +
        quad_alignv())

    expect_snapshot_error(stack_continuous("v") + quad_alignv())
    expect_snapshot_error(stack_continuous("v") + quad_alignh())
    expect_snapshot_error(stack_continuous("v", mtcars) +
        quad_alignh())

    # `stack_discrete()` update layout design correctly
    stack <- stack_discrete("h", small_mat) + quad_alignh()
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@horizontal
    )
    stack <- stack_discrete("h") +
        (quad_alignh(small_mat) + anno_left() + align_dendro())
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@horizontal
    )
    stack <- stack_discrete("h") +
        quad_alignh(small_mat) +
        (quad_alignh(small_mat) + anno_left() + align_dendro())
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@horizontal
    )
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 2L)@horizontal
    )

    stack <- stack_discrete("h") +
        (quad_alignh(small_mat) +
            anno_left() + align_dendro(k = 3L))
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@horizontal
    )
    stack <- stack_discrete("h") +
        quad_alignh(small_mat) +
        (quad_alignh(small_mat) +
            anno_left() + align_dendro(k = 3L))
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@horizontal
    )
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 2L)@horizontal
    )

    stack <- stack_discrete("h", small_mat) +
        quad_alignh() +
        anno_left(initialize = FALSE) +
        stack_discrete("h", small_mat) +
        align_dendro()
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@horizontal
    )
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@left@design
    )
    expect_snapshot_error(stack_discrete("h", small_mat) +
        quad_alignh() +
        anno_left(initialize = FALSE) +
        stack_crossh())
    expect_snapshot_error(stack_discrete("h", small_mat) +
        (quad_alignh() +
            anno_left(initialize = FALSE) +
            stack_crossh()))

    # `stack_alignv()` update layout coords correctly
    stack <- stack_alignv(small_mat) + quad_alignv()
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@vertical
    )
    stack <- stack_alignv() +
        (quad_alignv(small_mat) + anno_top() + align_dendro())
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@vertical
    )
    stack <- stack_alignv() +
        quad_alignv(small_mat) +
        (quad_alignv(small_mat) + anno_top() + align_dendro())
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@vertical
    )
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 2L)@vertical
    )

    stack <- stack_alignv() +
        (quad_alignv(small_mat) + anno_top() + align_dendro(k = 3L))
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@vertical
    )
    stack <- stack_alignv() +
        quad_alignv(small_mat) +
        (quad_alignv(small_mat) + anno_top() + align_dendro(k = 3L))
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@vertical
    )
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 2L)@vertical
    )

    stack <- stack_alignv(small_mat) +
        quad_alignv() +
        anno_top(initialize = FALSE) +
        stack_alignv(small_mat) +
        align_dendro()
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@vertical
    )
    expect_identical(
        stack@design,
        .subset2(stack@plot_list, 1L)@top@design
    )
    expect_snapshot_error(stack_alignv(small_mat) +
        quad_alignv() +
        anno_top(initialize = FALSE) +
        stack_crossv())
    expect_snapshot_error(stack_alignv(small_mat) +
        (quad_alignv() +
            anno_top(initialize = FALSE) +
            stack_crossv()))
})

test_that("`chain_layout_add()` function with stack_switch", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    # change parameters for stack self
    p <- stack_alignh(small_mat)
    p2 <- p + stack_active(sizes = unit(1, "cm")) -
        scheme_align(guides = "tlbr", free_labs = "tlbr")
    expect_identical(p2@sizes, unit(1, "cm"))
    schemes <- p2@schemes
    expect_identical(schemes$scheme_align$guides, "tlbr")
    expect_identical(schemes$scheme_align$free_labs, "tlbr")
    expect_identical(schemes$scheme_data, new_scheme_data(NULL))
})

test_that("`chain_layout_add()` function with ggalign_with_quad", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
    expect_snapshot_error(stack_discrete("v", small_mat) +
        align_dendro() +
        with_quad(geom_point()))
})

test_that("`chain_layout_add()` function with CrossLayout", {
})

test_that("`chain_layout_add()` function with layout_annotation", {

})

test_that("`chain_layout_add()` function with default", {

})
