#' @param labs Which axis labs to be free? A string containing one or more of
#' `r oxford_and(.tlbr)`.
#' @return
#' - `free_lab`: A modified version of `plot` with a `ggalign_free_lab` class.
#' @export
#' @rdname free
free_lab <- function(plot, labs = "tlbr") {
    assert_position(labs)
    UseMethod("free_lab")
}

#' @export
free_lab.default <- function(plot, labs = "tlbr") {
    attr(plot, "ggalign_free_labs") <- labs
    add_class(plot, "ggalign_free_lab")
}

#' @export
free_lab.ggalign_free_align <- function(plot, labs = "tlbr") {
    labs <- setdiff_position(
        labs,
        attr(plot, "ggalign_free_axes", exact = TRUE)
    )
    if (!nzchar(labs)) return(plot) # styler: off
    NextMethod()
}

#' @export
free_lab.ggalign_free_border <- function(plot, labs = "tlbr") {
    labs <- setdiff_position(
        labs,
        attr(plot, "ggalign_free_borders", exact = TRUE)
    )
    if (!nzchar(labs)) return(plot) # styler: off
    NextMethod()
}

#' @export
free_lab.ggalign_free_lab <- function(plot, labs = "tlbr") {
    attr(plot, "ggalign_free_labs") <- union_position(
        attr(plot, "ggalign_free_labs", exact = TRUE), labs
    )
    plot
}

####################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
patch.ggalign_free_lab <- function(x) {
    Parent <- NextMethod()
    labs <- setup_position(attr(x, "ggalign_free_labs", exact = TRUE))

    ggproto(
        "PatchFreeLab", Parent,
        align_border = function(self, gt, t, l, b, r) {
            if (self$is_alignpatches()) {
                self$data$gt_list <- .mapply(
                    function(gt, borders) {
                        if (is.null(borders)) return(gt) # styler: off
                        self$free_lab(gt, intersect(borders, labs))
                    },
                    list(
                        gt = .subset2(self$data, "gt_list"),
                        borders = .subset2(self$data, "borders_list")
                    ),
                    NULL
                )
            } else {
                gt <- self$free_lab(gt, labs)
            }
            ggproto_parent(Parent, self)$align_border(gt, t, l, b, r)
        },
        free_lab = function(self, gt, labs) {
            if (length(labs) && is.gtable(gt)) {
                panel_pos <- find_panel(gt)
                for (lab in labs) {
                    name <- switch(lab,
                        top = "xlab-axis-t",
                        left = "ylab-axis-l",
                        bottom = "xlab-axis-b",
                        right = "ylab-axis-r"
                    )
                    if (lab == "top") {
                        panel_border <- .subset2(panel_pos, "t")
                        gt <- liberate_area(
                            gt,
                            panel_border - 3L,
                            .subset2(panel_pos, "l"),
                            panel_border - 1L,
                            .subset2(panel_pos, "r"),
                            name = name,
                            vp = ~ viewport(
                                y = 0L, just = "bottom",
                                height = gtable_height(.x)
                            )
                        )
                    } else if (lab == "left") {
                        panel_border <- .subset2(panel_pos, "l")
                        gt <- liberate_area(
                            gt,
                            .subset2(panel_pos, "t"),
                            panel_border - 3L,
                            .subset2(panel_pos, "b"),
                            panel_border - 1L,
                            name = name,
                            vp = ~ viewport(
                                x = 1L, just = "right",
                                width = gtable_width(.x)
                            )
                        )
                    } else if (lab == "bottom") {
                        panel_border <- .subset2(panel_pos, "b")
                        gt <- liberate_area(
                            gt,
                            panel_border + 1L,
                            .subset2(panel_pos, "l"),
                            panel_border + 3L,
                            .subset2(panel_pos, "r"),
                            name = name,
                            vp = ~ viewport(
                                y = 1L, just = "top",
                                height = gtable_height(.x)
                            )
                        )
                    } else if (lab == "right") {
                        panel_border <- .subset2(panel_pos, "r")
                        gt <- liberate_area(
                            gt,
                            .subset2(panel_pos, "t"),
                            panel_border + 1L,
                            .subset2(panel_pos, "b"),
                            panel_border + 3L,
                            name = name,
                            vp = ~ viewport(
                                x = 0L, just = "left",
                                width = gtable_width(.x)
                            )
                        )
                    }
                }
            }
            gt
        }
    )
}
