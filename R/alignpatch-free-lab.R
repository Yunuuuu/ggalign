#' @param labs Which axis labs to be free? A string containing one or more of
#' `r oxford_and(.tlbr)`.
#' @return
#' - `free_lab`: A modified version of `plot` with a `FreeLab` class.
#' @export
#' @rdname free
free_lab <- S7::new_generic(
    "free_lab", "plot",
    function(plot, labs = "tlbr") S7_dispatch()
)

FreeLab <- S7::new_class(
    "FreeLab",
    properties = list(
        plot = S7::class_any,
        labs = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single string")
                }
                if (is.na(value)) {
                    return("cannot be missing (`NA`)")
                }
                if (grepl("[^tlbr]", value)) {
                    return(sprintf(
                        "can only contain the %s characters",
                        oxford_and(.tlbr)
                    ))
                }
            }
        )
    )
)

S7::method(free_lab, S7::class_any) <- function(plot, labs = "tlbr") {
    FreeLab(plot, labs)
}

S7::method(free_lab, alignpatches) <- function(plot, labs = "tlbr") {
    prop(plot, "plots") <- lapply(prop(plot, "plots"), free_lab, labs = labs)
    plot
}

#' @importFrom S7 prop
S7::method(free_lab, FreeLab) <- function(plot, labs = "tlbr") {
    old <- prop(plot, "labs")
    prop(plot, "labs") <- labs # will validate the input labs
    prop(plot, "labs", check = FALSE) <- union_position(old, labs)
    plot
}

####################################################
#' @importFrom ggplot2 ggproto ggproto_parent find_panel
#' @importFrom gtable gtable_height gtable_width
S7::method(alignpatch, FreeLab) <- function(x) {
    Parent <- alignpatch(prop(x, "plot"))
    ggproto(
        "PatchFreeLab", Parent,
        labs = setup_position(prop(x, "labs")),
        align_border = function(self, gt, t = NULL, l = NULL,
                                b = NULL, r = NULL) {
            if (is.gtable(gt)) {
                panel_pos <- find_panel(gt)
                for (lab in self$labs) {
                    name <- paste(
                        switch_position(lab, "xlab", "ylab"),
                        "axis", lab,
                        sep = "-"
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
            ggproto_parent(Parent, self)$align_border(gt, t, l, b, r)
        }
    )
}
