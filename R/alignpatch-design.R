#' Define the grid to compose plots in
#'
#' To control how different plots are laid out, you need to add a layout design
#' specification. If you are nesting grids, the layout is scoped to the current
#' nesting level.
#' @inheritParams alignpatches
#' @return A `layout_design` object.
#' @examples
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) +
#'     geom_bar(aes(gear)) +
#'     facet_wrap(~cyl)
#' align_plots(p1, p2, p3) +
#'     layout_design(nrow = 1L)
#' align_plots(p1, p2, p3) +
#'     layout_design(ncol = 1L)
#' @importFrom ggplot2 waiver is_waiver
#' @include utils-grid.R
#' @include utils-ggplot.R
#' @include alignpatch-area.R
#' @export
layout_design <- S7::new_class("layout_design",
    properties = list(
        ncol = S7::new_property(
            S7::new_union(S7::class_numeric, NULL),
            validator = function(value) {
                if (!is.null(value) && length(value) != 1L) {
                    return("must be a single number")
                }
            },
            setter = function(self, value) {
                if (identical(value, NA)) value <- NA_real_
                prop(self, "ncol") <- value
                self
            },
            default = NA_real_
        ),
        nrow = S7::new_property(
            S7::new_union(S7::class_numeric, NULL),
            validator = function(value) {
                if (!is.null(value) && length(value) != 1L) {
                    return("must be a single number")
                }
            },
            setter = function(self, value) {
                if (identical(value, NA)) value <- NA_real_
                prop(self, "nrow") <- value
                self
            },
            default = NA_real_
        ),
        byrow = S7::new_property(
            S7::class_logical,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single boolean value")
                }
            },
            default = NA
        ),
        widths = S7::new_property(
            S7::new_union(NULL, S7::class_numeric, S3_unit),
            setter = function(self, value) {
                if (identical(value, NA)) value <- NA_real_
                prop(self, "widths") <- value
                self
            }
        ),
        heights = S7::new_property(
            S7::new_union(NULL, S7::class_numeric, S3_unit),
            setter = function(self, value) {
                if (identical(value, NA)) value <- NA_real_
                prop(self, "heights") <- value
                self
            }
        ),
        area = S7::new_property(
            S7::new_union(S3_waiver, S3_area, NULL),
            setter = function(self, value) {
                if (!is_waiver(value) && !is.null(value)) {
                    value <- as_areas(value)
                }
                prop(self, "area", check = FALSE) <- value
                self
            },
            default = quote(waiver())
        ),
        guides = S7::new_property(
            S7::new_union(S7::class_character, NULL, S3_waiver),
            validator = function(value) {
                if (is_waiver(value) || is.null(value) ||
                    identical(value, NA_character_)) {
                    return(NULL)
                }
                if (length(value) != 1L || !nzchar(value) ||
                    grepl("[^tlbri]", value)) {
                    return(sprintf(
                        "can only be a string containing the %s characters",
                        oxford_and(c(.tlbr, "i"))
                    ))
                }
            },
            setter = function(self, value) {
                if (identical(value, NA)) value <- NA_character_
                prop(self, "guides") <- value
                self
            },
            default = NA_character_
        )
    )
)

#' @importFrom S7 prop prop<-
S7::method(ggalign_init, layout_design) <- function(x) {
    if (identical(prop(x, "ncol"), NA_real_)) {
        prop(x, "ncol", check = FALSE) <- NULL
    }
    if (identical(prop(x, "nrow"), NA_real_)) {
        prop(x, "nrow", check = FALSE) <- NULL
    }
    if (identical(prop(x, "byrow"), NA)) {
        prop(x, "byrow", check = FALSE) <- TRUE
    }
    prop(x, "widths", check = FALSE) <- prop(x, "widths") %||% NA
    prop(x, "heights", check = FALSE) <- prop(x, "heights") %||% NA
    prop(x, "area", check = FALSE) <- prop(x, "area") %|w|% NULL
    if (identical(prop(x, "guides"), NA_character_)) {
        prop(x, "guides", check = FALSE) <- waiver()
    }
    x
}

#' @importFrom ggplot2 is_waiver
S7::method(ggalign_update, list(layout_design, layout_design)) <-
    function(x, object) {
        if (!identical(prop(object, "ncol"), NA_real_)) {
            prop(x, "ncol", check = FALSE) <- prop(object, "ncol")
        }
        if (!identical(prop(object, "nrow"), NA_real_)) {
            prop(x, "nrow", check = FALSE) <- prop(object, "nrow")
        }
        if (!identical(prop(object, "byrow"), NA)) {
            prop(x, "byrow", check = FALSE) <- prop(object, "byrow")
        }
        if (!is.null(prop(object, "widths"))) {
            prop(x, "widths", check = FALSE) <- prop(object, "widths")
        }
        if (!is.null(prop(object, "heights"))) {
            prop(x, "heights", check = FALSE) <- prop(object, "heights")
        }
        if (!is_waiver(prop(object, "area"))) {
            prop(x, "area", check = FALSE) <- prop(object, "area")
        }
        if (!identical(prop(object, "guides"), NA_character_)) {
            prop(x, "guides", check = FALSE) <- prop(object, "guides")
        }
        x
    }

local(
    S7::method(`+`, list(layout_design, layout_design)) <-
        function(e1, e2) ggalign_update(e1, e2)
)
