#' Define the grid to compose plots in
#'
#' To control how different plots are laid out, you need to add a layout design
#' specification. If you are nesting grids, the layout is scoped to the current
#' nesting level.
#' @inheritParams align_plots
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
            S7::new_union(S3_waiver, S7::class_numeric, NULL),
            setter = function(self, value) {
                if (!is_waiver(value)) {
                    assert_number_whole(value,
                        min = 1, allow_null = TRUE,
                        arg = "@ncol"
                    )
                }
                prop(self, "ncol", check = FALSE) <- value
                self
            },
            default = quote(waiver())
        ),
        nrow = S7::new_property(
            S7::new_union(S3_waiver, S7::class_numeric, NULL),
            setter = function(self, value) {
                if (!is_waiver(value)) {
                    assert_number_whole(value,
                        min = 1, allow_null = TRUE,
                        arg = "@nrow"
                    )
                }
                prop(self, "nrow", check = FALSE) <- value
                self
            },
            default = quote(waiver())
        ),
        byrow = S7::new_property(
            S7::new_union(NULL, S7::class_logical),
            setter = function(self, value) {
                if (!is.null(value)) assert_bool(value, arg = "@byrow")
                prop(self, "byrow", check = FALSE) <- value
                self
            }
        ),
        widths = S7::new_property(
            S7::new_union(NULL, S7::class_numeric, S3_unit),
            setter = function(self, value) {
                if (is_na(value)) value <- NA_real_
                prop(self, "widths") <- value
                self
            }
        ),
        heights = S7::new_property(
            S7::new_union(NULL, S7::class_numeric, S3_unit),
            setter = function(self, value) {
                if (is_na(value)) value <- NA_real_
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
            setter = function(self, value) {
                if (is_na(value)) {
                    value <- NA_character_
                } else if (!is_waiver(value) && !is.null(value)) {
                    assert_guides(value, arg = "@guides")
                }
                prop(self, "guides", check = FALSE) <- value
                self
            },
            default = NA_character_
        )
    )
)

#' @importFrom S7 prop prop<-
S7::method(init_hook, layout_design) <- function(input) {
    prop(input, "ncol", check = FALSE) <- prop(input, "ncol") %|w|% NULL
    prop(input, "nrow", check = FALSE) <- prop(input, "nrow") %|w|% NULL
    prop(input, "byrow", check = FALSE) <- prop(input, "byrow") %||% TRUE
    prop(input, "widths", check = FALSE) <- prop(input, "widths") %||% NA
    prop(input, "heights", check = FALSE) <- prop(input, "heights") %||% NA
    prop(input, "area", check = FALSE) <- prop(input, "area") %|w|% NULL
    if (identical(prop(input, "guides"), NA_character_)) {
        prop(input, "guides", check = FALSE) <- waiver()
    }
    input
}

#' @importFrom ggplot2 is_waiver
local(
    S7::method(`+`, list(layout_design, layout_design)) <-
        function(e1, e2) {
            if (!is_waiver(prop(e2, "ncol"))) {
                prop(e1, "ncol", check = FALSE) <- prop(e2, "ncol")
            }
            if (!is_waiver(prop(e2, "nrow"))) {
                prop(e1, "nrow", check = FALSE) <- prop(e2, "nrow")
            }
            if (!is.null(prop(e2, "byrow"))) {
                prop(e1, "byrow", check = FALSE) <- prop(e2, "byrow")
            }
            if (!is.null(prop(e2, "widths"))) {
                prop(e1, "widths", check = FALSE) <- prop(e2, "widths")
            }
            if (!is.null(prop(e2, "heights"))) {
                prop(e1, "heights", check = FALSE) <- prop(e2, "heights")
            }
            if (!is_waiver(prop(e2, "area"))) {
                prop(e1, "area", check = FALSE) <- prop(e2, "area")
            }
            if (!identical(prop(e2, "guides"), NA_character_)) {
                prop(e1, "guides", check = FALSE) <- prop(e2, "guides")
            }
            e1
        }
)
