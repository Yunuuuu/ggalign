#' @importFrom S7 S7_dispatch
alignpatches_add <- S7::new_generic(
    "alignpatches_add", "object",
    function(object, patches, objectname) S7_dispatch()
)

S7::method(alignpatches_add, S7::class_any) <-
    function(object, patches, objectname) {
        if (is.null(object)) return(patches) # styler: off
        cli_abort(c(
            "Cannot add {objectname}",
            "x" = "Only other layout elements or compatible objects can be added."
        ))
    }

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
#' @importFrom ggplot2 waiver
#' @export
layout_design <- function(ncol = waiver(), nrow = waiver(), byrow = waiver(),
                          widths = waiver(), heights = waiver(),
                          area = waiver(), guides = NA, design = waiver()) {
    if (!is.waive(ncol)) {
        assert_number_whole(ncol, min = 1, allow_na = TRUE, allow_null = TRUE)
    }
    if (!is.waive(nrow)) {
        assert_number_whole(nrow, min = 1, allow_na = TRUE, allow_null = TRUE)
    }
    if (!is.waive(byrow)) assert_bool(byrow)
    area <- area %|w|% design
    if (!is.waive(area)) area <- as_areas(area)
    if (!identical(guides, NA) && !is.waive(guides) && !is.null(guides)) {
        assert_guides(guides)
    }
    structure(
        list(
            ncol = ncol,
            nrow = nrow,
            byrow = byrow,
            widths = widths,
            heights = heights,
            area = area,
            guides = guides
        ),
        class = c("layout_design", "plot_layout")
    )
}

S3_layout_design <- S7::new_S3_class("layout_design")

# Bypass S7 setter validation: update internal property via attr() directly
S7::method(alignpatches_add, S3_layout_design) <-
    function(object, patches, objectname) {
        old <- prop(patches, "layout")
        guides <- .subset2(object, "guides")
        object$guides <- NULL # guides need special consideration
        for (nm in names(object)) {
            if (is.waive(.subset2(object, nm))) next
            old[nm] <- list(.subset2(object, nm))
        }
        if (is.null(guides) || is.waive(guides)) {
            old["guides"] <- list(guides)
        } else if (!identical(guides, NA)) {
            old["guides"] <- list(setup_guides(guides))
        }
        attr(patches, "layout") <- old
        patches
    }

# plot_layout is from `patchwork` package
S7::method(alignpatches_add, S7::new_S3_class("plot_layout")) <-
    function(object, patches, objectname) {
        object$area <- object$design # pathwork use `design`
        object <- .subset(object, names(layout_design()))
        if (is.waive(object$guides)) {
            object$guides <- NA
        } else if (identical(object$guides, "auto")) {
            object$guides <- waiver()
        } else if (identical(object$guides, "collect")) {
            object$guides <- "tlbr"
        } else if (identical(object$guides, "keep")) {
            object["guides"] <- list(NULL)
        }
        alignpatches_add(
            add_class(object, "layout_design"),
            patches, objectname
        )
    }

##############################################################
#' Annotate the whole layout
#'
#' @inheritParams ggplot2::labs
#' @return A `layout_title` object.
#' @examples
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) +
#'     geom_bar(aes(gear)) +
#'     facet_wrap(~cyl)
#' align_plots(p1, p2, p3) +
#'     layout_title(title = "I'm title")
#' @importFrom ggplot2 waiver
#' @export
layout_title <- function(title = waiver(), subtitle = waiver(),
                         caption = waiver()) {
    if (!is.waive(title)) assert_string(title, allow_null = TRUE)
    if (!is.waive(subtitle)) assert_string(subtitle, allow_null = TRUE)
    if (!is.waive(caption)) assert_string(caption, allow_null = TRUE)
    structure(
        list(title = title, subtitle = subtitle, caption = caption),
        class = c("layout_title", "plot_annotation")
    )
}

S3_layout_title <- S7::new_S3_class("layout_title")

prop_layout_title <- function(property, ...) {
    force(property)
    S7::new_property(
        S7::class_list,
        setter = function(self, value) {
            prop(self, property) <- value
            self
        },
        ...,
        default = quote(list(title = NULL, subtitle = NULL, caption = NULL))
    )
}

layout_title_update <- function(old, new) {
    for (nm in names(new)) {
        if (is.waive(.subset2(new, nm))) next
        old[nm] <- list(.subset2(new, nm))
    }
    old
}

# Bypass S7 setter validation: update internal property via attr() directly
#' @importFrom S7 prop
S7::method(alignpatches_add, S3_layout_title) <-
    function(object, patches, objectname) {
        attr(patches, "titles") <- layout_title_update(
            prop(patches, "titles"), object
        )
        patches
    }

##############################################################
#' Modify theme of the layout
#'
#' @inherit ggplot2::theme
#' @param ... A [`theme()`][ggplot2::theme] object or additional element
#' specifications not part of base ggplot2. In general, these should also be
#' defined in the `element tree` argument. [`Splicing`][rlang::splice] a list
#' is also supported.
#'
#' @details
#' A [`theme()`][ggplot2::theme] object used to customize various elements of
#' the layout, including `guides`, `title`, `subtitle`, `caption`, `margins`,
#' `panel.border`, and `background`. By default, the theme will inherit from the
#' parent `layout`.
#'
#' - `guides`, `panel.border`, and `background` will always be used even for the
#' nested `alignpatches` object.
#'
#' - `title`, `subtitle`, `caption`, and `margins` will be added for the
#' top-level `alignpatches` object only.
#'
#' @examples
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) +
#'     geom_bar(aes(gear)) +
#'     facet_wrap(~cyl)
#' align_plots(
#'     p1 + theme(plot.background = element_blank()),
#'     p2 + theme(plot.background = element_blank()),
#'     p3 + theme(plot.background = element_blank())
#' ) +
#'     layout_theme(plot.background = element_rect(fill = "red"))
#' @importFrom ggplot2 theme
#' @include ggplot-theme.R
#' @export
layout_theme <- new_theme_class("layout_theme")

S3_layout_theme <- S7::new_S3_class("layout_theme")

layout_theme_update <- function(old, new) {
    if (is.null(old) || is.null(new)) return(new) # styler: off
    old + new
}

#' @importFrom S7 prop prop<-
#' @importFrom ggplot2 is_theme
prop_layout_theme <- function(property, ...) {
    force(property)
    S7::new_property(
        S7::class_any,
        validator = function(value) {
            if (!is.null(value) && !is_theme(value)) {
                return("must be a 'theme()' object'")
            }
        },
        setter = function(self, value) {
            prop(self, property) <- value
            self
        },
        ...,
        default = NULL
    )
}

# Bypass S7 setter validation: update internal property via attr() directly
#' @importFrom S7 prop
S7::method(alignpatches_add, S3_layout_theme) <-
    function(object, patches, objectname) {
        attr(patches, "theme") <- layout_theme_update(
            prop(patches, "theme"), object
        )
        patches
    }

#' @importFrom S7 prop
S7::method(alignpatches_add, S7::new_S3_class("plot_annotation")) <-
    function(object, patches, objectname) {
        attr(patches, "titles") <- layout_title_update(
            prop(patches, "titles"),
            .subset(object, names(layout_title()))
        )
        attr(patches, "theme") <- layout_theme_update(
            prop(patches, "theme"), .subset2(object, "theme")
        )
        patches
    }

#' Add layout annotation (internal use)
#'
#' This function is a placeholder for future extensions.
#' If you're trying to apply a theme, use [layout_theme()] instead.
#'
#' @param ... Currently unused. May accept a theme in the future.
#' @param theme A theme object. If not `waiver()`, an error will be raised.
#'
#' @return None. This function is used for input validation.
#' @importFrom ggplot2 is_theme
#' @export
#' @keywords internal
layout_annotation <- function(..., theme = waiver()) {
    if (is_theme(...elt(1L)) || !is.waive(theme)) {
        cli_abort("Please use {.fn layout_theme} instead; {.fn layout_annotation} is reserved for future extensions.")
    }
}

#' Arrange multiple plots into a grid
#'
#' @param ... <[dyn-dots][rlang::dyn-dots]> A list of plots, ususally the
#' ggplot object. Use `NULL` to indicate an empty spacer.
#' @param ncol,nrow The dimensions of the grid to create - if both are `NULL` it
#' will use the same logic as [`facet_wrap()`][ggplot2::facet_wrap] to set the
#' dimensions
#' @param byrow If `FALSE` the plots will be filled in in column-major order.
#' @param widths,heights The relative widths and heights of each column and row
#' in the grid. Will get repeated to match the dimensions of the grid. The
#' special value of `NA` will behave as `1null` unit unless a fixed aspect plot
#' is inserted in which case it will allow the dimension to expand or contract
#' to match the aspect ratio of the content.
#' @param area Specification of the location of areas in the layout. Can
#' either be specified as a text string or by concatenating calls to
#' [`area()`] together.
#' @param guides A string with one or more of `r oxford_and(c(.tlbr, "i"))`
#' indicating which side of guide legends should be collected. Defaults to
#' [`waiver()`][ggplot2::waiver()], which inherits from the parent layout. If
#' there is no parent layout, or if `NULL` is provided, no guides will be
#' collected.
#' @param theme A [`theme()`][ggplot2::theme] object used to customize various
#' elements of the layout. By default, the theme will inherit from the parent
#' `layout`.
#' @param design An alias for `area`, retained for backward compatibility.
#' @return An `AlignPatches` object.
#' @seealso
#'  - [layout_design()]
#'  - [layout_title()]
#'  - [layout_theme()]
#' @examples
#' # directly copied from patchwork
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) +
#'     geom_bar(aes(gear)) +
#'     facet_wrap(~cyl)
#' p4 <- ggplot(mtcars) +
#'     geom_bar(aes(carb))
#' p5 <- ggplot(mtcars) +
#'     geom_violin(aes(cyl, mpg, group = cyl))
#'
#' # Either add the plots as single arguments
#' align_plots(p1, p2, p3, p4, p5)
#'
#' # Or use bang-bang-bang to add a list
#' align_plots(!!!list(p1, p2, p3), p4, p5)
#'
#' # Match plots to areas by name
#' area <- "#BB
#'           AA#"
#' align_plots(B = p1, A = p2, area = area)
#'
#' # Compare to not using named plot arguments
#' align_plots(p1, p2, area = area)
#' @export
align_plots <- function(..., ncol = NULL, nrow = NULL, byrow = TRUE,
                        widths = NA, heights = NA, area = NULL,
                        guides = waiver(), theme = NULL, design = NULL) {
    plots <- rlang::dots_list(..., .ignore_empty = "all", .named = NULL)
    nms <- names(plots)
    area <- area %||% design
    if (!is.null(nms) && is.character(area)) {
        area_names <- unique(trimws(.subset2(strsplit(area, ""), 1L)))
        area_names <- sort(vec_set_difference(area_names, c("", "#")))
        if (all(nms %in% area_names)) {
            plot_list <- vector("list", length(area_names))
            names(plot_list) <- area_names
            plot_list[nms] <- plots
            plots <- plot_list
        }
    }

    for (plot in plots) {
        if (!has_s3_method(plot, "alignpatch", default = FALSE)) {
            cli_abort("Cannot align {.obj_type_friendly {plot}}")
        }
    }

    # setup layout parameters
    layout <- layout_design(
        ncol = ncol, nrow = nrow, byrow = byrow,
        widths = widths, heights = heights, area = area,
        guides = guides
    )
    out <- AlignPatches(plots = plots)
    out <- alignpatches_add(layout, out)
    objectname <- deparse(substitute(theme))
    if (!is.null(theme)) {
        out <- alignpatches_add(layout_theme(theme), out, objectname)
    }
    out
}

#' @importFrom S7 new_object S7_object prop prop<-
AlignPatches <- S7::new_class("AlignPatches",
    properties = list(
        plots = S7::new_property(
            S7::class_list,
            setter = function(self, value) {
                if (!is.null(prop(self, "plots"))) {
                    cli_abort("'@plots' is read-only")
                }
                prop(self, "plots", check = FALSE) <- value
                self
            }
        ),
        layout = S7::new_property(
            S7::class_list,
            setter = function(self, value) {
                if (!is.null(prop(self, "layout"))) {
                    cli_abort("{.field @layout} is read-only; use the '+' operator to update it.")
                }
                prop(self, "layout") <- value
                self
            },
            default = quote(list(
                ncol = NULL, nrow = NULL, byrow = TRUE,
                widths = NA, heights = NA, area = NULL,
                guides = waiver()
            ))
        ),
        titles = prop_layout_title("titles"),
        theme = prop_layout_theme("theme")
    )
)

local(S7::method(`$`, AlignPatches) <- function(x, name) prop(x, name))
local(S7::method(`[[`, AlignPatches) <- function(x, name) prop(x, name))

#' @importFrom rlang caller_env
local(
    S7::method(`+`, list(AlignPatches, S7::class_any)) <-
        function(e1, e2) {
            # Get the name of what was passed in as e2, and pass along so that
            # it can be displayed in error messages
            if (missing(e2)) {
                cli_abort(c(
                    "Cannot use {.code +} with a single argument.",
                    "i" = "Did you accidentally put {.code +} on a new line?"
                ))
            }
            e2name <- deparse(substitute(e2, env = caller_env(2L)))
            alignpatches_add(e2, e1, e2name)
        }
)
