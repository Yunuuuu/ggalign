#' Arrange multiple plots into a grid
#'
#' An internal S7 class that represents a collection of aligned plots
#' along with their layout configuration, titles, tags, and theme.
#'
#' @param ... <[dyn-dots][rlang::dyn-dots]> A list of plots, ususally the
#' ggplot object. Use `NULL` to indicate an empty spacer. Each input must
#' implement the [`patch()`] method.
#' @param ncol,nrow The number of columns and rows in the grid. Defaults to
#' `NULL`. If both are `NULL`, the layout dimensions are determined
#' automatically using the same logic as [`facet_wrap()`][ggplot2::facet_wrap].
#' @param byrow A logical value indicating whether plots should be filled in
#' row-major order (`TRUE`) or column-major order (`FALSE`). Defaults to
#' `TRUE`.
#' @param widths,heights The relative widths and heights of each column and row
#' in the grid. These values are recycled to match the grid dimensions.  The
#' special value `NA` is treated as a unit of `1null`, unless a fixed-aspect
#' plot is included — in that case, the affected dimension will expand or
#' contract to maintain the aspect ratio of the plot. Defaults to `NA`.
#' @param area A specification of the area layout. Can be defined either as a
#' character string or as a combination of calls to [`area()`]. Defaults to
#' `NULL`.
#' @param guides A string with one or more of `r oxford_and(c(.tlbr, "i"))`
#' indicating which side of guide legends should be collected. Defaults to
#' [`waiver()`][ggplot2::waiver()], which inherits from the parent layout. If
#' there is no parent layout, or if `NULL` is provided, no guides will be
#' collected.
#' @param theme A [`theme()`][ggplot2::theme] object used to customize various
#' elements of the layout. By default, the theme will inherit from the parent
#' `layout`.
#' @param design An alias for `area`, retained for backward compatibility.
#' @return An `alignpatches` object.
#' @seealso
#'  - [layout_design()]
#'  - [layout_title()]
#'  - [layout_theme()]
#'  - [layout_tags()]
#'
#' @section Properties:
#' - **plots**: A list of plot objects.
#' - **layout**: A list specifying layout options, including:
#'   - `ncol`, `nrow`, `byrow`: grid layout parameters.
#'   - `widths`, `heights`: relative dimensions of rows/columns.
#'   - `area`: custom area specification.
#'   - `guides`: guide handling.
#' - **titles**: A list specifying title options (`title`, `subtitle`,
#'   `caption`).
#' - **tags**: A list specifying tag options (`tags`, `sep`, `prefix`,
#'   `suffix`).
#' - **theme**: A theme configuration object.
#'
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
#'
#' @importFrom S7 new_object S7_object prop prop<-
#' @importFrom ggplot2 waiver theme
#' @include alignpatch-design.R
#' @include alignpatch-title.R
#' @include alignpatch-tags.R
#' @include alignpatch-theme.R
#' @export
alignpatches <- S7::new_class(
    "alignpatches",
    properties = list(
        plots = S7::class_list,
        layout = layout_design,
        titles = layout_title,
        tags = layout_tags,
        theme = ggplot2::class_theme
    ),
    constructor = function(..., ncol = NULL, nrow = NULL, byrow = TRUE,
                           widths = NA, heights = NA, area = NULL,
                           guides = waiver(), theme = NULL, design = NULL) {
        plots <- rlang::dots_list(..., .ignore_empty = "all", .named = NULL)
        nms <- names(plots)
        area <- area %||% design
        if (!is.null(nms) && is.character(area)) { # nocov start
            area_names <- unique(trimws(.subset2(strsplit(area, ""), 1L)))
            area_names <- sort(vec_set_difference(area_names, c("", "#")))
            if (all(nms %in% area_names)) {
                plot_list <- vector("list", length(area_names))
                names(plot_list) <- area_names
                plot_list[nms] <- plots
                plots <- plot_list
            }
        } # nocov end

        # setup layout parameters
        layout <- layout_design(
            ncol = ncol, nrow = nrow, byrow = byrow,
            widths = widths, heights = heights, area = area,
            guides = guides
        )
        new_object(
            S7_object(),
            plots = plots, layout = layout,
            titles = layout_title(), tags = layout_tags(),
            theme = theme %||% theme()
        )
    }
)

#' @export
#' @rdname alignpatches
align_plots <- alignpatches

#' @importFrom S7 prop
local(S7::method(`$`, alignpatches) <- function(x, i) prop(x, i))

#' @importFrom S7 prop
local(S7::method(`[[`, alignpatches) <- function(x, i) prop(x, i))

#' @importFrom S7 props
local(S7::method(`[`, alignpatches) <- function(x, i) props(x, names = i))

#' @importFrom ggplot2 ggproto
S7::method(patch, alignpatches) <- function(x) {
    ggproto(NULL, PatchAlignpatches, plot = x)
}

##############################################################
# For z in the gtable layout
# 0L: layout background
# 1L: background of the plot
# 2L: plot table
# 3L: foreground of the panel area
# 4L: legends
# 5L: tags
LAYOUT_BACKGROUND_Z <- 0L
PLOT_BACKGROUND_Z <- 1L
PLOT_TABLE_Z <- 2L
LAYOUT_FOREGROUND_Z <- 3L
GUIDE_LEGENDS_Z <- 4L
TAGS_Z <- 5L

#' Options passed to the Patch `gtable` method
#'
#' This class defines the options that can be passed to the `gtable` method of a
#' `Patch` object.  It includes:
#'
#' - `theme`: The theme to be applied, which can be either `NULL` or a ggplot2
#'   [theme][ggplot2::theme] object.
#' - `guides`: The guides for the plot, which can be `NULL` or a character
#'   vector.
#' - `tagger`: Either `NULL` (no tagging) or a `LayoutTagger` object that
#'   provides a `$tag_table` method (accepting the `gtable` and `theme`)
#'   used to add tag.
#'
#' @keywords internal
patch_options <- S7::new_class("patch_options",
    properties = list(
        theme = S7::new_union(NULL, ggplot2::class_theme),
        guides = S7::new_union(NULL, S7::class_character),
        tagger = S7::new_union(NULL, S7::new_S3_class("ggalign::LayoutTagger"))
    )
)

#' @importFrom ggplot2 ggproto
#' @noRd
PatchAlignpatches <- ggproto(
    "PatchAlignpatches", Patch,
    #' @importFrom gtable gtable gtable_add_grob
    #' @importFrom grid unit
    #' @importFrom ggplot2 wrap_dims calc_element zeroGrob theme_get
    #' @importFrom S7 prop prop<- set_props
    gtable = function(self, options) {
        patches <- lapply(prop(self$plot, "plots"), function(p) {
            out <- patch(p)
            if (!is.null(out) &&
                !inherits(out, "ggalign::Patch")) {
                cli_abort("{.fn alignpatch} must return a {.cls Patch} object")
            }
            out
        })
        layout <- ggalign_init(prop(self$plot, "layout"))

        # get the design areas and dims ------------------
        panel_widths <- prop(layout, "widths")
        panel_heights <- prop(layout, "heights")
        if (is.null(area <- prop(layout, "area"))) {
            if (is.null(layout@ncol) && length(panel_widths) > 1L) {
                layout@ncol <- length(panel_widths)
            }
            if (is.null(layout@nrow) && length(panel_heights) > 1L) {
                layout@nrow <- length(panel_heights)
            }
            dims <- wrap_dims(
                vec_size(patches),
                prop(layout, "nrow"),
                prop(layout, "ncol")
            )
            area <- create_area(dims[2L], dims[1L], prop(layout, "byrow"))
        } else {
            dims <- c(max(field(area, "b")), max(field(area, "r")))
        }

        # filter `plots` based on the design areas --------------------
        if (vec_size(area) < vec_size(patches)) {
            cli_warn("Too few patch areas to hold all plots. Dropping plots")
            plots <- vec_slice(patches, vec_seq_along(area))
        } else {
            area <- vec_slice(area, seq_along(patches))
        }

        # remove NULL patch -----------------------------------
        keep <- !vapply(patches, is.null, logical(1L), USE.NAMES = FALSE)
        patches <- vec_slice(patches, keep)

        # if no plots, we return empty gtable -----------------
        if (is_empty(patches)) return(make_patch_table()) # styler: off

        # add borders to patch --------------------------------
        area <- vec_slice(area, keep)
        borders_list <- vector("list", length(patches))
        for (i in seq_along(patches)) {
            borders_list[i] <- list(c(
                if (field(area, "t")[i] == 1L) "top" else NULL,
                if (field(area, "l")[i] == 1L) "left" else NULL,
                if (field(area, "b")[i] == .subset(dims, 1L)) {
                    "bottom"
                } else {
                    NULL
                },
                if (field(area, "r")[i] == .subset(dims, 2L)) {
                    "right"
                } else {
                    NULL
                }
            ))
        }

        # we define the global theme --------------------------
        # No parent theme provided
        if (is.null(theme <- prop(options, "theme"))) {
            top_level <- TRUE
            # by default, we use ggplot2 default theme
            theme <- prop(self$plot, "theme")
        } else {
            top_level <- FALSE
            theme <- theme + prop(self$plot, "theme")
        }
        theme <- complete_theme(theme)
        prop(options, "theme", check = FALSE) <- theme

        # by default, we won't collect any guide legends
        collected <- prop(options, "guides")
        guides <- prop(layout, "guides")
        if (is_string(guides)) {
            guides <- setup_guides(guides)
        } else if (is_waiver(guides)) {
            guides <- collected
        }

        #######################################################
        # 1. gtable: create the gtable for the patch, will set internal
        #    `gt`
        # 2. `collect_guides`, can change the internal `gt`
        # 3. set_sizes:
        #     - (To-Do) align_panel_spaces: can change the internal `gt`
        #     - align_panel, can change the internal `gt`
        #     - border_sizes, the widths and heights for the internal `gt`
        # 4. set_grobs: will call `align_border` and `place`, return the
        #    final gtable
        # setup gtable list ----------------------------------
        # A tagger can be:
        # - A single string representing the tag for the entire layout,
        # - NULL, meaning no tagging,
        # - Or a `LayoutTagger` object used to tag each plot individually.
        tag <- create_layout_tagger(
            prop(self$plot, "tags"),
            prop(options, "tagger")
        )
        if (is.null(tag) || inherits(tag, "ggalign::LayoutTagger")) {
            prop(options, "tagger", check = FALSE) <- tag
            tag <- NULL
        } else {
            # If tag is a single string, treat it as a single tag for the
            # whole layout
            prop(options, "tagger", check = FALSE) <- NULL
        }

        # Let each patch to determine whether to collect guides
        guides <- lapply(patches, function(patch) patch$guides(guides))

        # Always ensure that plots placed in a border collect their guides, if
        # any guides are to be collected in that border.
        border_with_guides <- unique(unlist(guides, FALSE, FALSE))

        # This prevents overlap, unless the guides will be collected by the
        # parent layout.
        border_with_guides <- setdiff(border_with_guides, collected)

        gt_list <- guides_list <- vector("list", length(patches))
        for (i in seq_along(patches)) {
            patch <- .subset2(patches, i)
            # we always collect guides in the borders, otherwise, they'll
            # overlap
            g <- union(
                .subset2(guides, i),
                intersect(border_with_guides, .subset2(borders_list, i))
            )
            gt <- patch$gtable(set_props(options, guides = g))
            components <- patch$decompose_guides(gt, g)
            guides_list[i] <- list(.subset2(components, "guides"))
            gt_list[i] <- list(.subset2(components, "gt"))
        }

        # prepare the output ----------------------------------
        # For z in the gtable layout
        # 0L: layout background
        # 1L: background of the plot
        # 2L: plot table
        # 3L: foreground of the panel area
        # 4L: legends
        # 5L: tags
        gt <- gtable(
            unit(rep(0L, TABLE_COLS * dims[2L]), "null"),
            unit(rep(0L, TABLE_ROWS * dims[1L]), "null")
        )

        # setup sizes for each row/column -----------------------
        gt <- self$set_sizes(
            patches, gt_list, area, dims,
            panel_widths, panel_heights,
            gt = gt
        )

        # add the panel position --------------------------------
        panel_pos <- list(
            t = TOP_BORDER + 1L,
            l = LEFT_BORDER + 1L,
            b = TABLE_ROWS * dims[1L] - BOTTOM_BORDER,
            r = TABLE_COLS * dims[2L] - RIGHT_BORDER
        )

        # add guides into the final gtable ----------------------
        # Guide legends must be attached before calling `$set_grobs()`, because
        # `$attach_guide_list()` adjusts the gtable sizes based on the guides.
        # The subsequent `$set_grobs()` call relies on these adjusted sizes.
        guides_list <- gather_guides(guides_list)
        # Separate guide legends between those to be collected by the parent
        # `alignpatches()` and those that remain attached to this subplot.
        if (is.null(collected)) {
            self$collected_guides <- list()
        } else {
            # Store guides to be collected by the parent `alignpatches()`
            self$collected_guides <- .subset(guides_list, collected)
            guides_list <- .subset(
                guides_list,
                setdiff(names(guides_list), collected)
            )
        }
        gt <- self$attach_guide_list(
            gt = gt,
            guide_list = guides_list,
            panel_pos = panel_pos,
            theme = theme
        )

        # add panel area ---------------------------------------
        gt <- gtable_add_grob(
            gt, list(zeroGrob()),
            t = .subset2(panel_pos, "t"),
            l = .subset2(panel_pos, "l"),
            b = .subset2(panel_pos, "b"),
            r = .subset2(panel_pos, "r"),
            z = 0L,
            name = "panel-area"
        )
        gt <- gtable_add_grob(
            gt,
            # foreground
            list(element_render(theme, "panel.border", fill = NA)),
            t = .subset2(panel_pos, "t"),
            l = .subset2(panel_pos, "l"),
            b = .subset2(panel_pos, "b"),
            r = .subset2(panel_pos, "r"),
            z = LAYOUT_FOREGROUND_Z,
            name = "panel-foreground"
        )
        gt <- table_add_tag(gt, tag, theme)

        # we only make the final grobs after sizes has been solved
        if (top_level) {
            # we'll add background in ggalign_gtable() method
            self$set_grobs(
                patches = patches,
                gt_list = gt_list,
                area = area,
                gt = gt
            )
        } else {
            # add background -----------------------------------
            if (is_theme_element(theme$plot.background)) {
                gt <- gtable_add_grob(gt,
                    element_render(theme, "plot.background"),
                    t = 1L, l = 1L, b = -1L, r = -1L,
                    name = "background", z = LAYOUT_BACKGROUND_Z
                )
            }
            self$patches <- patches
            self$gt_list <- gt_list
            self$borders_list <- borders_list
            self$area <- area
            gt
        }
    },
    decompose_guides = function(self, gt, guides) {
        on.exit(self$collected_guides <- NULL)
        list(gt = gt, guides = self$collected_guides)
    },
    align_border = function(self, gt, t = NULL, l = NULL, b = NULL, r = NULL) {
        on.exit(self$area <- NULL)
        # we only make the final grobs after sizes has been solved
        gt <- ggproto_parent(Patch, self)$align_border(gt, t, l, b, r)
        self$set_grobs(
            patches = self$patches,
            gt_list = self$gt_list,
            area = self$area,
            gt = gt
        )
    },

    #' @importFrom grid is.unit unit
    set_sizes = function(self, patches, gt_list, area, dims,
                         panel_widths, panel_heights, gt) {
        panel_widths <- rep(panel_widths, length.out = dims[2L])
        panel_heights <- rep(panel_heights, length.out = dims[1L])
        if (!is.unit(panel_widths)) panel_widths <- unit(panel_widths, "null")
        if (!is.unit(panel_heights)) {
            panel_heights <- unit(panel_heights, "null")
        }
        cols <- field(area, "l")
        rows <- field(area, "t")

        # For gtable with fixed panel sizes ------------------
        # we must ensure all plot panels fill well, which means we should use
        # the largest panel sizes. This matters when the panel sizes are all
        # absolute unit sizes.
        for (i in seq_along(gt_list)) {
            gt_cur <- .subset2(gt_list, i)
            if (!is.gtable(gt_cur)) next
            panel_pos <- find_panel(gt_cur)
            if (nrow(panel_pos) == 0L) next

            col <- .subset(cols, i)
            panel_width <- panel_widths[col]
            can_set_width <- is.na(as.numeric(panel_width))
            if (can_set_width || is_absolute_unit(panel_width)) {
                gt_panel_widths <- .subset2(gt_cur, "widths")[
                    .subset2(panel_pos, "l"):.subset2(panel_pos, "r")
                ]
                if (all(is_absolute_unit(gt_panel_widths))) {
                    if (can_set_width) {
                        panel_width <- sum(gt_panel_widths)
                    } else {
                        panel_width <- max(panel_width, sum(gt_panel_widths))
                    }
                    panel_widths[col] <- convertWidth(panel_width, "mm")
                }
            }

            row <- .subset(rows, i)
            panel_height <- panel_heights[row]
            can_set_height <- is.na(as.numeric(panel_height))
            if (can_set_height || is_absolute_unit(panel_height)) {
                gt_panel_heights <- .subset2(gt_cur, "heights")[
                    .subset2(panel_pos, "t"):.subset2(panel_pos, "b")
                ]
                if (all(is_absolute_unit(gt_panel_heights))) {
                    if (can_set_height) {
                        panel_height <- sum(gt_panel_heights)
                    } else {
                        panel_height <- max(panel_height, sum(gt_panel_heights))
                    }
                    panel_heights[row] <- convertHeight(panel_height, "mm")
                }
            }
        }

        # For gtable with fixed aspect ratio ------------------
        need_respect <- field(area, "l") == field(area, "r") &
            field(area, "t") == field(area, "b") &
            vapply(gt_list, is_respect, logical(1L), USE.NAMES = FALSE)

        # here we respect the aspect ratio when necessary -----
        # if the width or height is NA, we will guess the panel widths or
        # heights based on the fixed aspect ratio
        guess_widths <- which(is.na(as.numeric(panel_widths)))
        guess_heights <- which(is.na(as.numeric(panel_heights)))
        patch_index <- order(
            # we first set the widths for the fixed plot with heights set by
            # user
            cols %in% guess_widths & !rows %in% guess_heights,
            # we then set the heights for the fixed plot with widths set by user
            !cols %in% guess_widths & rows %in% guess_heights,
            # we set widths and heights for remaning plots
            # based on the number of plots in each row/column in the descending
            # order
            c(table(rows[need_respect]))[as.character(rows)],
            c(table(cols[need_respect]))[as.character(cols)],
            decreasing = TRUE
        )
        sizes_list <- respect_dims <- vector("list", length(patches))
        # guess_widths <- rep_len(FALSE, dims[2L])
        # guess_heights <- rep_len(FALSE, dims[1L])
        for (i in patch_index) {
            row <- .subset(rows, i)
            col <- .subset(cols, i)
            patch <- .subset2(patches, i)
            panel_aligned <- patch$align_panel(
                gt = .subset2(gt_list, i),
                panel_width = panel_widths[col],
                panel_height = panel_heights[row]
                # guess_width = guess_widths[col],
                # guess_height = guess_heights[row]
            )
            panel_widths[col] <- .subset2(panel_aligned, "width")
            panel_heights[row] <- .subset2(panel_aligned, "height")
            if (isTRUE(.subset2(panel_aligned, "respect"))) {
                respect_dims[[i]] <- matrix(
                    c(
                        (row - 1L) * TABLE_ROWS + TOP_BORDER + 1L,
                        (col - 1L) * TABLE_COLS + LEFT_BORDER + 1L
                    ),
                    nrow = 1L
                )
            }
            # guess_widths[col] <- isTRUE(
            #     .subset2(panel_aligned, "guess_width")
            # )
            # guess_heights[row] <- isTRUE(
            #     .subset2(panel_aligned, "guess_height")
            # )
            sizes_list[i] <- list(patch$border_sizes(.subset2(gt_list, i)))
        }
        if (!is.null(respect_dims <- inject(rbind(!!!respect_dims)))) {
            respect <- matrix(
                0L, TABLE_ROWS * dims[1L],
                TABLE_COLS * dims[2L]
            )
            respect[respect_dims] <- 1L
            gt$respect <- respect
        }

        # we set the widths/heights with no fixed plots to be 1 null
        if (any(guess_widths <- is.na(as.numeric(panel_widths)))) {
            panel_widths[guess_widths] <- unit(1L, "null")
        }
        if (any(guess_heights <- is.na(as.numeric(panel_heights)))) {
            panel_heights[guess_heights] <- unit(1L, "null")
        }

        # setup sizes for non-panel rows/columns --------------
        sizes <- table_sizes(
            sizes_list, panel_widths, panel_heights,
            area, dims[2L], dims[1L]
        )
        # setup the widths and heights ------------------------
        gt$widths <- .subset2(sizes, "widths")
        gt$heights <- .subset2(sizes, "heights")
        gt
    },

    #' @importFrom gtable gtable_add_grob
    set_grobs = function(self, patches, gt_list, area, gt) {
        widths <- .subset2(gt, "widths")
        heights <- .subset2(gt, "heights")
        for (i in seq_along(patches)) {
            loc <- vec_slice(area, i)
            # We must align the borders for the gtable grob with the
            # final plot area sizes
            l <- (field(loc, "l") - 1L) * TABLE_COLS + 1L
            l_widths <- widths[seq(l, l + LEFT_BORDER - 1L)]
            r <- field(loc, "r") * TABLE_COLS
            r_widths <- widths[seq(r - RIGHT_BORDER + 1L, r)]
            t <- (field(loc, "t") - 1L) * TABLE_ROWS + 1L
            t_heights <- heights[seq(t, t + TOP_BORDER - 1L)]
            b <- field(loc, "b") * TABLE_ROWS
            b_heights <- heights[seq(b - BOTTOM_BORDER + 1L, b)]
            patch <- .subset2(patches, i)
            gt <- patch$place(
                gt,
                patch$align_border(
                    .subset2(gt_list, i),
                    t_heights, l_widths,
                    b_heights, r_widths
                ),
                t, l, b, r, i, PLOT_BACKGROUND_Z, PLOT_TABLE_Z # bg_z, plot_z
            )
        }
        # arrange the grobs
        idx <- order(.subset2(.subset2(gt, "layout"), "z"))
        gt$layout <- vec_slice(.subset2(gt, "layout"), idx)
        gt$grobs <- .subset(.subset2(gt, "grobs"), idx)
        gt
    },
    attach_guide_list = function(self, gt, guide_list, panel_pos, theme) {
        if (length(guide_list)) {
            # https://github.com/tidyverse/ggplot2/blob/57ba97fa04dadc6fd73db1904e39a09d57a4fcbe/R/guides-.R#L512
            theme$legend.spacing <- theme$legend.spacing %||% unit(0.5, "lines")
            theme$legend.spacing.y <- calc_element("legend.spacing.y", theme)
            theme$legend.spacing.x <- calc_element("legend.spacing.x", theme)
            theme$legend.box.spacing <- calc_element(
                "legend.box.spacing", theme
            ) %||% unit(0.2, "cm")
            for (guide_pos in names(guide_list)) {
                gt <- self$attach_guides(
                    guide_pos = guide_pos,
                    guides = .subset2(guide_list, guide_pos),
                    theme = theme, panel_pos = panel_pos,
                    clip = "off", z = GUIDE_LEGENDS_Z,
                    name = sprintf("guide-box-collected-%s", guide_pos),
                    gt = gt
                )
            }
        }
        gt
    },
    #' @importFrom gtable gtable_width gtable_height
    #' @importFrom grid unit.c grobWidth grobHeight
    #' @importFrom ggplot2 find_panel zeroGrob
    attach_guides = function(self, guide_pos, guides, theme,
                             panel_pos, ..., gt) {
        guide_box <- assemble_guides(guides, guide_pos, theme = theme)
        if (guide_pos == "inside") {
            gt <- gtable_add_grob(
                x = gt,
                grobs = guide_box,
                t = panel_pos$t,
                l = panel_pos$l,
                b = panel_pos$b,
                r = panel_pos$r,
                ...
            )
            return(gt)
        }
        spacing <- theme$legend.box.spacing
        if (guide_pos == "left") {
            if (is.gtable(guide_box)) {
                legend_width <- gtable_width(guide_box)
                widths <- unit.c(spacing, legend_width)
            } else {
                legend_width <- grobWidth(guide_box)
                widths <- unit(c(0, 0), "mm")
            }
            gt <- gtable_add_grob(
                x = gt,
                grobs = guide_box,
                t = panel_pos$t,
                l = panel_pos$l - 6L,
                b = panel_pos$b,
                ...
            )
            gt$widths[.subset2(panel_pos, "l") - 5:6] <- widths
        } else if (guide_pos == "right") {
            if (is.gtable(guide_box)) {
                legend_width <- gtable_width(guide_box)
                widths <- unit.c(spacing, legend_width)
            } else {
                legend_width <- grobWidth(guide_box)
                widths <- unit(c(0, 0), "mm")
            }
            gt <- gtable_add_grob(
                x = gt,
                grobs = guide_box,
                t = panel_pos$t,
                l = panel_pos$r + 6L,
                b = panel_pos$b,
                ...
            )
            gt$widths[.subset2(panel_pos, "r") + 5:6] <- widths
        } else if (guide_pos == "bottom") {
            location <- theme$legend.location %||% "panel"
            place <- switch(location,
                panel = panel_pos,
                list(l = 1L, r = ncol(gt))
            )
            if (is.gtable(guide_box)) {
                legend_height <- gtable_height(guide_box)
                heights <- unit.c(spacing, legend_height)
            } else {
                legend_height <- grobHeight(guide_box)
                heights <- unit(c(0, 0), "mm")
            }
            gt <- gtable_add_grob(
                x = gt,
                grobs = guide_box,
                t = panel_pos$b + 6L,
                l = place$l,
                r = place$r,
                ...
            )
            gt$heights[.subset2(panel_pos, "b") + 5:6] <- heights
        } else if (guide_pos == "top") {
            location <- theme$legend.location %||% "panel"
            place <- switch(location,
                panel = panel_pos,
                list(l = 1L, r = ncol(gt))
            )
            if (is.gtable(guide_box)) {
                legend_height <- gtable_height(guide_box)
                heights <- unit.c(spacing, legend_height)
            } else {
                legend_height <- grobHeight(guide_box)
                heights <- unit(c(0, 0), "mm")
            }
            gt <- gtable_add_grob(
                x = gt,
                grobs = guide_box,
                t = panel_pos$t - 6L,
                l = place$l,
                r = place$r,
                ...
            )
            gt$heights[.subset2(panel_pos, "t") - 5:6] <- heights
        }
        gt
    }
)

#' @importFrom grid convertHeight convertWidth unit
table_sizes <- function(sizes_list, panel_widths, panel_heights,
                        area, ncol, nrow) {
    # `null` unit of the panel area will be converted into 0
    # we'll set the panel width and height afterward
    widths <- lapply(sizes_list, function(sizes) {
        if (is.null(sizes)) {
            left <- right <- NULL
        } else {
            left <- .subset2(sizes, "left")
            right <- .subset2(sizes, "right")
        }
        if (is.null(left)) {
            l <- rep_len(0, LEFT_BORDER)
        } else {
            l <- convertWidth(left, "mm", valueOnly = TRUE)
        }
        if (is.null(right)) {
            r <- rep_len(0, RIGHT_BORDER)
        } else {
            r <- convertWidth(right, "mm", valueOnly = TRUE)
        }
        c(l, 0, r)
    })
    widths <- vapply(seq_len(ncol * TABLE_COLS), function(i) {
        area_col <- locate_recycle_which_block(i, TABLE_COLS)
        col_loc <- locate_recycle_which_position(i, TABLE_COLS)
        if (col_loc == LEFT_BORDER + 1L) {
            return(0)
        }
        area_side <- if (col_loc <= LEFT_BORDER) "l" else "r"
        idx <- field(area, area_side) == area_col
        if (any(idx)) {
            max(
                vapply(.subset(widths, idx), function(width) {
                    .subset(width, col_loc)
                }, numeric(1L), USE.NAMES = FALSE),
                0L
            )
        } else {
            0L
        }
    }, numeric(1L), USE.NAMES = FALSE)
    heights <- lapply(sizes_list, function(sizes) {
        if (is.null(sizes)) {
            top <- bottom <- NULL
        } else {
            top <- .subset2(sizes, "top")
            bottom <- .subset2(sizes, "bottom")
        }
        if (is.null(top)) {
            t <- rep_len(0, TOP_BORDER)
        } else {
            t <- convertHeight(top, "mm", valueOnly = TRUE)
        }
        if (is.null(bottom)) {
            b <- rep_len(0, BOTTOM_BORDER)
        } else {
            b <- convertHeight(bottom, "mm", valueOnly = TRUE)
        }
        c(t, 0, b)
    })
    heights <- vapply(seq_len(nrow * TABLE_ROWS), function(i) {
        area_row <- locate_recycle_which_block(i, TABLE_ROWS)
        row_loc <- locate_recycle_which_position(i, TABLE_ROWS)
        if (row_loc == TOP_BORDER + 1L) {
            return(0)
        }
        area_side <- if (row_loc <= TOP_BORDER) "t" else "b"
        idx <- field(area, area_side) == area_row
        if (any(idx)) {
            max(
                vapply(
                    .subset(heights, idx), .subset, numeric(1L),
                    row_loc,
                    USE.NAMES = FALSE
                ),
                0L
            )
        } else {
            0L
        }
    }, numeric(1L), USE.NAMES = FALSE)

    # restore the panel sizes ----------------------------
    widths <- unit(widths, "mm")
    heights <- unit(heights, "mm")
    width_ind <- seq(LEFT_BORDER + 1L, by = TABLE_COLS, length.out = ncol)
    height_ind <- seq(TOP_BORDER + 1L, by = TABLE_ROWS, length.out = nrow)
    widths[width_ind] <- panel_widths
    heights[height_ind] <- panel_heights
    list(widths = widths, heights = heights)
}
