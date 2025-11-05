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

#' @importFrom ggplot2 ggproto
#' @noRd
PatchAlignpatches <- ggproto(
    "PatchAlignpatches", Patch,

    # @field alignpatches
    #
    # A list containing metadata used to align the plot.
    alignpatches = NULL,
    setup = function(self, options = NULL) {
        patches <- lapply(prop(self$plot, "plots"), function(p) {
            out <- patch(p)
            # Patch defines the interface
            if (!is.null(out) && !inherits(out, "ggalign::Patch")) {
                cli_abort("{.fn alignpatch} must return a {.cls Patch} object")
            }
            out
        })

        # get the design areas and dims ------------------
        layout <- ggalign_init(prop(self$plot, "layout"))
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
        area <- vec_slice(area, keep)

        #######################################################
        # prepare the final options ---------------------------
        options <- options %||% patch_options()
        # we define the guides --------------------------------
        collected <- prop(options, "guides")
        guides <- prop(layout, "guides")
        if (is_string(guides)) {
            guides <- setup_guides(guides)
        } else if (is_waiver(guides)) {
            guides <- collected
        }
        prop(options, "guides", check = FALSE) <- guides

        # we define the global theme -------------------------
        # No parent theme provided
        if (is.null(theme <- prop(options, "theme"))) {
            # by default, we use ggplot2 default theme
            theme <- prop(self$plot, "theme")
            top_level <- TRUE
        } else {
            theme <- theme + prop(self$plot, "theme")
            top_level <- FALSE
        }
        prop(options, "theme", check = FALSE) <- complete_theme(theme)

        # A tag can be:
        # - A single string representing the tag for the entire layout,
        # - NULL, meaning no tagging,
        # - Or a `LayoutTagger` object used to tag each plot individually.
        prop(options, "tag", check = FALSE) <- create_layout_tagger(
            prop(self$plot, "tags"),
            prop(options, "tag")
        )
        self$options <- options

        if (is_empty(patches)) {
            return(NULL)
        }

        #######################################################
        # define the options for the sub-plots ----------------
        # the `alignpatches` tag is a string, it means regarding the
        # alignpatches as a single plot, instead of tag each sub-plots, so we
        # remove the tag
        if (is_string(prop(options, "tag"))) {
            prop(options, "tag") <- NULL
        }

        borders_list <- vector("list", length(patches))
        for (i in seq_along(patches)) {
            patch <- .subset2(patches, i)
            # Let each patch to determine the options and data field
            patch$setup(options)

            # collect borders for each patch
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

        #######################################################
        # setup gtable list ----------------------------------
        # Always ensure that plots placed in a border collect their guides, if
        # any guides are to be collected in that border.
        # This prevents overlap, unless the guides will be collected by the
        # parent layout.
        border_with_guides <- unique(unlist(
            lapply(patches, function(patch) prop(patch$options, "guides")),
            FALSE, FALSE
        ))
        border_with_guides <- setdiff(border_with_guides, collected)
        gt_list <- guides_list <- vector("list", length(patches))
        for (i in seq_along(patches)) {
            patch <- .subset2(patches, i)
            # we always collect guides in the borders, otherwise, they'll
            # overlap
            g <- union(
                patch$get_option("guides"),
                intersect(border_with_guides, .subset2(borders_list, i))
            )
            patch$set_option("guides", g, check = FALSE)

            components <- patch$decompose_guides(patch$gtable())
            patch_gt <- .subset2(components, "gt")
            # If the plot uses a tag (a string), set it here: `$tag()`
            # modifies the gtable's size, so it must be executed before
            # `$set_sizes()`
            if (is_string(tag <- patch$get_option("tag"))) {
                theme <- patch$get_option("theme")
                tag_placement <- calc_element("plot.tag.placement", theme) %||%
                    "plot"
                tag_placement <- arg_match0(
                    tag_placement,
                    c("plot", "canvas"),
                    arg_nm = "plot.tag.placement",
                    error_call = quote(theme())
                )
                if (tag_placement == "plot") {
                    patch_gt <- patch$tag(
                        patch_gt, tag,
                        1, 1, nrow(patch_gt), ncol(patch_gt), Inf
                    )
                }
            }
            gt_list[i] <- list(patch_gt)
            guides_list[i] <- list(.subset2(components, "guides"))
        }

        # Separate guide legends between those to be collected by the parent
        # `alignpatches()` and those that remain attached to this subplot.
        guides_list <- gather_guides(guides_list)
        if (is.null(collected)) {
            collected_guides <- list()
            guides_list <- guides_list
        } else {
            # Store guides to be collected by the parent `alignpatches()`
            collected_guides <- .subset(guides_list, collected)
            guides_list <- .subset(
                guides_list,
                setdiff(names(guides_list), collected)
            )
        }

        # For gtable with fixed panel sizes we can directly set the panel sizes
        # we make sure all plot panels fill well, which means we should use the
        # largest panel sizes. This matters when the panel sizes are all
        # absolute unit sizes.
        panel_widths <- rep(panel_widths, length.out = dims[2L])
        panel_heights <- rep(panel_heights, length.out = dims[1L])
        if (!is.unit(panel_widths)) panel_widths <- unit(panel_widths, "null")
        if (!is.unit(panel_heights)) {
            panel_heights <- unit(panel_heights, "null")
        }
        cols <- field(area, "l")
        rows <- field(area, "t")

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

        self$alignpatches <- list(
            top_level = top_level,
            patches = patches,
            gt_list = gt_list,
            collected_guides = collected_guides,
            guides_list = guides_list,
            borders_list = borders_list,
            area = area,
            dims = dims,
            panel_widths = panel_widths,
            panel_heights = panel_heights
        )
    },

    #' @importFrom gtable gtable gtable_add_grob
    #' @importFrom grid unit
    #' @importFrom ggplot2 wrap_dims calc_element zeroGrob theme_get
    #' @importFrom S7 prop prop<-
    #' @importFrom rlang arg_match0 is_empty
    gtable = function(self) {
        if (is.null(theme <- self$get_option("theme"))) {
            cli_abort("Run `$setup()` to initialize the patches first.")
        }

        # if no plots, we do nothing --------------------------
        metadata <- self$alignpatches
        if (is_empty(.subset2(metadata, "patches"))) {
            return(make_patch_table())
        }

        # prepare the output ---------------------------------
        gt <- gtable(
            unit(rep(0L, TABLE_COLS * metadata$dims[2L]), "null"),
            unit(rep(0L, TABLE_ROWS * metadata$dims[1L]), "null")
        )

        # setup gtable list ----------------------------------
        for (i in seq_along(.subset2(metadata, "patches"))) {
            # If the plot uses a tag (a string), set it here: `$tag()`
            # modifies the gtable's size, so it must be executed before
            # `$set_sizes()`
            patch <- .subset2(.subset2(metadata, "patches"), i)
            if (is_string(tag <- prop(patch$options, "tag"))) {
                patch_theme <- prop(patch$options, "theme")
                tag_placement <- calc_element(
                    "plot.tag.placement", patch_theme
                ) %||% "plot"
                if (tag_placement == "canvas") {
                    loc <- vec_slice(.subset2(metadata, "area"), i)
                    l <- (field(loc, "l") - 1L) * TABLE_COLS + 1L
                    r <- field(loc, "r") * TABLE_COLS
                    t <- (field(loc, "t") - 1L) * TABLE_ROWS + 1L
                    b <- field(loc, "b") * TABLE_ROWS
                    gt <- patch$tag(gt, tag, t, l, b, r, TAGS_Z)
                }
            }
        }

        # setup sizes for each row/column -----------------------
        gt <- self$set_sizes(
            .subset2(metadata, "patches"),
            .subset2(metadata, "gt_list"),
            .subset2(metadata, "area"),
            .subset2(metadata, "dims"),
            .subset2(metadata, "panel_widths"),
            .subset2(metadata, "panel_heights"),
            gt = gt
        )

        # add the panel position --------------------------------
        panel_pos <- list(
            t = TOP_BORDER + 1L,
            l = LEFT_BORDER + 1L,
            b = TABLE_ROWS * metadata$dims[1L] - BOTTOM_BORDER,
            r = TABLE_COLS * metadata$dims[2L] - RIGHT_BORDER
        )
        gt <- self$attach_guide_list(
            gt = gt,
            guide_list = metadata$guides_list,
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

        # we only make the final grobs after sizes has been solved
        if (.subset2(metadata, "top_level")) {
            # we'll add background in ggalign_gtable() method
            self$set_grobs(
                .subset2(metadata, "patches"),
                .subset2(metadata, "gt_list"),
                .subset2(metadata, "area"),
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
            gt
        }
    },
    decompose_guides = function(self, gt) {
        list(gt = gt, guides = .subset2(self$alignpatches, "collected_guides"))
    },
    align_border = function(self, gt, t, l, b, r) {
        # we only make the final grobs after sizes has been solved
        gt <- ggproto_parent(Patch, self)$align_border(gt, t, l, b, r)
        metadata <- self$alignpatches
        self$set_grobs(
            .subset2(metadata, "patches"),
            .subset2(metadata, "gt_list"),
            .subset2(metadata, "area"),
            gt = gt
        )
    },

    #' @importFrom grid is.unit unit
    set_sizes = function(self, patches, gt_list, area, dims,
                         panel_widths, panel_heights, gt) {
        cols <- field(area, "l")
        rows <- field(area, "t")

        # For gtable with fixed aspect ratio ------------------
        need_respect <- cols == field(area, "r") &
            rows == field(area, "b") &
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
            widths = .subset2(gt, "widths"),
            heights = .subset2(gt, "heights"),
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
                t = .subset2(panel_pos, "t"),
                l = .subset2(panel_pos, "l"),
                b = .subset2(panel_pos, "b"),
                r = .subset2(panel_pos, "r"),
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
                t = .subset2(panel_pos, "t"),
                l = .subset2(panel_pos, "l") - 6L,
                b = .subset2(panel_pos, "b"),
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
                t = .subset2(panel_pos, "t"),
                l = .subset2(panel_pos, "r") + 6L,
                b = .subset2(panel_pos, "b"),
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
                t = .subset2(panel_pos, "b") + 6L,
                l = .subset2(place, "l"),
                r = .subset2(place, "r"),
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
                t = .subset2(panel_pos, "t") - 6L,
                l = .subset2(place, "l"),
                r = .subset2(place, "r"),
                ...
            )
            gt$heights[.subset2(panel_pos, "t") - 5:6] <- heights
        }
        gt
    }
)

#' @importFrom grid convertHeight convertWidth unit
table_sizes <- function(widths, heights, sizes_list,
                        panel_widths, panel_heights,
                        area, ncol, nrow) {
    # `null` unit of the panel area will be converted into 0
    # we'll set the panel width and height afterward
    widths <- convertWidth(widths, "mm", valueOnly = TRUE)
    subplots_widths <- lapply(sizes_list, function(sizes) {
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
    subplots_widths <- vapply(seq_len(ncol * TABLE_COLS), function(i) {
        area_col <- locate_recycle_which_block(i, TABLE_COLS)
        col_loc <- locate_recycle_which_position(i, TABLE_COLS)
        if (col_loc == LEFT_BORDER + 1L) {
            return(0)
        }
        area_side <- if (col_loc <= LEFT_BORDER) "l" else "r"
        idx <- field(area, area_side) == area_col
        if (any(idx)) {
            max(
                vapply(.subset(subplots_widths, idx), function(width) {
                    .subset(width, col_loc)
                }, numeric(1L), USE.NAMES = FALSE),
                0L
            )
        } else {
            0L
        }
    }, numeric(1L), USE.NAMES = FALSE)
    widths <- pmax(widths, subplots_widths)

    heights <- convertHeight(heights, "mm", valueOnly = TRUE)
    subplots_heights <- lapply(sizes_list, function(sizes) {
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
    subplots_heights <- vapply(seq_len(nrow * TABLE_ROWS), function(i) {
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
                    .subset(subplots_heights, idx), .subset, numeric(1L),
                    row_loc,
                    USE.NAMES = FALSE
                ),
                0L
            )
        } else {
            0L
        }
    }, numeric(1L), USE.NAMES = FALSE)
    heights <- pmax(heights, subplots_heights)

    # restore the panel sizes ----------------------------
    widths <- unit(widths, "mm")
    heights <- unit(heights, "mm")
    width_ind <- seq(LEFT_BORDER + 1L, by = TABLE_COLS, length.out = ncol)
    height_ind <- seq(TOP_BORDER + 1L, by = TABLE_ROWS, length.out = nrow)
    widths[width_ind] <- panel_widths
    heights[height_ind] <- panel_heights
    list(widths = widths, heights = heights)
}
