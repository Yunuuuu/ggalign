#' Plot dendrogram tree
#'
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' [`geom_segment()`][ggplot2::geom_segment].
#' @param plot_dendrogram A boolean value indicates whether plot the dendrogram
#' tree.
#' @param plot_cut_height A boolean value indicates whether plot the cut height.
#' @section ggplot2 specification:
#' `align_dendro` initializes a ggplot `data` and `mapping`.
#'
#' The internal `ggplot` object will always use a default mapping of
#' `aes(x = .data$x, y = .data$y)`.
#'
#' The default ggplot data is the `node` coordinates with `edge` data attached
#' in [`ggalign`][ggalign_attr()] attribute, in addition, a
#' [`geom_segment`][ggplot2::geom_segment] layer with a data frame of the `edge`
#' coordinates will be added when `plot_dendrogram = TRUE`.
#'
#' dendrogram `node` and `edge` contains following columns:
#'
#'   - `.panel`: Tree branch groups, used to create ggplot2 facet. Similar with
#'              `panel` column, but always give the correct branch for usage of
#'              the ggplot facet.
#'  - `.names` and `.index`: a character names (only applicable when names
#'    exists) and an integer index of the original data.
#'   - `label`: node label text
#'   - `x` and `y`: x-axis and y-axis coordinates for current node or the start
#'                  node of the current edge.
#'   - `xend` and `yend`: the x-axis and y-axis coordinates of the terminal node
#'                        for current edge.
#'   - `branch`: which branch current node or edge is. You can use this column
#'               to color different groups.
#'   - `leaf`: A logical value indicates whether current node is a leaf.
#'   - `panel`: which panel current node is, if we split the plot into panel
#'              using [`facet_grid`][ggplot2::facet_grid], this column will show
#'              which panel current node or edge is from. Note: some nodes may
#'              fall outside panel (between two panel), so there are possible
#'              `NA` values in this column.
#'   - `panel1` and `panel2`: The panel1 and panel2 variables have the same
#'     functionality as `panel`, but they are specifically for the `edge` data
#'     and correspond to both nodes of each edge.
#'
#' @param merge_dendrogram A single boolean value, indicates whether we should
#' merge multiple dendrograms, only used when previous groups have been
#' established. Default: `FALSE`.
#' @inheritParams align_hclust
#' @inheritParams dendrogram_data
#' @inheritParams ggalign
#' @inheritSection align Discrete Axis Alignment
#' @examples
#' # align_dendro will always add a plot area
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_dendro()
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_dendro(k = 3L)
#'
#' @importFrom ggplot2 aes
#' @importFrom rlang list2
#' @export
align_dendro <- function(mapping = aes(), ...,
                         distance = "euclidean",
                         method = "complete",
                         use_missing = "pairwise.complete.obs",
                         reorder_dendrogram = FALSE,
                         merge_dendrogram = FALSE,
                         reorder_group = FALSE,
                         k = NULL, h = NULL, cutree = NULL,
                         plot_dendrogram = TRUE,
                         plot_cut_height = NULL, root = NULL,
                         center = FALSE, type = "rectangle",
                         size = NULL, data = NULL,
                         no_axes = NULL, active = NULL) {
    reorder_dendrogram <- allow_lambda(reorder_dendrogram)
    if (!rlang::is_bool(reorder_dendrogram) &&
        !is.null(reorder_dendrogram) &&
        !is.function(reorder_dendrogram)) {
        cli_abort(
            "{.arg reorder_dendrogram} must be a single boolean value or a function"
        )
    }

    assert_number_whole(k, allow_null = TRUE)
    assert_number_decimal(h, allow_null = TRUE)
    assert_bool(plot_cut_height, allow_null = TRUE, arg = "plot_cut_height")
    assert_bool(merge_dendrogram)
    assert_bool(reorder_group)
    cutree <- allow_lambda(cutree)
    assert_(cutree, is.function, "a function", allow_null = TRUE)
    assert_bool(plot_dendrogram)
    assert_mapping(mapping)
    if (is.null(data)) {
        if (inherits(method, "hclust") || inherits(method, "dendrogram")) {
            data <- NULL # no need for data
        } else {
            data <- waiver()
        }
    }
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    plot <- ggplot(mapping = mapping)
    if (plot_dendrogram) {
        plot <- plot + ggplot2::geom_segment(
            mapping = aes(
                x = .data$x, y = .data$y,
                xend = .data$xend, yend = .data$yend
            ),
            ...,
            stat = "identity",
            data = function(data) ggalign_attr(data, "edge")
        )
    }
    align(
        align = AlignDendro,
        params = list(
            distance = distance, method = method, use_missing = use_missing,
            k = k, h = h, plot_cut_height = plot_cut_height,
            center = center, type = type, root = root,
            reorder_dendrogram = reorder_dendrogram,
            merge_dendro = merge_dendrogram,
            reorder_group = reorder_group,
            cutree = cutree
        ),
        no_axes = no_axes, active = active,
        size = size,
        schemes = default_schemes(th = theme_no_panel()),
        data = data,
        plot = plot
    )
}

#' @include align-hclust.R
AlignDendro <- ggproto("AlignDendro", AlignHclust,
    #' @importFrom ggplot2 aes ggplot
    #' @importFrom rlang inject
    setup_plot = function(self, plot) {
        ggadd_default(plot, aes(x = .data$x, y = .data$y)) + switch_direction(
            self$direction,
            ggplot2::labs(x = "height"),
            ggplot2::labs(y = "height")
        )
    },
    # other arguments
    extra_params = c("plot_cut_height", "center", "type", "root"),
    build_plot = function(self, plot, design, extra_design = NULL,
                          previous_design = NULL) {
        params <- self$params
        plot_cut_height <- .subset2(params, "plot_cut_height")
        center <- .subset2(params, "center")
        type <- .subset2(params, "type")
        root <- .subset2(params, "root")
        panel <- .subset2(design, "panel")
        index <- .subset2(design, "index")
        if (is_continuous_design(extra_design)) {
            extra_panel <- NULL
            extra_index <- NULL
        } else {
            extra_panel <- .subset2(extra_design, "panel")
            extra_index <- .subset2(extra_design, "index")
        }

        statistics <- .subset2(self, "statistics")
        direction <- self$direction
        priority <- switch_direction(direction, "left", "right")
        dendrogram_panel <- self$panel[index]
        if (!is.null(dendrogram_panel) &&
            # we allow to change the panel level name, but we prevent
            # from changing the underlying factor level (the underlying
            # ordering)
            !all(as.integer(dendrogram_panel) == as.integer(panel))) {
            cli_abort("you cannot do sub-splitting in dendrogram groups")
        }

        if (self$multiple_tree) {
            branches <- levels(panel)
            data <- vector("list", length(statistics))
            start <- 0L
            for (i in seq_along(data)) {
                tree <- .subset2(statistics, i)
                n <- stats::nobs(tree)
                end <- start + n
                data[[i]] <- dendrogram_data(
                    tree,
                    priority = priority,
                    center = center,
                    type = type,
                    leaf_pos = seq(start + 1L, end),
                    leaf_braches = rep_len(.subset(branches, i), n),
                    reorder_branches = FALSE,
                    root = root,
                    double = self$in_linear
                )
                start <- end
            }
            data <- lapply(list_transpose(data), function(dat) {
                ans <- vec_rbind(!!!dat, .names_to = "parent")
                ans$ggpanel <- factor(.subset2(ans, "ggpanel"), branches)
                ans
            })
        } else {
            if (nlevels(panel) > 1L && type == "triangle" && self$in_linear) {
                cli_warn(c(paste(
                    "{.arg type} of {.arg triangle}",
                    "is not well support for facet dendrogram"
                ), i = "will use {.filed rectangle} dendrogram instead"))
                type <- "rectangle"
            }
            data <- dendrogram_data(
                statistics,
                priority = priority,
                center = center,
                type = type,
                leaf_braches = as.character(panel),
                # panel has been reordered by the dendrogram index
                reorder_branches = FALSE,
                root = root,
                double = self$in_linear
            )
        }
        node <- .subset2(data, "node")
        edge <- .subset2(data, "edge")
        node <- rename(node, c(ggpanel = ".panel", index = ".index"))
        edge <- rename(edge, c(ggpanel = ".panel"))

        # add names
        if (!is.null(self$labels)) {
            node$.names <- .subset(self$labels, .subset2(node, ".index"))
        }
        if (is_horizontal(direction)) {
            edge <- rename(
                edge,
                c(x = "y", xend = "yend", y = "x", yend = "xend")
            )
            node <- rename(node, c(x = "y", y = "x"))
        }
        # we do some tricks, since ggplot2 won't remove the attributes
        # we attach the `edge` data
        plot <- gguse_data(plot, ggalign_attr_set(node, list(edge = edge)))

        if (plot_cut_height && !is.null(height <- .subset2(self, "height"))) {
            plot <- plot +
                switch_direction(
                    direction,
                    ggplot2::geom_vline(
                        xintercept = height, linetype = "dashed"
                    ),
                    ggplot2::geom_hline(
                        yintercept = height, linetype = "dashed"
                    )
                )
        }
        position <- .subset2(self, "position")
        if (!self$in_linear || # for circular layout
            # for bottom annotation, reverse y-axis
            (!is.null(position) && position == "bottom")) {
            plot <- reverse_continuous_scale(plot, "y")
        } else if (!is.null(position) && position == "left") {
            # for left annotation, reverse x-axis
            plot <- reverse_continuous_scale(plot, "x")
        }

        # always turn off clip, this is what dendrogram dependends on
        old_coord <- plot$coordinates
        if (!identical(old_coord$clip, "off")) {
            # to prevent from changing the input of user.
            plot$coordinates <- ggproto(NULL, old_coord, clip = "off")
        }
        plot
    }
)

tree_one_node <- function(index, label) {
    structure(
        index,
        class = "dendrogram",
        leaf = TRUE,
        height = 0,
        label = label,
        members = 1L
    )
}
