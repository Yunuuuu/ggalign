#' Reorder or Group layout based on hierarchical clustering
#'
#' @param data A matrix-like object. By default, it inherits from the layout
#'   `matrix`.
#' @inheritParams align
#' @inheritParams align_gg
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' [`geom_segment()`][ggplot2::geom_segment].
#' @inheritParams hclust2
#' @inheritParams dendrogram_data
#' @param reorder_dendrogram A single boolean value indicating whether to
#' reorder the dendrogram based on the means. Alternatively, you can provide a
#' custom function that accepts an [`hclust`][stats::hclust] object and the data
#' used to generate the tree, returning either an [`hclust`][stats::hclust] or
#' [`dendrogram`][stats::as.dendrogram] object. Default is `FALSE`.
#' @param merge_dendrogram A single boolean value, indicates whether we should
#' merge multiple dendrograms, only used when previous groups have been
#' established. Default: `FALSE`.
#' @param reorder_group A single boolean value, indicates whether we should do
#' Hierarchical Clustering between groups, only used when previous groups have
#' been established. Default: `FALSE`.
#' @param k An integer scalar indicates the desired number of groups.
#' @param h A numeric scalar indicates heights where the tree should be cut.
#' @param cutree A function used to cut the [`hclust`][stats::hclust] tree. It
#' should accept four arguments: the [`hclust`][stats::hclust] tree object,
#' `distance` (only applicable when `method` is a string or a function for
#' performing hierarchical clustering), k (the number of clusters), and h (the
#' height at which to cut the tree). By default, [`cutree()`][stats::cutree()]
#' is used.
#' @param plot_dendrogram A boolean value indicates whether plot the dendrogram
#' tree.
#' @param plot_cut_height A boolean value indicates whether plot the cut height.
#' @section ggplot2 specification:
#' `align_dendro` initializes a ggplot `data` and `mapping`.
#'
#' The internal will always use a default mapping of `aes(x = .data$x, y =
#' .data$y)`.
#'
#' The default ggplot data is the `node` coordinates with `edge` data attached
#' in [`ggalign`][ggalign_attr()] attribute, in addition, a
#' [`geom_segment`][ggplot2::geom_segment] layer with a data of the `edge`
#' coordinates will be added.
#'
#' `node` and tree segments `edge` coordinates contains following columns:
#'   - `index`: the original index in the tree for the current node
#'   - `label`: node label text
#'   - `x` and `y`: x-axis and y-axis coordinates for current node or the start
#'                  node of the current edge.
#'   - `xend` and `yend`: the x-axis and y-axis coordinates of the terminal node
#'                        for current edge.
#'   - `branch`: which branch current node or edge is. You can use this column
#'               to color different groups.
#'   - `panel`: which panel current node is, if we split the plot into panel
#'              using [`facet_grid`][ggplot2::facet_grid], this column will show
#'              which panel current node or edge is from. Note: some nodes may
#'              fall outside panel (between two panel), so there are possible
#'              `NA` values in this column.
#'   - `.panel`: Similar with `panel` column, but always give the correct branch
#'              for usage of the ggplot facet.
#'   - `panel1` and `panel2`: The panel1 and panel2 variables have the same
#'     functionality as `panel`, but they are specifically for the `edge` data
#'     and correspond to both nodes of each edge.
#'   - `leaf`: A logical value indicates whether current node is a leaf.
#'
#' @inheritSection align Axis Alignment for Observations
#' @return A `"AlignDendro"` object.
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_dendro()
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_dendro(k = 3L)
#' @seealso
#' - [dendrogram_data()]
#' - [hclust2()]
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
                         no_axes = NULL, context = NULL,
                         free_guides = deprecated(), free_spaces = deprecated(),
                         plot_data = deprecated(), theme = deprecated(),
                         free_labs = deprecated(), set_context = deprecated(),
                         order = deprecated(), name = deprecated()) {
    reorder_dendrogram <- allow_lambda(reorder_dendrogram)
    if (!rlang::is_bool(reorder_dendrogram) &&
        !is.null(reorder_dendrogram) &&
        !is.function(reorder_dendrogram)) {
        cli::cli_abort(
            "{.arg reorder_dendrogram} must be a single boolean value or a function"
        )
    }
    assert_bool(merge_dendrogram)
    assert_bool(reorder_group)
    cutree <- allow_lambda(cutree)
    assert_(cutree, is.function, "a function", null_ok = TRUE)
    assert_bool(plot_dendrogram)
    assert_mapping(mapping)
    if (is.null(data)) {
        if (inherits(method, "hclust") || inherits(method, "dendrogram")) {
            data <- NULL # no need for data
        } else {
            data <- waiver()
        }
    }
    assert_context(context)
    context <- update_context(context, new_context(
        active = plot_dendrogram, order = NA_integer_, name = NA_character_
    ))
    context <- deprecate_context(context, "align_dendro",
        set_context = set_context, order = order, name = name
    )
    align(
        align_class = AlignDendro,
        params = list(
            distance = distance, method = method, use_missing = use_missing,
            k = k, h = h, plot_cut_height = plot_cut_height,
            segment_params = rlang::list2(...),
            center = center, type = type, root = root,
            reorder_dendrogram = reorder_dendrogram,
            merge_dendro = merge_dendrogram,
            reorder_group = reorder_group,
            cutree = cutree,
            mapping = mapping,
            plot_dendrogram = plot_dendrogram
        ),
        free_guides = free_guides,
        free_labs = free_labs, free_spaces = free_spaces,
        plot_data = plot_data, theme = theme,
        no_axes = no_axes, context = context,
        size = size,
        controls = new_controls(),
        data = data
    )
}

#' @importFrom ggplot2 ggproto aes
AlignDendro <- ggproto("AlignDendro", Align,
    #' @importFrom stats reorder
    setup_params = function(self, nobs, params) {
        call <- .subset2(self, "call")
        assert_number(.subset2(params, "k"),
            null_ok = TRUE, arg = "k",
            call = call
        )
        assert_number(.subset2(params, "h"),
            null_ok = TRUE, arg = "h", call = call
        )
        assert_bool(
            .subset2(params, "plot_cut_height"),
            null_ok = TRUE,
            arg = "plot_cut_height", call = call
        )
        # setup the default value for `plot_cut_height`
        params$plot_cut_height <- .subset2(params, "plot_cut_height") %||% (
            # we by default don't draw the height of the user-provided cutree
            # since function like `dynamicTreeCut` will merge tree
            (!is.null(params$k) || !is.null(params$h)) && is.null(params$cutree)
        )
        # setup the default value for `plot_cut_height`
        if (isTRUE(params$reorder_dendrogram)) {
            params$reorder_dendrogram <- function(tree, data) {
                if (!inherits(tree, "dendrogram")) {
                    tree <- stats::as.dendrogram(tree)
                }
                reorder(x = tree, wts = rowMeans(data), agglo.FUN = mean)
            }
        } else if (is.function(params$reorder_dendrogram)) {
            user_reorder <- params$reorder_dendrogram
            params$reorder_dendrogram <- function(tree, data) {
                # we ensure, what we input for user is a hclust object.
                if (!inherits(tree, "hclust")) tree <- stats::as.hclust(tree)
                ans <- user_reorder(tree, data)
                if (!inherits(ans, "hclust") && !inherits(ans, "dendrogram")) {
                    cli::cli_abort(
                        "{.fn reorder_dendrogram} must return a {.cls hclust} or {.cls dendrogram} object",
                        call = call
                    )
                }
                ans
            }
        }
        # initialize the internal parameters
        self$multiple_tree <- FALSE
        self$height <- NULL
        self$panel <- NULL
        params
    },
    setup_data = function(self, params, data) {
        ans <- fortify_matrix(data)
        assert_(
            ans, function(x) is.numeric(x),
            "numeric",
            arg = "data",
            call = .subset2(self, "call")
        )
        ans
    },
    nobs = function(self, params) {
        if (inherits(tree <- .subset2(params, "method"), "hclust")) {
            self$labels <- .subset2(tree, "labels")
            length(.subset2(tree, "order"))
        } else { # a dendrogram
            self$labels <- labels(tree)
            stats::nobs(tree)
        }
    },
    compute = function(self, panel, index, distance, method, use_missing,
                       reorder_dendrogram, k = NULL, h = NULL, cutree = NULL) {
        data <- .subset2(self, "data")
        if (!is.null(data) && nrow(data) < 2L) {
            cli::cli_abort(c(
                "Cannot do Hierarchical Clustering",
                i = "must have >= 2 observations to cluster"
            ), call = .subset2(self, "call"))
        }
        # if the old panel exist, we do sub-clustering
        if (!is.null(panel) && is.null(k) && is.null(h) && is.null(cutree)) {
            if (is.null(data)) {
                cli::cli_abort(c(
                    "Cannot do sub-clustering",
                    i = "Try to provide the {.arg data}"
                ), call = .subset2(self, "call"))
            }
            children <- vector("list", nlevels(panel))
            names(children) <- levels(panel)
            labels <- vec_names(data)

            # we do clustering within each group ---------------
            for (g in levels(panel)) {
                idx <- which(panel == g)
                gdata <- vec_slice(data, idx)
                if (nrow(gdata) == 1L) {
                    children[[g]] <- tree_one_node(idx, .subset(labels, idx))
                } else {
                    child <- stats::as.dendrogram(hclust2(
                        gdata,
                        distance = distance,
                        method = method,
                        use_missing = use_missing
                    ))
                    # we restore the actual index of the original matrix
                    child <- stats::dendrapply(child, function(x) {
                        if (stats::is.leaf(x)) {
                            ans <- .subset(idx, x)
                            attributes(ans) <- attributes(x)
                            ans
                        } else {
                            x
                        }
                    })
                    if (is.function(reorder_dendrogram)) {
                        child <- reorder_dendrogram(child, gdata)
                    }
                    children[[g]] <- child
                }
            }
            return(children) # can be a list of `dendrogram` or `hclust` or mix
        }
        hclust2(data, distance, method, use_missing)
    },
    #' @importFrom stats order.dendrogram
    layout = function(self, panel, index, distance, method, use_missing,
                      reorder_dendrogram, merge_dendro, reorder_group,
                      k, h, cutree, plot_cut_height) {
        statistics <- .subset2(self, "statistics")
        if (!is.null(panel) && is.null(k) && is.null(h) && is.null(cutree)) {
            # reordering the dendrogram ------------------------
            if (nlevels(panel) > 1L && reorder_group) {
                data <- .subset2(self, "data")
                parent_levels <- levels(panel)
                parent_data <- t(sapply(parent_levels, function(g) {
                    colMeans(vec_slice(data, panel == g), na.rm = TRUE)
                }))
                rownames(parent_data) <- parent_levels
                parent <- hclust2(
                    parent_data,
                    distance = distance, method = method,
                    use_missing = use_missing
                )
                # reorder parent based on the parent tree
                if (is.function(reorder_dendrogram)) {
                    parent <- reorder_dendrogram(parent, parent_data)
                }
                # we always ensure the parent is a dendrogram
                # since we'll use `merge_dendrogram()` which requires a
                # dendrogram
                parent <- stats::as.dendrogram(parent)
                panel <- factor(panel, parent_levels[order.dendrogram(parent)])
                # we don't cutree, so we won't draw the height line
                # self$draw_params$height <- attr(ans, "cutoff_height")
            } else {
                parent <- NULL
            }

            # merge children tree ------------------------------
            if (nlevels(panel) == 1L) {
                statistics <- .subset2(statistics, 1L)
            } else if (merge_dendro) {
                # `merge_dendrogram` will follow the order of the parent
                statistics <- lapply(statistics, stats::as.dendrogram)
                statistics <- merge_dendrogram(parent, statistics)
            } else {
                # if no parent tree, and we havn't merged the tree
                # we must manually reorder the dendrogram
                if (!is.null(parent)) {
                    statistics <- .subset(statistics, levels(panel))
                }
                self$multiple_tree <- TRUE
            }
        } else {
            distance <- attr(statistics, "distance")
            if (is.function(reorder_dendrogram)) {
                statistics <- reorder_dendrogram(
                    statistics, .subset2(self, "data")
                )
            }
            if (!is.null(k) || !is.null(h) || !is.null(cutree)) {
                if (is.null(cutree)) {
                    cutree <- function(tree, dist, k, h) {
                        if (!is.null(k)) {
                            stats::cutree(tree, k = k)
                        } else {
                            stats::cutree(tree, h = h)
                        }
                    }
                    # For `cutree`, we always respect the height user specified
                    # For user defined function, we always calculate
                    # height from the number of `panels`
                    if (is.null(k) && plot_cut_height) self$height <- h
                }
                # we need `hclust` object to cutree
                statistics <- stats::as.hclust(statistics)
                panel <- cutree(statistics, distance, k, h)
                if (is.null(self$height) && plot_cut_height) {
                    self$height <- cutree_k_to_h(
                        statistics, vec_unique_count(panel)
                    )
                }
            }
        }
        # save the modified `statistics`
        self$statistics <- statistics
        if (self$multiple_tree) {
            index <- unlist(lapply(statistics, order2), FALSE, FALSE)
        } else {
            index <- order2(statistics)
        }
        # reorder panel factor levels to following the dendrogram order
        if (!is.null(panel)) {
            panel <- factor(panel, unique(panel[index]))
            # save panel information, in case of user change it
            self$panel <- panel
        }
        list(panel, index)
    },
    #' @importFrom ggplot2 aes ggplot
    ggplot = function(self, plot_dendrogram, mapping, segment_params) {
        if (!plot_dendrogram) {
            return(NULL)
        }
        direction <- .subset2(self, "direction")
        ggplot(
            mapping = add_default_mapping(
                mapping, aes(x = .data$x, y = .data$y)
            )
        ) +
            rlang::inject(
                ggplot2::geom_segment(
                    mapping = aes(
                        x = .data$x, y = .data$y,
                        xend = .data$xend, yend = .data$yend
                    ),
                    !!!segment_params,
                    stat = "identity",
                    data = function(data) ggalign_attr(data, "edge")
                )
            ) +
            switch_direction(
                direction,
                ggplot2::labs(x = "height"),
                ggplot2::labs(y = "height")
            )
    },
    draw = function(self, panel, index, extra_panel, extra_index,
                    # other argumentds
                    plot_cut_height, center, type, root) {
        direction <- .subset2(self, "direction")
        statistics <- .subset2(self, "statistics")
        priority <- switch_direction(direction, "left", "right")
        dendrogram_panel <- self$panel
        if (!is.null(dendrogram_panel) &&
            # we can change the panel level name, but we prevent
            # from changing the underlying factor level
            !all(as.integer(dendrogram_panel) == as.integer(panel))) {
            cli::cli_abort("you cannot do sub-splitting in dendrogram groups")
        }

        if (self$multiple_tree) {
            # if we have multiple tree, the underlying panel name may be
            # changed by other `align_*` functions, here, we match the new
            # panel level
            branches <- levels(panel)
            # reorder the children based on the new branches
            statistics <- statistics[match(branches, levels(dendrogram_panel))]
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
                    root = root
                )
                start <- end
            }
            data <- lapply(transpose(data), function(dat) {
                ans <- vec_rbind(!!!dat, .names_to = "parent")
                ans$.panel <- factor(.subset2(ans, ".panel"), branches)
                ans
            })
        } else {
            if (nlevels(panel) > 1L && type == "triangle") {
                cli::cli_warn(c(paste(
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
                leaf_braches = .subset(as.character(panel), index),
                reorder_branches = FALSE,
                root = root
            )
        }
        node <- .subset2(data, "node")
        edge <- .subset2(data, "edge")
        if (is_horizontal(direction)) {
            edge <- rename(
                edge,
                c(x = "y", xend = "yend", y = "x", yend = "xend")
            )
            node <- rename(node, c(x = "y", y = "x"))
        }
        plot <- .subset2(self, "plot")
        # we do some tricks, since ggplot2 won't remove the attributes
        # we attach the `edge` data in `ggalign` attribute
        plot$data <- structure(node, ggalign = list(edge = edge))
        position <- .subset2(self, "position")
        if (is.null(position) || !isTRUE(plot$coordinates$default)) {
            # if the dendrogram is in a normal stack layout
            # or if user has set the coordinate, we won't reverse
            # the dendrogram height axis
        } else if (position == "bottom") { # in the bottom, reverse y-axis
            plot <- plot + ggplot2::coord_trans(y = "reverse", clip = "off")
        } else if (position == "left") { # in the left, reverse x-axis
            plot <- plot + ggplot2::coord_trans(x = "reverse", clip = "off")
        }

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
