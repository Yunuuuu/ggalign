#' Plot Phylogenetics tree
#'
#' @param phylo A [`phylo`][ape::as.phylo] object.
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' [`geom_segment()`][ggplot2::geom_segment].
#' @param ladderize A single string of `r oxford_or(c("left", "right"))`,
#' indicating whether to ladderize the tree. Ladderizing arranges the tree so
#' that the smallest clade is positioned on the `"right"` or the `"left"`. By
#' default, `NULL` means the tree will not be ladderized.
#' @inheritParams fortify_data_frame.phylo
#' @inheritParams ggalign
#' @export
align_phylo <- function(phylo, ..., ladderize = NULL, type = "rectangle",
                        center = FALSE, tree_type = NULL,
                        no_axes = NULL, active = NULL,
                        size = NULL) {
    if (!is.null(ladderize)) {
        ladderize <- arg_match0(ladderize, c("left", "right"))
        rlang::check_installed("ape", "to ladderize phylogenetics tree")
    }
    assert_s3_class(phylo, "phylo")
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    no_axes <- no_axes %||%
        getOption(sprintf("%s.align_no_axes", pkg_nm()), default = TRUE)
    align(
        align = AlignPhylo,
        phylo = phylo,
        ladderize = ladderize,
        no_axes = no_axes,
        plot = ggplot() +
            ggplot2::geom_segment(
                mapping = aes(
                    x = .data$x, y = .data$y,
                    xend = .data$xend, yend = .data$yend
                ),
                ...,
                stat = "identity",
                data = function(data) ggalign_attr(data, "edge")
            ),
        params = list(type = type, center = center, tree_type = tree_type),
        active = active,
        size = size
    )
}

AlignPhylo <- ggproto("AlignPhylo", Align,
    interact_layout = function(self, layout) {
        layout <- ggproto_parent(Align, self)$interact_layout(layout)

        # we keep the names from the layout data for usage
        tip_labels <- self$phylo$tip.label
        if (is.null(tip_labels)) {
            cli_abort(
                "{.arg phylo} must have tip labels to match the layout data",
                call = self$call
            )
        } else if (vec_duplicate_any(tip_labels)) {
            cli_abort(
                "{.arg phylo} cannot have duplicated tip labels",
                call = self$call
            )
        }

        # we ensure the layout data has names to match the phylo tree
        if (is.null(layout_labels <- vec_names(layout@data))) {
            cli_abort(c(
                sprintf(
                    "Cannot add %s to %s", object_name(self),
                    self$layout_name
                ),
                i = sprintf(
                    "%s has no labels (rownames) to match {.arg phylo}",
                    self$layout_name
                )
            ))
        } else if (vec_duplicate_any(layout_labels)) {
            cli_abort(c(
                sprintf(
                    "Cannot add %s to %s", object_name(self),
                    self$layout_name
                ),
                i = sprintf("%s has duplicated labels", self$layout_name)
            ))
        }
        assert_mismatch_nobs(
            self, .subset2(layout@design, "nobs"), vec_size(tip_labels),
            arg = "phylo"
        )

        # we keep the names from the layout data for usage
        self$labels <- layout_labels
        layout
    },
    compute = function(self, panel, index) {
        phylo <- self$phylo
        # why R CMD check doesn't give error even I don't add ape to dependency
        if (!is.null(self$ladderize)) {
            phylo <- ape::ladderize(phylo,
                right = identical(ladderize, "right")
            )
        }
        inject(fortify_data_frame.phylo(data = phylo, !!!self$params))
    },
    align = function(self, panel, index) {
        data <- self$statistics
        tip <- vec_slice(data, .subset2(data, "tip"))
        ordered <- .subset2(tip, "label")[order(.subset2(tip, "x"))]
        index <- match(ordered, self$labels)
        if (!is.null(panel) && nlevels(panel) > 1L &&
            !all(index == reorder_index(panel, index))) {
            layout_name <- self$layout_name
            object_name <- object_name(self)
            cli_abort(c(
                sprintf("Cannot add %s to %s", object_name, layout_name),
                i = sprintf(
                    "Group of %s will disrupt the ordering index of %s", layout_name, object_name
                )
            ), call = self$call)
        }
        list(panel, index)
    },
    setup_plot = function(self, plot) {
        ggadd_default(plot, aes(x = .data$x, y = .data$y)) + switch_direction(
            self$direction,
            ggplot2::labs(x = "timing"),
            ggplot2::labs(y = "timing")
        )
    },
    build_plot = function(self, plot, design, extra_design = NULL,
                          previous_design = NULL) {
        if (!is.null(panel <- .subset2(design, "panel")) &&
            nlevels(panel) > 1L) {
            layout_name <- self$layout_name
            object_name <- object_name(self)
            cli_abort(c(
                sprintf("Cannot add %s to %s", object_name, layout_name),
                i = sprintf("%s cannot span multiple panels", object_name)
            ))
        }

        data <- self$statistics
        edge <- ggalign_attr(data, "edge")
        node <- data
        node$.panel <- unique(panel)
        edge$.panel <- unique(panel)

        # add names
        if (!is.null(node$label)) {
            node$.names <- node$label
        }
        if (!is.null(edge$label)) {
            edge$.names <- edge$label
        }
        if (is_horizontal(self$direction)) {
            edge <- rename(
                edge,
                c(x = "y", xend = "yend", y = "x", yend = "xend")
            )
            node <- rename(node, c(x = "y", y = "x"))
        }
        plot <- gguse_data(plot, ggalign_attr_set(node, list(edge = edge)))
        position <- self$position
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
    },
    summary_align = function(self) c(TRUE, FALSE)
)

#' @inherit fortify_matrix title
#' @description This method allows a [`phylo`][ape::as.phylo] object to be
#' directly input into `stack_discrete()` or `circle_discrete()`. This makes it
#' possible to add [`align_phylo()`] to the stack independently, as
#' [`align_phylo()`] requires the layout to have labels.
#' @param data A [`phylo`][ape::as.phylo] object.
#' @inheritParams rlang::args_dots_empty
#' @return A one-column matrix where the tip labels are the values, and the row
#' names will also be the tip labels.
#' @family fortify_matrix methods
#' @export
fortify_matrix.phylo <- function(data, ...) {
    rlang::check_dots_empty()
    if (is.null(labels <- data$tip.label)) {
        cli_abort(
            "{.arg data} {.cls phylo} object must have tip labels to match the layout data"
        )
    }
    as.matrix(vec_set_names(labels, labels))
}

#' @inherit fortify_data_frame.default title description
#' @inheritParams rlang::args_dots_empty
#' @inheritParams fortify_data_frame.dendrogram
#' @param tree_type A single string, one of
#' `r oxford_or(c("phylogram", "cladogram"))`, indicating the type of tree.
#' - `phylogram`: Represents a phylogenetic tree where branch lengths indicate
#'   evolutionary distance or time.
#' - `cladogram`: Represents a tree where branch lengths are not used, or the
#'   branches do not reflect evolutionary time.
#'
#' Usually, you don't need to modify this.
#'
#' @param tip_pos The x-coordinates of the tip. Must be the same length
#' of the number of tips in `tree`.
#' @return A `data frame` with the node coordinates:
#'   - `.index`: the original index in the tree for the the tip/node.
#'   - `label`: the tip/node label text.
#'   - `x` and `y`: x-axis and y-axis coordinates for the tip/node.
#'   - `tip`: A logical value indicates whether current node is a tip.
#' @section ggalign attributes:
#'  `edge`: A `data frame` for edge coordinates:
#'
#'  - `x` and `y`: x-axis and y-axis coordinates for the start node of the edge.
#'  - `xend` and `yend`: the x-axis and y-axis coordinates of the terminal node
#'                       for edge.
#' @family fortify_data_frame methods
#' @export
fortify_data_frame.phylo <- function(data, ..., type = "rectangle",
                                     center = FALSE,
                                     tree_type = NULL, tip_pos = NULL) {
    rlang::check_dots_empty()
    type <- arg_match0(type, c("rectangle", "triangle"))
    rectangle <- type == "rectangle"
    edge <- data$edge
    edge_lengths <- data$edge.length
    if (!is.null(tree_type)) {
        tree_type <- arg_match0(tree_type, c("phylogram", "cladogram"))
        if (tree_type == "phylogram" && is.null(edge_lengths)) {
            cli_warn(c(
                "Cannot use {.code tree_type = 'phylogram'}",
                "No branch length found in {.arg data}"
            ))
        }
    }
    if (identical(tree_type, "cladogram")) {
        edge_lengths <- NULL
    }
    parent <- edge[, 1L, drop = TRUE]
    child <- edge[, 2L, drop = TRUE]
    tip_labels <- data$tip.label
    node_labels <- data$node.label
    N <- length(tip_labels)
    if (is.null(tip_pos)) {
        tip_pos <- seq_len(N)
    } else if (length(tip_pos) != N) {
        cli_abort(
            "{.arg tip_pos} must have the same length as the number of tips in {.arg data}"
        )
    }
    i <- 0L # tip index
    phylo_data <- function(index, level, timing) {
        if (any(select <- parent == index)) {
            # recursively for each child
            data <- list(index = child[select])
            # if we have edge length, timing should be available
            if (!is.null(edge_lengths)) {
                data <- c(data, list(timing = timing + edge_lengths[select]))
            }
            data <- list_transpose(.mapply(
                function(index, timing = NULL) {
                    phylo_data(index, level = level + 1L, timing = timing)
                },
                data, NULL
            ))

            # integrate the data for each child
            node <- vec_rbind(!!!.subset2(data, "node"))
            edge <- vec_rbind(!!!.subset2(data, "edge"))

            # all coordinate for direct children nodes -------------
            direct_leaves_x <- unlist(
                .subset2(data, "x"),
                recursive = FALSE, use.names = FALSE
            )
            direct_leaves_y <- unlist(
                .subset2(data, "y"),
                recursive = FALSE, use.names = FALSE
            )

            # prepare node data ------------------------------------
            # all x coordinate for children nodes ------------------
            # used if center is `TRUE`, we'll calculate the center position
            # among all children nodes
            leaves <- vec_slice(node, .subset2(node, "tip")) # all leaves

            # x coordinate for current node: the midpoint
            if (center) {
                x <- sum(range(.subset2(leaves, "x"))) / 2L
            } else {
                x <- sum(range(direct_leaves_x)) / 2L
            }

            # y coordinate for current node
            if (is.null(edge_lengths) && is.null(timing)) {
                y <- min(direct_leaves_y) * level / (level + 1L)
            } else {
                y <- timing
            }

            # there is no node data for the root
            node <- vec_rbind(data_frame0(
                .index = index,
                label = node_labels[index - N],
                x = x, y = y, tip = FALSE
            ), node)

            # if it's the `rectangle`
            if (rectangle) {
                # vertical lines
                vertical_lines <- data_frame0(
                    x = direct_leaves_x,
                    xend = direct_leaves_x,
                    y = y,
                    yend = direct_leaves_y
                )
                added_edge <- vec_rbind(
                    vertical_lines,
                    # horizontal line
                    data_frame0(
                        x = x,
                        xend = direct_leaves_x,
                        y = y,
                        yend = y
                    )
                )
            } else {
                added_edge <- data_frame0(
                    x = x,
                    xend = direct_leaves_x,
                    y = y,
                    yend = direct_leaves_y
                )
            }
            if (is.null(edge)) {
                edge <- added_edge
            } else {
                edge <- vec_rbind(added_edge, edge)
            }
            list(node = node, edge = edge, x = x, y = y)
        } else if (any(select <- child == index)) { # for the tip
            i <<- i + 1L
            x <- tip_pos[i]
            if (is.null(edge_lengths)) {
                y <- 1L
            } else {
                y <- edge_lengths[select] + timing
            }
            list(
                node = data_frame0(
                    .index = index,
                    label = tip_labels[index],
                    x = x,
                    y = y,
                    tip = TRUE
                ),
                edge = NULL,
                x = x, y = y
            )
        } else {
            cli_abort("Invalid {.cls phylo} object")
        }
    }

    # from ape::is.rooted, this should be the most ancester
    ans <- phylo_data(N + 1L, 0L, timing = 0)
    ggalign_attr_set(.subset2(ans, "node"), list(edge = .subset2(ans, "edge")))
}
