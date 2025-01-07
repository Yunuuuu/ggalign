#' Plot Phylogenetics tree
#'
#' @param phylo A [`phylo`][ape::as.phylo] object.
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' [`geom_segment()`][ggplot2::geom_segment].
#' @param ladderize A boolean value indicates whether to ladderize the tree. See
#' [`ladderize()`][ape::ladderize()].
#' @inheritParams ggalign
#' @export
align_phylo <- function(phylo, ..., ladderize = TRUE,
                        no_axes = NULL, active = NULL,
                        size = NULL) {
    assert_s3_class(phylo, "phylo")
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    no_axes <- no_axes %||%
        getOption(sprintf("%s.align_no_axes", pkg_nm()), default = TRUE)
    new_ggalign_plot(
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
        params = list(),
        active = active,
        size = size
    )
}

AlignPhylo <- ggproto("AlignPhylo", Align,
    interact_layout = function(self, layout) {
        layout_name <- self$layout_name
        object_name <- object_name(self)
        # check plot is compatible with the layout
        if (is_layout_continuous(layout)) {
            # `Align` object is special for discrete variables
            cli_abort(c(
                sprintf("Cannot add %s to %s", object_name, layout_name),
                i = sprintf("%s cannot align discrete variables", layout_name)
            ))
        }

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
        design <- layout@design
        layout_nobs <- .subset2(design, "nobs")

        # If `nobs` is `NULL`, it means we don't initialize the layout
        # observations, we initialize `nobs` with the `Align` obect
        if (is.null(layout_nobs)) {
            layout_nobs <- length(tip_labels)
            layout_labels <- NULL
        } else if (length(tip_labels) != layout_nobs) {
            cli_abort(sprintf(
                "%s (nobs: %d) is not compatible with the %s (nobs: %d)",
                object_name, length(tip_labels), layout_name, layout_nobs
            ))
        } else if (is.null(layout_labels <- vec_names(layout_data))) {
            cli_abort(c(
                sprintf("Cannot add %s to %s", object_name, layout_name),
                i = sprintf(
                    "%s has no labels to match {.arg phylo}",
                    layout_name
                )
            ))
        } else if (vec_duplicate_any(layout_labels)) {
            cli_abort(c(
                sprintf("Cannot add %s to %s", object_name, layout_name),
                i = sprintf("%s has duplicated labels", layout_name)
            ))
        }

        # we keep the names from the layout data for usage
        self$labels <- layout_labels
        design["nobs"] <- list(layout_nobs)
        layout@design <- design
        layout
    },
    compute = function(self, panel, index) {
        if (!is.null(panel) && nlevels(panel) > 1L) {
            layout_name <- self$layout_name
            object_name <- object_name(self)
            cli_abort(c(
                sprintf("Cannot add %s to %s", object_name, layout_name),
                i = sprintf("%s cannot span multiple panels", object_name)
            ))
        }
        phylo <- self$phylo
        # why R CMD check doesn't give error even I don't add ape to dependency
        if (isTRUE(self$ladderize)) phylo <- ape::ladderize(phylo)
        phylo
    },
    align = function(self, panel, index) {
        ordering <- order2(self$phylo)
        if (is.null(self$labels)) {
            index <- ordering
        } else {
            index <- match(self$phylo$tip.label[ordering], self$labels)
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

        data <- inject(fortify_data_frame.phylo(
            data = self$phylo, !!!self$params
        ))
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

#' @inherit fortify_data_frame.default
#' @inheritParams dendrogram_data
#' @param tree_type A single string, one of
#' `r oxford_or(c("phylogram", "cladogram"))`, indicating the type of tree.
#' Usually, you don't need to modify this.
#' @param tip_pos The x-coordinates of the tip. Must be the same length
#' of the number of tips in `tree`.
#' @details
#' A `data frame` with the node coordinates:
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
#' @export
fortify_data_frame.phylo <- function(data, ..., type = "rectangle",
                                     center = FALSE,
                                     tree_type = NULL, tip_pos = NULL) {
    type <- arg_match0(type, c("rectangle", "triangle"))
    rectangle <- type == "rectangle"
    edge <- data$edge
    edge_lengths <- data$edge.length
    if (!is.null(tree_type)) {
        tree_type <- arg_match0(tree_type, c("phylogram", "cladogram"))
        if (tree_type == "phylogram" && is.null(edge_lengths)) {
            cli_warn(c(
                "Cannot use {.code tree_type = 'phylogram'}",
                "No branch length found in {.arg x}"
            ))
        }
    }
    if (is.null(edge_lengths) || identical(tree_type, "cladogram")) {
        edge_lengths <- seq_len(nrow(edge))
    }
    parent <- edge[, 1L, drop = TRUE]
    child <- edge[, 2L, drop = TRUE]
    tip_labels <- data$tip.label
    N <- length(tip_labels)
    if (is.null(tip_pos)) {
        tip_pos <- seq_len(N)
    } else if (length(tip_pos) != N) {
        cli_abort(
            "{.arg tip_pos} must have the same length as the number of tips in {.arg x}"
        )
    }
    i <- 0L # tip index
    phylo_data <- function(index, timing, from_root = TRUE) {
        if (any(select <- parent == index)) {
            y <- timing
            data <- list_transpose(.mapply(
                function(index, timing) {
                    phylo_data(index, timing, from_root = FALSE)
                },
                list(
                    index = child[select],
                    timing = timing + edge_lengths[select]
                ), NULL
            ))
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

            # there is no node data for the root
            node <- vec_rbind(data_frame0(
                .index = index, label = NA,
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
            y <- edge_lengths[select] + timing
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
    ans <- phylo_data(N + 1L, 0L)
    ggalign_attr_set(.subset2(ans, "node"), list(edge = .subset2(ans, "edge")))
}
