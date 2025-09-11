#' Plot Phylogenetics tree
#'
#' @param phylo A [`phylo`][ape::as.phylo] object.
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' [`geom_segment()`][ggplot2::geom_segment].
#' @param split A logical scalar indicating whether to split the phylogenetic
#'   tree into separate subtrees when multiple panel groups are present.
#' @param ladderize A single string of `r oxford_or(c("left", "right"))`,
#' indicating whether to ladderize the tree. Ladderizing arranges the tree so
#' that the smallest clade is positioned on the `"right"` or the `"left"`. By
#' default, `NULL` means the tree will not be ladderized.
#' @inheritParams fortify_data_frame.phylo
#' @inheritParams ggalign
#' @export
align_phylo <- function(phylo, ..., mapping = aes(),
                        split = FALSE, ladderize = NULL, type = "rectangle",
                        center = FALSE, tree_type = NULL, root = NULL,
                        active = NULL, size = NULL, no_axes = deprecated()) {
    if (lifecycle::is_present(no_axes)) {
        lifecycle::deprecate_stop(
            "1.1.0",
            "align_phylo(no_axes = )",
            details = "Please add `theme()` to the ggplot instead"
        )
    }
    assert_s3_class(phylo, c("phylo", "multiPhylo"))
    if (is.null(phylo$tip.label)) {
        cli_abort("{.arg phylo} must have tip labels")
    }
    assert_bool(split)
    if (split) {
        rlang::check_installed("ape", "to split phylogenetic tree")
    }
    if (!is.null(ladderize)) {
        rlang::check_installed("ape", "to ladderize phylogenetic tree")
        ladderize <- arg_match0(ladderize, c("left", "right"))
    }
    assert_active(active)
    active <- active_update(active(use = TRUE), active)
    align(
        align = AlignPhylo,
        phylo = phylo,
        split = split, ladderize = ladderize,
        plot = ggplot(mapping = mapping) +
            ggplot2::geom_segment(
                mapping = aes(
                    x = .data$x, y = .data$y,
                    xend = .data$xend, yend = .data$yend
                ),
                ...,
                stat = "identity",
                data = function(data) ggalign_attr(data, "edge")
            ),
        data_params = list(
            type = type, center = center,
            tree_type = tree_type, root = root
        ),
        active = active,
        size = size
    )
}

AlignPhylo <- ggproto("AlignPhylo", CraftAlign,
    interact_layout = function(self, layout) {
        layout <- ggproto_parent(CraftAlign, self)$interact_layout(layout)

        # we keep the names from the layout data for usage
        tip_labels <- self$phylo$tip.label
        if (is.na(layout_nobs <- prop(layout@domain, "nobs"))) {
            prop(layout@domain, "nobs") <- vec_size(tip_labels)
        } else {
            assert_mismatch_nobs(
                self, layout_nobs, vec_size(tip_labels),
                arg = "phylo"
            )
        }
        self$labels <- tip_labels

        # initialize the internal parameters
        self$panel <- NULL
        layout
    },
    compute = function(self, panel, index) {
        phylo <- self$phylo
        # R CMD check won't give error even we don't add `ape` to the dependency
        if (!is.null(self$ladderize)) {
            phylo <- ape::ladderize(phylo,
                right = identical(self$ladderize, "right")
            )
        }
        # If multiple panel groups exist, handle according to `split` or
        # ordering rules
        if (!is.null(panel) && nlevels(panel) > 1L) {
            if (self$split) {
                phylo <- lapply(split(phylo$tip.label, panel), function(tips) {
                    ape::keep.tip(phylo, tips)
                })
                # Store the panel used for splitting to ensure consistency in
                # later steps
                self$panel <- panel
            }
        }
        phylo
    },
    align = function(self, panel, index) {
        if (inherits(self$statistics, c("phylo", "multiPhylo"))) {
            # For a single tree
            new_index <- order2(self$statistics)
            if (!is.null(panel) && nlevels(panel) > 1L &&
                !all(new_index == reorder_index(panel, new_index))) {
                layout_name <- self$layout_name
                object_name <- object_name(self)
                cli_abort(
                    c(
                        sprintf("Cannot add %s to %s", object_name, layout_name),
                        i = sprintf(
                            "Group of %s will disrupt the ordering index of %s",
                            layout_name, object_name
                        ),
                        i = "If you need to group tips, set {.code split = TRUE} to split the phylogenetic tree into subtrees."
                    ),
                    call = self$call
                )
            }
        } else {
            # for multiple trees
            tip_labels <- unlist(
                lapply(self$statistics, function(phylo) phylo$tip.label),
                recursive = FALSE, use.names = FALSE
            )
            new_index <- match(tip_labels, self$labels)
        }
        list(panel, new_index)
    },
    init_plot = function(self, plot) {
        ggadd_default(plot, aes(x = .data$x, y = .data$y)) + switch_direction(
            self$direction,
            ggplot2::labs(x = "timing"),
            ggplot2::labs(y = "timing")
        )
    },
    build_plot = function(self, plot, domain, extra_domain = NULL,
                          previous_domain = NULL) {
        phylo <- self$statistics
        panel <- prop(domain, "panel")
        data_params <- self$data_params
        direction <- self$direction
        priority <- switch_direction(direction, "left", "right")

        if (inherits(phylo, c("phylo", "multiPhylo"))) {
            # For a single tree
            if (nlevels(panel) > 1L && data_params$type == "triangle") {
                cli_warn(c(
                    "{.arg type} = {.val triangle} is not well supported for faceted phylogenetic trees.",
                    i = "Switching to {.val rectangle} layout."
                ))
                data_params$type <- "rectangle"
            }
            data <- inject(fortify_data_frame.phylo(
                data = phylo,
                !!!data_params,
                priority = priority,
                tip_clades = as.character(panel),
                # panel has been reordered by the index
                reorder_clades = FALSE,
                double = TRUE,
                data_arg = "phylo",
                call = self$call
            ))
            edge <- ggalign_attr(data, "edge")
            node <- data
        } else {
            # for multiple trees
            phylo_panel <- self$panel[prop(domain, "index")]
            # Allow renaming panel levels, but prevent changing the underlying
            # ordering
            if (!is.null(phylo_panel) &&
                !all(as.integer(phylo_panel) == as.integer(panel))) {
                cli_abort("you cannot do sub-grouping in phylogenetic tree groups")
            }
            groups <- levels(panel)
            data <- vector("list", length(phylo))
            start <- 0L
            data_params$root <- NULL
            for (i in seq_along(phylo)) {
                tree <- .subset2(phylo, i)
                n <- length(tree$tip.label)
                end <- start + n
                data[[i]] <- inject(fortify_data_frame.phylo(
                    data = tree,
                    !!!data_params,
                    priority = priority,
                    root = .subset(groups, i),
                    tip_pos = seq(start + 1L, end),
                    tip_clades = NULL,
                    reorder_clades = FALSE,
                    double = TRUE,
                    data_arg = "phylo",
                    call = self$call
                ))
                start <- end
            }

            # Combine node and edge data into unified tibbles
            data <- lapply(
                list(
                    node = data,
                    edge = lapply(data, ggalign_attr, "edge")
                ),
                function(dat) {
                    ans <- vec_rbind(!!!dat, .names_to = NULL)
                    ans$.panel <- factor(.subset2(ans, ".panel"), groups)
                    ans
                }
            )
            edge <- .subset2(data, "edge")
            node <- .subset2(data, "node")
        }

        # add names
        if (!is.null(node$label)) {
            node$.names <- node$label
        }
        if (!is.null(edge$label)) {
            edge$.names <- edge$label
        }
        if (is_horizontal(direction)) {
            edge <- rename(
                edge,
                c(x = "y", xend = "yend", y = "x", yend = "xend")
            )
            node <- rename(node, c(x = "y", y = "x"))
        }
        plot <- gguse_data(plot, ggalign_data_set(node, edge = edge))
        position <- self$position
        if (!is.null(position) && position == "top") {
            # for top annotation, reverse y-axis
            plot <- reverse_continuous_axis(plot, "y")
        } else if (!is.null(position) && position == "right") {
            # for right annotation, reverse x-axis
            plot <- reverse_continuous_axis(plot, "x")
        }
        plot
    },
    finish_plot = function(self, plot, schemes, theme) {
        # always turn off clip, this is what dendrogram dependends on
        old_coord <- plot$coordinates
        if (!identical(old_coord$clip, "off")) {
            # to prevent from changing the input of user.
            plot$coordinates <- ggproto(NULL, old_coord, clip = "off")
        }
        ggproto_parent(CraftAlign, self)$finish_plot(plot, schemes, theme)
    },
    summary_align = function(self) c(TRUE, FALSE)
)
