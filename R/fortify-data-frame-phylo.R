#' @inherit fortify_data_frame.default title description
#' @inheritParams rlang::args_dots_empty
#' @inheritParams fortify_data_frame.dendrogram
#' @param tip_pos The x-coordinates of the tips. Must be the same length
#' of the number of tips in `data`.
#' @param tip_clades Clades of the tips. Must be the same length of the
#' number of tips in `data`.
#' @param reorder_clades A single boolean value, indicates whether reorder the
#' provided `tip_clades` based on the actual ordering index.
#' @param clade_gap A single numeric value indicates the gap between different
#' clades.
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
#'   - `.panel`: Similar with `panel` column, but always give the correct
#'              panel for usage of the ggplot facet.
#'   - `.index`: the original index in the tree for the the tip/node.
#'   - `label`: the tip/node label text.
#'   - `x` and `y`: x-axis and y-axis coordinates for the tip/node.
#'   - `clade`: which clade the node is. You can use this column to color
#'               different clades.
#'   - `panel`: which panel the node is, if we split the plot into panel
#'              using [facet_grid][ggplot2::facet_grid], this column will show
#'              which panel the node is from. Note: some nodes may
#'              fall outside panel (between two panels), so there are possible
#'              `NA` values in this column.
#'   - `tip`: A logical value indicates whether current node is a tip.
#' @section ggalign attributes:
#'  `edge`: A `data frame` for edge coordinates:
#'
#'  - `.panel`: Similar with `panel` column, but always give the correct
#'              panel for usage of the ggplot facet.
#'  - `x` and `y`: x-axis and y-axis coordinates for the start node of the edge.
#'  - `xend` and `yend`: the x-axis and y-axis coordinates of the terminal node
#'                       for edge.
#'  - `clade`: which panel the edge is. You can use this column to color
#'              different groups.
#'  - `panel1` and `panel2`: The panel1 and panel2 columns have the same
#'     functionality as `panel`, but they are specifically for the `edge` data
#'     and correspond to both nodes of each edge.
#' @family fortify_data_frame
#' @export
fortify_data_frame.phylo <- function(data, ...,
                                     priority = "right",
                                     center = FALSE,
                                     type = "rectangle",
                                     tree_type = NULL,
                                     tip_pos = NULL, tip_clades = NULL,
                                     reorder_clades = TRUE,
                                     clade_gap = NULL,
                                     root = NULL,
                                     double = TRUE,
                                     data_arg = NULL, call = NULL) {
    call <- call %||% current_call()
    data_arg <- data_arg %||% "data"
    rlang::check_dots_empty(call = call)
    assert_bool(center, call = call)
    assert_bool(reorder_clades, call = call)
    type <- arg_match0(type, c("rectangle", "triangle"), error_call = call)
    priority <- arg_match0(priority, c("left", "right"), error_call = call)
    rectangle <- type == "rectangle"
    edge <- data$edge
    edge_lengths <- data$edge.length
    if (!is.null(tree_type)) {
        tree_type <- arg_match0(tree_type,
            c("phylogram", "cladogram"),
            error_call = call
        )
        if (tree_type == "phylogram" && is.null(edge_lengths)) {
            cli_warn(c(
                "Cannot use {.code tree_type = 'phylogram'}",
                "No branch length found in {.arg {data_arg}}"
            ))
            tree_type <- "cladogram"
        }
    }
    if (identical(tree_type, "cladogram")) {
        edge_lengths <- NULL
    }
    parent <- edge[, 1L, drop = TRUE]
    child <- edge[, 2L, drop = TRUE]
    tip_labels <- data$tip.label
    if (is.null(tip_labels)) {
        cli_abort("{.arg {data_arg}} must be a {.cls phylo} object with tip labels")
    }
    node_labels <- data$node.label
    N <- length(tip_labels)
    if (is.null(tip_pos)) {
        tip_pos <- seq_len(N)
    } else if (length(tip_pos) != N) {
        cli_abort(
            "{.arg tip_pos} must have the same length as the number of tips in {.arg {data_arg}}",
            call = call
        )
    }
    if (is.null(tip_clades)) {
        root <- root %||% "root"
    } else if (anyNA(tip_clades)) {
        cli_abort("`NA` is not allowed in {.arg tip_clades}",
            call = call
        )
    } else if (length(tip_clades) != N) {
        cli_abort(
            "{.arg tip_clades} must be of the same length of {.arg tree}",
            call = call
        )
    } else if (is.character(tip_clades)) {
        root <- root %||% "root"
    } else if (is.factor(tip_clades)) {
        tip_clades <- as.character(tip_clades)
        root <- root %||% "root"
    } else if (is.numeric(tip_clades)) {
        root <- root %||% (min(tip_clades) - 1L)
    } else {
        cli_abort("{.arg tip_clades} must be a character or numeric",
            call = call
        )
    }

    if (!is.null(tip_clades) && reorder_clades) {
        tip_clades <- .subset(tip_clades, order2(data))
    }

    assert_number_decimal(
        clade_gap,
        min = 0, allow_infinite = FALSE,
        allow_null = TRUE, call = call
    )
    clade_gap <- clade_gap %||% 0

    # the root value shouldn't be the same of `tip_clades.`
    if (!is_scalar(root)) {
        cli_abort("{.arg root} must be of length 1", call = call)
    } else if (is.na(root)) {
        cli_abort("{.arg root} cannot be `NA`", call = call)
    } else if (any(root == tip_clades)) {
        cli_abort(
            "{.arg root} cannot match any value in {.arg tip_clades}",
            call = call
        )
    }

    i <- 0L # tip index
    clade_lvls <- NULL
    last_clade <- root
    total_gap <- 0
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
            direct_leaves_clade <- unlist(
                .subset2(data, "clade"),
                recursive = FALSE, use.names = FALSE
            )
            direct_leaves_panel <- unlist(
                .subset2(data, "panel"),
                recursive = FALSE, use.names = FALSE
            )
            direct_leaves_ggpanel <- unlist(
                .subset2(data, "ggpanel"),
                recursive = FALSE, use.names = FALSE
            )

            # all x coordinate for children nodes --------------------
            # used if center is `TRUE`, we'll calculate the center position
            # among all children nodes
            tips <- vec_slice(node, .subset2(node, "tip")) # all tips

            # prepare node data ------------------------------------
            # x coordinate for current node: the midpoint
            if (center) {
                x <- sum(range(.subset2(tips, "x"))) / 2L
            } else {
                x <- sum(range(direct_leaves_x)) / 2L
            }

            # y coordinate for current node
            if (is.null(edge_lengths) && is.null(timing)) {
                y <- min(direct_leaves_y) * level / (level + 1L)
            } else {
                y <- timing
            }

            # we assign the `panel` for current node
            panel_ranges <- split(.subset2(tips, "x"), .subset2(tips, "panel"))
            panel_ranges <- panel_ranges[
                order(vapply(panel_ranges, min, numeric(1L), USE.NAMES = FALSE))
            ]
            full_panel <- names(panel_ranges)

            if (is.null(tip_clades)) { # no clades
                ggpanel <- panel <- clade <- root
            } else {
                # we assign the clade for current node
                clade <- unique(direct_leaves_clade)
                # if two children are different, this clade should be
                # `root`, this is often used to color the segments
                if (length(clade) > 1L) clade <- root

                # we assign the `panel` for current node
                panel <- NA
                for (i in seq_along(panel_ranges)) {
                    if (x < min(.subset2(panel_ranges, i))) {
                        panel <- NA
                        break
                    } else if (x <= max(.subset2(panel_ranges, i))) {
                        panel <- .subset2(full_panel, i)
                        break
                    }
                }
                # if the node is between two facet panels
                # we choose `ggpanel` by the priority
                if (is.na(ggpanel <- panel)) {
                    # it's not possible for an node live outside the
                    # all panels - the left or right most. So `i` won't be 1 or
                    # length(ranges). we don't need to check the argument
                    ggpanel <- switch(priority,
                        left = .subset(full_panel, i - 1L),
                        right = .subset(full_panel, i)
                    )
                }
            }

            # there is no node data in dendrogram root
            if (index <= N) {
                if (is.null(node_labels)) {
                    label <- NA
                } else {
                    label <- node_labels[index - N]
                }
                node <- vec_rbind(
                    node,
                    data_frame0(
                        index = NA,
                        label = label,
                        x = x, y = y, clade = clade, tip = FALSE,
                        panel = panel, ggpanel = ggpanel
                    )
                )
            }

            # if it's the `rectangle`
            if (rectangle) {
                # vertical lines
                # Using `branch` here for consistency with
                # `make_dendrogram_horizontal()`; it will be renamed to `clade`
                # at the final stage.
                vertical_lines <- data_frame0(
                    x = direct_leaves_x,
                    xend = direct_leaves_x,
                    y = direct_leaves_y,
                    yend = rep_len(y, length(direct_leaves_y)),
                    branch = direct_leaves_clade,
                    panel1 = direct_leaves_panel,
                    panel2 = direct_leaves_panel,
                    ggpanel = direct_leaves_ggpanel
                )
                # horizontal lines
                # if the horizontal lines spanned multiple panels
                # we double the left line and the right line
                start_panel <- direct_leaves_panel[1L]
                end_panel <- direct_leaves_panel[length(direct_leaves_panel)]
                start_ggpanel <- direct_leaves_ggpanel[1L]
                end_ggpanel <- direct_leaves_ggpanel[
                    length(direct_leaves_ggpanel)
                ]
                added_edge <- vec_rbind(
                    vertical_lines,
                    # left horizontal line
                    make_dendrogram_horizontal(
                        x0 = min(direct_leaves_x),
                        x1 = x,
                        panel0 = start_panel,
                        panel1 = panel,
                        ggpanel0 = start_ggpanel,
                        ggpanel1 = ggpanel,
                        y = y,
                        branch = clade,
                        panel_ranges = panel_ranges,
                        full_panel = full_panel,
                        double = double
                    ),
                    # right horizontal line
                    make_dendrogram_horizontal(
                        x0 = x,
                        x1 = max(direct_leaves_x),
                        panel0 = panel,
                        panel1 = end_panel,
                        ggpanel0 = ggpanel,
                        ggpanel1 = end_ggpanel,
                        y = y,
                        branch = clade,
                        panel_ranges = panel_ranges,
                        full_panel = full_panel,
                        double = double
                    )
                )
            } else {
                added_edge <- data_frame0( # nocov start
                    x = rep_len(x, 2L),
                    xend = direct_leaves_x,
                    y = rep_len(y, 2L),
                    yend = direct_leaves_y,
                    branch = direct_leaves_clade,
                    panel1 = rep_len(panel, 2L),
                    panel2 = direct_leaves_panel,
                    ggpanel = rep_len(ggpanel, 2L)
                ) # nocov end
            }
            if (is.null(edge)) {
                edge <- added_edge
            } else {
                edge <- vec_rbind(edge, added_edge)
            }
            list(
                node = node, edge = edge, x = x, y = y,
                clade = clade, panel = panel, ggpanel = ggpanel
            )
        } else if (any(select <- child == index)) { # for the tip
            if (is.null(edge_lengths)) {
                y <- 1L
            } else {
                y <- timing
            }
            i <<- i + 1L
            label <- tip_labels[index]
            x <- tip_pos[i] + total_gap
            if (is.null(tip_clades)) {
                clade <- root
            } else {
                clade <- .subset(tip_clades, i)
            }
            # for every new clade, we saved the clade for later use, in order
            # to order the clade levels, and we add a gap between two clade
            if (clade != last_clade) {
                clade_lvls <<- c(clade_lvls, clade)
                x <- x + clade_gap
                total_gap <<- total_gap + clade_gap
            }
            last_clade <<- clade
            node <- data_frame0(
                index = index, label = label,
                x = x, y = y, clade = clade,
                tip = TRUE, panel = clade,
                ggpanel = clade
            )
            list(
                node = node, edge = NULL, x = x, y = y,
                clade = clade, panel = clade, ggpanel = clade
            )
        } else {
            cli_abort(
                "Invalid {.cls phylo} object provided in {.arg {data_arg}}",
                call = call
            )
        }
    }

    # from ape::is.rooted, `N+1` should be the most ancester
    ans <- phylo_data(N + 1L, 0L, timing = 0)
    node <- .subset2(ans, "node")
    edge <- .subset2(ans, "edge")

    # set factor levels for clade and panel ---------------
    panel_lvls <- clade_lvls
    clade_lvls <- c(clade_lvls, root)
    node$panel <- factor(.subset2(node, "panel"), panel_lvls)
    node$clade <- factor(.subset2(node, "clade"), clade_lvls)
    node$ggpanel <- factor(.subset2(node, "ggpanel"), panel_lvls)
    if (!is.null(edge)) {
        edge$panel1 <- factor(.subset2(edge, "panel1"), panel_lvls)
        edge$panel2 <- factor(.subset2(edge, "panel2"), panel_lvls)
        edge$clade <- factor(.subset2(edge, "branch"), clade_lvls)
        edge$branch <- NULL
        edge$ggpanel <- factor(.subset2(edge, "ggpanel"), panel_lvls)
    }
    node <- rename(node, c(ggpanel = ".panel", index = ".index"))
    edge <- rename(edge, c(ggpanel = ".panel"))
    ggalign_data_set(node, edge = edge)
}
