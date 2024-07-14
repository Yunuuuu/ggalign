#' Generate Tree Structures with Hierarchical Clustering
#'
#' @param matrix A numeric matrix, or data frame.
#' @param distance A string of distance measure to be used. This must be one of
#' "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#' Correlation coefficient can be also used, including "pearson", "spearman" or
#' "kendall". In this way, `1 - cor` will be used as the distance. In addition,
#' you can also provide a [dist][stats::dist] object directly or a function
#' return a [dist][stats::dist] object.
#' @param method A string of the agglomeration method to be used. This should be
#' (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single",
#' "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or
#' "centroid" (= UPGMC). you can also provide a function which returns a
#' [hclust][stats::hclust] object.
#' @param use_missing An optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation
#' of) one of the strings "everything", "all.obs", "complete.obs",
#' "na.or.complete", or "pairwise.complete.obs". Only used when `distance` is a
#' correlation coefficient string.
#' @seealso
#'  - [cor][stats::cor]
#'  - [dist][stats::dist]
#'  - [hclust][stats::hclust]
#' @return A [hclust][stats::hclust] object.
#' @importFrom rlang is_string
#' @export
hclust2 <- function(matrix,
                    distance = "euclidean",
                    method = "complete",
                    use_missing = "pairwise.complete.obs") {
    if (is_string(distance)) {
        distance <- match.arg(
            distance, c(
                "euclidean", "maximum", "manhattan", "canberra",
                "binary", "minkowski", "pearson", "spearman", "kendall"
            )
        )
        d <- switch(distance,
            euclidean = ,
            maximum = ,
            manhattan = ,
            canberra = ,
            binary = ,
            minkowski = stats::dist(matrix, method = distance),
            pearson = ,
            spearman = ,
            kendall = stats::as.dist(
                1 - stats::cor(t(matrix), use = use_missing, method = distance)
            ),
            cli::cli_abort("Unsupported {.arg distance} method specified")
        )
    } else if (is.function(distance)) {
        d <- distance(matrix)
        if (inherits(distance, "dist")) {
            cli::cli_abort("{.arg distance} must return a {.cls dist} object")
        }
    } else if (inherits(distance, "dist")) {
        d <- distance
    } else {
        cli::cli_abort(paste(
            "{.arg distance} can only be a {.cls string}, {.cls dist}",
            "object, or a {.cls function} return {.cls dist}"
        ))
    }
    if (is_string(method)) {
        ans <- stats::hclust(d, method = method)
    } else if (is.function(method)) {
        ans <- method(d)
        if (inherits(ans, "hclust")) {
            cli::cli_abort("{.arg method} must return a {.cls hclust} object")
        }
    } else {
        cli::cli_abort(paste(
            "{.arg method} can only be a {.cls string},",
            "or a {.cls function} return {.cls hclust}"
        ))
    }
    ans
}

#' Dendrogram plot
#'
#' @param tree A [hclust][stats::hclust] object.
#' @param mapping Default list of aesthetic mappings to use for plot.
#' @param ... Additional arguments passed to
#' [geom_segment][ggplot2::geom_segment]
#' @param k An integer scalar indicates the desired number of groups.
#' @param h A numeric scalar indicates heights where the tree should be cut.
#' @param plot_cut_height A boolean value indicates whether plot the cut height.
#' @inheritParams dendrogram_data
#' @param leaf_label A boolean value indicates whether plot the leaf labels.
#' @param leaf_guide A function used to create the leaf label guide.  Passed to
#' [scale_x_continuous][ggplot2::scale_x_continuous].
#' @return A [ggplot][ggplot2::ggplot] object.
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' ggdendrogram(hc)
#' ggdendrogram(hc, aes(color = branch), h = 80)
#' ggdendrogram(hc, leaf_label = FALSE)
#' ggdendrogram(hc, aes(color = branch),
#'     leaf_braches = cutree(hc, 4L), type = "t"
#' )
#' ggdendrogram(hc, aes(color = branch),
#'     leaf_braches = cutree(hc, 4L), type = "r"
#' )
#' @importFrom ggplot2 waiver aes
#' @export
ggdendrogram <- function(tree, mapping = NULL, ...,
                         center = FALSE, type = "rectangle",
                         k = NULL, h = NULL,
                         plot_cut_height = NULL,
                         leaf_braches = NULL, branch_gap = NULL, root = NULL,
                         leaf_label = TRUE, leaf_guide = waiver()) {
    assert_s3_class(tree, "hclust")
    assert_bool(plot_cut_height, null_ok = TRUE)
    assert_bool(leaf_label)
    if (is.null(leaf_braches)) {
        if (!is.null(k)) {
            if (!is_scalar(k)) {
                cli::cli_abort("{.arg k} must be a single number")
            }
            height <- cutree_k_to_h(tree, k)
            leaf_braches <- stats::cutree(tree, h = height)
        } else if (!is.null(h)) {
            if (!is_scalar(height <- h)) {
                cli::cli_abort("{.arg h} must be a single number")
            }
            leaf_braches <- stats::cutree(tree, h = height)
        } else {
            height <- NULL
        }
    } else {
        height <- cutree_k_to_h(tree, length(unique(leaf_braches)))
    }
    plot_cut_height <- plot_cut_height %||% !is.null(height)
    data <- dendrogram_data(tree,
        center = center,
        type = type, leaf_pos = NULL,
        leaf_braches = leaf_braches,
        branch_gap = branch_gap,
        root = root
    )

    node <- .subset2(data, "node")
    edge <- .subset2(data, "edge")
    default_mapping <- aes(x = .data$x, y = .data$y)
    edge_mapping <- aes(xend = .data$xend, yend = .data$yend)
    if (inherits(mapping, "uneval")) {
        for (nm in names(mapping)) {
            edge_mapping[[nm]] <- .subset2(mapping, nm)
            default_mapping[[nm]] <- .subset2(mapping, nm)
        }
    } else if (!is.null(mapping)) {
        cli::cli_abort(c(
            "{.arg mapping} must be created with {.fn aes}.",
            "x" = "You've supplied {.obj_type_friendly {mapping}}."
        ))
    }
    p <- ggplot2::ggplot(node, default_mapping) +
        ggplot2::geom_segment(
            mapping = edge_mapping,
            ...,
            stat = "identity",
            data = edge
        ) +
        ggplot2::labs(y = "height")
    if (plot_cut_height && !is.null(height)) {
        p <- p + ggplot2::geom_hline(yintercept = height, linetype = "dashed")
    }
    if (leaf_label) {
        leaves <- node[.subset2(node, "leaf"), , drop = FALSE]
        leaves <- leaves[order(.subset2(leaves, "x")), , drop = FALSE]
        p <- p + ggplot2::scale_x_continuous(
            name = NULL,
            breaks = .subset2(leaves, "x"),
            labels = .subset2(leaves, "label"),
            guide = leaf_guide
        )
    }
    p
}

cutree_k_to_h <- function(tree, k) {
    if (is.null(n1 <- nrow(tree$merge)) || n1 < 1) {
        cli::cli_abort("invalid {.arg tree} ({.field merge} component)")
    }
    n <- n1 + 1
    if (is.unsorted(tree$height)) {
        cli::cli_abort(
            "the 'height' component of 'tree' is not sorted (increasingly)"
        )
    }
    mean(tree$height[c(n - k, n - k + 1L)])
}

#' Dengrogram x and y coordinates
#'
#' @param tree A [hclust][stats::hclust] or a [dendrogram][stats::as.dendrogram]
#' object.
#' @param center A boolean value. if `TRUE`, nodes are plotted centered with
#' respect to the leaves in the branch. Otherwise (default), plot them in the
#' middle of all direct child nodes.
#' @param type A string indicates the plot type, `"rectangle"` or `"triangle"`.
#' @param leaf_pos The x-coordinates of the leaf node. Must be the same length
#' of the number of observations in `tree`.
#' @param leaf_braches Branches of the leaf node. Must be the same length of the
#' number of observations in `tree`. Usually come from [cutree][stats::cutree].
#' @param branch_gap A numeric value indicates the gap between different
#' branches. If a [unit] object is provided, it'll convert into `native` values.
#' @param root A length one string or numeric indicates the root branch.
#' @return A list of 2 data.frame. One for node coordinates, another for edge
#' coordinates.
#' @examples
#' dendrogram_data(hclust(dist(USArrests), "ave"))
#' @importFrom grid is.unit
#' @export
dendrogram_data <- function(tree, center = FALSE,
                            type = "rectangle",
                            leaf_pos = NULL,
                            leaf_braches = NULL,
                            branch_gap = NULL,
                            root = NULL) {
    dend <- check_tree(tree)
    assert_bool(center)
    type <- match.arg(type, c("rectangle", "triangle"))
    N <- stats::nobs(dend)
    rectangle <- type == "rectangle"
    if (is.null(leaf_pos)) {
        leaf_pos <- seq_len(N)
    } else if (length(leaf_pos) != N) {
        cli::cli_abort(
            "{.arg leaf_pos} must be of the same length of {.arg tree}"
        )
    }

    # if no branches provided, all branch will be regarded as the root branch
    if (is.null(leaf_braches)) {
        root <- root %||% "root"
    } else if (anyNA(leaf_braches)) {
        cli::cli_abort("`NA` is not allowed in {.arg leaf_braches}")
    } else if (length(leaf_braches) != N) {
        cli::cli_abort(
            "{.arg leaf_braches} must be of the same length of {.arg tree}"
        )
    } else if (is.character(leaf_braches)) {
        root <- root %||% "root"
    } else if (is.factor(leaf_braches)) {
        leaf_braches <- as.character(leaf_braches)
        root <- root %||% "root"
    } else if (is.numeric(leaf_braches)) {
        root <- root %||% (min(leaf_braches) - 1L)
    }
    # branch_gap must be a numeric value
    # and the length must be equal to `length(unique(leaf_braches)) - 1L`
    if (is.unit(branch_gap)) {
        branch_gap <- grid::convertUnit(branch_gap, "native", valueOnly = TRUE)
    } else if (is.numeric(branch_gap)) {
        if (!is_scalar(branch_gap) &&
            !is.null(leaf_braches) &&
            length(branch_gap) != (length(unique(leaf_braches)) - 1L)) {
            cli::cli_abort(paste(
                "{.arg branch_gap} must be of length",
                "{.code length(unique(leaf_braches)) - 1}"
            ))
        }
    } else if (is.null(branch_gap)) {
        branch_gap <- 0
    } else {
        cli::cli_abort("{.arg branch_gap} must be numeric value.")
    }

    if (!is_scalar(root)) {
        cli::cli_abort("{.arg root} must be of length 1")
    } else if (anyNA(root)) {
        cli::cli_abort("{.arg root} cannot be `NA`")
    } else if (any(root == leaf_braches)) {
        cli::cli_abort(
            "{.arg root} cannot contain value in {.arg leaf_braches}"
        )
    }
    i <- 0L # leaf index
    branch_levels <- root
    last_branch <- root
    total_gap <- 0 # will made x always be numeric
    .dendrogram_data <- function(dend, from_root = TRUE) {
        if (stats::is.leaf(dend)) { # base version
            index <- as.integer(dend) # the column index of the original data
            y <- attr(dend, "height") %||% 0
            label <- attr(dend, "label") %||% NA_character_
            i <<- i + 1L
            if (is.null(leaf_braches)) {
                branch <- root
            } else {
                branch <- .subset(leaf_braches, index)
            }

            x <- .subset(leaf_pos, i) + total_gap
            # for every new branch, we saved the branch for later use, in order
            # to order the branch levels, and we add a gap between two branch
            if (branch != last_branch) {
                branch_levels <<- c(branch_levels, branch)
                x <- x + branch_gap
                total_gap <<- total_gap + branch_gap
            }
            last_branch <<- branch

            node <- data_frame0(
                index = index, label = label,
                x = x, y = y, branch = branch,
                leaf = TRUE, panel = branch,
                ggpanel = branch
            )
            list(
                # current node
                node = node, edge = NULL,
                x = x, y = y,
                branch = branch,
                panel = branch,
                ggpanel = branch
            )
        } else if (inherits(dend, "dendrogram")) { # recursive version
            # the parent height  -------------------------------------
            y <- attr(dend, "height")

            # for the children nodes ---------------------------------
            data <- transpose(lapply(dend, .dendrogram_data, from_root = FALSE))

            # node should be the direct children
            node <- do.call(rbind, .subset2(data, "node"))
            edge <- do.call(rbind, .subset2(data, "edge"))

            # all x coordinate for children nodes --------------------
            # used if center is `TRUE`, we'll calculate the center position
            # among all children nodes
            leaves <- node[.subset2(node, "leaf"), ]

            # all coordinate for direct children nodes -------------
            # following should be length 2
            direct_leaves_x <- unlist(
                .subset2(data, "x"),
                recursive = FALSE, use.names = FALSE
            )
            direct_leaves_y <- unlist(
                .subset2(data, "y"),
                recursive = FALSE, use.names = FALSE
            )
            direct_leaves_branch <- unlist(
                .subset2(data, "branch"),
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

            # prepare node data ------------------------------------
            # x coordinate for current branch: the midpoint
            if (center) {
                x <- sum(range(.subset2(leaves, "x"))) / 2L
            } else {
                x <- sum(direct_leaves_x) / 2L
            }
            if (is.null(leaf_braches)) { # only one panel
                ggpanel <- panel <- branch <- root
            } else {
                branch <- unique(direct_leaves_branch)
                # if two children leaves are different, this branch should be
                # root
                if (length(branch) > 1L) branch <- root

                # we check the panel of current branch (used by ggplot2)
                left_panel <- .subset(direct_leaves_panel, 1L)
                right_panel <- .subset(direct_leaves_panel, 2L)
                if (anyNA(direct_leaves_panel) || left_panel != right_panel) {
                    ranges <- split(
                        .subset2(leaves, "x"),
                        .subset2(leaves, "panel")
                    )
                    ranges <- ranges[order(vapply(ranges, min, numeric(1L)))]
                    panel <- NA
                    for (panel in names(ranges)) {
                        if (x < min(.subset2(ranges, panel))) {
                            panel <- NA
                            break
                        } else if (x <= max(.subset2(ranges, panel))) {
                            break
                        }
                    }
                } else {
                    panel <- left_panel
                }
                # above is the real panel, but for ggplot2, NA value will create
                #     another panel, so patchwork align won't work
                #     here we always using the right panel
                if (is.na(ggpanel <- panel)) {
                    # if the real panel is NA, we choose the right panel
                    ggpanel <- .subset(direct_leaves_ggpanel, 2L)
                }
            }
            # there is no node data in dendrogram root
            if (!from_root) {
                node <- rbind(node, data_frame0(
                    index = NA_integer_, label = NA_character_,
                    x = x, y = y, branch = branch, leaf = FALSE,
                    panel = panel, ggpanel = ggpanel
                ))
            }
            if (rectangle) {
                added_edge <- data_frame0(
                    # 2 vertical lines + 2 horizontal lines
                    x = c(direct_leaves_x, rep_len(x, 2L)),
                    xend = rep(direct_leaves_x, times = 2L),
                    y = c(direct_leaves_y, y, y),
                    yend = rep_len(y, 4L),
                    branch = rep(direct_leaves_branch, times = 2L),
                    ggpanel = c(direct_leaves_ggpanel, rep_len(ggpanel, 2L)),
                    panel1 = c(direct_leaves_panel, rep_len(panel, 2L)),
                    panel2 = rep(direct_leaves_panel, times = 2L)
                )
            } else {
                added_edge <- data_frame0(
                    x = rep_len(x, 2L),
                    xend = direct_leaves_x,
                    y = direct_leaves_y,
                    yend = rep_len(y, 2L),
                    branch = direct_leaves_branch,
                    ggpanel = rep_len(ggpanel, 2L),
                    panel1 = rep_len(panel, 2L),
                    panel2 = direct_leaves_panel
                )
            }
            if (is.null(edge)) {
                edge <- added_edge
            } else {
                edge <- rbind(edge, added_edge)
            }
            list(
                node = node, edge = edge,
                x = x, y = y, branch = branch,
                panel = panel, ggpanel = ggpanel
            )
        } else {
            cli::cli_abort("{.arg dend} must be a {.cls dendrogram} object")
        }
    }
    ans <- .subset(.dendrogram_data(dend), c("node", "edge"))

    # set factor levels
    panel_levels <- setdiff(branch_levels, root)
    ans <- lapply(ans, function(df) {
        df$branch <- factor(.subset2(df, "branch"), branch_levels)
        df$ggpanel <- factor(.subset2(df, "ggpanel"), panel_levels)
        rownames(df) <- NULL
        df
    })
    ans$node$panel <- factor(
        .subset2(.subset2(ans, "node"), "panel"),
        panel_levels
    )
    ans$edge$panel1 <- factor(
        .subset2(.subset2(ans, "edge"), "panel1"),
        panel_levels
    )
    ans$edge$panel2 <- factor(
        .subset2(.subset2(ans, "edge"), "panel2"),
        panel_levels
    )
    ans
}

check_tree <- function(tree, arg = rlang::caller_arg(tree),
                       call = rlang::caller_call()) {
    if (inherits(tree, "hclust")) {
        stats::as.dendrogram(tree)
    } else if (inherits(tree, "dendrogram")) {
        tree
    } else {
        cli::cli_abort(paste(
            "{.arg {arg}} must be a {.cls hclust}",
            "or a {.cls dendrogram} object."
        ))
    }
}
