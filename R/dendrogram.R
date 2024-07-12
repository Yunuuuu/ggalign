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
#' @export
hclust2 <- function(matrix,
                    distance = "euclidean",
                    method = "complete",
                    use_missing = "pairwise.complete.obs") {
    if (rlang::is_string(distance)) {
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
    if (rlang::is_string(method)) {
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
#' @param mapping Additional aesthetic mappings to be added into
#' [geom_segment][ggplot2::geom_segment].
#' @param ... Additional arguments passed to
#' [geom_segment][ggplot2::geom_segment]
#' @inheritParams dendrogram_data
#' @return A [ggplot][ggplot2::ggplot] object.
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' ggdendrogram(hc)
#' ggdendrogram(hc, leaf_label = FALSE)
#' ggdendrogram(hc, aes(color = branch),
#'     leaf_braches = cutree(hc, 4L), type = "t"
#' )
#' ggdendrogram(hc, aes(color = branch),
#'     leaf_braches = cutree(hc, 4L), type = "r"
#' )
#' @importFrom ggplot2 waiver
#' @export
ggdendrogram <- function(tree, mapping = NULL, ...,
                         center = FALSE, type = "rectangle",
                         cutree_k = NULL, cutree_height = NULL,
                         leaf_braches = NULL, branch_gap = NULL, root = NULL,
                         leaf_label = TRUE, leaf_guide = waiver()) {
    assert_s3_class(tree, "hclust")
    if (is.null(leaf_braches) &&
        (!is.null(cutree_k) || !is.null(cutree_height))) {
        leaf_braches <- stats::cutree(tree, k = cutree_k, h = cutree_height)
    }
    data <- dendrogram_data(tree,
        center = center,
        type = type, leaf_pos = NULL,
        leaf_braches = leaf_braches,
        branch_gap = branch_gap,
        root = root
    )

    node <- .subset2(data, "node")
    edge <- .subset2(data, "edge")
    default_mapping <- ggplot2::aes(x = .data$x, y = .data$y)
    edge_mapping <- ggplot2::aes(xend = .data$xend, yend = .data$yend)
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
#' @param root A length one string or numeric indicates the root branch.
#' @return A list of 2 data.frame. One for node coordinates, another for edge
#' coordinates.
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
    if (grid::is.unit(branch_gap)) {
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

            x <- .subset(leaf_pos, i)
            # for every new branch, we saved the branch for later use, in order
            # to order the branch levels, and we add a gap between two branch
            if (branch != last_branch) {
                branch_levels <<- c(branch_levels, branch)
                x <- x + branch_gap
            }
            last_branch <<- branch

            node <- data.frame(
                index = index, label = label,
                x = x, y = y, branch = branch,
                leaf = TRUE,
                stringsAsFactors = FALSE
            )
            list(
                # all leaves, used to calculate midpoint when `center = TRUE`
                children_leaves = x,
                # current node
                node = node, edge = NULL,
                x = x, y = y, branch = branch
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
            children_leaves <- unlist(
                .subset2(data, "children_leaves"),
                recursive = FALSE, use.names = FALSE
            )

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

            # prepare node data ------------------------------------
            # x coordinate for current branch: the midpoint
            if (center) {
                x <- sum(range(children_leaves)) / 2L
            } else {
                x <- sum(direct_leaves_x) / 2L
            }
            if (is.null(leaf_braches)) {
                branch <- root
            } else {
                branch <- unique(direct_leaves_branch)
                # if two children leaves are different, this branch should be
                # root
                if (length(branch) > 1L) branch <- root
            }
            # there is no node data in dendrogram root
            if (!from_root) {
                node <- rbind(node, data.frame(
                    index = NA_integer_, label = NA_character_,
                    x = x, y = y, branch = branch, leaf = FALSE
                ))
            }
            if (rectangle) {
                span_branch <- c(direct_leaves_branch, rep_len(branch, 2L))
                added_edge <- data.frame(
                    # 2 vertical lines + 2 horizontal lines
                    x = rep(direct_leaves_x, times = 2L),
                    xend = c(direct_leaves_x, rep_len(x, 2L)),
                    y = c(direct_leaves_y, y, y),
                    yend = rep_len(y, 4L),
                    branch = rep(direct_leaves_branch, times = 2L),

                    # we add `span` indicating whether this segment span
                    # multiple panels
                    #
                    # for vertical lines, we chech the children branch only
                    # since vertical lines should only have one branch,
                    # if it's `root` then the line should span multiple
                    # panels.
                    #
                    # For horizontal liens, we check the parent branch,
                    # since horizontal lines should have two branch points,
                    # and the parent branch will be more highest, if it's
                    # `root`, then the horizontal lines should span multiple
                    # panels.
                    span = span_branch == root
                )
            } else {
                span_branch <- rep_len(branch, 2L)
                added_edge <- data.frame(
                    x = direct_leaves_x,
                    xend = rep(x, 2L),
                    y = direct_leaves_y,
                    yend = rep_len(y, 2L),
                    branch = direct_leaves_branch,
                )
            }
            if (is.null(leaf_braches)) { # all nodes should be root node
                added_edge$span <- FALSE
            } else {
                added_edge$span <- span_branch == root
            }
            if (is.null(edge)) {
                edge <- added_edge
            } else {
                edge <- rbind(edge, added_edge)
            }
            list(
                children_leaves = children_leaves,
                node = node, edge = edge,
                x = x, y = y, branch = branch
            )
        } else {
            cli::cli_abort("{.arg dend} must be a {.cls dendrogram} object")
        }
    }
    ans <- .subset(.dendrogram_data(dend), c("node", "edge"))
    # 1. remove rownames to keep data tidy
    # 2. branch should be a factor ordered by x if it exists
    lapply(ans, function(df) {
        rownames(df) <- NULL
        df$branch <- factor(.subset2(df, "branch"), branch_levels)
        df
    })
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
