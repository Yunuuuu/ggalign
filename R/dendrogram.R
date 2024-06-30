#' Generate Tree Structures with Hierarchical Clustering
#'
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
hclust2 <- function(data,
                    distance = "euclidean",
                    method = "complete",
                    use_missing = "pairwise.complete.obs") {
    data <- build_matrix(data)
    if (rlang::is_string(distance)) {
        d <- switch(distance,
            euclidean = ,
            maximum = ,
            manhattan = ,
            canberra = ,
            binary = ,
            minkowski = stats::dist(data, method = distance),
            pearson = ,
            spearman = ,
            kendall = stats::as.dist(
                1 - stats::cor(t(data), use = use_missing, method = distance)
            ),
            cli::cli_abort("Unsupported {.arg distance} method specified")
        )
    } else if (is.function(distance)) {
        d <- distance(data)
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
#' ggdendrogram(hc, aes(color = branch),
#'     leaf_braches = cutree(hc, 4L), type = "t"
#' )
#' ggdendrogram(hc, aes(color = branch),
#'     leaf_braches = cutree(hc, 4L), type = "r"
#' )
#' @export
ggdendrogram <- function(tree, mapping = NULL, ..., center = FALSE,
                         type = "rectangle", leaf_pos = NULL,
                         leaf_braches = NULL, root = NULL,
                         leaf_label = TRUE,
                         leaf_label_angle = -60L,
                         leaf_label_hjust = 0L) {
    data <- dendrogram_data(tree,
        center = center,
        type = type, leaf_pos = leaf_pos,
        leaf_braches = leaf_braches, root = root
    )
    node <- .subset2(data, "node")
    edge <- .subset2(data, "edge")
    edge_mapping <- ggplot2::aes(xend = .data$xend, yend = .data$yend)
    if (inherits(mapping, "uneval")) {
        for (nm in names(mapping)) edge_mapping[[nm]] <- .subset2(mapping, nm)
    } else if (!is.null(mapping)) {
        cli::cli_abort(c(
            "{.arg mapping} must be created with {.fn aes}.",
            "x" = "You've supplied {.obj_type_friendly {mapping}}."
        ))
    }
    p <- ggplot2::ggplot(node, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_segment(
            mapping = edge_mapping,
            ...,
            stat = "identity",
            data = edge
        )
    if (leaf_label) {
        leaves <- node[.subset2(node, "leaf"), , drop = FALSE]
        leaves <- leaves[order(.subset2(leaves, "x")), , drop = FALSE]
        p <- p + ggplot2::scale_x_continuous(
            name = NULL,
            breaks = .subset2(leaves, "x"),
            labels = .subset2(leaves, "label"),
            guide = ggplot2::guide_axis(
                theme = ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        # colour = colour,
                        angle = leaf_label_angle,
                        hjust = leaf_label_hjust
                    )
                )
            )
        )
    }
    p
}

ggscale_map <- function(plot, scale, value) {
    build <- ggplot2::ggplot_build(plot)
    if (!is.null(scale <- build$plot$scales$get_scales(scale))) {
        scale$map(value)
    } else {
        cli::cli_abort("Cannot find {.field {scale}}")
    }
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
#' @return A list of 2 data.frames. One for node coordinates, another for edge
#' coordinates.
dendrogram_data <- function(tree, center = FALSE,
                            type = "rectangle",
                            leaf_pos = NULL,
                            leaf_braches = NULL,
                            root = NULL) {
    dend <- check_tree(tree)
    N <- stats::nobs(dend)
    i <- 0L # leaf index
    type <- match.arg(type, c("rectangle", "triangle"))
    rectangle <- type == "rectangle"
    if (is.null(leaf_pos)) {
        leaf_pos <- seq_len(N)
    } else if (length(leaf_pos) != N) {
        cli::cli_abort(
            "{.arg leaf_pos} must be of the same length of {.arg tree}"
        )
    }
    if (is.null(leaf_braches)) {
        root <- root %||% "root"
        branch_levels <- NULL
    } else if (length(leaf_braches) != N) {
        cli::cli_abort(
            "{.arg leaf_braches} must be of the same length of {.arg tree}"
        )
    } else if (is.factor(leaf_braches)) {
        branch_levels <- levels(leaf_braches)
        leaf_braches <- as.character(leaf_braches)
        root <- root %||% "root"
    } else if (is.character(leaf_braches)) {
        root <- root %||% "root"
        branch_levels <- sort(unique(leaf_braches))
    } else if (is.numeric(leaf_braches)) {
        root <- root %||% (min(leaf_braches) - 1L)
        branch_levels <- sort(unique(leaf_braches))
    }
    if (!is_scalar(root)) {
        cli::cli_abort("{.arg root} must be of length 1")
    } else if (any(root == branch_levels)) {
        cli::cli_abort(
            "{.arg root} cannot contain value in {.arg leaf_braches}"
        )
    }
    branch_levels <- c(root, branch_levels)
    .dendrogram_data <- function(dend) {
        if (stats::is.leaf(dend)) { # base version
            index <- as.integer(dend) # the column index of the original data
            y <- attr(dend, "height") %||% 0
            label <- attr(dend, "label") %||% NA_character_
            i <<- i + 1L
            x <- .subset(leaf_pos, i)
            branch <- .subset(leaf_braches, index) %||% root
            node <- data.frame(
                index = index, label = label,
                x = x, y = y, branch = branch,
                leaf = TRUE, stringsAsFactors = FALSE
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
            data <- transpose(lapply(dend, .dendrogram_data))
            # node should be the direct children
            node <- do.call(rbind, .subset2(data, "node"))
            edge <- do.call(rbind, .subset2(data, "edge"))

            # all x coordinate for children nodes --------------------
            children_leaves <- unlist(
                .subset2(data, "children_leaves"),
                recursive = FALSE, use.names = FALSE
            )

            # all coordinate for direct children nodes -------------
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

            # horizontal line ----------------------------------------
            # extend of current horizontal line

            # x coordinate current tree branch: the midpoint
            if (center) {
                x <- sum(range(children_leaves)) / 2L
            } else {
                x <- sum(direct_leaves_x) / 2L
            }
            branch <- unique(direct_leaves_branch)
            if (length(branch) > 1L) branch <- root
            node <- rbind(
                node,
                data.frame(
                    index = NA_integer_, label = NA_character_,
                    x = x, y = y, branch = branch, leaf = FALSE
                )
            )
            if (rectangle) {
                added_edge <- data.frame(
                    x = rep(direct_leaves_x, times = 2L),
                    xend = c(direct_leaves_x, x, x),
                    y = c(direct_leaves_y, y, y),
                    yend = rep_len(y, 4L),
                    branch = rep(direct_leaves_branch, times = 2L)
                )
            } else {
                added_edge <- data.frame(
                    x = direct_leaves_x,
                    xend = rep_len(x, 2L),
                    y = direct_leaves_y,
                    yend = rep_len(y, 2L),
                    branch = direct_leaves_branch
                )
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
    # remove rowname to keep data tidy and branch should be a factor
    lapply(.subset(.dendrogram_data(dend), c("node", "edge")), function(df) {
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
