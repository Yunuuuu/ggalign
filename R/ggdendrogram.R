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
