#' Reorder or Group observations based on hierarchical clustering
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function aligns observations within the layout according to a
#' hierarchical clustering tree, enabling reordering or grouping of elements
#' based on clustering results.
#'
#' @param data A matrix-like object. By default, it inherits from the layout
#'   `matrix`.
#' @inheritParams align_discrete
#' @inheritParams hclust2
#' @param reorder_dendrogram A single boolean value indicating whether to
#' reorder the dendrogram based on the means. Alternatively, you can provide a
#' custom function that accepts an [`hclust`][stats::hclust] object and the data
#' used to generate the tree, returning either an [`hclust`][stats::hclust] or
#' [`dendrogram`][stats::as.dendrogram] object. Default is `FALSE`.
#' @param reorder_group A single boolean value, indicates whether we should do
#' Hierarchical Clustering between groups, only used when previous groups have
#' been established. Default: `FALSE`.
#' @param k An integer scalar indicates the desired number of groups.
#' @param h A numeric scalar indicates heights where the tree should be cut.
#' @param cutree A function used to cut the [`hclust`][stats::hclust] tree. It
#' should accept four arguments: the [`hclust`][stats::hclust] tree object,
#' `distance` (only applicable when `method` is a string or a function for
#' performing hierarchical clustering), `k` (the number of clusters), and `h`
#' (the height at which to cut the tree). By default,
#' [`cutree()`][stats::cutree()] is used.
#' @inheritSection align_discrete Axis Alignment for Observations
#' @seealso
#' - [`dendrogram_data()`]
#' - [`hclust2()`]
#' @examples
#' # align_hclust won't add a dendrogram
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_hclust(k = 3L)
#' @export
align_hclust <- function(distance = "euclidean",
                         method = "complete",
                         use_missing = "pairwise.complete.obs",
                         reorder_dendrogram = FALSE,
                         reorder_group = FALSE,
                         k = NULL, h = NULL, cutree = NULL,
                         data = NULL, active = NULL) {
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
    assert_bool(reorder_group)
    cutree <- allow_lambda(cutree)
    assert_(cutree, is.function, "a function", allow_null = TRUE)
    if (is.null(data)) {
        if (inherits(method, "hclust") || inherits(method, "dendrogram")) {
            data <- NULL # no need for data
        } else {
            data <- waiver()
        }
    }
    assert_active(active)
    active <- update_active(active, new_active(use = FALSE))
    align_discrete(
        align = AlignHclust,
        params = list(
            distance = distance, method = method, use_missing = use_missing,
            k = k, h = h,
            reorder_dendrogram = reorder_dendrogram,
            reorder_group = reorder_group,
            cutree = cutree,
            # used by align_dendro
            plot_cut_height = FALSE,
            merge_dendro = FALSE
        ),
        active = active,
        schemes = default_schemes(),
        data = data,
        plot = NULL
    )
}

#' @export
summary.AlignHclust <- function(object, ...) {
    params <- .subset2(object, "input_params")
    c(TRUE, !is.null(.subset2(params, "k")) || !is.null(.subset2(params, "h")))
}

#' @importFrom ggplot2 ggproto aes
AlignHclust <- ggproto("AlignHclust", AlignDiscrete,
    #' @importFrom stats reorder
    setup_params = function(self, nobs, params) {
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
                if (!inherits(ans, "hclust") &&
                    !inherits(ans, "dendrogram")) {
                    cli_abort(
                        "{.fn reorder_dendrogram} must return a {.cls hclust} or {.cls dendrogram} object",
                        call = self$call
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
            cli_abort(c(
                "Cannot do Hierarchical Clustering",
                i = "must have >= 2 observations to cluster"
            ), call = .subset2(self, "call"))
        }
        # if the old panel exist, we do sub-clustering
        if (!is.null(panel) && is.null(k) && is.null(h) && is.null(cutree)) {
            if (is.null(data)) {
                cli_abort(c(
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
    align = function(self, panel, index, distance, method, use_missing,
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
                # we have a function named merge_dendrogram(), so we use
                # parameter `merge_dendro`
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
    }
)
