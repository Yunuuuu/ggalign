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
#' @inheritParams align
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
#' @inheritSection align Discrete Axis Alignment
#' @seealso [`hclust2()`]
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
    assert_active(active)
    active <- update_active(active, new_active(use = FALSE))
    .align_hclust(
        align = AlignHclust,
        distance = distance,
        method = method,
        use_missing = use_missing,
        reorder_dendrogram = reorder_dendrogram,
        reorder_group = reorder_group,
        k = k, h = h, cutree = cutree, data = data, active = active
    )
}

.align_hclust <- function(align, ..., plot = NULL,
                          distance = "euclidean",
                          method = "complete",
                          use_missing = "pairwise.complete.obs",
                          reorder_dendrogram = FALSE,
                          reorder_group = FALSE,
                          k = NULL, h = NULL, cutree = NULL,
                          data = NULL, schemes = NULL, active = NULL,
                          call = caller_call()) {
    reorder_dendrogram <- allow_lambda(reorder_dendrogram)
    if (!rlang::is_bool(reorder_dendrogram) &&
        !is.null(reorder_dendrogram) &&
        !is.function(reorder_dendrogram)) {
        cli_abort(
            "{.arg reorder_dendrogram} must be a single boolean value or a function",
            call = call
        )
    }
    assert_number_whole(k, allow_null = TRUE, call = call)
    assert_number_decimal(h, allow_null = TRUE, call = call)
    assert_bool(reorder_group, call = call)
    cutree <- allow_lambda(cutree)
    assert_(cutree, is.function, "a function", allow_null = TRUE, call = call)
    if (inherits(method, "hclust")) {
        if (vec_size(.subset2(method, "order")) == 0L) {
            cli_abort("{.cls hclust} defined in {.arg method} cannot be empty",
                call = call
            )
        }
    } else if (inherits(method, "dendrogram")) {
        if (stats::nobs(method) == 0L) {
            cli_abort(
                "{.cls dendrogram} defined in {.arg method} cannot be empty",
                call = call
            )
        }
    }

    if (isTRUE(reorder_dendrogram)) {
        reorder_dendrogram <- function(tree, data) {
            if (!inherits(tree, "dendrogram")) {
                tree <- stats::as.dendrogram(tree)
            }
            reorder(x = tree, wts = rowMeans(data), agglo.FUN = mean)
        }
    } else if (is.function(reorder_dendrogram)) {
        user_reorder <- reorder_dendrogram
        reorder_dendrogram <- function(tree, data) {
            # we ensure, what we input for user is a `hclust` object.
            if (!inherits(tree, "hclust")) tree <- stats::as.hclust(tree)
            ans <- user_reorder(tree, data)
            if (!inherits(ans, "hclust") &&
                !inherits(ans, "dendrogram")) {
                cli_abort(
                    "{.fn reorder_dendrogram} must return a {.cls hclust} or {.cls dendrogram} object",
                    call = call
                )
            }
            ans
        }
    }

    align(
        align = align,
        distance = distance, method = method,
        use_missing = use_missing,
        reorder_dendrogram = reorder_dendrogram,
        reorder_group = reorder_group,
        k = k, h = h, cutree = cutree,
        active = active,
        ..., # additional fields to be added, used by align_dendro
        schemes = schemes %||% default_schemes(),
        data = data,
        plot = plot,
        call = call
    )
}

#' @importFrom ggplot2 ggproto aes
AlignHclust <- ggproto("AlignHclust", Align,
    interact_layout = function(self, layout) {
        if (inherits(self$method, "hclust") ||
            inherits(self$method, "dendrogram")) {
            layout <- ggproto_parent(Align, self)$interact_layout(layout)
            if (inherits(self$method, "hclust")) {
                nobs <- vec_size(.subset2(self$method, "order"))
            } else {
                nobs <- stats::nobs(self$method)
            }

            if (is.null(layout_nobs <- .subset2(layout@design, "nobs"))) {
                layout@design["nobs"] <- list(nobs)
            } else {
                assert_mismatch_nobs(self, layout_nobs, nobs, arg = "method")
            }
        } else {
            layout <- ggproto_parent(AlignReorder, self)$interact_layout(layout)
        }

        # initialize the internal parameters
        self$multiple_tree <- FALSE
        self$height <- NULL
        self$panel <- NULL
        layout
    },
    compute = function(self, panel, index) {
        if (!is.null(self$data) && vec_size(self$data) < 2L) {
            cli_abort(c(
                "Cannot do Hierarchical Clustering",
                i = "must have >= 2 observations to cluster"
            ), call = self$call)
        }

        # if the old panel exist, we do sub-clustering
        if (!is.null(panel) && is.null(self$k) && is.null(self$h) &&
            is.null(self$cutree)) {
            # in this way, we prevent sub-clustering
            if (inherits(self$method, "hclust") ||
                inherits(self$method, "dendrogram")) {
                cli_abort(
                    "{.arg method} cannot be a {.cls hclust} or {.cls dendrogram} when previous layout panel groups exist",
                    call = self$call
                )
            }
            children <- vector("list", nlevels(panel))
            names(children) <- levels(panel)
            labels <- vec_names(self$data)

            # we do clustering within each group ---------------
            for (g in levels(panel)) {
                idx <- which(panel == g)
                gdata <- vec_slice(self$data, idx)
                if (vec_size(gdata) == 1L) {
                    children[[g]] <- tree_one_node(idx, .subset(labels, idx))
                } else {
                    child <- stats::as.dendrogram(hclust2(
                        gdata,
                        distance = self$distance,
                        method = self$method,
                        use_missing = self$use_missing
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
                    if (is.function(self$reorder_dendrogram)) {
                        child <- self$reorder_dendrogram(child, gdata)
                    }
                    children[[g]] <- child
                }
            }
            return(children) # can be a list of `dendrogram` or `hclust` or mix
        }
        hclust2(self$data, self$distance, self$method, self$use_missing)
    },
    #' @importFrom stats order.dendrogram
    align = function(self, panel, index) {
        statistics <- self$statistics
        if (!is.null(panel) && is.null(self$k) && is.null(self$h) &&
            is.null(self$cutree)) {
            # reordering the dendrogram ------------------------
            if (nlevels(panel) > 1L && self$reorder_group) {
                parent_levels <- levels(panel)
                parent_data <- t(sapply(parent_levels, function(g) {
                    colMeans(vec_slice(self$data, panel == g), na.rm = TRUE)
                }))
                rownames(parent_data) <- parent_levels
                parent <- hclust2(
                    parent_data,
                    distance = self$distance,
                    method = self$method,
                    use_missing = self$use_missing
                )
                # reorder parent based on the parent tree
                if (is.function(self$reorder_dendrogram)) {
                    parent <- self$reorder_dendrogram(parent, parent_data)
                }
                # we always ensure the parent is a dendrogram
                # since we'll call `merge_dendrogram()` which requires a
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
            } else if (isTRUE(self$merge_dendro)) {
                # we have a function named merge_dendrogram(), so we use
                # `merge_dendro` as the argument name
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
            # hclust2() will attach the distance used
            distance <- attr(statistics, "distance")
            if (is.function(self$reorder_dendrogram)) {
                statistics <- self$reorder_dendrogram(statistics, self$data)
            }
            if (!is.null(self$k) || !is.null(self$h) || !is.null(self$cutree)) {
                if (is.null(self$cutree)) {
                    self$cutree <- function(tree, dist, k, h) {
                        if (!is.null(k)) {
                            stats::cutree(tree, k = k)
                        } else {
                            stats::cutree(tree, h = h)
                        }
                    }
                }
                # we need `hclust` object to cutree
                statistics <- stats::as.hclust(statistics)
                panel <- self$cutree(statistics, distance, self$k, self$h)
                # For `cutree`, we always respect the height user specified
                # For user defined function, we always calculate
                # height from the number of `panels`
                if (isTRUE(self$plot_cut_height)) {
                    self$height <- self$h %||% cutree_k_to_h(
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
    summary_align = function(self, ...) {
        c(TRUE, !is.null(self$k) || !is.null(self$h) || !is.null(self$cutree))
    }
)
