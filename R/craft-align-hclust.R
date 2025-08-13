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
    active <- active_update(active(use = FALSE), active)
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
        if (vec_size(.subset2(method, "order")) == 0L) { # nocov start
            cli_abort("{.cls hclust} defined in {.arg method} cannot be empty",
                call = call
            )
        } # nocov end
    } else if (inherits(method, "dendrogram")) {
        if (stats::nobs(method) == 0L) { # nocov start
            cli_abort(
                "{.cls dendrogram} defined in {.arg method} cannot be empty",
                call = call
            )
        } # nocov end
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
            if (!inherits(tree, "hclust")) tree <- stats::as.hclust(tree) # nocov
            ans <- user_reorder(tree, data)
            if (!inherits(ans, "hclust") &&
                !inherits(ans, "dendrogram")) {
                cli_abort( # nocov start
                    "{.fn reorder_dendrogram} must return a {.cls hclust} or {.cls dendrogram} object",
                    call = call
                ) # nocov end
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
AlignHclust <- ggproto("AlignHclust", CraftAlign,
    interact_layout = function(self, layout) {
        if (inherits(self$method, c("hclust", "dendrogram"))) {
            layout <- ggproto_parent(CraftAlign, self)$interact_layout(layout)
            if (inherits(self$method, "hclust")) {
                nobs <- vec_size(.subset2(self$method, "order"))
            } else {
                nobs <- stats::nobs(self$method)
            }

            if (is.na(layout_nobs <- prop(layout@domain, "nobs"))) {
                prop(layout@domain, "nobs") <- nobs
            } else {
                assert_mismatch_nobs(self, layout_nobs, nobs, arg = "method")
            }
            if (inherits(self$method, "dendrogram")) {
                self$labels <- labels(self$method)
            } else {
                self$labels <- .subset2(self$method, "labels")
            }
        } else {
            # will add labels
            layout <- ggproto_parent(AlignOrder2, self)$interact_layout(layout)
        }

        # initialize the internal parameters
        self$height <- NULL
        self$panel <- NULL
        layout
    },
    compute = function(self, panel, index) {
        if (!is.null(self$data) && vec_size(self$data) < 2L) {
            cli_abort(
                c(
                    "Cannot do Hierarchical Clustering",
                    i = "must have >= 2 observations to cluster"
                ),
                call = self$call
            )
        }

        # if the old panel exist, we do sub-clustering
        if (!is.null(panel) && is.null(self$k) && is.null(self$h) &&
            is.null(self$cutree)) {
            # in this way, we prevent sub-clustering
            if (inherits(self$method, c("hclust", "dendrogram"))) {
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
        if (inherits(statistics, c("hclust", "dendrogram"))) {
            # hclust2() will attach the distance used
            distance <- attr(statistics, "distance")
            if (is.function(self$reorder_dendrogram)) {
                statistics <- self$reorder_dendrogram(statistics, self$data)
            }
            if (!is.null(self$k) || !is.null(self$h) || !is.null(self$cutree)) {
                if (is.null(cutree <- self$cutree)) {
                    cutree <- function(tree, dist, k, h) {
                        if (!is.null(k)) {
                            stats::cutree(tree, k = k)
                        } else {
                            stats::cutree(tree, h = h)
                        }
                    }
                }
                # we need `hclust` object to cutree
                statistics <- stats::as.hclust(statistics)
                panel <- cutree(statistics, distance, self$k, self$h)
                # For `cutree`, we always respect the height user specified
                # For user defined function, we always calculate
                # height from the number of `panels`
                if (isTRUE(self$plot_cut_height)) {
                    self$height <- self$h %||% cutree_k_to_h(
                        statistics, vec_unique_count(panel)
                    )
                }
            }
        } else {
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
                # we always ensure the parent is a dendrogram, since we'll call
                # `merge_dendrogram()` which requires a dendrogram
                parent <- stats::as.dendrogram(parent)
                panel <- factor(panel, parent_levels[order2(parent)])
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
            } else if (!is.null(parent)) {
                # if no parent tree, and we havn't merged the tree
                # we must manually reorder the dendrogram
                statistics <- .subset(statistics, levels(panel))
            }
        }

        # save the modified `statistics`
        self$statistics <- statistics
        if (inherits(statistics, c("hclust", "dendrogram"))) {
            index <- order2(statistics)
        } else {
            index <- unlist(lapply(statistics, order2), FALSE, FALSE)
        }

        # reorder panel factor levels to following the dendrogram order
        if (!is.null(panel)) {
            panel <- factor(panel, unique(panel[index]))
            # Store the panel used for splitting to ensure consistency in
            # later steps
            if (!inherits(statistics, c("hclust", "dendrogram"))) {
                self$panel <- panel
            }
        }
        list(panel, index)
    },
    summary_align = function(self, ...) {
        c(TRUE, !is.null(self$k) || !is.null(self$h) || !is.null(self$cutree))
    }
)

#' Generate Tree Structures with Hierarchical Clustering
#'
#' @param matrix A numeric matrix, or data frame.
#' @param distance A string of distance measure to be used. This must be one of
#' `"euclidean"`, `"maximum"`, `"manhattan"`, `"canberra"`, `"binary"` or
#' `"minkowski"`.  Correlation coefficient can be also used, including
#' `"pearson"`, `"spearman"` or `"kendall"`. In this way, `1 - cor` will be used
#' as the distance. In addition, you can also provide a [`dist`][stats::dist]
#' object directly or a function return a [`dist`][stats::dist] object. Use
#' `NULL`, if you don't want to calculate the distance.
#' @param method A string of the agglomeration method to be used. This should be
#' (an unambiguous abbreviation of) one of `"ward.D"`, `"ward.D2"`, `"single"`,
#' `"complete"`, `"average"` (= UPGMA), `"mcquitty"` (= WPGMA), `"median"` (=
#' WPGMC) or `"centroid"` (= UPGMC). You can also provide a function which
#' accepts the calculated distance (or the input matrix if `distance` is `NULL`)
#' and returns a [`hclust`][stats::hclust] object. Alternative, you can supply
#' an object which can be coerced to [`hclust`][stats::hclust].
#' @param use_missing An optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation
#' of) one of the strings `"everything"`, `"all.obs"`, `"complete.obs"`,
#' `"na.or.complete"`, or `"pairwise.complete.obs"`. Only used when `distance`
#' is a correlation coefficient string.
#' @seealso
#'  - [cor()][stats::cor]
#'  - [dist()][stats::dist]
#'  - [hclust()][stats::hclust]
#' @examples
#' hclust2(dist(USArrests), method = "ward.D")
#' @return A [hclust][stats::hclust] object.
#' @importFrom rlang is_string try_fetch
#' @export
hclust2 <- function(matrix, distance = "euclidean", method = "complete",
                    use_missing = "pairwise.complete.obs") {
    method <- allow_lambda(method)
    if (!is_string(method) && !is.function(method)) {
        ans <- try_fetch( # nocov start
            stats::as.hclust(method),
            error = function(cnd) {
                cli_abort(paste(
                    "{.arg method} can only be a {.cls string},",
                    "{.cls function} or an object which can be coerced to",
                    "{.cls hclust}."
                ), parent = cnd)
            }
        )
        return(ans) # nocov end
    }
    if (is.null(distance)) {
        d <- matrix
    } else {
        d <- make_dist(matrix, distance, use_missing)
    }
    if (is_string(method)) {
        ans <- stats::hclust(d, method = method)
    } else if (is.function(method)) { # nocov start
        ans <- method(d)
        ans <- try_fetch(
            stats::as.hclust(ans),
            error = function(cnd) {
                cli_abort(paste(
                    "{.arg method} must return an object which",
                    "can be coerced to {.cls hclust}"
                ), parent = cnd)
            }
        ) # nocov end
    }
    if (!is.null(distance)) attr(ans, "distance") <- d
    ans
}

#' @importFrom rlang arg_match0
make_dist <- function(matrix, distance, use_missing,
                      arg = caller_arg(distance), call = caller_call()) {
    distance <- allow_lambda(distance)
    if (is_string(distance)) {
        distance <- arg_match0(distance, c(
            "euclidean", "maximum", "manhattan", "canberra",
            "binary", "minkowski", "pearson", "spearman", "kendall"
        ), arg_nm = arg, error_call = call)
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
            cli_abort("Unsupported {.arg {arg}} specified", call = call)
        )
    } else if (is.function(distance)) { # nocov start
        if (!inherits(d <- distance(matrix), "dist")) {
            cli_abort(
                "{.arg {arg}} must return a {.cls dist} object",
                call = call
            )
        }
    } else if (inherits(distance, "dist")) {
        d <- distance
    } else {
        cli_abort(paste(
            "{.arg {arg}} can only be a {.cls string}, {.cls dist}",
            "object, or a {.cls function} return {.cls dist}"
        ), call = call)
    } # nocov end
    d
}

cutree_k_to_h <- function(tree, k) {
    if (is.null(n1 <- nrow(tree$merge)) || n1 < 1) {
        cli_abort("invalid {.arg tree} ({.field merge} component)") # nocov
    }
    n <- n1 + 1
    if (is.unsorted(tree$height)) {
        cli_abort( # nocov start
            "the 'height' component of 'tree' is not sorted (increasingly)"
        ) # nocov end
    }
    mean(tree$height[c(n - k, n - k + 1L)])
}
