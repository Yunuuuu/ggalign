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
        ans <- try_fetch(
            stats::as.hclust(method),
            error = function(cnd) {
                cli_abort(paste(
                    "{.arg method} can only be a {.cls string},",
                    "{.cls function} or an object which can be coerced to",
                    "{.cls hclust}."
                ), parent = cnd)
            }
        )
        return(ans)
    }
    if (is.null(distance)) {
        d <- matrix
    } else {
        d <- make_dist(matrix, distance, use_missing)
    }
    if (is_string(method)) {
        ans <- stats::hclust(d, method = method)
    } else if (is.function(method)) {
        ans <- method(d)
        ans <- try_fetch(
            stats::as.hclust(ans),
            error = function(cnd) {
                cli_abort(paste(
                    "{.arg method} must return an object which",
                    "can be coerced to {.cls hclust}"
                ), parent = cnd)
            }
        )
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
    } else if (is.function(distance)) {
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
    }
    d
}

#' @inherit fortify_data_frame.default title description
#'
#' @param data A [`hclust`][stats::hclust] or a
#' [`dendrogram`][stats::as.dendrogram] object.
#' @param center A boolean value. if `TRUE`, nodes are plotted centered with
#' respect to all leaves/tips in the branch. Otherwise (default), plot them in
#' the middle of the direct child nodes.
#' @param type A string indicates the plot type, `"rectangle"` or `"triangle"`.
#' @param leaf_pos The x-coordinates of the leaf node. Must be the same length
#' of the number of observations in `tree`.
#' @param leaf_braches Branches of the leaf node. Must be the same length of the
#' number of observations in `tree`. Usually come from [cutree][stats::cutree].
#' @param reorder_branches A single boolean value, indicates whether reorder the
#' provided `leaf_braches` based on the actual index.
#' @param branch_gap A single numeric value indicates the gap between different
#' branches.
#' @param root A length one string or numeric indicates the root branch.
#' @param priority A string of "left" or "right". if we draw from `right` to
#' `left`, the left will override the right, so we take the `"left"` as the
#' priority. If we draw from `left` to `right`, the right will override the
#' left, so we take the `"right"` as priority. This is used by
#' [`align_dendro()`] to provide support of facet operation in ggplot2.
#' @param double A single logical value indicating whether horizontal lines
#' should be doubled when segments span multiple branches. If `TRUE`, the
#' horizontal lines will be repeated for each branch that the segment spans. If
#' `FALSE`, only one horizontal line will be drawn. This is used by
#' [`align_dendro()`] to provide support of facet operation in ggplot2.
#' @inheritParams fortify_data_frame
#' @return A `data frame` with the node coordinates:
#'   - `.panel`: Similar with `panel` column, but always give the correct
#'              branch for usage of the ggplot facet.
#'   - `.index`: the original index in the tree for the the node
#'   - `label`: node label text
#'   - `x` and `y`: x-axis and y-axis coordinates for the node
#'   - `branch`: which branch the node is. You can use this column to color
#'               different groups.
#'   - `panel`: which panel the node is, if we split the plot into panel
#'              using [facet_grid][ggplot2::facet_grid], this column will show
#'              which panel the node is from. Note: some nodes may
#'              fall outside panel (between two panels), so there are possible
#'              `NA` values in this column.
#'   - `leaf`: A logical value indicates whether the node is a leaf.
#' @section ggalign attributes:
#'  `edge`: A `data frame` for edge coordinates:
#'  - `.panel`: Similar with `panel` column, but always give the correct
#'              branch for usage of the ggplot facet.
#'  - `x` and `y`: x-axis and y-axis coordinates for the start node of the edge.
#'  - `xend` and `yend`: the x-axis and y-axis coordinates of the terminal node
#'                       for edge.
#'  - `branch`: which branch the edge is. You can use this column to color
#'              different groups.
#'  - `panel1` and `panel2`: The panel1 and panel2 columns have the same
#'     functionality as `panel`, but they are specifically for the `edge` data
#'     and correspond to both nodes of each edge.
#' @examples
#' fortify_data_frame(hclust(dist(USArrests), "ave"))
#' @importFrom grid is.unit
#' @importFrom stats order.dendrogram
#' @importFrom rlang arg_match0
#' @family fortify_data_frame
#' @export
fortify_data_frame.dendrogram <- function(data, ...,
                                          priority = "right",
                                          center = FALSE,
                                          type = "rectangle",
                                          leaf_pos = NULL,
                                          leaf_braches = NULL,
                                          reorder_branches = TRUE,
                                          branch_gap = NULL,
                                          root = NULL,
                                          double = TRUE,
                                          data_arg = NULL, call = NULL) {
    call <- call %||% current_call()
    data_arg <- data_arg %||% "data"
    rlang::check_dots_empty(call = call)
    assert_bool(center, call = call)
    assert_bool(reorder_branches, call = call)
    type <- arg_match0(type, c("rectangle", "triangle"), error_call = call)
    priority <- arg_match0(priority, c("left", "right"), error_call = call)
    N <- stats::nobs(data)
    rectangle <- type == "rectangle"
    if (is.null(leaf_pos)) {
        leaf_pos <- seq_len(N)
    } else if (length(leaf_pos) != N) {
        cli_abort(
            "{.arg leaf_pos} must be of the same length of {.arg tree}",
            call = call
        )
    }

    # if no branches provided, all branch will be regarded as the `root`
    if (is.null(leaf_braches)) {
        root <- root %||% "root"
    } else if (anyNA(leaf_braches)) {
        cli_abort("`NA` is not allowed in {.arg leaf_braches}",
            call = call
        )
    } else if (length(leaf_braches) != N) {
        cli_abort(
            "{.arg leaf_braches} must be of the same length of {.arg tree}",
            call = call
        )
    } else if (is.character(leaf_braches)) {
        root <- root %||% "root"
    } else if (is.factor(leaf_braches)) {
        leaf_braches <- as.character(leaf_braches)
        root <- root %||% "root"
    } else if (is.numeric(leaf_braches)) {
        root <- root %||% (min(leaf_braches) - 1L)
    } else {
        cli_abort("{.arg leaf_braches} must be a character or numeric",
            call = call
        )
    }

    if (!is.null(leaf_braches) && reorder_branches) {
        leaf_braches <- .subset(leaf_braches, order.dendrogram(data))
    }

    # check `branch_gap`
    if (is.numeric(branch_gap)) {
        if (!is_scalar(branch_gap)) {
            cli_abort("{.arg branch_gap} must be of length 1",
                call = call
            )
        }
    } else if (is.null(branch_gap)) {
        branch_gap <- 0
    } else {
        cli_abort("{.arg branch_gap} must be numeric value.",
            call = call
        )
    }

    # the root value shouldn't be the same of leaf branches.
    if (!is_scalar(root)) {
        cli_abort("{.arg root} must be of length 1", call = call)
    } else if (anyNA(root)) {
        cli_abort("{.arg root} cannot be `NA`", call = call)
    } else if (any(root == leaf_braches)) {
        cli_abort(
            "{.arg root} cannot contain value in {.arg leaf_braches}",
            call = call
        )
    }

    # initialize values
    i <- 0L # leaf index
    branch_levels <- NULL
    last_branch <- root
    total_gap <- 0
    dendrogram_data <- function(dend, from_root = TRUE) {
        if (stats::is.leaf(dend)) { # base version
            index <- as.integer(dend) # the column index of the original data
            y <- attr(dend, "height") %||% 0
            label <- attr(dend, "label") %||% NA
            i <<- i + 1L
            if (is.null(leaf_braches)) {
                branch <- root
            } else {
                branch <- .subset(leaf_braches, i)
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
                # current node information
                x = x, y = y,
                branch = branch,
                panel = branch,
                ggpanel = branch
            )
        } else if (inherits(dend, "dendrogram")) { # recursive version
            # the parent height  -------------------------------------
            y <- attr(dend, "height")

            # for the children nodes ---------------------------------
            data <- list_transpose(
                lapply(dend, dendrogram_data, from_root = FALSE)
            )

            # node should be the direct children
            node <- vec_rbind(!!!.subset2(data, "node"))
            edge <- vec_rbind(!!!.subset2(data, "edge"))

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

            # all x coordinate for children nodes --------------------
            # used if center is `TRUE`, we'll calculate the center position
            # among all children nodes
            leaves <- vec_slice(node, .subset2(node, "leaf")) # all leaves

            # we assign the `panel` for current branch node
            ranges <- split(
                .subset2(leaves, "x"),
                .subset2(leaves, "panel")
            )
            ranges <- ranges[
                order(vapply(ranges, min, numeric(1L), USE.NAMES = FALSE))
            ]
            full_panel <- names(ranges)

            # x coordinate for current branch: the midpoint
            if (center) {
                x <- sum(range(.subset2(leaves, "x"))) / 2L
            } else {
                x <- sum(direct_leaves_x) / 2L
            }
            if (is.null(leaf_braches)) { # no branches
                ggpanel <- panel <- branch <- root
            } else {
                # we assign the branch for current branch node
                branch <- unique(direct_leaves_branch)
                # if two children leaves are different, this branch should be
                # `root`, this is often used to color the segments
                if (length(branch) > 1L) branch <- root

                # we assign the `panel` for current branch node
                panel <- NA
                for (i in seq_along(ranges)) {
                    if (x < min(.subset2(ranges, i))) {
                        panel <- NA
                        break
                    } else if (x <= max(.subset2(ranges, i))) {
                        panel <- .subset2(full_panel, i)
                        break
                    }
                }
                # if the node is between two panels, no panel
                # we choose the priority
                if (is.na(ggpanel <- panel)) {
                    # it's not possible for an branch node live outside the
                    # all panels - the left or right most. So `i` won't be 1 or
                    # length(ranges). we don't need to check the argument
                    ggpanel <- switch(priority,
                        left = .subset(full_panel, i - 1L),
                        right = .subset(full_panel, i)
                    )
                }
            }

            # there is no node data in dendrogram root
            if (!from_root) {
                node <- vec_rbind(node, data_frame0(
                    index = NA, label = NA,
                    x = x, y = y, branch = branch, leaf = FALSE,
                    panel = panel, ggpanel = ggpanel
                ))
            }

            # if it's the `rectangle`
            if (rectangle) {
                # 2 vertical lines
                vertical_lines <- data_frame0(
                    x = direct_leaves_x,
                    xend = direct_leaves_x,
                    y = direct_leaves_y,
                    yend = rep_len(y, 2L),
                    branch = direct_leaves_branch,
                    panel1 = direct_leaves_panel,
                    panel2 = direct_leaves_panel,
                    ggpanel = direct_leaves_ggpanel
                )
                # 2 horizontal lines
                # if the horizontal lines spanned multiple panels
                # we double the left line and the right line
                added_edge <- vec_rbind(
                    vertical_lines,
                    # left horizontal line
                    make_horizontal(
                        c(direct_leaves_x[1L], x),
                        panels = c(direct_leaves_panel[1L], panel),
                        ggpanels = c(direct_leaves_ggpanel[1L], ggpanel),
                        y = y,
                        branch = direct_leaves_branch[1L],
                        ranges = ranges,
                        full_panel = full_panel,
                        double = double
                    ),
                    # right horizontal line
                    make_horizontal(
                        c(x, direct_leaves_x[2L]),
                        panels = c(panel, direct_leaves_panel[2L]),
                        ggpanels = c(ggpanel, direct_leaves_ggpanel[2L]),
                        y = y,
                        branch = direct_leaves_branch[2L],
                        ranges = ranges,
                        full_panel = full_panel,
                        double = double
                    )
                )
            } else {
                added_edge <- data_frame0(
                    x = rep_len(x, 2L),
                    xend = direct_leaves_x,
                    y = rep_len(y, 2L),
                    yend = direct_leaves_y,
                    branch = direct_leaves_branch,
                    panel1 = rep_len(panel, 2L),
                    panel2 = direct_leaves_panel,
                    ggpanel = rep_len(ggpanel, 2L)
                )
            }
            if (is.null(edge)) {
                edge <- added_edge
            } else {
                edge <- vec_rbind(edge, added_edge)
            }
            list(
                node = node, edge = edge,
                x = x, y = y, branch = branch,
                panel = panel, ggpanel = ggpanel
            )
        } else {
            cli_abort("Invalid {.cls dendrogram} provided in {.arg {data_arg}}",
                call = call
            )
        }
    }
    ans <- dendrogram_data(data)
    node <- .subset2(ans, "node")
    edge <- .subset2(ans, "edge")

    # set factor levels for branch and panel ---------------
    panel_levels <- branch_levels
    branch_levels <- c(branch_levels, root)
    node$panel <- factor(.subset2(node, "panel"), panel_levels)
    node$branch <- factor(.subset2(node, "branch"), branch_levels)
    node$ggpanel <- factor(.subset2(node, "ggpanel"), panel_levels)
    if (!is.null(edge)) {
        edge$panel1 <- factor(.subset2(edge, "panel1"), panel_levels)
        edge$panel2 <- factor(.subset2(edge, "panel2"), panel_levels)
        edge$branch <- factor(.subset2(edge, "branch"), branch_levels)
        edge$ggpanel <- factor(.subset2(edge, "ggpanel"), panel_levels)
    }
    node <- rename(node, c(ggpanel = ".panel", index = ".index"))
    edge <- rename(edge, c(ggpanel = ".panel"))
    ggalign_data_set(node, edge = edge)
}

#' @param ... Additional arguments passed to `dendrogram` method.
#' @export
#' @rdname fortify_data_frame.dendrogram
fortify_data_frame.hclust <- function(data, ...) {
    fortify_data_frame.dendrogram(stats::as.dendrogram(data), ...)
}

#' @param ggpanels Won't be `NA`
#' @noRd
make_horizontal <- function(x, panels, ggpanels, y, branch,
                            ranges, full_panel = names(ranges),
                            double = TRUE) {
    if (!isTRUE(double) || identical(ggpanels[1L], ggpanels[2L])) {
        # in the same panel
        data_frame0(
            x = x[1L],
            xend = x[2L],
            y = y,
            yend = y,
            branch = branch,
            panel1 = panels[1L],
            panel2 = panels[2L],
            ggpanel = ggpanels[1L]
        )
    } else {
        index <- match(ggpanels, full_panel)
        ending <- index[2L] # right index
        panel0 <- panels[1L]
        ggpanel0 <- ggpanels[1L]
        point0 <- x[1L] # the left point coordinate x
        out <- vector("list", diff(index))
        right_index <- (index[1L] + 1L):ending
        for (i in seq_along(right_index)) {
            i1 <- .subset(right_index, i) # right index
            if (i1 == ending) {
                point1 <- x[2L]
                panel1 <- panels[2L]
                ggpanel1 <- ggpanels[2L]
            } else {
                point1 <- mean(range(.subset2(ranges, i1)))
                ggpanel1 <- panel1 <- .subset(full_panel, i1)
            }
            out[[i]] <- data_frame0(
                x = c(point0, point1),
                xend = c(point1, point0),
                y = y,
                yend = y,
                branch = branch,
                panel1 = c(panel0, panel1),
                panel2 = c(panel1, panel0),
                ggpanel = c(ggpanel0, ggpanel1)
            )
            point0 <- point1
            panel0 <- panel1
            ggpanel0 <- ggpanel1
        }
        vec_rbind(!!!out)
    }
}

# this function won't set the right `midpoint`, but `dendrogram_data` function
# won't use it, so, it has no hurt to use.
merge_dendrogram <- function(parent, children) {
    if (is.null(parent)) { # if no parent, call the merge function from `stats`
        return(Reduce(function(x, y) {
            merge(x, y, adjust = "none")
        }, children))
    }
    children_heights <- vapply(
        children, attr, numeric(1L), "height",
        USE.NAMES = FALSE
    )
    parent_branch_heights <- tree_branch_heights(parent)
    cutoff_height <- max(children_heights) + min(parent_branch_heights) * 0.5
    .merge_dendrogram <- function(dend) {
        if (stats::is.leaf(dend)) { # base version, leaf should be the index
            .subset2(children, dend)
        } else { # for a branch, we should update the members, height
            attrs <- attributes(dend)
            # we recursively run for each node of current branch
            dend <- lapply(dend, .merge_dendrogram)
            heights <- vapply(dend, attr, numeric(1L), "height",
                USE.NAMES = FALSE
            )
            n_members <- vapply(dend, attr, integer(1L), "members",
                USE.NAMES = FALSE
            )
            # we update height and members
            attrs$height <- .subset2(attrs, "height") + max(heights)
            attrs$members <- sum(n_members)
            attributes(dend) <- attrs
            dend
        }
    }
    ans <- .merge_dendrogram(parent)
    attr(ans, "cutoff_height") <- cutoff_height
    ans
}

#' @importFrom stats reorder
reorder_dendrogram <- function(dend, wts) {
    if (inherits(dend, "hclust")) dend <- stats::as.dendrogram(dend)
    reorder(x = dend, wts = wts, agglo.FUN = mean)
}

cutree_k_to_h <- function(tree, k) {
    if (is.null(n1 <- nrow(tree$merge)) || n1 < 1) {
        cli_abort("invalid {.arg tree} ({.field merge} component)")
    }
    n <- n1 + 1
    if (is.unsorted(tree$height)) {
        cli_abort(
            "the 'height' component of 'tree' is not sorted (increasingly)"
        )
    }
    mean(tree$height[c(n - k, n - k + 1L)])
}

tree_branch_heights <- function(dend) {
    if (stats::is.leaf(dend)) {
        return(NULL)
    } else {
        c(
            attr(dend, "height"),
            unlist(lapply(dend, tree_branch_heights), FALSE, FALSE)
        )
    }
}
