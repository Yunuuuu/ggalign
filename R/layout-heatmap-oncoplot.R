#' Create an OncoPrint
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The `ggoncoplot()` function generates `oncoPrint` visualizations that display
#' genetic alterations in a matrix format. This function is especially useful
#' for visualizing complex genomic data, such as mutations, copy number
#' variations, and other genomic alterations in cancer research.
#'
#' @details
#' `ggoncoplot()` is a wrapper around the [`ggheatmap()`] function, designed to
#' simplify the creation of `OncoPrint`-style visualizations. The function
#' automatically processes the input character matrix by splitting the encoded
#' alterations (delimited by `r oxford_or(c(";", ":", ",", "|"))`) into
#' individual genomic events and unnesting the columns for visualization.
#'
#' @param data A character matrix which encodes the alterations, you can use
#' `r oxford_or(c(";", ":", ",", "|"))` to separate multiple alterations.
#' @inheritParams heatmap_layout
#' @param map_width,map_height A named numeric value defines the width/height of
#' each alterations.
#'
#' @param reorder_row A boolean value indicating whether to reorder the rows
#' based on the frequency of alterations. You can set this to `FALSE`, then add
#' `align_order(~rowSums(!is.na(.x)), reverse = TRUE)` to achieve the same
#' result. You may also need to set `strit = FALSE` in [`align_order()`] if
#' there are already groups.
#'
#' @param reorder_column A boolean value indicating whether to reorder the
#' columns based on the characteristics of the alterations. You can set this to
#' `FALSE`, then add `align_order2(memo_order)` to achieve the same result. You
#' may also need to set `strit = FALSE` in [`align_order2()`] if there are
#' already groups.
#'
#' @param remove_duplicates A logical value indicating whether to remove
#' duplicated variants within the same cell.
#'
#' @param filling Same as [`ggheatmap()`], but only `"tile"` can be used.
#' @examples
#' # A simple example from `ComplexHeatmap`
#' mat <- read.table(textConnection(
#'     "s1,s2,s3
#' g1,snv;indel,snv,indel
#' g2,,snv;indel,snv
#' g3,snv,,indel;snv"
#' ), row.names = 1, header = TRUE, sep = ",", stringsAsFactors = FALSE)
#'
#' ggoncoplot(mat, map_width = c(snv = 0.5), map_height = c(indel = 0.9)) +
#'     guides(fill = "none") +
#'     anno_top(size = 0.5) +
#'     ggalign() +
#'     geom_bar(aes(fill = value), data = function(x) {
#'         subset(x, !is.na(value))
#'     }) +
#'     anno_right(size = 0.5) +
#'     ggalign() +
#'     geom_bar(aes(fill = value), orientation = "y", data = function(x) {
#'         subset(x, !is.na(value))
#'     }) &
#'     scale_fill_brewer(palette = "Dark2", na.translate = FALSE)
#' @inherit heatmap_layout return
#' @importFrom ggplot2 aes
#' @export
ggoncoplot <- function(data = NULL, mapping = aes(), ...,
                       map_width = NULL, map_height = NULL,
                       reorder_row = reorder_column,
                       reorder_column = TRUE,
                       remove_duplicates = FALSE,
                       width = NA, height = NA, filling = waiver(),
                       theme = NULL, active = NULL) {
    UseMethod("ggoncoplot")
}

#' @export
ggoncoplot.NULL <- function(data = NULL, mapping = aes(), ...) {
    cli_abort("{.fn ggoncoplot} only accept a valid character matrix")
}

#' @export
ggoncoplot.functon <- ggoncoplot.NULL

#' @export
ggoncoplot.formula <- ggoncoplot.functon

#' @importFrom ggplot2 aes
#' @importFrom rlang arg_match0
#' @export
#' @rdname ggoncoplot
ggoncoplot.default <- function(data = NULL, mapping = aes(), ...,
                               map_width = NULL, map_height = NULL,
                               reorder_row = reorder_column,
                               reorder_column = TRUE,
                               remove_duplicates = FALSE,
                               width = NA, height = NA, filling = waiver(),
                               theme = NULL, active = NULL) {
    # prepare the matrix
    data <- fortify_matrix(data = data, ...)
    if (!is.character(data)) {
        cli_abort("{.arg data} must be a character matrix")
    }

    assert_bool(reorder_column)
    assert_bool(reorder_row)
    assert_bool(remove_duplicates)

    # convert empty string into NA
    data <- trimws(data, whitespace = "[\\h\\v]")
    data[data == ""] <- NA_character_

    # check filling
    if (isTRUE(filling) || is.waive(filling)) {
        filling <- "tile"
    } else if (isFALSE(filling)) {
        filling <- NULL
    } else if (!is.null(filling)) {
        filling <- arg_match0(filling, c("tile", "raster"))
        if (filling == "raster") {
            cli_warn("Cannot use {.fn geom_raster} in oncoplot")
            filling <- "tile"
        }
    }

    # prepare the plot data action
    pdata <- function(data) {
        vars <- strsplit(data$value, split = "\\s*[;:,|]\\s*", perl = TRUE)
        if (remove_duplicates) vars <- lapply(vars, vec_unique)
        lvls <- ggalign_lvls_get(data)
        data <- vec_rep_each(data, list_sizes(vars))
        value <- unlist(vars, recursive = FALSE, use.names = FALSE)
        if (!is.null(lvls)) value <- factor(value, levels = lvls)
        data$value <- value
        data
    }

    # draw the oncoplot
    ans <- heatmap_layout(
        data = data, mapping = mapping,
        width = width, height = height,
        theme = theme, active = active, filling = NULL
    ) -
        # set the default `scheme_data()`
        scheme_data(data = pdata)

    # prepare counts matrix to reorder the column or rows
    if (reorder_column || reorder_row) {
        counts <- !is.na(data)
        storage.mode(counts) <- "integer"
        weights <- rowSums(counts)
        row_index <- order(weights, decreasing = TRUE)
    }

    if (reorder_row) {
        ans <- ans + anno_left() + align_order(row_index, reverse = TRUE)
    }
    if (reorder_column) {
        column_scores <- .memo_order(vec_slice(counts, row_index))
        ans <- ans +
            anno_top() +
            align_order(order(column_scores, decreasing = TRUE))
    }

    # reset the active context
    ans <- ans + quad_active()
    if (!is.null(filling)) {
        # we always make sure heatmap body has such action data
        ans <- ans + scheme_data(data = pdata)

        # set mapping for width and height
        tile_mapping <- aes(
            .data$.x, .data$.y,
            fill = .data$value,
            width = replace_na(map_width[.data$value], 1),
            height = replace_na(map_height[.data$value], 1)
        )
        if (!is.null(map_width)) {
            if (!rlang::is_named(map_width) || !is.numeric(map_width)) {
                cli_abort("{.arg map_width} must be a named numeric")
            }
        } else {
            tile_mapping$width <- NULL
        }
        if (!is.null(map_height)) {
            if (!rlang::is_named(map_height) || !is.numeric(map_height)) {
                cli_abort("{.arg map_height} must be a named numeric")
            }
        } else {
            tile_mapping$height <- NULL
        }
        # check if user has provided and manual fill mapping
        if (!is.null(.subset2(ans@plot$mapping, "fill"))) {
            tile_mapping$fill <- NULL
        }
        ans <- ans + ggplot2::geom_tile(tile_mapping)
    }
    ans
}

#' Sort matrix for better visualization
#'
#' Helper function used to order the Oncoplot samples. Typically, you would use
#' this in combination with [`align_order2()`], e.g.,
#' `align_order2(memo_order)`.
#'
#' @param x A matrix, where `NA` values will be treated as empty.
#' @return A vector of ordering weights.
#' @export
memo_order <- function(x) {
    # For `align_order2()`, rows are considered as the observations
    # `.memo_order` will regard the columns as the observations
    .memo_order(t(x), counts = FALSE, reorder_rows = TRUE)
}

# Following code is modified from
# <https://gist.github.com/armish/564a65ab874a770e2c26>
.memo_order <- function(x, counts = TRUE, reorder_rows = FALSE) {
    if (!isTRUE(counts)) {
        x <- !is.na(x)
        storage.mode(x) <- "integer"
    }
    if (isTRUE(reorder_rows)) {
        row_index <- order(rowSums(x), decreasing = TRUE)
        x <- vec_slice(x, row_index)
    }
    structure(
        apply(x, 2L, function(x) {
            score <- 2^(length(x) - seq_along(x))
            score[x == 0L] <- 0
            sum(score)
        }),
        class = "memo_weights"
    )
}

#' @export
#' @rdname order2
order2.memo_weights <- function(x) order(x, decreasing = TRUE)
