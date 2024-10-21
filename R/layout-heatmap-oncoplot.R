#' Create `OncoPrint` Visualizations from Genetic Alteration Data
#'
#' @description
#' The `ggoncoplot()` function generates `oncoPrint` visualizations that display
#' genetic alterations in a matrix format. This function is especially useful
#' for visualizing complex genomic data, such as mutations, copy number
#' variations, and other genomic alterations in cancer research.
#'
#' @details
#' `ggoncoplot()` is a wrapper around the [ggheatmap()] function, designed to
#' simplify the creation of `OncoPrint`-style visualizations. The function
#' automatically processes the input character matrix by splitting the encoded
#' alterations (delimited by `[;:,|]`) into individual genomic events and
#' unnesting the columns for visualization.
#'
#' Additionally, a predefined reordering function, adapted from
#' <https://gist.github.com/armish/564a65ab874a770e2c26>, is included to enhance
#' the organization of the alterations.
#'
#' @param data A character matrix which encodes the alterations, you can use
#' `[;:,|]` to separate multiple alterations.
#' @inheritParams heatmap_layout
#' @inheritDotParams heatmap_layout -data -mapping
#' @param map_width,map_height A named numeric value defines the width/height of
#' each alterations.
#' @param reorder_row,reorder_column A boolean value indicating whether to
#' reorder the rows/columns based on the frequency or characteristics of the
#' alterations.
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
#'     # Note that guide legends from `geom_tile` and `geom_bar` are different.
#'     # Although they appear similar, the internal mechanisms won't collapse
#'     # the guide legends. Therefore, we remove the guide legends from
#'     # `geom_tile`.
#'     guides(fill = "none") +
#'     hmanno("t", size = 0.5) +
#'     ggalign() +
#'     geom_bar(aes(.x, fill = value), data = function(x) {
#'         subset(x, !is.na(value))
#'     }) +
#'     hmanno("r", size = 0.5) +
#'     ggalign() +
#'     geom_bar(aes(fill = value), orientation = "y", data = function(x) {
#'         subset(x, !is.na(value))
#'     }) &
#'     scale_fill_brewer(palette = "Dark2", na.translate = FALSE)
#' @inherit heatmap_layout return
#' @importFrom vctrs vec_slice list_sizes vec_rep_each
#' @importFrom ggplot2 aes
#' @export
ggoncoplot <- function(data, mapping = aes(), ...,
                       map_width = NULL, map_height = NULL,
                       reorder_row = reorder_column,
                       reorder_column = TRUE) {
    UseMethod("ggoncoplot")
}

#' @export
ggoncoplot.NULL <- function(data, mapping = aes(), ...,
                            map_width = NULL, map_height = NULL,
                            reorder_row = reorder_column,
                            reorder_column = TRUE) {
    cli::cli_abort("{.fn ggoncoplot} only accept a valid character matrix")
}

#' @export
ggoncoplot.functon <- ggoncoplot.NULL

#' @export
ggoncoplot.formula <- ggoncoplot.functon

#' @export
ggoncoplot.default <- function(data, mapping = aes(), ...,
                               map_width = NULL, map_height = NULL,
                               reorder_row = reorder_column,
                               reorder_column = TRUE) {
    # prepare the matrix
    data <- fortify_heatmap(data)
    if (!is.character(data)) {
        cli::cli_abort("{.arg data} must be a character matrix")
    }

    assert_bool(reorder_column)
    assert_bool(reorder_row)

    # convert empty string into NA
    data <- trimws(data, whitespace = "[\\h\\v]")
    data[data == ""] <- NA_character_

    # set mapping for width and height
    default_mapping <- aes(width = .data$width, height = .data$height)
    if (!is.null(map_width)) {
        if (!rlang::is_named(map_width) || !is.numeric(map_width)) {
            cli::cli_abort("{.arg map_width} must be a named numeric")
        }
    } else {
        default_mapping$width <- NULL
    }
    if (!is.null(map_height)) {
        if (!rlang::is_named(map_height) || !is.numeric(map_height)) {
            cli::cli_abort("{.arg map_height} must be a named numeric")
        }
    } else {
        default_mapping$height <- NULL
    }

    if (length(default_mapping)) {
        mapping <- add_default_mapping(mapping, default_mapping)
    }

    # prepare counts matrix to reorder the column or rows
    if (reorder_column || reorder_row) {
        counts <- !is.na(data)
        storage.mode(counts) <- "integer"
        weights <- rowSums(counts)
        row_index <- order(weights, decreasing = TRUE)
    }

    # draw the oncoplot
    ans <- .heatmap_layout(
        data = data, mapping = mapping, ...,
        nobs_list = list(x = ncol(data), y = nrow(data)),
        call = current_call()
    )
    if (reorder_row) {
        ans <- ans + hmanno("l") + align_order(row_index, reverse = TRUE)
    }
    if (reorder_column) {
        column_scores <- apply(vec_slice(counts, row_index), 2L, function(x) {
            score <- 2^(length(x) - seq_along(x))
            score[x == 0L] <- 0
            sum(score)
        })
        ans <- ans +
            hmanno("t") +
            align_order(order(column_scores, decreasing = TRUE))
    }
    ans + hmanno(NULL, action = plot_action(data = function(data) {
        value_list <- strsplit(data$value, split = "[;:,|]", perl = TRUE)
        data$value <- NULL
        data <- vec_rep_each(data, list_sizes(value_list))
        data$value <- unlist(value_list, recursive = FALSE, use.names = FALSE)
        if (!is.null(map_width)) {
            width <- map_width[data$value]
            data$width <- ifelse(is.na(width), 1, width)
        }
        if (!is.null(map_height)) {
            height <- map_height[data$value]
            data$height <- ifelse(is.na(height), 1, height)
        }
        data
    }))
}
