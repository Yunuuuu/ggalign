#' Create `OncoPrint` Visualizations from Genetic Alteration Data
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
#' `FALSE`, then add `align_reorder(memo_order)` to achieve the same result. You
#' may also need to set `strit = FALSE` in [`align_reorder()`] if there are
#' already groups.
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
#'     # Note that guide legends from `geom_tile` and `geom_bar` are different.
#'     # Although they appear similar, the internal mechanisms won't collapse
#'     # the guide legends. Therefore, we remove the guide legends from
#'     # `geom_tile`.
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
                               width = NA, height = NA, filling = waiver(),
                               theme = NULL, active = NULL) {
    # prepare the matrix
    data <- fortify_matrix(data = data, ...)
    if (!is.character(data)) {
        cli_abort("{.arg data} must be a character matrix")
    }

    assert_bool(reorder_column)
    assert_bool(reorder_row)

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
        value_list <- strsplit(data$value,
            split = "\\s*[;:,|]\\s*", perl = TRUE
        )
        lvls <- ggalign_lvls_get(data)
        data <- vec_rep_each(data, list_sizes(value_list))
        value <- unlist(value_list, recursive = FALSE, use.names = FALSE)
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
#' this in combination with [`align_reorder()`], e.g.,
#' `align_reorder(memo_order)`.
#'
#' @param x A matrix, where `NA` values will be treated as empty.
#' @return A vector of ordering weights.
#' @export
memo_order <- function(x) {
    # For `align_reorder()`, rows are considered as the samples
    # `.memo_order` will regard the columns as the samples
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

#' @inheritParams rlang::args_dots_empty
#' @inherit fortify_matrix title description return
#' @param data A [`MAF`][maftools::read.maf] object.
#' @param genes An atomic character defines the genes to draw.
#' @param n_top A single number indicates how many top genes to be drawn.
#' @param remove_empty_samples A single boolean value indicating whether to drop
#' samples without any genomic alterations.
#' @param collapse_vars A single boolean value indicating whether to collapse
#' multiple alterations in the same sample and gene into a single value
#' `"Multi_Hit"`. Alternatively, you can provide a single string indicates the
#' collapsed values.
#' @param use_syn A single boolean value indicates whether to include synonymous
#' variants when Classifies SNPs into transitions and transversions.
#' @inheritParams fortify_matrix
#' @section ggalign attributes:
#'  - `gene_summary`: gene summary informations. See
#'    `maftools::getGeneSummary()` for details.
#'  - `sample_summary`: sample summary informations. See
#'    `maftools::getSampleSummary()` for details.
#'  - `sample_anno`: sample clinical informations. See
#'    `maftools::getClinicalData()` for details.
#'  - `n_genes`: Total of genes.
#'  - `n_samples`: Total of samples.
#'  - `titv`: A list of `data.frames` with Transitions and Transversions
#'    summary. See `maftools::titv()` for details.
#' @family fortify_matrix methods
#' @importFrom utils getFromNamespace
#' @importFrom rlang is_string
#' @export
fortify_matrix.MAF <- function(data, ..., genes = NULL, n_top = NULL,
                               remove_empty_samples = TRUE,
                               collapse_vars = TRUE,
                               use_syn = TRUE,
                               data_arg = caller_arg(data),
                               call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    rlang::check_installed(
        "maftools", "to make alterations matrix from `MAF` object"
    )
    if (isTRUE(collapse_vars)) {
        collapse_vars <- "Multi_Hit"
    } else if (isFALSE(collapse_vars)) {
        collapse_vars <- NULL
    } else if (is_string(collapse_vars)) {
        if (collapse_vars == "") {
            cli_abort("{.arg collapse_vars} cannot be an empty string",
                call = call
            )
        }
    } else {
        cli_abort(
            paste(
                "{.arg collapse_vars} must be a single boolean value or a string,",
                "but you provide {.obj_type_friendly {collapse_vars}}"
            ),
            call = call
        )
    }

    getSampleSummary <- getFromNamespace("getSampleSummary", "maftools")
    getGeneSummary <- getFromNamespace("getGeneSummary", "maftools")
    getClinicalData <- getFromNamespace("getClinicalData", "maftools")
    sample_summary <- new_data_frame(getSampleSummary(data))
    gene_summary <- new_data_frame(getGeneSummary(data))
    sample_anno <- new_data_frame(getClinicalData(data))

    titv <- getFromNamespace("titv", "maftools")
    titv <- titv(data, useSyn = use_syn, plot = FALSE)
    titv <- lapply(titv, new_data_frame)

    # we transform the data into a normal data frame
    data <- new_data_frame(data@data)[
        c("Tumor_Sample_Barcode", "Hugo_Symbol", "Variant_Classification")
    ]
    n_genes <- vec_unique_count(.subset2(data, "Hugo_Symbol"))
    n_samples <- vec_unique_count(.subset2(data, "Tumor_Sample_Barcode"))

    # filter by genes
    if (!is.null(genes)) {
        # reorder the gene annotation based on the provided genes
        genes <- vec_cast(genes, character())
        if (vec_any_missing(genes)) {
            cli_abort("{.arg genes} cannot contain missing values",
                call = call
            )
        }
        if (vec_duplicate_any(genes)) {
            cli_abort("{.arg genes} cannot contain duplicated values",
                call = call
            )
        }
        gene_summary <- vec_slice(
            gene_summary,
            vec_as_location(
                genes,
                n = vec_size(gene_summary),
                names = .subset2(gene_summary, "Hugo_Symbol"),
                missing = "error"
            )
        )
    }
    genes <- .subset2(gene_summary, "Hugo_Symbol")
    if (!is.null(n_top)) {
        n_top <- min(n_top, vec_size(genes))
        index <- vec_slice(
            order(gene_summary$AlteredSamples, decreasing = TRUE),
            seq_len(n_top)
        )
        index <- sort(index) # don't change the order, we do only subset
        genes <- vec_slice(genes, index)
        gene_summary <- vec_slice(gene_summary, index)
    }
    data <- vec_slice(data, .subset2(data, "Hugo_Symbol") %in% genes)
    indices <- vec_group_loc(data[c("Tumor_Sample_Barcode", "Hugo_Symbol")])
    vars <- .subset2(data, "Variant_Classification")
    lvls <- levels(vars) %||% sort(vec_unique(vars))
    var_list <- vec_chop(as.character(vars), indices = .subset2(indices, "loc"))
    if (is.null(collapse_vars)) {
        vars <- vapply(var_list, function(var) {
            var <- vec_unique(var)
            if (length(var) > 1L) {
                paste(var, collapse = ";")
            } else {
                var
            }
        }, character(1L), USE.NAMES = FALSE)
    } else {
        vars <- vapply(var_list, function(var) {
            var <- vec_unique(var)
            if (length(var) > 1L) {
                return(collapse_vars)
            } else {
                var
            }
        }, character(1L), USE.NAMES = FALSE)
        if (any(vars == collapse_vars)) lvls <- c(lvls, collapse_vars)
    }
    ans <- vec_cbind(
        .subset2(indices, "key"),
        new_data_frame(list(Variant_Classification = vars))
    )

    # restore all samples
    ans <- right_join(ans, data_frame0(
        Tumor_Sample_Barcode = vec_unique(sample_summary$Tumor_Sample_Barcode)
    ))

    # if `maftools` is installed, `data.table` must have been installed
    # No need to check if `data.table` is installed
    dcast <- getFromNamespace("dcast", "data.table")
    setDT <- getFromNamespace("setDT", "data.table")
    setDF <- getFromNamespace("setDF", "data.table")
    setDT(ans)
    ans <- dcast(ans, Hugo_Symbol ~ Tumor_Sample_Barcode,
        value.var = "Variant_Classification"
    )
    setDF(ans)
    rownms <- .subset2(ans, "Hugo_Symbol")
    ans <- as.matrix(ans[setdiff(names(ans), "Hugo_Symbol")])
    rownames(ans) <- rownms

    # reorder the rows based on the `genes` specified
    ans <- vec_slice(ans, genes)

    # filter samples when necessary
    if (remove_empty_samples) {
        keep <- colSums(!is.na(ans)) > 0L
        ans <- ans[, keep, drop = FALSE]
    }

    # reorder columns based on the sample ordering
    sample_summary <- vec_slice(
        sample_summary,
        vec_as_location(
            colnames(ans),
            n = vec_size(sample_summary),
            names = vec_cast(sample_summary$Tumor_Sample_Barcode, character())
        )
    )
    sample_anno <- vec_slice(
        sample_anno,
        vec_as_location(
            colnames(ans),
            n = vec_size(sample_anno),
            names = sample_anno$Tumor_Sample_Barcode
        )
    )
    titv <- lapply(titv, function(data) {
        data <- left_join(
            data_frame0(Tumor_Sample_Barcode = colnames(ans)),
            data
        )
        vec_slice(data, vec_as_location(
            colnames(ans),
            n = vec_size(data),
            names = vec_cast(data$Tumor_Sample_Barcode, character())
        ))
    })
    ggalign_data_set(ans,
        sample_summary = sample_summary,
        gene_summary = gene_summary,
        sample_anno = sample_anno,
        n_samples = n_samples, n_genes = n_genes, titv = titv,
        .lvls = lvls
    )
}

#' @inheritParams rlang::args_dots_empty
#' @inherit fortify_matrix title description return
#' @param data A [`GISTIC`][maftools::readGistic] object.
#' @param n_top A single number indicates how many top bands to be drawn.
#' @param bands An atomic character defines the bands to draw.
#' @param ignored_bands An atomic character defines the bands to be ignored.
#' @param sample_anno A data frame of sample clinical features to be added.
#' @param remove_empty_samples A single boolean value indicating whether to drop
#' samples without any genomic alterations.
#' @inheritParams fortify_matrix
#' @section ggalign attributes:
#'  - `sample_anno`: sample clinical informations provided in `sample_anno`.
#'  - `sample_summary`: sample copy number summary informations. See
#'    `data@@cnv.summary` for details.
#'  - `cytoband_summary`: cytoband summary informations. See
#'    `data@@cytoband.summary` for details.
#'  - `gene_summary`: gene summary informations. See
#'    `data@@gene.summary` for details.
#'  - `summary`: A data frame of summary information. See `data@@summary` for
#'    details.
#' @family fortify_matrix methods
#' @export
fortify_matrix.GISTIC <- function(data, ..., n_top = NULL, bands = NULL,
                                  ignored_bands = NULL, sample_anno = NULL,
                                  remove_empty_samples = TRUE,
                                  data_arg = caller_arg(data),
                                  call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    rlang::check_installed(
        "maftools",
        "to make CNV matrix from `GISTIC` object"
    )
    assert_number_whole(n_top,
        allow_null = TRUE,
        call = call
    )
    assert_character(bands,
        allow_null = TRUE,
        call = call
    )
    assert_character(ignored_bands,
        allow_null = TRUE,
        call = call
    )
    assert_s3_class(sample_anno, "data.frame",
        allow_null = TRUE,
        call = call
    )
    assert_bool(remove_empty_samples,
        call = call
    )
    cn_mat <- data@cnMatrix
    if (is.null(bands)) {
        bands <- rownames(cn_mat)
    } else {
        bands <- intersect(bands, rownames(cn_mat))
    }
    if (!is.null(ignored_bands)) {
        bands <- setdiff(bands, ignored_bands)
    }
    if (!is.null(bands)) {
        cn_mat <- vec_slice(cn_mat, rownames(cn_mat) %in% bands)
    }
    if (!is.null(n_top)) {
        cn_mat <- vec_slice(cn_mat, seq_len(min(n_top, nrow(cn_mat))))
    }
    if (remove_empty_samples) {
        keep <- colSums(cn_mat != "") > 0L
        cn_mat <- cn_mat[, keep, drop = FALSE]
    }
    if (!is.null(sample_anno)) {
        loc <- vec_locate_matches(
            colnames(cn_mat),
            .subset2(sample_anno, "Tumor_Sample_Barcode") %||%
                .subset2(sample_anno, 1L),
            relationship = "one-to-one",
            needles_arg = "data",
            haystack_arg = "sample_anno",
            error_call = call
        )
        sample_anno <- vec_slice(sample_anno, .subset2(loc, "haystack"))
    }
    sample_summary <- new_data_frame(data@cnv.summary)
    sample_summary <- vec_slice(
        sample_summary,
        vec_as_location(
            colnames(cn_mat),
            n = vec_size(sample_summary),
            names = vec_cast(sample_summary$Tumor_Sample_Barcode, character())
        )
    )
    gene_summary <- new_data_frame(data@gene.summary)
    cytoband_sumamry <- new_data_frame(data@cytoband.summary)
    cytoband_sumamry <- vec_slice(
        cytoband_sumamry,
        vec_as_location(
            rownames(cn_mat),
            n = vec_size(cytoband_sumamry),
            names = vec_cast(cytoband_sumamry$Unique_Name, character())
        )
    )
    ggalign_data_set(cn_mat,
        sample_anno = sample_anno,
        sample_summary = sample_summary,
        cytoband_sumamry = cytoband_sumamry,
        gene_summary = gene_summary,
        sumamry = data@summary
    )
}
