#' Build data for the heatmap layout
#'
#' @param data Any objects to be plot with [ggheatmap()].
#' @inheritParams rlang::args_dots_used
#' @return A matrix used by [ggheatmap()].
#' @seealso
#' - [fortify_heatmap.matrix]
#' - [fortify_heatmap.MAF]
#' @export
fortify_heatmap <- function(data, ...) {
    rlang::check_dots_used()
    UseMethod("fortify_heatmap")
}

#' @inherit fortify_heatmap title description return
#' @inheritParams fortify_heatmap
#' @param ... Not used currently.
#' @export
#' @rdname fortify_heatmap.matrix
fortify_heatmap.matrix <- function(data, ...) data

#' @importFrom rlang try_fetch
#' @export
fortify_heatmap.default <- function(data, ...) {
    try_fetch(
        as.matrix(data),
        error = function(cnd) {
            cli::cli_abort(paste0(
                "{.arg data} must be a {.cls matrix}, ",
                "or an object coercible by {.fn fortify_heatmap}, or a valid ",
                "{.cls matrix}-like object coercible by {.fn as.matrix}"
            ), parent = cnd)
        }
    )
}

#' @export
fortify_heatmap.NULL <- function(data, ...) NULL

#' @export
fortify_heatmap.function <- function(data, ...) data

#' @export
fortify_heatmap.formula <- function(data, ...) rlang::as_function(data)

#' @inherit fortify_heatmap title description return
#' @inheritParams fortify_heatmap
#' @param ... Not used currently.
#' @param genes An atomic character defines the genes to draw.
#' @param n_top A single number indicates how many top genes to be drawn.
#' @export
#' @rdname fortify_heatmap.MAF
fortify_heatmap.MAF <- function(data, ..., genes = NULL, n_top = NULL,
                                remove_empty_samples = TRUE) {
    rlang::check_installed(
        "maftools", "to make mutation matrix from `MAF` object"
    )
    createOncoMatrix <- getFromNamespace("createOncoMatrix", "maftools")
    getGeneSummary <- getFromNamespace("getGeneSummary", "maftools")
    getSampleSummary <- getFromNamespace("getSampleSummary", "maftools")
    gene_anno <- getGeneSummary(data)
    if (is.null(genes)) {
        genes <- gene_anno$Hugo_Symbol
    } else { # reorder the gene annotation based on the provided genes
        genes <- unique(genes)
        gene_anno <- gene_anno[match(genes, gene_anno$genes), ]
    }
    if (!is.null(n_top)) {
        gene_anno <- gene_anno[
            order(gene_anno$AlteredSamples, decreasing = TRUE),
        ][seq_len(n_top), ]
        genes <- gene_anno$Hugo_Symbol
    }
    ans <- .subset2(
        createOncoMatrix(m = data, g = genes, add_missing = TRUE),
        "oncoMatrix"
    )
    sample_anno <- getSampleSummary(data)
    sample_anno$Tumor_Sample_Barcode <- as.character(
        sample_anno$Tumor_Sample_Barcode
    )
    sample_anno <- sample_anno[
        match(colnames(ans), sample_anno$Tumor_Sample_Barcode),
    ]
    if (remove_empty_samples) {
        keep <- sample_anno$total > 0L
        sample_anno <- sample_anno[keep, ]
        ans <- ans[, keep]
    }
    structure(ans, row_anno = gene_anno, col_anno = sample_anno)
}
