#' Build a Matrix
#'
#' This function converts various objects into a matrix format.
#'
#' @param data Any objects to be converted to a matrix.
#' @inheritParams rlang::args_dots_used
#' @return A matrix containing the converted data.
#' @seealso
#' - [`fortify_matrix.MAF`] for converting [`MAF`][maftools::read.maf] objects.
#' @export
fortify_matrix <- function(data, ...) {
    rlang::check_dots_used()
    UseMethod("fortify_matrix")
}

#' @inherit fortify_matrix title description return
#' @inheritParams fortify_matrix
#' @param ... Not used currently.
#' @export
#' @rdname fortify_matrix.matrix
fortify_matrix.matrix <- function(data, ...) data

#' @importFrom rlang try_fetch
#' @export
fortify_matrix.default <- function(data, ...) {
    try_fetch(
        as.matrix(data),
        error = function(cnd) {
            cli::cli_abort(paste0(
                "{.arg data} must be a {.cls matrix}, ",
                "or an object coercible by {.fn fortify_matrix}, or a valid ",
                "{.cls matrix}-like object coercible by {.fn as.matrix}"
            ), parent = cnd)
        }
    )
}

#' @export
fortify_matrix.waiver <- function(data, ...) data

#' @export
fortify_matrix.NULL <- function(data, ...) NULL

#' @export
fortify_matrix.function <- function(data, ...) data

#' @export
fortify_matrix.formula <- function(data, ...) rlang::as_function(data)

#' @inherit fortify_matrix title description return
#' @inheritParams fortify_matrix
#' @param ... Not used currently.
#' @param genes An atomic character defines the genes to draw.
#' @param n_top A single number indicates how many top genes to be drawn.
#' @param remove_empty_samples A single boolean value indicating whether to drop
#' samples without any genomic alterations.
#' @param collapse_vars A single boolean value indicating whether to collapse
#' multiple alterations in the same sample and gene into a single value
#' `"Multi_Hit"`.
#' @section ggalign attributes:
#'  - `gene_anno`: gene summary informations
#'  - `sample_anno`: sample summary informations
#'  - `n_genes`: Total of genes
#'  - `n_samples`: Total of samples
#'  - `breaks`: factor levels of `Variant_Classification`, if `collapse_vars =
#'    TRUE`, `"Multi_Hit"` will be added in the end.
#' @export
#' @rdname fortify_matrix.MAF
fortify_matrix.MAF <- function(data, ..., genes = NULL, n_top = NULL,
                               remove_empty_samples = TRUE,
                               collapse_vars = FALSE) {
    rlang::check_installed(
        "maftools", "to make mutation matrix from `MAF` object"
    )
    sample_anno <- data@variant.classification.summary
    gene_anno <- data@gene.summary

    # we transform the data into a normal data frame
    data <- new_data_frame(data@data)[
        c("Tumor_Sample_Barcode", "Hugo_Symbol", "Variant_Classification")
    ]
    n_genes <- vec_unique_count(.subset2(data, "Hugo_Symbol"))
    n_samples <- vec_unique_count(.subset2(data, "Tumor_Sample_Barcode"))

    # filter by genes
    if (is.null(genes)) {
        genes <- gene_anno$Hugo_Symbol
    } else { # reorder the gene annotation based on the provided genes
        genes <- intersect(as.character(genes), gene_anno$genes)
        gene_anno <- vec_slice(gene_anno, match(genes, gene_anno$genes))
    }
    if (!is.null(n_top)) {
        gene_anno <- vec_slice(
            gene_anno,
            order(gene_anno$AlteredSamples, decreasing = TRUE)[seq_len(n_top)]
        )
        genes <- gene_anno$Hugo_Symbol
    }
    data <- vec_slice(data, .subset2(data, "Hugo_Symbol") %in% genes)
    indices <- vec_group_loc(data[c("Tumor_Sample_Barcode", "Hugo_Symbol")])
    vars <- .subset2(data, "Variant_Classification")
    lvls <- levels(vars) %||% sort(unique(vars))
    if (collapse_vars) {
        vars <- vapply(
            vec_chop(as.character(vars), indices = .subset2(indices, "loc")),
            function(var) {
                if (length(var) > 1L) {
                    return("Multi_Hit")
                } else {
                    var
                }
            }, character(1L),
            USE.NAMES = FALSE
        )
        if (any(vars == "Multi_Hit")) lvls <- c(lvls, "Multi_Hit")
    } else {
        vars <- vapply(
            vec_chop(as.character(vars), indices = .subset2(indices, "loc")),
            function(var) {
                if (length(var) > 1L) {
                    paste(var, collapse = ";")
                } else {
                    var
                }
            }, character(1L),
            USE.NAMES = FALSE
        )
    }
    ans <- vec_cbind(
        .subset2(indices, "key"),
        new_data_frame(list(Variant_Classification = vars))
    )
    # if `maftools` is installed, `data.table` must have been installed
    # No need to check if data.table is installed
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
    ans <- ans[match(genes, rownms), , drop = FALSE]

    # filter by samples
    sample_anno$Tumor_Sample_Barcode <- as.character(
        sample_anno$Tumor_Sample_Barcode
    )
    sample_anno <- sample_anno[
        match(colnames(ans), sample_anno$Tumor_Sample_Barcode),
    ]
    if (remove_empty_samples) {
        keep <- sample_anno$total > 0L
        sample_anno <- sample_anno[keep, ]
        ans <- ans[, keep, drop = FALSE]
    }
    structure(ans, ggalign = list(
        gene_anno = gene_anno, sample_anno = sample_anno,
        n_genes = n_genes, n_samples = n_samples, breaks = lvls
    ))
}
