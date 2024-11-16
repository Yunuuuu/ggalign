#' Build a Matrix
#'
#' This function converts various objects into a matrix format.
#'
#' @param data An object to be converted to a matrix.
#' @inheritParams rlang::args_dots_used
#' @return A matrix.
#' @seealso
#' - [`fortify_matrix.default()`]
#' - [`fortify_matrix.MAF()`]
#' @export
fortify_matrix <- function(data, ...) {
    rlang::check_dots_used()
    UseMethod("fortify_matrix")
}

#' @inherit fortify_matrix
#' @description
#' By default, it calls [`as.matrix()`] to build a matrix.
#' @family fortify_matrix methods
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
fortify_matrix.matrix <- function(data, ...) data

#' @export
fortify_matrix.waiver <- function(data, ...) data

#' @export
fortify_matrix.NULL <- function(data, ...) NULL

#' @export
fortify_matrix.function <- function(data, ...) data

#' @export
fortify_matrix.formula <- function(data, ...) rlang::as_function(data)

#' @inherit fortify_matrix.default
#' @inheritParams fortify_matrix.default
#' @param ... Not used currently.
#' @param genes An atomic character defines the genes to draw.
#' @param n_top A single number indicates how many top genes to be drawn.
#' @param remove_empty_samples A single boolean value indicating whether to drop
#' samples without any genomic alterations.
#' @param collapse_vars A single boolean value indicating whether to collapse
#' multiple alterations in the same sample and gene into a single value
#' `"Multi_Hit"`.
#' @param use_syn A single boolean value indicates whether to include synonymous
#' variants when Classifies SNPs into transitions and transversions.
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
#' @export
fortify_matrix.MAF <- function(data, ..., genes = NULL, n_top = NULL,
                               remove_empty_samples = TRUE,
                               collapse_vars = TRUE,
                               use_syn = TRUE) {
    rlang::check_installed(
        "maftools", "to make alterations matrix from `MAF` object"
    )
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
        gene_summary <- vec_slice(
            gene_summary,
            vec_as_location(
                vec_unique(vec_cast(genes, character())),
                n = vec_size(gene_summary),
                names = .subset2(gene_summary, "Hugo_Symbol"),
                missing = "remove"
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
    if (collapse_vars) {
        vars <- vapply(var_list, function(var) {
            var <- vec_unique(var)
            if (length(var) > 1L) {
                return("Multi_Hit")
            } else {
                var
            }
        }, character(1L), USE.NAMES = FALSE)
        if (any(vars == "Multi_Hit")) lvls <- c(lvls, "Multi_Hit")
    } else {
        vars <- vapply(var_list, function(var) {
            var <- vec_unique(var)
            if (length(var) > 1L) {
                paste(var, collapse = ";")
            } else {
                var
            }
        }, character(1L), USE.NAMES = FALSE)
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
    ggalign_attr_set(ans, list(
        sample_summary = sample_summary,
        gene_summary = gene_summary,
        # gene_summary = gene_summary,
        sample_anno = sample_anno,
        n_genes = n_genes, n_samples = n_samples,
        titv = titv, .__ggalign_oncoplot_breaks__ = lvls
    ))
}
