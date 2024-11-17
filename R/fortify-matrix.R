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
            cli_abort(paste0(
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
#' `"Multi_Hit"`. Alternatively, you can provide a single string indicates the
#' collapsed values.
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
#' @importFrom rlang is_string
#' @export
fortify_matrix.MAF <- function(data, ..., genes = NULL, n_top = NULL,
                               remove_empty_samples = TRUE,
                               collapse_vars = TRUE,
                               use_syn = TRUE) {
    rlang::check_installed(
        "maftools", "to make alterations matrix from `MAF` object"
    )
    if (isTRUE(collapse_vars)) {
        collapse_vars <- "Multi_Hit"
    } else if (is_string(collapse_vars)) {
        if (collapse_vars == "") {
            cli_abort("{.arg collapse_vars} cannot be a empty string")
        }
    } else if (isFALSE(collapse_vars)) {
        collapse_vars <- NULL
    } else {
        cli_abort(paste(
            "{.arg collapse_vars} must be a single boolean value or a string,",
            "but you provide {.obj_type_friendly {collapse_vars}}"
        ))
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
    ggalign_attr_set(ans, list(
        sample_summary = sample_summary,
        gene_summary = gene_summary,
        # gene_summary = gene_summary,
        sample_anno = sample_anno,
        n_samples = n_samples, n_genes = n_genes,
        titv = titv, .__ggalign_oncoplot_breaks__ = lvls
    ))
}

#' @inherit fortify_matrix.default
#' @inheritParams fortify_matrix.default
#' @param ... Not used currently.
#' @param n_top A single number indicates how many top bands to be drawn.
#' @param bands An atomic character defines the bands to draw.
#' @param ignored_bands An atomic character defines the bands to be ignored.
#' @param sample_anno A data frame of sample clinical features to be added.
#' @param remove_empty_samples A single boolean value indicating whether to drop
#' samples without any genomic alterations.
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
                                  remove_empty_samples = TRUE) {
    rlang::check_installed(
        "maftools",
        "to make CNV matrix from `GISTIC` object"
    )
    assert_number_whole(n_top, allow_null = TRUE)
    assert_character(bands, allow_null = TRUE)
    assert_character(ignored_bands, allow_null = TRUE)
    assert_s3_class(sample_anno, "data.frame", allow_null = TRUE)
    assert_bool(remove_empty_samples)
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
                sample_anno[[1L]],
            relationship = "one-to-one",
            needles_arg = "data",
            haystack_arg = "sample_anno"
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
    attrs <- list(
        sample_anno = sample_anno,
        sample_summary = sample_summary,
        cytoband_sumamry = cytoband_sumamry,
        gene_summary = gene_summary,
        sumamry = data@summary
    )
    ggalign_attr_set(cn_mat, attrs)
}
