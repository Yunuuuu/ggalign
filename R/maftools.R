#' Build a matrix from a maftools object
#'
#' @description
#' Convert `MAF` object to a matrix:
#'  - `fortify_matrix.MAF`: Extract genomic alterations for genes.
#'  - `fortify_matrix.MAF_pathways`: Extract genomic alterations for pathways.
#'    [`tune.MAF()`] helps convert `MAF` object to a `MAF_pathways` object.
#'
#' @inheritParams rlang::args_dots_empty
#' @param data A [`MAF`][maftools::read.maf] object.
#' @param genes An atomic character defines the genes to draw.
#' @param n_top A single number indicates how many top genes to be drawn.
#' @param remove_empty_genes A single boolean value indicats whether to drop
#' genes without any genomic alterations.
#' @param remove_empty_samples A single boolean value indicats whether to drop
#' samples without any genomic alterations.
#' @param collapse_vars A single boolean value indicating whether to collapse
#' multiple alterations in the same sample and gene into a single value
#' `"Multi_Hit"`. Alternatively, you can provide a single string indicates the
#' collapsed values.
#' @param use_syn A single boolean value indicates whether to include synonymous
#' variants when Classifies SNPs into transitions and transversions.
#' @param missing_genes A string, either `"error"` or `"remove"`, specifying the
#' action for handling missing genes.
#' @inheritParams fortify_matrix
#' @section ggalign attributes:
#'  For `fortify_matrix.MAF`:
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
#'
#' @family fortify_matrix methods
#' @importFrom utils getFromNamespace
#' @importFrom rlang is_string
#' @export
fortify_matrix.MAF <- function(data, ..., genes = NULL, n_top = NULL,
                               remove_empty_genes = TRUE,
                               remove_empty_samples = TRUE,
                               collapse_vars = TRUE,
                               use_syn = TRUE, missing_genes = "error",
                               data_arg = caller_arg(data),
                               call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    rlang::check_installed(
        "maftools", "to make alterations matrix from `MAF` object"
    )
    # prepare arguments
    missing_genes <- arg_match0(
        missing_genes, c("error", "remove"),
        error_call = call
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

    # filter by genes --------------------------------------
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
        if (identical(missing_genes, "remove")) {
            genes <- genes[genes %in% .subset2(gene_summary, "Hugo_Symbol")]
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

    # restore all samples, this will introduce `NA` in `Hugo_Symbol`
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
    ans <- vec_slice(ans, !is.na(.subset2(ans, "Hugo_Symbol")))

    # convert data into a matrix
    if (remove_empty_genes) {
        # convert to a matrix
        ans <- as.matrix(column_to_rownames(ans, "Hugo_Symbol"))

        # reorder the rows based on the `genes` specified
        index <- match(genes, rownames(ans))
        ans <- vec_slice(ans, index[!is.na(index)])
    } else {
        # restore all genes
        ans <- right_join(ans, data_frame0(Hugo_Symbol = genes))

        # convert to a matrix
        ans <- as.matrix(column_to_rownames(ans, "Hugo_Symbol"))

        # reorder the rows based on the `genes` specified
        ans <- vec_slice(ans, genes)
    }

    # filter samples when necessary
    if (remove_empty_samples) {
        keep <- colSums(!is.na(ans)) > 0L
        ans <- ans[, keep, drop = FALSE]
    }

    # reorder the rows based on the pathways ordering
    gene_summary <- vec_slice(
        gene_summary,
        vec_as_location(
            rownames(ans),
            n = vec_size(gene_summary),
            names = vec_cast(gene_summary$Hugo_Symbol, character())
        )
    )

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

#' Convert the shape of a MAF for fortify method
#'
#' @param data A [`MAF`][maftools::read.maf] object.
#' @param shape Not used currently.
#' @seealso [`fortify_matrix.MAF_pathways()`]
#' @family tune methods
#' @export
tune.MAF <- function(data, shape = NULL) {
    if (!is.null(shape)) {
        cli_abort("{.arg shape} cannot be used currently for {.cls MAF} object")
    }
    new_tune(data, class = "MAF_pathways")
}

#' @param pathdb A string of `r oxford_or(c("smgbp", "sigpw"))`, or a named list
#' of genes to define the pathways.
#' @param remove_empty_pathways A single boolean value indicats whether to drop
#' pathways without any genomic alterations.
#' @section ggalign attributes:
#'  For `fortify_matrix.MAF_pathways`:
#'  - `gene_list`: the pathway contents.
#'  - `pathway_summary`: pathway summary informations. See
#'    `maftools::pathways()` for details.
#'  - `sample_summary`: sample summary informations. See
#'    `maftools::getSampleSummary()` for details.
#'  - `sample_anno`: sample clinical informations. See
#'    `maftools::getClinicalData()` for details.
#' @export
#' @rdname fortify_matrix.MAF
fortify_matrix.MAF_pathways <- function(data, ..., pathdb = "smgbp",
                                        remove_empty_pathways = TRUE,
                                        remove_empty_samples = TRUE,
                                        data_arg = caller_arg(data),
                                        call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    rlang::check_installed(
        "maftools", "to make alterations matrix from `MAF` object"
    )
    get_pw_summary <- getFromNamespace("get_pw_summary", "maftools")
    maf <- tune_data(data)
    if (rlang::is_string(pathdb)) {
        pathdb <- arg_match0(pathdb, c("sigpw", "smgbp"))
        pathway_summary <- get_pw_summary(maf, pathways = pathdb)
    } else if (is.data.frame(pathdb)) {
        cli_abort("{.arg pathdb} cannot be a data frame", call = call)
    } else if (is.list(pathdb)) {
        if (!rlang::is_named(pathdb)) {
            cli_abort(sprintf(
                "{.arg pathdb} must be a named list of a single string of %s",
                oxford_or(c("sigpw", "smgbp"))
            ), call = call)
        }
        pathdb <- data_frame0(
            Pathway = factor(
                vec_rep_each(names(pathdb), lengths(pathdb)),
                names(pathdb)
            ),
            Gene = unlist(pathdb, FALSE, FALSE)
        )
        pathdb <- vec_unique(pathdb)
        pathway_summary <- get_pw_summary(maf, pathways = pathdb)
    }
    getSampleSummary <- getFromNamespace("getSampleSummary", "maftools")
    getClinicalData <- getFromNamespace("getClinicalData", "maftools")
    sample_summary <- new_data_frame(getSampleSummary(maf))
    sample_anno <- new_data_frame(getClinicalData(maf))

    gene_list <- attr(pathway_summary, "genes") # a list of genes
    ans <- new_data_frame(maf@data)[
        c("Tumor_Sample_Barcode", "Hugo_Symbol", "Variant_Classification")
    ]
    full_genes <- unlist(gene_list, FALSE, FALSE)
    ans$pathways <- vec_slice(
        vec_set_names(
            vec_rep_each(names(gene_list), lengths(gene_list)),
            full_genes
        ),
        if_else(ans$Hugo_Symbol %in% full_genes,
            ans$Hugo_Symbol, NA_character_
        )
    )
    ans$Alt <- if_else(is.na(.subset2(ans, "pathways")), NA_character_, "Alt")
    ans <- vec_unique(ans[c("Tumor_Sample_Barcode", "pathways", "Alt")])

    # if `maftools` is installed, `data.table` must have been installed
    # No need to check if `data.table` is installed
    dcast <- getFromNamespace("dcast", "data.table")
    setDT <- getFromNamespace("setDT", "data.table")
    setDF <- getFromNamespace("setDF", "data.table")
    setDT(ans)
    ans <- dcast(ans, pathways ~ Tumor_Sample_Barcode,
        value.var = "Alt", fill = NA_character_
    )
    setDF(ans)
    ans <- vec_slice(ans, !is.na(.subset2(ans, "pathways")))

    # convert data into a matrix
    if (remove_empty_pathways) {
        # convert to a matrix
        ans <- as.matrix(column_to_rownames(ans, "pathways"))

        # reorder the rows based on the `pathways` specified
        index <- match(names(gene_list), rownames(ans))
        gene_list <- gene_list[!is.na(index)]
        ans <- vec_slice(ans, index[!is.na(index)])
    } else {
        # restore all pathways
        ans <- right_join(ans, data_frame0(pathways = names(gene_list)))

        # convert to a matrix
        ans <- as.matrix(column_to_rownames(ans, "pathways"))

        # reorder the rows based on the `pathways` specified
        ans <- vec_slice(ans, names(gene_list))
    }

    # filter samples when necessary
    if (remove_empty_samples) {
        keep <- colSums(!is.na(ans)) > 0L
        ans <- ans[, keep, drop = FALSE]
    }

    # reorder the rows based on the pathways ordering
    setDF(pathway_summary)
    pathway_summary <- vec_slice(
        pathway_summary,
        vec_as_location(
            rownames(ans),
            n = vec_size(pathway_summary),
            names = vec_cast(pathway_summary$Pathway, character())
        )
    )

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
    ggalign_data_set(ans,
        gene_list = gene_list,
        pathway_summary = pathway_summary,
        sample_summary = sample_summary,
        sample_anno = sample_anno
    )
}

#' Build a matrix from a maftools object
#'
#' @inheritParams rlang::args_dots_empty
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
