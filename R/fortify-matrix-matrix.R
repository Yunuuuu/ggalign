#' Build a matrix
#'
#' @param data A matrix object.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams fortify_matrix
#' @section shape:
#'  - `upset`: [`fortify_matrix.matrix_upset()`]
#'  - `oncoplot`: [`fortify_matrix.matrix_oncoplot()`]
#' @family fortify_matrix methods
#' @export
fortify_matrix.matrix <- fortify_matrix.waiver

#' Convert the shape of a matrix for fortify method
#'
#' @param data A matrix.
#' @param shape A string of `r oxford_or(c("upset", "oncoplot"))`.
#' @seealso
#' - [`fortify_matrix.matrix()`]
#' - [`fortify_matrix.matrix_upset()`]
#' - [`fortify_matrix.matrix_oncoplot()`]
#' @family tune methods
#' @export
tune.matrix <- function(data, shape) {
    shape <- rlang::arg_match0(shape, c("upset", "oncoplot"))
    if (identical(shape, "oncoplot")) {
        if (!is.character(data)) {
            cli_abort(
                "{.arg data} must be a character matrix to use {shape} shape"
            )
        }
    }
    new_tune(data, class = sprintf("matrix_%s", shape))
}

#' @inherit fortify_matrix.list_upset title
#' @description
#' Converts a matrix suitable for creating an UpSet plot. [`tune.matrix()`]
#' helps convert `matrix` object to a `matrix_upset` object.
#' @param data A matrix where each row represents an element, and each column
#' defines a set. The values in the matrix indicate whether the element is part
#' of the set. Any non-missing value signifies that the element exists in the
#' set.
#' @inheritParams fortify_matrix.list_upset
#' @inheritDotParams fortify_matrix.list_upset
#' @inheritSection fortify_matrix.list_upset ggalign attributes
#' @seealso [`tune.matrix()`]
#' @family fortify_matrix methods
#' @export
fortify_matrix.matrix_upset <- function(data, ..., data_arg = NULL,
                                        call = NULL) {
    call <- call %||% current_call()
    data <- !is.na(tune_data(data))
    elements <- vec_seq_along(data)
    fortify_matrix.list_upset(
        lapply(seq_len(ncol(data)), function(i) {
            .subset(elements, data[, i, drop = TRUE])
        }),
        ...,
        data_arg = data_arg,
        call = call
    )
}

#' Build a Matrix for OncoPrint
#'
#' @description
#' Converts a matrix suitable for creating an OncoPrint. [`tune.matrix()`]
#' helps convert `matrix` object to a `matrix_oncoplot` object.
#'
#' @param data A matrix where each row represents an genes, and each column
#' represents samples. The values in the matrix indicate whether the element is
#' part of the set.
#' @inheritParams fortify_matrix.MAF
#' @section ggalign attributes:
#'  - `gene_summary`: An integer vector of the altered samples for each gene.
#'  - `sample_summary`: An integer vector of the altered genes for each sample.
#'  - `n_genes`: Total number of genes.
#'  - `n_samples`: Total number of samples.
#'
#' @seealso [`tune.matrix()`]
#' @family fortify_matrix methods
#' @export
fortify_matrix.matrix_oncoplot <- function(data, ...,
                                           genes = NULL, n_top = NULL,
                                           remove_empty_genes = TRUE,
                                           remove_empty_samples = TRUE,
                                           missing_genes = "error",
                                           data_arg = NULL,
                                           call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)

    # check arguments
    missing_genes <- arg_match0(
        missing_genes, c("error", "remove"),
        error_call = call
    )

    data <- tune_data(data)
    alt <- !is.na(data)
    storage.mode(alt) <- "integer"
    gene_summary <- rowSums(alt)
    sample_summary <- colSums(alt)
    n_genes <- nrow(data)
    n_samples <- ncol(data)

    # filter by genes --------------------------------------
    if (!is.null(genes)) {
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
            genes <- genes[genes %in% rownames(data)]
        }
        if (is_empty(genes)) {
            cli_abort("No {.arg genes} remain", call = call)
        }
        index <- vec_as_location(
            genes,
            n = vec_size(data),
            names = rownames(data),
            missing = "error"
        )
        data <- vec_slice(data, index)
        alt <- vec_slice(alt, index)
        gene_summary <- vec_slice(gene_summary, index)
    }

    if (!is.null(n_top)) {
        n_top <- min(n_top, vec_size(alt))
        index <- vec_slice(
            order(rowSums(alt), decreasing = TRUE),
            seq_len(n_top)
        )
        data <- vec_slice(data, index)
        alt <- vec_slice(alt, index)
        gene_summary <- vec_slice(gene_summary, index)
    }

    # convert data into a matrix
    if (remove_empty_genes) {
        keep <- rowSums(alt) > 0L
        data <- vec_slice(data, keep)
        gene_summary <- vec_slice(gene_summary, keep)
    }

    # filter samples when necessary
    if (remove_empty_samples) {
        keep <- colSums(alt) > 0L
        data <- data[, keep, drop = FALSE]
        sample_summary <- vec_slice(sample_summary, keep)
    }

    ggalign_data_set(data,
        sample_summary = sample_summary,
        gene_summary = gene_summary,
        n_samples = n_samples,
        n_genes = n_genes
    )
}
