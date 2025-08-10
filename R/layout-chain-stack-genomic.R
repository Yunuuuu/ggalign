#' Create a stack Layout for Genomic Data
#'
#' `stack_genomic()` constructs a stack layout specifically for genomic
#' data. It is a specialized variant of `stack_continuous()` that applies
#' default axis limits and coerces the first column of each plot's data to use
#' chromosome (`seqname`) identifiers-matching those in the layout data-as
#' factor levels.
#'
#' @param data The input data, which can be:
#'   - A `character` string ("hg19" or "hg38") to load a predefined cytoband
#'     reference.
#'   - A `data.frame` with at least three columns: `chromosome`, `start`, and
#'     `end` positions.
#'   - A genomic object convertible via `fortify_data_frame()`.
#' @param ... Additional arguments passed to specific methods or
#' [`fortify_data_frame()`].
#' @inheritParams stack_continuous
#' @return A `stack_layout` object representing the genomic layout.
#' @export
stack_genomic <- function(direction, data = NULL, ...,
                          theme = NULL, sizes = NA) {
    UseMethod("stack_genomic", data)
}

#' @export
#' @rdname stack_genomic
stack_genomicv <- function(data = NULL, ...) {
    stack_genomic(data = data, direction = "v", ...)
}

#' @export
#' @rdname stack_genomic
stack_genomich <- function(data = NULL, ...) {
    stack_genomic(data = data, direction = "h", ...)
}

#' @export
stack_genomic.NULL <- function(direction, data, ...) {
    cli_abort("{.arg data} must be provided to initialize `stack_genomic()`")
}

#' @export
stack_genomic.waiver <- stack_genomic.NULL

#' @export
stack_genomic.character <- function(direction, data, ...) {
    data <- arg_match0(data, c("hg19", "hg38"))
    data <- readRDS(
        pkg_extdata(
            switch(data,
                hg19 = "ref_cytoband_hg19.rds",
                hg38 = "ref_cytoband_hg38.rds"
            ),
            mustWork = TRUE
        )
    )
    stack_genomic(direction = direction, data = data, ...)
}

#' @export
#' @keywords internal
stack_genomic.default <- function(direction, data = NULL, ...,
                                  theme = NULL, sizes = NA) {
    direction <- check_direction(direction)
    data <- fortify_data_frame(data = data, ...)
    assert_genomic_data(data)
    data[[1L]] <- as.factor(data[[1L]])
    # seqnames, start, end
    # Special considerations for `data.table`, we cannot use `data[1:2]`
    groups <- vec_split(
        data.frame(start = data[[2L]], end = data[[3L]]),
        data[[1L]]
    )
    ranges <- lapply(
        .subset2(groups, "val"),
        function(d) genomic_range(.subset2(d, 1L), .subset2(d, 2L))
    )
    lvls <- levels(data[[1L]])
    names(ranges) <- .subset2(groups, "key")
    ranges <- ranges[lvls]
    limits <- ContinuousDomain(!!!ranges, facet_lvls = lvls)
    ranges <- vec_rbind(!!!ranges, .names_to = "seqnames")
    ranges$seqnames <- factor(ranges$seqnames, levels = lvls)
    new_stack_layout(
        name = "stack_genomic",
        data = ggalign_data_set(data, seqnames = lvls, ranges = ranges),
        direction = direction, domain = limits,
        schemes = default_schemes(data), theme = theme, sizes = sizes
    )
}
