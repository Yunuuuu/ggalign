#' Create a Circular Layout for Genomic Data
#'
#' `circle_genomic()` constructs a circular layout specifically for genomic
#' data. It is a specialized variant of `circle_continuous()` that applies
#' default axis limits and coerces the first column of each plot's data to use
#' chromosome (`seqname`) identifiers-matching those in the layout data-as
#' factor levels.
#'
#' @param data The input data, which can be:
#'   - A `character` string ("hg19" or "hg38") to load a predefined cytoband
#'     reference.
#'   - A `data.frame` with at least three columns: `chromosome`, `start`, and
#'     `end` positions.
#'   - A genomic object convertible via [`fortify_data_frame()`].
#' @param ... Additional arguments passed to specific methods or
#' [`fortify_data_frame()`].
#' @inheritParams circle_continuous
#' @return A `circle_layout` object representing the genomic layout.
#' @export
circle_genomic <- function(data, ..., radial = NULL,
                           direction = "outward",
                           sector_spacing = NULL,
                           theme = NULL) {
    UseMethod("circle_genomic")
}

#' @export
circle_genomic.NULL <- function(data, ...) {
    cli_abort("{.arg data} must be provided to initialize `circle_genomic()`")
}

#' @export
circle_genomic.waiver <- circle_genomic.NULL

#' @export
circle_genomic.character <- function(data, ...) {
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
    circle_genomic(data, ...)
}

#' @export
#' @keywords internal
circle_genomic.default <- function(data, ..., radial = NULL,
                                   direction = "outward",
                                   sector_spacing = NULL,
                                   theme = NULL) {
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
    limits <- ContinuousDomain(!!!ranges, facet = lvls)
    ranges <- vec_rbind(!!!ranges, .names_to = "seqnames")
    ranges$seqnames <- factor(ranges$seqnames, levels = lvls)
    new_circle_layout(
        name = "circle_genomic",
        data = ggalign_data_set(data, seqnames = lvls, ranges = ranges),
        domain = limits,
        radial = radial, direction = direction,
        sector_spacing = sector_spacing,
        schemes = default_schemes(data), theme = theme
    )
}

genomic_range <- function(start, end) {
    if (length(start) == 1) return(c(start = start, end = end)) # styler: off
    ordering <- order(start)
    s <- start[ordering[1L]]
    e <- end[ordering[1L]]
    for (i in ordering[-1L]) {
        if (start[ordering[i]] - e > 1) {
            cli_abort("Input genomic ranges cannot contain intervals")
        }
        e <- end[ordering[i]]
    }
    c(start = s, end = e)
}
