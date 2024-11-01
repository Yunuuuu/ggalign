#' @importFrom grid gpar
#' @export
grid::gpar

#' @importFrom grid unit
#' @export
grid::unit

#' @importFrom grid unitType absolute.size
is_absolute_unit <- function(x) {
    unitType(absolute.size(x)) != "null"
}

#' @importFrom grid unitType
is_null_unit <- function(x) unitType(x) == "null"

#' @importFrom gtable gtable_trim
subset_gt <- function(gt, index, trim = TRUE) {
    gt$layout <- vec_slice(.subset2(gt, "layout"), index)
    gt$grobs <- .subset(.subset2(gt, "grobs"), index)
    if (trim) gtable_trim(gt) else gt
}

gtable_trim_widths <- function(gt) {
    layout <- .subset2(gt, "layout")
    w <- range(.subset2(layout, "l"), .subset2(layout, "r"))
    gt$widths <- .subset2(gt, "widths")[seq.int(w[1L], w[2L])]
    if (is.matrix(respect <- .subset2(gt, "respect"))) {
        respect <- respect[, seq.int(w[1L], w[2L]), drop = FALSE]
        if (all(respect == 0L)) respect <- FALSE
        gt$respect <- respect
    }
    layout$l <- .subset2(layout, "l") - w[1L] + 1L
    layout$r <- .subset2(layout, "r") - w[1L] + 1L
    gt$layout <- layout
    gt
}

gtable_trim_heights <- function(gt) {
    layout <- .subset2(gt, "layout")
    h <- range(.subset2(layout, "t"), .subset2(layout, "b"))
    gt$heights <- .subset2(gt, "heights")[seq.int(h[1L], h[2L])]
    if (is.matrix(respect <- .subset2(gt, "respect"))) {
        respect <- respect[seq.int(h[1L], h[2L]), , drop = FALSE]
        if (all(respect == 0L)) respect <- FALSE
        gt$respect <- respect
    }
    layout$t <- .subset2(layout, "t") - h[1L] + 1L
    layout$b <- .subset2(layout, "b") - h[1L] + 1L
    gt$layout <- layout
    gt
}

trim_area <- function(area) {
    w <- min(.subset2(area, "l"), .subset2(area, "r"))
    h <- min(.subset2(area, "t"), .subset2(area, "b"))
    area$l <- .subset2(area, "l") - w + 1L
    area$r <- .subset2(area, "r") - w + 1L
    area$t <- .subset2(area, "t") - h + 1L
    area$b <- .subset2(area, "b") - h + 1L
    area
}

liberate_area <- function(gt, top, left, bottom, right,
                          clip = "inherit", name = NULL, vp = NULL) {
    if (any(remove <- grob_in_area(gt, top, right, bottom, left))) {
        liberated <- gt[top:bottom, left:right]
        if (is.function(vp <- allow_lambda(vp))) {
            liberated$vp <- vp(liberated)
        } else if (inherits(vp, "viewport")) {
            liberated$vp <- vp
        }
        liberated$respect <- FALSE
        name <- name %||% paste(
            .subset2(.subset2(liberated, "layout"), "name"),
            collapse = "; "
        )
        gt <- subset_gt(gt, !remove, trim = FALSE)
        gt <- gtable_add_grob(
            gt, list(liberated), top, left, bottom, right,
            z = max(.subset2(.subset2(liberated, "layout"), "z")),
            clip = clip, name = name
        )
    }
    gt
}

grob_in_area <- function(gt, top, right, bottom, left) {
    .subset2(.subset2(gt, "layout"), "l") >= left &
        .subset2(.subset2(gt, "layout"), "t") >= top &
        .subset2(.subset2(gt, "layout"), "r") <= right &
        .subset2(.subset2(gt, "layout"), "b") <= bottom
}
