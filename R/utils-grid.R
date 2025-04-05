#' @importFrom grid gpar
#' @export
grid::gpar

#' @importFrom grid unit
#' @export
grid::unit

is.gList <- function(x) inherits(x, "gList")

#' @importFrom grid unitType absolute.size
is_absolute_unit <- function(x) unitType(absolute.size(x)) != "null"

#' @importFrom grid unitType
is_null_unit <- function(x) unitType(x) == "null"

is_empty_grob <- function(x) inherits(x, c("zeroGrob", "null"))

# `current.transform()` transforms from *inches* within the current viewport to
# *inches* on the overall device.
grid_solve_loc <- function(loc, trans, valueOnly = FALSE) {
    x <- grid::convertX(loc$x, "inches", valueOnly = TRUE)
    y <- grid::convertY(loc$y, "inches", valueOnly = TRUE)
    out <- matrix(c(x, y, rep_len(1, length(x))), ncol = 3L) %*%
        trans
    out <- list(x = out[, 1L, drop = TRUE], y = out[, 2L, drop = TRUE])
    if (!valueOnly) out <- lapply(out, unit, "inches")
    out
}

loc_device2vp <- function(x, y, valueOnly = FALSE) {
    assert_s3_class(x, "unit")
    assert_s3_class(y, "unit")
    if (length(x) != length(y)) {
        cli_abort("{.arg x} and {.arg y} must have the same length")
    }
    assert_bool(valueOnly)
    grid_solve_loc(
        list(x = x, y = y),
        solve(grid::current.transform()),
        valueOnly = valueOnly
    )
}

loc_vp2device <- function(x, y, valueOnly = FALSE) {
    assert_s3_class(x, "unit")
    assert_s3_class(y, "unit")
    if (length(x) != length(y)) {
        cli_abort("{.arg x} and {.arg y} must have the same length")
    }
    assert_bool(valueOnly)
    grid::deviceLoc(x, y, valueOnly = valueOnly)
}

# # allow the missing value in the unit for `str` method
# ggalign_unit <- function(x, ...) UseMethod("ggalign_unit")
# #' @export
# ggalign_unit.default <- function(x, ...) ggalign_unit(as.numeric(x), ...)
# #' @export
# ggalign_unit.numeric <- function(x, units = "null", data = NULL, ...) {
#     add_class(unit(x, units, data = data), "ggalign_unit")
# }
# #' @export
# ggalign_unit.unit <- function(x, ...) add_class(x, "ggalign_unit")
# is_ggalign_unit <- function(x) inherits(x, "ggalign_unit")

# # allow the missing value in the unit for `str` method
#' @importFrom utils str
#' @export
str.unit <- function(object, ...) obj_str(object, ...)

#' @export
vec_ptype_abbr.unit <- function(x, ...) fclass(x)

#' @export
obj_str_footer.unit <- function(x, ..., indent.str = " ", nest.lev = 0,
                                give.attr = TRUE) {
    if (!isTRUE(give.attr)) {
        return(invisible(x))
    }
    attr <- attributes(x)
    attr[["class"]] <- NULL
    attr[["names"]] <- NULL
    if (length(attr) == 0) {
        return(invisible(x))
    }
    indent.str <- paste0(" ", indent.str)
    for (nm in names(attr)) {
        cat(indent.str, paste0("- attr(*, \"", nm, "\"):"), sep = "")
        utils::str(
            attr[[nm]],
            no.list = TRUE, ...,
            nest.lev = nest.lev + 1L,
            indent.str = indent.str
        )
    }
    invisible(x)
}

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

liberate_area <- function(
    gt,
    top,
    left,
    bottom,
    right,
    clip = "inherit",
    name = NULL,
    vp = NULL) {
    if (any(remove <- grob_in_area(gt, top, right, bottom, left))) {
        liberated <- gt[top:bottom, left:right]
        if (is.function(vp <- allow_lambda(vp))) {
            liberated$vp <- vp(liberated)
        } else if (inherits(vp, "viewport")) {
            liberated$vp <- vp
        }
        liberated$respect <- FALSE
        name <- name %||%
            paste(
                .subset2(.subset2(liberated, "layout"), "name"),
                collapse = "; "
            )
        gt <- subset_gt(gt, !remove, trim = FALSE)
        gt <- gtable_add_grob(
            gt,
            list(liberated),
            top,
            left,
            bottom,
            right,
            z = max(.subset2(.subset2(liberated, "layout"), "z")),
            clip = clip,
            name = name
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

compute_null_width <- function(x, unitTo = "mm", valueOnly = FALSE) {
    compute_null_unit(x, "width", unitTo = unitTo, valueOnly = valueOnly)
}

compute_null_height <- function(x, unitTo = "mm", valueOnly = FALSE) {
    compute_null_unit(x, "height", unitTo = unitTo, valueOnly = valueOnly)
}

#' @importFrom grid unit convertHeight convertWidth
compute_null_unit <- function(
    x,
    type = c("width", "height"),
    unitTo = "mm",
    valueOnly = FALSE) {
    null <- is_null_unit(x) # null unit
    if (type == "width") {
        ans <- convertWidth(x, unitTo, valueOnly = TRUE)
        total <- convertWidth(unit(1, "npc"), unitTo = unitTo, valueOnly = TRUE)
    } else {
        ans <- convertHeight(x, unitTo, valueOnly = TRUE)
        total <- convertHeight(
            unit(1, "npc"),
            unitTo = unitTo,
            valueOnly = TRUE
        )
    }
    if (any(null)) {
        null_size <- total - sum(ans[!null])
        # other units in the same row/ column also have unit null
        coef <- as.numeric(x[null])
        ans[null] <- (null_size / sum(coef)) * coef
    }
    if (isTRUE(valueOnly)) {
        return(ans)
    }
    unit(ans, unitTo)
}
