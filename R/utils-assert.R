#' @importFrom rlang caller_arg caller_env
assert_gp <- function(gp, arg = caller_arg(gp),
                      call = caller_env()) {
    assert_s3_class(gp, "gpar", arg = arg, call = call)
}

#' @importFrom rlang caller_arg caller_env
assert_mapping <- function(mapping, arg = caller_arg(mapping),
                           call = caller_call()) {
    if (!inherits(mapping, "uneval")) {
        cli::cli_abort(c("{.arg {arg}} must be created with {.fn aes}.",
            x = "You've supplied {.obj_type_friendly {mapping}}."
        ), call = call)
    }
}

assert_mismatch_nobs <- function(align, n, nobs, msg, arg) {
    if (n != nobs) {
        cli::cli_abort(paste(
            "{.arg {arg}} of {.fn {snake_class(align)}}", msg,
            sprintf(
                "the same length of layout %s-axis (%d)",
                to_coord_axis(.subset2(align, "direction")), n
            )
        ), call = .subset2(align, "call"))
    }
}

assert_sub_split <- function(align, panels) {
    if (!is.null(panels)) {
        cli::cli_abort(c(
            "{.fn {snake_class(align)}} cannot do sub-split",
            i = sprintf(
                "Group of layout %s-axis already exists",
                to_coord_axis(.subset2(align, "direction"))
            )
        ), call = .subset2(align, "call"))
    }
}

assert_reorder <- function(align, panels, strict) {
    if (!is.null(panels) && strict) {
        cli::cli_abort(c(
            "{.fn {snake_class(align)}} cannot reordering {axis}-axis",
            i = sprintf(
                "Group of layout %s-axis already exists",
                to_coord_axis(.subset2(align, "direction"))
            ),
            i = "try to set {.code strict = FALSE} to reorder within each group"
        ), call = .subset2(align, "call"))
    }
}
