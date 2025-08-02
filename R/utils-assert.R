# `assert_*()` functions will do the side effects
# `check_*()` functions will return the arguments
#' @importFrom rlang caller_arg caller_call
assert_gp <- function(gp, arg = caller_arg(gp), call = caller_call()) {
    assert_s3_class(gp, "gpar", arg = arg, call = call)
}

#' @importFrom rlang caller_arg caller_call
assert_mapping <- function(mapping, arg = caller_arg(mapping),
                           call = caller_call()) {
    if (!inherits(mapping, "uneval")) {
        cli_abort(c("{.arg {arg}} must be created with {.fn aes}.",
            x = "You've supplied {.obj_type_friendly {mapping}}."
        ), call = call)
    }
}

assert_mismatch_nobs <- function(align, n, nobs, arg) {
    if (n != nobs) {
        cli_abort(sprintf(
            "{.arg %s} (nobs: %d) of %s is not compatible with the %s (nobs: %d)",
            arg, nobs, object_name(align), align$layout_name, n
        ))
    }
}

assert_sub_split <- function(align, panel) {
    if (!is.null(panel)) {
        cli_abort(c(
            sprintf("%s cannot do sub-split", object_name(align)),
            i = sprintf(
                "Group of layout %s-axis already exists",
                to_coord_axis(align$direction)
            )
        ), call = align$call)
    }
}

assert_reorder <- function(align, panel, index, strict) {
    if (!is.null(panel) && nlevels(panel) > 1L && strict &&
        !all(index == reorder_index(panel, index))) {
        layout_name <- align$layout_name
        object_name <- object_name(align)
        cli_abort(c(
            sprintf("Cannot add %s to %s", object_name, layout_name),
            i = sprintf(
                "Group of %s will disrupt the ordering index of %s", layout_name, object_name
            ),
            i = "try to set {.code strict = FALSE} to reorder within each group"
        ), call = align$call)
    }
}

assert_position <- function(position, arg = caller_arg(position),
                            call = caller_call()) {
    assert_string(position, empty_ok = FALSE, arg = arg, call = call)
    if (grepl("[^tlbr]", position)) {
        cli_abort(sprintf(
            "{.arg {arg}} can only contain the %s characters",
            oxford_and(.tlbr)
        ), call = call)
    }
}

assert_guides <- function(guides, arg = caller_arg(guides),
                          call = caller_call()) {
    assert_string(guides, empty_ok = FALSE, arg = arg, call = call)
    if (grepl("[^tlbri]", guides)) {
        cli_abort(sprintf(
            "{.arg {arg}} can only contain the %s characters",
            oxford_and(c(.tlbr, "i"))
        ), call = call)
    }
}

assert_layout_position <- function(position, arg = caller_arg(position),
                                   call = caller_call()) {
    if (!is.waive(position) && !is.null(position)) {
        assert_position(position, arg = arg, call = call)
    }
}

assert_layout_guides <- function(guides, arg = caller_arg(guides),
                                 call = caller_call()) {
    if (!is.waive(guides) && !is.null(guides)) {
        assert_guides(guides, arg = arg, call = call)
    }
}

#' @importFrom grid is.unit
check_stack_sizes <- function(sizes, arg = caller_arg(sizes),
                              call = caller_call()) {
    if (!(all(is.na(sizes)) || is.numeric(sizes) || is.unit(sizes))) {
        cli_abort(
            "{.arg {arg}} must be a numeric or {.cls unit} object",
            call = call
        )
    }
    l <- length(sizes)
    if (l != 1L && l != 3L) {
        cli_abort(
            "{.arg {arg}} must have size `1` or `3`, not size {l}",
            call = call
        )
    }
    sizes
}

#' @importFrom grid is.unit
check_size <- function(size, arg = caller_arg(size), call = caller_call()) {
    if (!is_scalar(size) &&
        !(is.na(size) || is.numeric(size) || is.unit(size))) {
        cli_abort(
            "{.arg {arg}} must be a single numeric or unit object",
            call = call
        )
    }
    size
}

#' @importFrom rlang arg_match0
check_direction <- function(direction, arg = caller_arg(direction),
                            call = caller_call()) {
    direction <- arg_match0(direction, c("h", "v"),
        arg_nm = arg, error_call = call
    )
    switch(direction, h = "horizontal", v = "vertical") # styler: off
}

#' @importFrom rlang is_named
assert_limits <- function(limits, allow_null = TRUE, arg = caller_arg(limits),
                          call = caller_call()) {
    if (is.null(limits) && allow_null) {
        return(invisible(NULL))
    }
    if (!is_continuous_domain(limits)) {
        cli_abort(
            "{.arg {arg}} must be specified with {.fn continuous_limits}",
            call = call
        )
    }
    if (is_named(prop(limits, "spec"))) {
        cli_abort(
            "{.arg {arg}} shouldn't be created with {.arg x}/{.arg y} argument in {.fn continuous_limits}",
            call = call
        )
    }
}

check_scheme_data <- function(data, arg = caller_arg(data),
                              call = caller_call()) {
    if (!is.waive(data) && !is.null(data) &&
        !is.function(data <- allow_lambda(data))) {
        cli_abort(paste(
            "{.arg {arg}} must be a function,",
            "{.code NULL} or {.fn waiver}"
        ), call = call)
    }
    data
}

check_stack_context <- function(what, arg = caller_arg(what),
                                call = caller_call()) {
    if (is.null(what)) return(NA_integer_) # styler: off
    if (.rlang_check_number(what, allow_decimal = FALSE, min = 1) != 0L &&
        !is_string(what)) {
        cli_abort(
            "{.arg {arg}} must be a single positive integer number or string",
            call = call
        )
    }
    what
}

check_order <- function(order, arg = caller_arg(order), call = caller_call()) {
    if (is.null(order)) {
        NA_integer_
    } else if (.rlang_check_number(order, allow_decimal = FALSE) == 0L) {
        as.integer(order)
    } else {
        cli_abort("{.arg {arg}} must be single integer number", call = call)
    }
}

assert_active <- function(x, allow_null = TRUE,
                          arg = caller_arg(x), call = caller_call()) {
    if (is.null(x) && allow_null) {
        return(invisible(NULL))
    }
    if (!is_active(x)) {
        cli_abort(
            "{.arg {arg}} must be created by {.fn active}",
            call = call
        )
    }
}

assert_obs_size <- function(obs_size, arg = caller_arg(obs_size),
                            call = caller_call()) {
    if (.rlang_check_number(obs_size, allow_decimal = TRUE,  # styler: off
                            .Machine$double.eps, 1) != 0L) { # styler: off
        cli_abort(
            "{.arg {arg}} must be a single number in `(0, 1]`",
            call = call
        )
    }
}
