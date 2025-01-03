# `assert_*()` functions will do the side effects
# `check_*()` functions will return the arguments
#' @importFrom rlang caller_arg caller_env
assert_gp <- function(gp, arg = caller_arg(gp),
                      call = caller_env()) {
    assert_s3_class(gp, "gpar", arg = arg, call = call)
}

#' @importFrom rlang caller_arg caller_env
assert_mapping <- function(mapping, arg = caller_arg(mapping),
                           call = caller_call()) {
    if (!inherits(mapping, "uneval")) {
        cli_abort(c("{.arg {arg}} must be created with {.fn aes}.",
            x = "You've supplied {.obj_type_friendly {mapping}}."
        ), call = call)
    }
}

assert_mismatch_nobs <- function(align, n, nobs, action, arg) {
    if (n != nobs) {
        cli_abort(sprintf(
            "{.arg %s} of %s %s the same length of layout %s-axis (%d)",
            arg, object_name(align), action,
            to_coord_axis(align$direction), n
        ), call = align$call)
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

assert_reorder <- function(align, panel, strict) {
    if (!is.null(panel) && strict) {
        axis <- to_coord_axis(align$direction)
        cli_abort(c(
            sprintf("%s cannot reordering %s-axis", object_name(align), axis),
            i = sprintf("Group of layout %s-axis already exists", axis),
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

assert_layout_position <- function(position, arg = caller_arg(position),
                                   call = caller_call()) {
    if (!is.waive(position) && !is.null(position)) {
        assert_position(position, arg = arg, call = call)
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
            "{.arg {arg}} must have size 1 or 3, not size {l}",
            call = call
        )
    }
    if (!is.unit(sizes)) sizes <- unit(sizes, "null")
    sizes
}

#' @importFrom rlang arg_match0
check_direction <- function(direction, arg = caller_arg(direction),
                            call = caller_call()) {
    direction <- arg_match0(direction, c("h", "v"), error_call = call)
    switch(direction,
        h = "horizontal",
        v = "vertical"
    )
}

assert_limits <- function(limits, allow_null = TRUE, arg = caller_arg(limits),
                          call = caller_call()) {
    if (is.null(limits) && allow_null) {
        return(invisible(NULL))
    }
    if (!inherits(limits, "continuous_limits")) {
        cli_abort(
            "{.arg {arg}} must be specified with {.fn continuous_limits}",
            call = call
        )
    }
    if (rlang::is_named(limits)) {
        cli_abort(
            "{.arg {arg}} shouldn't be created with {.arg x}/{.arg y} argument in {.fn continuous_limits}",
            call = call
        )
    }
}

#' @importFrom grid is.unit
check_size <- function(size, arg = caller_arg(size), call = caller_call()) {
    vec_check_size(size, size = 1L, arg = arg, call = call)
    if (!(is.na(size) || is.numeric(size) || is.unit(size))) {
        cli_abort(
            "{.arg {arg}} must be a single numeric or unit object",
            call = call
        )
    }
    if (!is.unit(size)) size <- unit(size, "null")
    size
}

check_scheme_data <- function(data, arg = caller_arg(data),
                              call = caller_call()) {
    data <- allow_lambda(data)
    if (!is.waive(data) && !is.null(data) && !is.function(data)) {
        cli_abort(paste(
            "{.arg {arg}} must be a function,",
            "{.code NULL} or {.fn waiver}"
        ), call = call)
    }
    data
}

check_stack_context <- function(what, arg = caller_arg(what),
                                call = caller_call()) {
    if (is.null(what)) return(what) # styler: off
    if (!is_scalar(what) || !(is.numeric(what) || is.character(what))) {
        cli_abort("{.arg {arg}} must be a single number or string",
            call = call
        )
    } else if (anyNA(what)) {
        cli_abort("{.arg {arg}} cannot be {.code NA}", call = call)
    } else if (is.numeric(what)) {
        what <- vec_cast(what, integer(), x_arg = arg, call = call)
        if (what <= 0L) {
            cli_abort(
                "{.arg {arg}} must be a positive integer",
                call = call
            )
        }
    }
    what
}

check_reorder <- function(reorder, arg = caller_arg(reorder),
                          call = caller_call()) {
    if (!is.null(reorder)) {
        reorder <- arg_match0(reorder, c("hand1", "hand2"))
    }
    reorder
}

check_order <- function(order, arg = caller_arg(order), call = caller_call()) {
    if (is.null(order)) {
        order <- NA_integer_
    } else if (!is_scalar(order) || (!is.numeric(order) && !is.na(order))) {
        cli_abort("{.arg {arg}} must be a single number", call = call)
    } else {
        order <- vec_cast(order, integer(), x_arg = arg, call = call)
    }
    order
}

#' @importFrom ggplot2 Facet
assert_facet <- function(x, arg = caller_arg(x), call = caller_call()) {
    # Check if we can overide core functions of Facet
    valid_init <- identical(
        body(environment(Facet$init_scales)$f),
        body(environment(x$init_scales)$f)
    )
    valid_train <- identical(
        body(environment(Facet$train_scales)$f),
        body(environment(x$train_scales)$f)
    )
    valid_finish <- identical(
        body(environment(Facet$finish_data)$f),
        body(environment(x$finish_data)$f)
    )
    if (!all(c(valid_init, valid_train, valid_finish))) {
        cli_warn(c(
            "Unknown facet: {.obj_type_friendly {x}}.",
            i = "Overriding facetted scales may be unstable."
        ))
    }
}

assert_align <- function(x, arg = caller_arg(x), call = caller_call()) {
    if (!inherits(x, "scheme_align")) {
        cli_abort("{.arg {arg}} must be created by {.fn scheme_align}",
            call = call
        )
    }
}

assert_active <- function(x, allow_null = TRUE,
                          arg = caller_arg(x), call = caller_call()) {
    if (is.null(x) && allow_null) {
        return(invisible(NULL))
    }
    if (!inherits(x, "ggalign_active")) {
        cli_abort("{.arg {arg}} must be created by {.fn active}",
            call = call
        )
    }
}
