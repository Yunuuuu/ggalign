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

assert_sub_split <- function(align, panel) {
    if (!is.null(panel)) {
        cli::cli_abort(c(
            "{.fn {snake_class(align)}} cannot do sub-split",
            i = sprintf(
                "Group of layout %s-axis already exists",
                to_coord_axis(.subset2(align, "direction"))
            )
        ), call = .subset2(align, "call"))
    }
}

assert_reorder <- function(align, panel, strict) {
    if (!is.null(panel) && strict) {
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

assert_position <- function(position, arg = caller_arg(position),
                            call = caller_call()) {
    assert_string(position, empty_ok = FALSE, arg = arg, call = call)
    if (grepl("[^tlbr]", position)) {
        cli::cli_abort(sprintf(
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

check_stack_sizes <- function(sizes, arg = caller_arg(sizes),
                              call = caller_call()) {
    l <- length(sizes)
    if (!(l == 1L || l == 3L) ||
        !(all(is.na(sizes)) || is.numeric(sizes) || is.unit(sizes))) {
        cli::cli_abort(paste(
            "{.arg {arg}} must be",
            "a numeric or unit object of length 3"
        ), call = call)
    }
    if (l == 1L) sizes <- rep(sizes, length.out = 3L)
    if (!is.unit(sizes)) sizes <- unit(sizes, "null")
    sizes
}

check_size <- function(size, arg = caller_arg(size), call = caller_call()) {
    if (!is_scalar(size) ||
        !(is.na(size) || is.numeric(size) || is.unit(size))) {
        cli::cli_abort(
            "{.arg {arg}} must be a single numeric or unit object",
            call = call
        )
    }
    if (!is.unit(size)) size <- unit(size, "null")
    size
}

check_plot_data <- function(data, arg = caller_arg(data),
                            call = caller_call()) {
    data <- allow_lambda(data)
    if (!is.waive(data) && !is.null(data) && !is.function(data)) {
        cli::cli_abort(paste(
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
        cli::cli_abort("{.arg {arg}} must be a single number or string",
            call = call
        )
    } else if (anyNA(what)) {
        cli::cli_abort("{.arg {arg}} cannot be {.code NA}", call = call)
    } else if (is.numeric(what)) {
        what <- vec_cast(what, integer(), x_arg = arg, call = call)
        if (what <= 0L) {
            cli::cli_abort(
                "{.arg {arg}} must be a positive integer",
                call = call
            )
        }
    }
    what
}

check_order <- function(order, arg = caller_arg(order), call = caller_call()) {
    if (is.null(order)) {
        order <- NA_integer_
    } else if (!is_scalar(order) || (!is.numeric(order) && !is.na(order))) {
        cli::cli_abort("{.arg {arg}} must be a single number", call = call)
    } else {
        order <- NA_integer_
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
        cli::cli_warn(c(
            "Unknown facet: {.obj_type_friendly {x}}.",
            i = "Overriding facetted scales may be unstable."
        ))
    }
}

assert_action <- function(x, arg = caller_arg(x), call = caller_call()) {
    if (!inherits(x, "plot_action")) {
        cli::cli_abort("{.arg {arg}} must be created by {.fn plot_action}",
            call = call
        )
    }
}

assert_context <- function(x, arg = caller_arg(x), call = caller_call()) {
    if (!is.null(x) && !inherits(x, "ggalign_context")) {
        cli::cli_abort("{.arg {arg}} must be created by {.fn context}",
            call = call
        )
    }
}
