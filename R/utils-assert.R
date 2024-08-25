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

assert_facetted_scales <- function(object, object_name, plot_name,
                                   call = caller_call()) {
    if (any(vapply(object, rlang::is_formula, logical(1L)))) {
        cli::cli_abort(
            c(
                "Cannot add {.code {object_name}} into {plot_name}",
                paste(
                    "{.fn facetted_pos_scales} formula is not supported in",
                    "{.pkg ggalign}"
                )
            ),
            call = call
        )
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

check_guides <- function(guides, arg = caller_arg(guides),
                         call = caller_call()) {
    if (isFALSE(guides) || is.null(guides)) {
        guides <- character()
    } else if (isTRUE(guides)) {
        BORDERS
    } else {
        check_borders(guides, arg = arg, call = call)
    }
}

check_borders <- function(borders, arg = caller_arg(borders),
                          call = caller_call()) {
    if (length(borders) == 0L || !all(borders %in% BORDERS)) {
        cli::cli_abort(sprintf(
            "only %s are allowed in {.arg {arg}}",
            oxford_comma(style_val(BORDERS))
        ), call = call)
    } else {
        unique(borders)
    }
}

check_labs <- function(labs, arg = caller_arg(labs), call = caller_call()) {
    if (is.character(labs)) {
        template <- list(
            t = "t",
            l = "l",
            b = "b",
            r = "r",
            x = c("t", "b"),
            xlab = c("t", "b"),
            xlabs = c("t", "b"),
            y = c("l", "r"),
            ylab = c("l", "r"),
            ylabs = c("l", "r"),
            `xlab-t` = "t",
            `xlab-b` = "b",
            `ylab-l` = "l",
            `ylab-r` = "r"
        )
        ans <- .subset(template, labs)
        if (any(unknown <- vapply(ans, is.null, logical(1L)))) { # nolint
            cli::cli_abort("Cannot determine the labs: {labs[unknown]}",
                call = call
            )
        }
        unique(unlist(ans, FALSE, FALSE))
    } else {
        cli::cli_abort("{.arg {arg}} must be a character", call = call)
    }
}

check_layout_labs <- function(x, arg = caller_arg(x), call = caller_call()) {
    if (is.null(x) || isFALSE(x)) {
        NULL
    } else if (isTRUE(x)) {
        BORDERS
    } else {
        check_labs(x, arg = arg, call = call)
    }
}

check_ggelements <- function(x, arg = caller_arg(x), call = caller_call()) {
    if (is.character(x)) {
        template <- list(
            x = c("xlab-t", "axis-t", "strip-t", "xlab-b", "axis-b", "strip-b"),
            y = c("ylab-l", "axis-l", "strip-l", "ylab-r", "axis-r", "strip-r"),
            xlab = c("xlab-t", "xlab-b"),
            xlabs = c("xlab-t", "xlab-b"),
            ylab = c("ylab-l", "ylab-r"),
            ylabs = c("ylab-l", "ylab-r"),
            lab = c("xlab-t", "xlab-b", "ylab-l", "ylab-r"),
            axis = c("axis-t", "axis-b", "axis-l", "axis-r"),
            axes = c("axis-t", "axis-b", "axis-l", "axis-r"),
            strip = c("strip-t", "strip-b", "strip-l", "strip-r"),
            strips = c("strip-t", "strip-b", "strip-l", "strip-r")
        )
        template <- c(GGELEMENTS, template)
        valid_elements <- unlist(GGELEMENTS,
            recursive = FALSE, use.names = FALSE
        )
        direct <- intersect(x, valid_elements)
        indirect <- setdiff(x, valid_elements)
        ans <- .subset(template, indirect)
        if (any(unknown <- vapply(ans, is.null, logical(1L)))) { # nolint
            cli::cli_abort(paste(
                "Cannot determine the ggplot elements:",
                "{indirect[unknown]}"
            ), call = call)
        }
        union(direct, unlist(ans, FALSE, FALSE))
    } else {
        cli::cli_abort("{.arg {arg}} must be a character", call = call)
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
    if (l == 1L) sizes <- rep(sizes, times = 3L)
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

check_plot_data <- function(plot_data, arg = caller_arg(plot_data),
                            call = caller_call()) {
    plot_data <- allow_lambda(plot_data)
    if (!is.waive(plot_data) &&
        !is.null(plot_data) &&
        !is.function(plot_data)) {
        cli::cli_abort(paste(
            "{.arg {arg}} must be a function,",
            "{.code NULL} or {.fn waiver()}"
        ), call = call)
    }
    plot_data
}

check_stack_context <- function(what, arg = caller_arg(what),
                                call = caller_call()) {
    if (is.null(what)) {
        what <- NA
    } else if (!is_scalar(what) || !(is.numeric(what) || is.character(what))) {
        cli::cli_abort("{.arg {arg}} must be a single number or string",
            call = call
        )
    } else if (anyNA(what)) {
        cli::cli_abort("{.arg {arg}} cannot be {.code NA}", call = call)
    } else if (is.numeric(what)) {
        what <- as.integer(what)
        if (what <= 0L) {
            cli::cli_abort(
                "{.arg {arg}} must be a positive integer",
                call = call
            )
        }
    }
    what
}
