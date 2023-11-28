# Standalone file: do not edit by hand
# Source: <https://github.com/Yunuuuu/biomisc/blob/main/R/standalone-obj-type.R>
# ----------------------------------------------------------------------
#
# ---
# repo: Yunuuuu/biomisc
# file: standalone-obj-type.R
# last-updated: 2023-05-01
# license: https://unlicense.org
# imports: rlang (>= 1.1.0)
# ---
#
# ## Changelog
#
# 2023-05-01:
# - `obj_type_friendly()` now only displays the first class of S3 objects.
#
# 2023-03-30:
# - `stop_input_type()` now handles `I()` input literally in `arg`.
#
# 2022-10-04:
# - `obj_type_friendly(value = TRUE)` now shows numeric scalars
#   literally.
# - `stop_friendly_type()` now takes `show_value`, passed to
#   `obj_type_friendly()` as the `value` argument.
#
# 2022-10-03:
# - Added `allow_na` and `allow_null` arguments.
# - `NULL` is now backticked.
# - Better friendly type for infinities and `NaN`.
#
# 2022-09-16:
# - Unprefixed usage of rlang functions with `rlang::` to
#   avoid onLoad issues when called from rlang (#1482).
#
# 2022-08-11:
# - Prefixed usage of rlang functions with `rlang::`.
#
# 2022-06-22:
# - `friendly_type_of()` is now `obj_type_friendly()`.
# - Added `obj_type_oo()`.
#
# 2021-12-20:
# - Added support for scalar values and empty vectors.
# - Added `stop_input_type()`
#
# 2021-06-30:
# - Added support for missing arguments.
#
# 2021-04-19:
# - Added support for matrices and arrays (#141).
# - Added documentation.
# - Added changelog.

# Following code was modified from rlang
#' Return English-friendly type
#' @param x Any R object.
#' @param value Whether to describe the value of `x`. Special values
#'   like `NA` or `""` are always described.
#' @param length Whether to mention the length of vectors and lists.
#' @return A string describing the type. Starts with an indefinite
#'   article, e.g. "an integer vector".
#' @noRd
obj_type_friendly <- function(x, value = TRUE, length = FALSE) {
    if (rlang::is_missing(x)) {
        return("absent")
    }

    if (is.object(x)) {
        if (inherits(x, "quosure")) {
            type <- "quosure"
        } else {
            type <- class(x)[[1L]]
        }
        return(sprintf("a <%s> object", type))
    }

    if (!rlang::is_vector(x)) {
        return(.rlang_as_friendly_type(typeof(x)))
    }

    n_dim <- length(dim(x))

    if (!n_dim) {
        if (!rlang::is_list(x) && length(x) == 1) {
            if (rlang::is_na(x)) {
                return(switch(typeof(x),
                    logical = "`NA`",
                    integer = "an integer `NA`",
                    double =
                        if (is.nan(x)) {
                            "`NaN`"
                        } else {
                            "a numeric `NA`"
                        },
                    complex = "a complex `NA`",
                    character = "a character `NA`",
                    .rlang_stop_unexpected_typeof(x)
                ))
            }

            show_infinites <- function(x) {
                if (x > 0) {
                    "`Inf`"
                } else {
                    "`-Inf`"
                }
            }
            str_encode <- function(x, width = 30, ...) {
                if (nchar(x) > width) {
                    x <- substr(x, 1, width - 3)
                    x <- paste0(x, "...")
                }
                encodeString(x, ...)
            }

            if (value) {
                if (is.numeric(x) && is.infinite(x)) {
                    return(show_infinites(x))
                }

                if (is.numeric(x) || is.complex(x)) {
                    number <- as.character(round(x, 2))
                    what <- if (is.complex(x)) "the complex number" else "the number"
                    return(paste(what, number))
                }

                return(switch(typeof(x),
                    logical = if (x) "`TRUE`" else "`FALSE`",
                    character = {
                        what <- if (nzchar(x)) "the string" else "the empty string"
                        paste(what, str_encode(x, quote = "\""))
                    },
                    raw = paste("the raw value", as.character(x)),
                    .rlang_stop_unexpected_typeof(x)
                ))
            }

            return(switch(typeof(x),
                logical = "a logical value",
                integer = "an integer",
                double = if (is.infinite(x)) show_infinites(x) else "a number",
                complex = "a complex number",
                character = if (nzchar(x)) "a string" else "\"\"",
                raw = "a raw value",
                .rlang_stop_unexpected_typeof(x)
            ))
        }

        if (length(x) == 0) {
            return(switch(typeof(x),
                logical = "an empty logical vector",
                integer = "an empty integer vector",
                double = "an empty numeric vector",
                complex = "an empty complex vector",
                character = "an empty character vector",
                raw = "an empty raw vector",
                list = "an empty list",
                .rlang_stop_unexpected_typeof(x)
            ))
        }
    }

    vec_type_friendly(x, length = length)
}

vec_type_friendly <- function(x, length = FALSE) {
    if (!rlang::is_vector(x)) {
        rlang::abort("`x` must be a vector.")
    }
    type <- typeof(x)
    dims <- dim(x)
    n_dim <- length(dims)

    if (type == "list") {
        if (n_dim < 2) {
            out <- "a list"
        } else if (is.data.frame(x)) {
            out <- "a data frame"
        } else if (n_dim == 2) {
            out <- "a list matrix"
        } else {
            out <- "a list array"
        }
    } else {
        type <- switch(type,
            logical = "a logical %s",
            integer = "an integer %s",
            numeric = ,
            double = "a double %s",
            complex = "a complex %s",
            character = "a character %s",
            raw = "a raw %s",
            type = paste0("a ", type, " %s")
        )
        if (n_dim < 2) {
            kind <- "vector"
        } else if (n_dim == 2) {
            kind <- "matrix"
        } else {
            kind <- "array"
        }
        out <- sprintf(type, kind)
    }

    if (length) {
        if (n_dim) {
            out <- paste0(out, " (", paste0(dims, collapse = "*"), ")")
        } else {
            out <- paste0(out, sprintf(" of length %s", length(x)))
        }
    }
    out
}

.rlang_as_friendly_type <- function(type) {
    switch(type,
        list = "a list",
        NULL = "`NULL`",
        environment = "an environment",
        externalptr = "a pointer",
        weakref = "a weak reference",
        S4 = "an S4 object",
        name = ,
        symbol = "a symbol",
        language = "a call",
        pairlist = "a pairlist node",
        expression = "an expression vector",
        char = "an internal string",
        promise = "an internal promise",
        ... = "an internal dots object",
        any = "an internal `any` object",
        bytecode = "an internal bytecode object",
        primitive = ,
        builtin = ,
        special = "a primitive function",
        closure = "a function",
        type
    )
}

.rlang_stop_unexpected_typeof <- function(x, call = rlang::caller_env()) {
    rlang::abort(sprintf("Unexpected type <%s>.", typeof(x)), call = call)
}

#' Return OO type
#' @param x Any R object.
#' @return One of `"bare"` (for non-OO objects), `"S3"`, `"S4"`,
#'   `"R6"`, or `"R7"`.
#' @noRd
obj_type_oo <- function(x) {
    if (!is.object(x)) {
        return("bare")
    }

    class <- inherits(x, c("R6", "R7_object"), which = TRUE)

    if (class[[1]]) {
        "R6"
    } else if (class[[2]]) {
        "R7"
    } else if (isS4(x)) {
        "S4"
    } else {
        "S3"
    }
}
