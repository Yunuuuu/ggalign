# Standalone file: do not edit by hand
# Source: <https://github.com/Yunuuuu/standalone/blob/main/R/standalone-assert.R>
# ----------------------------------------------------------------------
#
# ---
# repo: Yunuuuu/standalone
# file: standalone-assert.R
# last-updated: 2024-11-10
# license: https://unlicense.org
# dependencies: [standalone-obj-type.R]
# imports: rlang
# ---

# ## Changelog
# 2024-11-10:
# - Added support for S3 object
#
# nocov start

#' Report if an argument is a specific class
#'
#' @param x The object type which does not conform to `what`. Its
#'   `obj_type_friendly()` is taken and mentioned in the error message.
#' @param what The friendly expected type as a string. Can be a
#'   character vector of expected types, in which case the error
#'   message mentions all of them in an "or" enumeration.
#' @param show_value Passed to `value` argument of `obj_type_friendly()`.
#' @param show_length Passed to `length` argument of `obj_type_friendly()`.
#' @param ... Arguments passed to [rlang::abort()].
#' @importFrom rlang is_missing
#' @noRd
assert_ <- function(x, check, what,
                    allow_null = FALSE,
                    allow_na = FALSE,
                    show_value = TRUE,
                    show_length = FALSE,
                    ...,
                    arg = caller_arg(x),
                    call = caller_env()) {
    if (!is_missing(x) && ((allow_null && is.null(x)) || check(x))) {
        return(invisible(NULL))
    }
    stop_input_type(x, what,
        allow_na = allow_na,
        allow_null = allow_null,
        show_value = show_value,
        show_length = show_length,
        ...,
        arg = arg, call = call
    )
}

.standalone_types_check_assert_call <- .Call

# scalar object ----------------------------------
assert_string <- function(x,
                          ...,
                          allow_empty = TRUE,
                          allow_na = FALSE,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
    assert_(
        x = x,
        check = function(x) {
            .assert_check_is_string(
                x,
                allow_empty = allow_empty,
                allow_na = allow_na,
                allow_null = allow_null
            )
        },
        what = "a single string",
        ...,
        allow_na = allow_na,
        allow_null = allow_null,
        arg = arg,
        call = call
    )
}

#' @importFrom rlang is_string
.assert_check_is_string <- function(x,
                                    allow_empty,
                                    allow_na,
                                    allow_null) {
    if (is_string(x) && !is.na(x)) {
        if (allow_empty || x != "") {
            return(TRUE)
        }
    }

    if (allow_null && is.null(x)) {
        return(TRUE)
    }

    if (allow_na && (identical(x, NA) || identical(x, NA_character_))) {
        return(TRUE)
    }

    FALSE
}

IS_NUMBER_true <- 0
IS_NUMBER_false <- 1
IS_NUMBER_oob <- 2

#' @importFrom rlang ffi_standalone_check_number_1.0.7 is_missing
assert_number_decimal <- function(x,
                                  ...,
                                  min = NULL,
                                  max = NULL,
                                  allow_infinite = TRUE,
                                  allow_na = FALSE,
                                  allow_null = FALSE,
                                  arg = caller_arg(x),
                                  call = caller_env()) {
    if (is_missing(x)) {
        exit_code <- IS_NUMBER_false
    } else if (0 == (exit_code <- .standalone_types_check_assert_call(
        ffi_standalone_check_number_1.0.7,
        x,
        allow_decimal = TRUE,
        min,
        max,
        allow_infinite,
        allow_na,
        allow_null
    ))) {
        return(invisible(NULL))
    }

    .stop_not_number(
        x,
        ...,
        exit_code = exit_code,
        allow_decimal = TRUE,
        min = min,
        max = max,
        allow_na = allow_na,
        allow_null = allow_null,
        arg = arg,
        call = call
    )
}

#' @importFrom rlang ffi_standalone_check_number_1.0.7 is_missing
assert_number_whole <- function(x,
                                ...,
                                min = NULL,
                                max = NULL,
                                allow_infinite = FALSE,
                                allow_na = FALSE,
                                allow_null = FALSE,
                                arg = caller_arg(x),
                                call = caller_env()) {
    if (is_missing(x)) {
        exit_code <- IS_NUMBER_false
    } else if (0 == (exit_code <- .standalone_types_check_assert_call(
        ffi_standalone_check_number_1.0.7,
        x,
        allow_decimal = FALSE,
        min,
        max,
        allow_infinite,
        allow_na,
        allow_null
    ))) {
        return(invisible(NULL))
    }

    .stop_not_number(
        x,
        ...,
        exit_code = exit_code,
        allow_decimal = FALSE,
        min = min,
        max = max,
        allow_na = allow_na,
        allow_null = allow_null,
        arg = arg,
        call = call
    )
}

#' @importFrom rlang abort
.stop_not_number <- function(x,
                             ...,
                             exit_code,
                             allow_decimal,
                             min,
                             max,
                             allow_na,
                             allow_null,
                             arg,
                             call) {
    if (allow_decimal) {
        what <- "a number"
    } else {
        what <- "a whole number"
    }
    if (exit_code == IS_NUMBER_oob) {
        min <- min %||% -Inf
        max <- max %||% Inf

        if (min > -Inf && max < Inf) {
            what <- sprintf("%s between %s and %s", what, min, max)
        } else if (x < min) {
            what <- sprintf("%s larger than or equal to %s", what, min)
        } else if (x > max) {
            what <- sprintf("%s smaller than or equal to %s", what, max)
        } else {
            abort("Unexpected state in OOB check", .internal = TRUE)
        }
    }
    stop_input_type(
        x,
        what,
        ...,
        allow_na = allow_na,
        allow_null = allow_null,
        arg = arg,
        call = call
    )
}

#' @importFrom rlang ffi_standalone_is_bool_1.0.7
assert_bool <- function(x,
                        ...,
                        allow_na = FALSE,
                        allow_null = FALSE,
                        arg = caller_arg(x),
                        call = caller_env()) {
    if (!missing(x) &&
        .standalone_types_check_assert_call(
            ffi_standalone_is_bool_1.0.7,
            x,
            allow_na,
            allow_null
        )) {
        return(invisible(NULL))
    }

    stop_input_type(
        x,
        c("`TRUE`", "`FALSE`"),
        ...,
        allow_na = allow_na,
        allow_null = allow_null,
        arg = arg,
        call = call
    )
}

# atomic vector ------------------------------------
#' @importFrom rlang abort
assert_character <- function(x,
                             ...,
                             allow_na = TRUE,
                             allow_null = FALSE,
                             arg = caller_arg(x),
                             call = caller_env()) {
    if (!missing(x)) {
        if (is.character(x)) {
            if (!allow_na && anyNA(x)) {
                abort(
                    sprintf("`%s` can't contain NA values.", arg),
                    arg = arg, call = call
                )
            }
            return(invisible(NULL))
        }

        if (allow_null && is.null(x)) {
            return(invisible(NULL))
        }
    }
    stop_input_type(
        x,
        "a character vector",
        ...,
        allow_na = FALSE,
        allow_null = allow_null,
        arg = arg,
        call = call
    )
}

#' @importFrom rlang abort
assert_logical <- function(x,
                           ...,
                           allow_na = TRUE,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
    if (!missing(x)) {
        if (is.logical(x)) {
            if (!allow_na && anyNA(x)) {
                abort(
                    sprintf("`%s` can't contain NA values.", arg),
                    arg = arg, call = call
                )
            }
            return(invisible(NULL))
        }
        if (allow_null && is.null(x)) {
            return(invisible(NULL))
        }
    }

    stop_input_type(
        x,
        "a logical vector",
        ...,
        allow_na = FALSE,
        allow_null = allow_null,
        arg = arg,
        call = call
    )
}

# S3 object ----------------------------------------
#' @importFrom rlang is_string is_missing
assert_s3_class <- function(x, is_class, what, ...,
                            arg = caller_arg(x),
                            call = caller_env()) {
    if (is.character(is_class)) {
        class <- is_class
        is_class <- function(x) inherits(x, what = class)
        if (is_missing(what)) what <- sprintf("a <%s>", class)
    }
    assert_(
        x = x, check = is_class,
        what = what,
        ...,
        arg = arg, call = call
    )
}

# nocov end
