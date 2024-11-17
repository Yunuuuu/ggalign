#' Order observations based on weights
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Ordering observations based on summary weights or a specified ordering
#' character or integer index.
#'
#' @param weights A summary function which accepts a data and returns the
#' weights for each observations. Alternatively, you can provide an ordering
#' index as either an integer or a character. Since characters have been
#' designated as character indices, if you wish to specify a function name as a
#' string, you must enclose it with [`I()`].
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' function provided in `weights` argument.
#' @param reverse A boolean value. Should the sort order be in reverse?
#' @param strict A boolean value indicates whether the order should be strict.
#' If previous groups has been established, and strict is `FALSE`, this will
#' reorder the observations in each group.
#' @param data A `matrix`, `data frame`, or atomic vector used as the input for
#' the `weights` function. Alternatively, you can specify a `function`
#' (including purrr-like lambda syntax) that will be applied to the layout
#' matrix, transforming it as necessary for weight calculations. By default, it
#' will inherit from the layout matrix.
#' @inheritParams align
#' @inheritParams align_gg
#' @return A `"AlignOrder"` object.
#' @inheritSection align Axis Alignment for Observations
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_left() +
#'     align_order(I("rowMeans"))
#' @importFrom ggplot2 waiver
#' @importFrom rlang list2
#' @export
align_order <- function(weights = rowMeans, ...,
                        reverse = FALSE, strict = TRUE, data = NULL,
                        active = NULL, set_context = deprecated(),
                        name = deprecated()) {
    if (is.numeric(weights) ||
        (is.character(weights) && !inherits(weights, "AsIs"))) {
        # vec_duplicate_any is slight faster than `anyDuplicated`
        if (vec_any_missing(weights) || vec_duplicate_any(weights)) {
            cli_abort(paste(
                "{.arg order} must be an ordering numeric or character",
                "without missing value or ties"
            ))
        } else if (is.numeric(weights)) {
            weights <- vec_cast(weights, integer())
        }
        if (!is.null(data) && !is.waive(data)) {
            cli_warn(c(
                "{.arg data} won't be used",
                i = "{.arg order} is not a {.cls function}"
            ))
        }
        # we always inherit from parent layout
        # in this way, we obtain the names of the layout data
        data <- NULL
    } else {
        weights <- rlang::as_function(weights)
        data <- data %||% waiver()
    }
    assert_bool(strict)
    assert_bool(reverse)
    assert_active(active)
    active <- update_active(active, new_active(
        use = FALSE, order = NA_integer_, name = NA_character_
    ))
    active <- deprecate_active(active, "align_order",
        set_context = set_context, name = name
    )
    align(
        align_class = AlignOrder,
        params = list(
            weights = weights,
            weights_params = list2(...),
            reverse = reverse,
            strict = strict
        ),
        active = active,
        check.param = TRUE,
        data = data
    )
}

#' @importFrom ggplot2 ggproto
#' @importFrom rlang inject is_atomic
AlignOrder <- ggproto("AlignOrder", Align,
    nobs = function(params) length(.subset2(params, "weights")),
    setup_params = function(self, nobs, params) {
        if (!is.function(weights <- .subset2(params, "weights"))) {
            assert_mismatch_nobs(
                self, nobs, length(weights),
                msg = "must be an ordering integer or character index of",
                arg = "weights"
            )
        }
        params
    },
    compute = function(self, panel, index, weights, weights_params, strict) {
        assert_reorder(self, panel, strict)
        if (is.function(weights)) {
            data <- .subset2(self, "data")
            ans <- inject(weights(data, !!!weights_params))
            if (!is_atomic(ans)) {
                cli_abort(
                    "{.arg weights} must return an atomic weights",
                    call = .subset2(self, "call")
                )
            }
            assert_mismatch_nobs(
                self, nrow(data), length(ans),
                msg = "must return weights with",
                arg = "weights"
            )
        } else {
            ans <- NULL
        }
        ans
    },
    layout = function(self, panel, index, weights, reverse) {
        if (is.function(weights)) {
            index <- order(.subset2(self, "statistics"))
        } else {
            index <- vec_as_location(weights,
                n = length(weights),
                names = .subset2(self, "labels"),
                missing = "error",
                call = .subset2(self, "call")
            )
            if (vec_duplicate_any(index)) {
                cli_abort(
                    "find ties in the ordering index {.arg weights}",
                    call = .subset2(self, "call")
                )
            }
        }
        if (reverse) index <- rev(index)
        list(panel, index)
    }
)
