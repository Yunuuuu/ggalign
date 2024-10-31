#' Order layout observations based on weights
#'
#' @param wts A summary function which accepts a data and returns the weights
#' for each observations. Alternatively, you can provide an ordering index as
#' either an integer or a character. Since characters have been designated as
#' character indices, if you wish to specify a function name as a string, you
#' must enclose it with [`I()`].
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' function provided in `wts` argument.
#' @param reverse A boolean value. Should the sort order be in reverse?
#' @param strict A boolean value indicates whether the order should be strict.
#' If previous groups has been established, and strict is `FALSE`, this will
#' reorder the observations in each group.
#' @param data A `matrix`, `data frame`, or atomic vector used as the input for
#' the `wts` function. Alternatively, you can specify a `function` (including
#' purrr-like lambda syntax) that will be applied to the layout matrix,
#' transforming it as necessary for weight calculations. By default, it will
#' inherit from the layout matrix.
#' @inheritParams align_dendro
#' @inheritParams align
#' @inherit align return
#' @inheritSection align Aligned Axis
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_left() +
#'     align_order(I("rowMeans"))
#' @importFrom vctrs vec_cast vec_duplicate_any
#' @importFrom ggplot2 waiver
#' @export
align_order <- function(wts = rowMeans, ...,
                        reverse = FALSE, strict = TRUE, data = NULL,
                        context = NULL, set_context = deprecated(),
                        name = deprecated()) {
    if (is.numeric(wts) ||
        (is.character(wts) && !inherits(wts, "AsIs"))) {
        # vec_duplicate_any is slight faster than `anyDuplicated`
        if (anyNA(wts) || vec_duplicate_any(wts)) {
            cli::cli_abort(paste(
                "{.arg order} must be an ordering numeric or character",
                "without missing value or ties"
            ))
        } else if (is.numeric(wts)) {
            wts <- vec_cast(wts, integer())
        }
        if (!is.null(data) && !is.waive(data)) {
            cli::cli_warn(c(
                "{.arg data} won't be used",
                i = "{.arg order} is not a {.cls function}"
            ))
        }
        # we always inherit from parent layout
        # in this way, we obtain the names of the layout data
        data <- waiver()
    } else {
        wts <- rlang::as_function(wts)
        data <- data %||% waiver()
    }
    assert_bool(strict)
    assert_bool(reverse)
    assert_s3_class(context, "plot_context", null_ok = TRUE)
    context <- update_context(context, new_context(
        active = FALSE, order = NA_integer_, name = NA_character_
    ))
    context <- deprecate_context(context, "align_order",
        set_context = set_context, name = name
    )
    align(
        align_class = AlignOrder,
        params = list(
            wts = wts,
            wts_params = rlang::list2(...),
            reverse = reverse,
            strict = strict
        ),
        context = context,
        check.param = TRUE,
        data = data
    )
}

#' @importFrom vctrs vec_cast vec_duplicate_any
#' @importFrom ggplot2 ggproto
AlignOrder <- ggproto("AlignOrder", Align,
    nobs = function(params) length(.subset2(params, "wts")),
    setup_params = function(self, nobs, params) {
        if (!is.function(.subset2(params, "wts"))) {
            assert_mismatch_nobs(self, nobs,
                length(.subset2(params, "wts")),
                msg = "must be an ordering integer index or character of",
                arg = "wts"
            )
        }
        params
    },
    compute = function(self, panel, index, wts, wts_params, strict) {
        assert_reorder(self, panel, strict)
        if (is.function(wts)) {
            data <- .subset2(self, "data")
            ans <- rlang::inject(wts(data, !!!wts_params))
            if (!rlang::is_atomic(ans)) {
                cli::cli_abort(
                    "{.arg wts} must return an atomic weights",
                    call = .subset2(self, "call")
                )
            }
            assert_mismatch_nobs(
                self, nrow(data), length(ans),
                msg = "must return weights with",
                arg = "wts"
            )
        } else {
            ans <- NULL
        }
        ans
    },
    layout = function(self, panel, index, wts, reverse) {
        if (!is.function(wts)) {
            if (is.numeric(wts)) {
                index <- wts
                if (any(index < 1L) || any(index > length(wts))) {
                    cli::cli_abort(paste(
                        "Outliers found in the provided ordering",
                        "integer index {.arg wts}"
                    ), call = .subset2(self, "call"))
                } else if (vec_duplicate_any(index)) {
                    cli::cli_abort(
                        "find ties when coercing {.arg wts} into integer",
                        call = .subset2(self, "call")
                    )
                }
            } else if (is.null(layout_labels <- .subset2(self, "labels"))) {
                cli::cli_abort(
                    c(sprintf(
                        "No names found in layout %s-axis",
                        to_coord_axis(.subset2(self, "direction"))
                    ), i = "Cannot use ordering character index {.arg wts}"),
                    call = .subset2(self, "call")
                )
            } else if (anyNA(index <- match(wts, layout_labels))) {
                cli::cli_abort(sprintf(
                    "{.arg wts} contains invalid names: %s",
                    style_val(wts[is.na(index)])
                ), call = .subset2(self, "call"))
            }
        } else {
            index <- order(.subset2(self, "statistics"))
        }
        if (reverse) index <- rev(index)
        list(panel, index)
    }
)
