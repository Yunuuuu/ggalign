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
#' @inheritSection align Discrete Axis Alignment
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_left() +
#'     align_order(I("rowMeans"))
#' @importFrom ggplot2 waiver
#' @importFrom rlang list2
#' @export
align_order <- function(weights = rowMeans, ...,
                        reverse = FALSE, strict = TRUE, data = NULL,
                        active = NULL) {
    if (is.numeric(weights) ||
        (is.character(weights) && !inherits(weights, "AsIs"))) {
        # vec_duplicate_any is slight faster than `anyDuplicated`
        if (vec_any_missing(weights) || vec_duplicate_any(weights)) {
            cli_abort(paste(
                "{.arg weights} must be an ordering numeric or character",
                "without missing value or ties"
            ))
        } else if (is.numeric(weights)) {
            weights <- vec_cast(weights, integer())
        }
        if (vec_size(weights) == 0L) {
            cli_abort("{.arg weights} cannot be empty")
        }
        if (!is.null(data)) {
            cli_warn(c(
                "{.arg data} won't be used",
                i = "{.arg weights} is not a {.cls function}"
            ))
        }
    } else {
        weights <- rlang::as_function(weights)
        data <- data %||% waiver()
    }
    assert_bool(strict)
    assert_bool(reverse)
    assert_active(active)
    active <- active_update(active(use = FALSE), active)
    align(
        align = AlignOrder,
        weights = weights,
        params = list2(...),
        reverse = reverse,
        strict = strict,
        active = active,
        data = data
    )
}

#' @importFrom ggplot2 ggproto
#' @importFrom rlang inject is_atomic
AlignOrder <- ggproto("AlignOrder", CraftAlign,
    interact_layout = function(self, layout) {
        if (is.function(self$weights)) {
            layout <- ggproto_parent(AlignOrder2, self)$interact_layout(layout)
        } else {
            layout <- ggproto_parent(CraftAlign, self)$interact_layout(layout)
            if (is.na(layout_nobs <- prop(layout@domain, "nobs"))) {
                prop(layout@domain, "nobs") <- vec_size(self$weights)
            } else {
                assert_mismatch_nobs(
                    self, layout_nobs, vec_size(self$weights),
                    arg = "weights"
                )
            }
            self$labels <- vec_names(layout@data)
        }
        layout
    },
    compute = function(self, panel, index) {
        if (is.function(self$weights)) {
            ans <- inject(self$weights(self$data, !!!self$params))
            if (!is_atomic(ans)) {
                cli_abort(
                    "{.arg weights} must return an atomic weights",
                    call = self$call
                )
            }
            assert_mismatch_nobs(
                self, vec_size(self$data), vec_size(ans),
                arg = "weights"
            )
            ans
        } else {
            NULL
        }
    },
    align = function(self, panel, index) {
        if (is.function(self$weights)) {
            index <- order(self$statistics)
        } else {
            index <- vec_as_location(
                self$weights,
                n = vec_size(self$weights),
                names = self$labels,
                missing = "error",
                call = self$call
            )
        }
        if (self$reverse) index <- rev(index)
        assert_reorder(self, panel, index, self$strict)
        list(panel, index)
    },
    summary_align = function(self) c(TRUE, FALSE)
)
