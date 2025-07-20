Domain <- S7::new_class("Domain", abstract = TRUE)

#' Set continuous limits for the layout
#'
#' @description
#' To align continuous axes, it is important to keep the limits consistent
#' across all plots in the layout. You can set the limits by passing a function
#' directly to the `limits` or `xlim`/`ylim` argument, using `...` only.
#' Alternatively, you can add a `ContinuousDomain` object to the layout. For
#' the `quad_layout()` function, you must specify `x`/`y` arguments. For other
#' layouts, you should pass the limits using `...` directly.
#'
#' @param ... A list of two numeric values, specifying the left/lower limit and
#' the right/upper limit of the scale.
#' @inheritParams layout_expand
#' @importFrom rlang list2
#' @export
continuous_limits <- function(..., x = waiver(), y = waiver()) {
    if (...length() > 0L && (!is.waive(x) || !is.waive(y))) {
        cli_abort(
            "Cannot mix the usage of {.arg ...} with {.arg x}/{.arg y} argument"
        )
    }
    ContinuousDomain(..., x = x, y = y)
}

ContinuousDomain <- S7::new_class(
    "ContinuousDomain",
    parent = Domain,
    properties = list(spec = S7::class_list),
    constructor = function(..., x = waiver(), y = waiver()) {
        if (...length() > 0L) {
            spec <- list2(...)
            names(spec) <- NULL
        } else {
            spec <- list(x = x, y = y)
        }
        S7::new_object(Domain, spec = ans)
    }
)

DiscreteDomain <- S7::new_class(
    "DiscreteDomain",
    parent = Domain,
    properties = list(
        panel = S7::new_property(
            S7::class_any,
            validator = function(value) {
                if (!is.null(value) && !is.factor(value)) {
                    return("must be a factor")
                }
            },
            default = NULL
        ),
        index = S7::new_property(
            S7::class_any,
            validator = function(value) {
                if (!is.null(value) && !is.integer(value)) {
                    return("must be an integer")
                }
            },
            default = NULL
        ),
        nobs = S7::new_property(
            S7::class_integer,
            validator = function(value) {
                if (length(value) != 1) {
                    return("must be of length 1")
                }
            },
            default = NA_integer_
        )
    ),
    validator = function(self) {
        if (is.na(self@nobs) && (!is.null(self@panel) || !is.null(self@index))) {
            return("'nobs' must be initialized before 'panel' or 'index'")
        }
    }
)

reorder_index <- function(panel, index = NULL) {
    index <- index %||% seq_along(panel)
    unlist(split(index, panel[index]), recursive = FALSE, use.names = FALSE)
}
