Domain <- S7::new_class("Domain", abstract = TRUE)

#' Set Expansion for the Layout
#'
#' @description
#' To align axes, it is important to keep the expansion consistent across all
#' plots in the layout. You can add a `layout_expand` object to the layout. For
#' the `quad_layout()` function, you must specify `x` and `y` arguments. For
#' other layouts, you can pass the expansion values using `...` directly.
#'
#' @param ... A list of range expansion constants, used to add padding around
#' the data to ensure they are placed some distance away from the axes. Use the
#' convenience function [`expansion()`][ggplot2::expansion()] to generate the
#' values.
#' @param x,y Same as `...`, but specifically for `quad_layout()`.
#'
#' @importFrom rlang list2
#' @keywords internal
layout_expand <- function(..., x = waiver(), y = waiver()) {
    if (...length() > 0L && (!is.waive(x) || !is.waive(y))) {
        cli_abort(
            "Cannot mix the usage of {.arg ...} with {.arg x}/{.arg y} argument"
        )
    }
    if (...length() > 0L) {
        ans <- list2(...)
        names(ans) <- NULL
    } else {
        ans <- list(x = x, y = y)
    }
    structure(ans, class = "ggalign_layout_expand")
}

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

#' @importFrom S7 new_object S7_object
ContinuousDomain <- S7::new_class(
    "ContinuousDomain",
    parent = Domain,
    properties = list(
        spec = S7::new_property(
            S7::class_list,
            setter = function(self, value) {
                if (!is.null(prop(self, "spec"))) {
                    cli_abort("'@spec' is read-only")
                }
                prop(self, "spec") <- value
                self
            }
        )
    ),
    constructor = function(..., x = waiver(), y = waiver()) {
        if (...length() > 0L) {
            spec <- list2(...)
            names(spec) <- NULL
        } else {
            spec <- list(x = x, y = y)
        }
        new_object(S7_object(), spec = spec)
    }
)

#' @keywords internal
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
        if (is.na(prop(self, "nobs")) &&
            (!is.null(prop(self, "panel")) || !is.null(prop(self, "index")))) {
            return("'nobs' must be initialized before 'panel' or 'index'")
        }
    }
)

#' @importFrom S7 S7_inherits
is_continuous_domain <- function(x) S7_inherits(x, ContinuousDomain)

#' @importFrom S7 S7_inherits
is_discrete_domain <- function(x) S7_inherits(x, DiscreteDomain)

reorder_index <- function(panel, index = NULL) {
    index <- index %||% seq_along(panel)
    unlist(split(index, panel[index]), recursive = FALSE, use.names = FALSE)
}

domain_init <- S7::new_generic("domain_init", "domain")

S7::method(domain_init, ContinuousDomain) <- function(domain) domain

S7::method(domain_init, DiscreteDomain) <- function(domain) {
    # if `nobs` is not initialized, it means no `Align` object exist
    # it's not necessary to initialize the `panel` and `index`
    # this is for `stack_layout` which may have no data
    if (is.na(nobs <- domain@nobs)) {
        return(domain)
    }
    panel <- prop(domain, "panel") %||% factor(rep_len(1L, nobs))
    index <- prop(domain, "index") %||% reorder_index(panel)
    DiscreteDomain(panel[index], index, nobs)
}

S7::method(domain_init, S7::class_any) <- function(domain) {
    # `NULL` is a un-defined `ContinuousDomain`
    if (is.null(domain)) return(domain) # styler: off
    cli_abort("{.arg domain} must be a valid {.cls Domain} object")
}

############################################################
discrete_domain_update <- function(old, new, old_name, new_name,
                                   call = caller_call()) {
    old_nobs <- prop(old, "nobs")
    new_nobs <- prop(new, "nobs")
    if (is.na(new_nobs)) { # no `nobs` provided
        nobs <- old_nobs
    } else if (is.na(old_nobs)) {
        nobs <- new_nobs
    } else if (!identical(new_nobs, old_nobs)) {
        cli_abort(sprintf(
            "%s (nobs: %d) is not compatible with the %s (nobs: %d)",
            new_name, new_nobs, old_name, old_nobs
        ), call = call)
    } else {
        nobs <- new_nobs
    }

    # check panel
    old_panel <- prop(old, "panel")
    new_panel <- prop(new, "panel")

    if (is.null(new_panel)) { # no panel provided
        panel <- old_panel
    } else if (!is.null(old_panel) && !(new_panel %nest% old_panel)) {
        cli_abort(sprintf(
            "%s disrupt the previously established panel groups of %s",
            new_name, old_name
        ), call = call)
    } else {
        panel <- new_panel
    }

    # check index
    old_index <- prop(old, "index")
    new_index <- prop(new, "index")
    if (is.null(new_index)) {
        index <- old_index
    } else {
        index <- new_index
    }

    # we always make the index following the panel
    if (!is.null(panel) && !is.null(index)) {
        index <- reorder_index(panel, index)
    }

    # we always prevent from reordering twice.
    if (!is.null(old_index) && !all(old_index == index)) {
        cli_abort(sprintf(
            "%s disrupt the previously established ordering index of %s",
            new_name, old_name
        ), call = call)
    }
    DiscreteDomain(panel, index, nobs)
}
