#' Get Data from the Attribute Attached by ggalign
#'
#' @description
#' `ggalign_attr` retrieves supplementary information stored as attributes
#' during the layout rendering process. These attributes—typically added during
#' data transformation by functions such as [`fortify_matrix()`] or
#' [`fortify_data_frame()`]—may contain filtered data, auxiliary metadata, or
#' other context essential for downstream operations.
#'
#' Factor level information, stored as a separate attribute, can be accessed via
#' `ggalign_lvls`.
#'
#' @details
#' Attributes attached to the data are especially useful when the input data is
#' transformed in ways that limit access to the complete dataset. For example,
#' [`fortify_matrix.MAF()`] might filter mutation data while adding attributes
#' that retain important context, such as the total number of observations, for
#' detailed or aggregated analyses. Additionally, it stores the levels of
#' `Variant_Classification` for further usage.
#'
#' @param x Data used, typically inherited from the layout `r rd_layout()`.
#' @param field A string specifying the particular data to retrieve from the
#' attached attribute. If `NULL`, the entire attached attribute list will be
#' returned.
#' @param check A boolean indicating whether to check if the `field` exists. If
#' `TRUE`, an error will be raised if the specified `field` does not exist.
#' @return
#' - `ggalign_attr`: The specified data from the attached supplementary data or
#' `NULL` if it is unavailable.
#' - `ggalign_lvls`: The attached supplementary levels or `NULL` if it is
#'   unavailable.
#'
#' @export
ggalign_attr <- function(x, field = NULL, check = TRUE) {
    assert_string(field, allow_empty = FALSE, allow_null = TRUE)
    if (is.null(x <- ggalign_attr_get(x)) || is.null(field)) {
        return(x)
    }
    if (isTRUE(check) && !rlang::has_name(x, field)) {
        cli_abort("Cannot find {field} in {.arg x}")
    }
    .subset2(x, field)
}

#' @export
#' @rdname ggalign_attr
ggalign_lvls <- function(x) ggalign_lvls_get(x)

#' Attach supplementary data and levels for ggalign
#'
#' @param .data Input data for the layout.
#' @param ... <[dyn-dots][rlang::dyn-dots]> A list of data to be attached.
#' @param .lvls A character vector representing the attached levels.
#' @note Used by developers in [`fortify_matrix()`], [`fortify_data_frame()`],
#'   and other related methods.
#' @seealso [`ggalign_attr()`]/[`ggalign_lvls()`]
#' @importFrom rlang list2
#' @export
ggalign_data_set <- function(.data, ..., .lvls = NULL) {
    if (...length() > 0L) {
        .data <- ggalign_attr_set(.data, list2(...))
    }
    if (!is.null(.lvls)) {
        .data <- ggalign_lvls_set(.data, .lvls)
    }
    # to prevent the print of attributes
    if (!is.null(ggalign_attr_get(.data)) ||
        !is.null(ggalign_lvls_get(.data))) {
        .data <- add_class(.data, "ggalign_data")
    }
    .data
}

#' @export
print.ggalign_data <- function(x, ...) {
    print(
        remove_class(
            ggalign_lvls_remove(ggalign_attr_remove(x)),
            "ggalign_data"
        )
    )
    invisible(x)
}

ggalign_attr_set <- function(x, values) {
    attr(x, ".__ggalign_attr__") <- values
    x
}

ggalign_attr_get <- function(x) attr(x, ".__ggalign_attr__", exact = TRUE)

ggalign_attr_remove <- function(x) ggalign_attr_set(x, NULL)

ggalign_lvls_set <- function(x, lvls) {
    attr(x, ".__ggalign_levels__") <- lvls
    x
}

ggalign_lvls_get <- function(x) attr(x, ".__ggalign_levels__", exact = TRUE)

ggalign_lvls_remove <- function(x) ggalign_lvls_set(x, NULL)

# we keep a special attribute across all data
# this is used to pass additional annotation informations
ggalign_data_restore <- function(data, original) {
    if (is.null(data) || is.waive(data)) return(data) # styler: off
    if (is.null(ggalign_attr_get(data)) && # no attached attribute
        # the original has attached attribute
        !is.null(value <- ggalign_attr_get(original))) {
        data <- ggalign_attr_set(data, value)
    }

    if (is.null(ggalign_lvls_get(data)) && # no attached levels
        # the original has attached levels
        !is.null(value <- ggalign_lvls_get(original))) {
        data <- ggalign_lvls_set(data, value)
    }
    # to prevent the print of attributes
    if (!is.null(ggalign_attr_get(data)) ||
        !is.null(ggalign_lvls_get(data))) {
        data <- add_class(data, "ggalign_data")
    }
    data
}
