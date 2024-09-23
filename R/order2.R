#' Ordering Permutation
#'
#' `order2` returns a permutation which rearranges its first argument into
#' ascending order. By default, `order2` will call [order()] directly.
#' @param x Any objects can be extracting ordering.
#' @return An integer vector unless any of the inputs has `2^31` or more
#' elements, when it is a double vector.
#' @examples
#' order2(sample(10L))
#' order2(letters)
#' @export
order2 <- function(x) UseMethod("order2")

#' @export
#' @rdname order2
order2.default <- function(x) order(x)

#' @export
#' @rdname order2
order2.hclust <- function(x) x$order

#' @importFrom stats order.dendrogram
#' @export
#' @rdname order2
order2.dendrogram <- function(x) order.dendrogram(x)

#' @importFrom utils getFromNamespace
#' @export
#' @rdname order2
order2.ser_permutation_vector <- function(x) {
    rlang::check_installed(
        "seriation", "to extract order from `ser_permutation_vector`"
    )
    getFromNamespace("get_order", "seriation")(x)
}

#' @importFrom utils getFromNamespace
#' @export
#' @rdname order2
order2.ser_permutation <- function(x) {
    rlang::check_installed(
        "seriation", "to extract order from `ser_permutation`"
    )
    getFromNamespace("get_order", "seriation")(x)
}
