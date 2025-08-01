#' Layout operator
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'  - `+`: Adds elements to the active plot in the active layout.
#'  - `&`: Applies elements to all plots in the layout.
#'  - `-`: Adds elements to multiple plots in the layout.
#'
#' @details
#' The `+` operator is straightforward and should be used as needed.
#'
#' In order to reduce code repetition `ggalign` provides two operators for
#' adding ggplot elements (geoms, themes, facets, etc.) to multiple/all plots in
#' `r rd_layout()`: `-` and `&`.
#'
#' @param e1 A `r rd_layout()`.
#' @param e2 An object to be added to the plot.
#' @return A modified `Layout` object.
#' @examples
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 7)
#' ggheatmap(small_mat) +
#'     anno_top() +
#'     ggalign() +
#'     geom_point(aes(y = value))
#'
#' # `&` operator apply it to all plots
#' ggheatmap(small_mat) +
#'     anno_top() +
#'     align_dendro() &
#'     theme(panel.border = element_rect(
#'         colour = "red", fill = NA, linewidth = unit(2, "mm")
#'     ))
#'
#' # If the active layout is the annotation stack, the `-` operator will only
#' # add the elements to all plots in the active annotation stack:
#' ggheatmap(small_mat) +
#'     anno_left(size = 0.2) +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     align_dendro(aes(color = branch), k = 3L) -
#'     # Modify the the color scales of all plots in the left annotation
#'     scale_color_brewer(palette = "Dark2")
#'
#' # If the active layout is the `stack_layout()` itself, `-`
#' # applies the elements to all plots in the layout except the nested
#' # `ggheatmap()`/`quad_layout()`.
#' stack_alignv(small_mat) +
#'     align_dendro() +
#'     ggtitle("I'm from the parent stack") +
#'     ggheatmap() +
#'     # remove any active context
#'     stack_active() +
#'     align_dendro() +
#'     ggtitle("I'm from the parent stack") -
#'     # Modify the the color scales of all plots in the stack layout except the
#'     # heatmap layout
#'     scale_color_brewer(palette = "Dark2") -
#'     # set the background of all plots in the stack layout except the heatmap
#'     # layout
#'     theme(plot.background = element_rect(fill = "red"))
#'
#' @name layout-operator
local(S7::method(`+`, list(LayoutProto, S7::class_any)) <- function(e1, e2) {
    if (missing(e2)) {
        cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }

    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2, env = caller_env(2L)))
    layout_add(e1, e2, e2name)
})

#' @include layout-.R
local(S7::method(`-`, list(LayoutProto, S7::class_any)) <- function(e1, e2) {
    if (missing(e2)) {
        cli_abort(c(
            "Cannot use {.code -} with a single argument.",
            "i" = "Did you accidentally put {.code -} on a new line?"
        ))
    }

    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2, env = caller_env(2L)))
    layout_subtract(e1, e2, e2name)
})

#' @include layout-.R
local(S7::method(`&`, list(LayoutProto, S7::class_any)) <- function(e1, e2) {
    if (missing(e2)) {
        cli_abort(c(
            "Cannot use {.code &} with a single argument.",
            "i" = "Did you accidentally put {.code &} on a new line?"
        ))
    }

    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2, env = caller_env(2L)))
    layout_and_add(e1, e2, e2name)
})

#################################################################
layout_add <- function(layout, object, object_name) {
    UseMethod("layout_add")
}

#' @export
`layout_add.ggalign::QuadLayout` <- function(layout, object, object_name) {
    quad_layout_add(object, layout, object_name)
}

#' @export
`layout_add.ggalign::ChainLayout` <- function(layout, object, object_name) {
    chain_layout_add(object, layout, object_name)
}

#################################################################
layout_subtract <- function(layout, object, object_name) {
    UseMethod("layout_subtract")
}

#' @export
`layout_subtract.ggalign::QuadLayout` <- function(layout, object, object_name) {
    quad_layout_subtract(object, layout, object_name)
}

#' @export
`layout_subtract.ggalign::ChainLayout` <- function(layout, object, object_name) {
    chain_layout_subtract(object, layout, object_name)
}

#################################################################
# we use and_add suffix here, since `and` is very similar with `add`.
layout_and_add <- function(layout, object, object_name) {
    UseMethod("layout_and_add")
}

#' @export
`layout_and_add.ggalign::QuadLayout` <- function(layout, object, object_name) {
    quad_layout_and_add(object, layout, object_name)
}

#' @export
`layout_and_add.ggalign::ChainLayout` <- function(layout, object, object_name) {
    chain_layout_and_add(object, layout, object_name)
}

# For objects cannot be used with `-` or `&`
#' @include layout-quad-operator.R
#' @include layout-chain-operator.R
lapply(
    c(
        "quad_layout_subtract", "chain_layout_subtract",
        "quad_layout_and_add", "chain_layout_and_add"
    ),
    function(genname) {
        params <- .subset2(strsplit(genname, "_"), 1L)

        # function argument list
        pairlist <- rlang::pairlist2(object = , layout = , object_name = )
        names(pairlist) <- c("object", .subset(params, 1L), "object_name")
        operator <- switch(.subset(params, 3L),
            subtract = "-",
            and = "&"
        )
        # styler: off
        for (class in c("ggplot", "quad_active", "quad_anno", "layout_title",
                        "layout_theme", "ggalign::CraftBox", 
                        "ggalign::ChainLayout",
                        "ggalign::QuadLayout", "continuous_limits")) {
            # styler: on
            registerS3method(
                genname, class,
                rlang::new_function(pairlist, substitute(
                    {
                        cli_abort(c(
                            sprintf(
                                "Cannot add %s with {.code %s}",
                                name, operator
                            ),
                            i = "Try to use {.code +} instead"
                        ))
                    },
                    list(
                        name = switch(class,
                            CraftBox = ,
                            ChainLayout = ,
                            QuadLayout = quote(object_name(object)),
                            # for all others
                            "{.var {object_name}}"
                        ),
                        operator = operator
                    )
                ))
            )
        }
    }
)
