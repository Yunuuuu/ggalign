rd_layout <- function() {
    sprintf("%s or [`stack_layout()`] object", rd_quad())
}

rd_quad <- function() "[`quad_layout()`]/[`ggheatmap()`]"

rd_chain_what <- function() {
    paste(
        "A single number or string of the plot elements in the layout.",
        "If `NULL`, will remove any active context"
    )
}

rd_quad_position <- function(action) {
    sprintf(
        "A string of %s indicates which annotation stack should be %s",
        oxford_or(.TLBR), action
    )
}

rd_layout_data <- function() {
    paste(
        "Default dataset to use for the layout. If not specified, it must be",
        "supplied in each plot added to the layout"
    )
}

rd_gg_aesthetics <- function(...) {
    ans <- ggfun("rd_aesthetics")(...)
    ans <- sub("link[=", "link[ggplot2:", ans, fixed = TRUE)
    sub("(vignette\\([^)]+)\\)", "\\1, package = \"ggplot2\")", ans)
}

rd_collect_family <- function(family,
                              section_title = paste(family, "family"),
                              code_style = TRUE) {
    # get blocks objects from the roxygenize function
    blocks <- NULL
    pos <- sys.nframe()
    while (pos > 0L) {
        if (!is.null(call <- sys.call(-pos))) {
            fn <- eval(.subset2(call, 1L), sys.frame(-(pos + 1L)))
            env <- sys.frame(-pos)
            if (identical(fn, getFromNamespace("roxygenize", "roxygen2")) &&
                exists("blocks", envir = env, inherits = FALSE)) {
                blocks <- get("blocks", envir = env, inherits = FALSE)
                break
            }
        }
        pos <- pos - 1L
    }
    blocks <- blocks[
        vapply(blocks, function(block) {
            getFromNamespace("block_has_tags", "roxygen2")(block, "family") &&
                identical(
                    getFromNamespace("block_get_tag_value", "roxygen2")(
                        block, "family"
                    ),
                    family
                )
        }, logical(1L), USE.NAMES = FALSE)
    ]
    if (length(blocks) == 0L) {
        return(character())
    }
    funs <- vapply(blocks, function(block) {
        as.character(.subset2(block$call, 2L))
    }, character(1L), USE.NAMES = FALSE)
    if (code_style) {
        items <- sprintf("\\code{\\link[=%s]{%s()}}", funs, funs)
    } else {
        items <- sprintf("\\link[=%s]{%s()}", funs, funs)
    }
    c(
        sprintf("@section %s:", section_title),
        "\\itemize{",
        sprintf("  \\item %s", items),
        "}"
    )
}
