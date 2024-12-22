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
