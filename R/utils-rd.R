rd_values <- function(x, quote = TRUE, code = TRUE, sep = ", ", final = "and") {
    if (quote) x <- paste0("\"", x, "\"")
    if (code) x <- paste0("`", x, "`")
    oxford_comma(x, sep = sep, final = final)
}

rd_layout <- function() "[heatmap_layout()] or [stack_layout()] object"

rd_theme <- function() {
    paste(
        "One of:",
        "- `NULL`: will inherit from the parent layout directly.",
        "- [`theme()`][ggplot2::theme]: will be added with the parent layout theme.",
        sep = "\n"
    )
}

rd_stack_what <- function() {
    paste(
        "Options include::",
        "- A single number or string of the plot elements in the stack layout.",
        "- `NULL`: remove any active context",
        sep = "\n"
    )
}

rd_heatmap_size <- function() {
    "Heatmap body width/height, can be a [unit][grid::unit] object"
}
