rd_values <- function(x, quote = TRUE, code = TRUE, sep = ", ", final = "and") {
    if (quote) x <- paste0("\"", x, "\"")
    if (code) x <- paste0("`", x, "`")
    oxford_comma(x, sep = sep, final = final)
}

rd_layout <- function() "[heatmap_layout()] or [stack_layout()] object"

rd_theme <- function() {
    paste(
        "One of:",
        "- [`waiver()`][ggplot2::waiver()]: will inherit from the parent layout.",
        "- `NULL`: Use the [default theme][theme_ggalign].",
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

rd_pos <- function(x, null, default = "it inherits from the parent layout") {
    paste(
        "A string containing one or more of", rd_values(.tlbr),
        sprintf("indicates %s.", x),
        sprintf("If `NULL`, %s.", null),
        "If [`waiver()`][ggplot2::waiver()], it will inherit from the parent layout"
    )
}

rd_guides <- function() {
    rd_pos(
        "which side of guide legends should be collected",
        "no guide legends will be collected"
    )
}

rd_free_guides <- function() {
    paste(
        "Options include:",
        "- [`waiver()`][ggplot2::waiver()]: inherits behavior from the layout.",
        "- `NULL`: no guide legends will be collected for the plot.",
        paste(
            "- A string containing one or more of",
            rd_values(.tlbr),
            "indicates which side of guide legends",
            "should be collected for the plot."
        ),
        sep = "\n"
    )
}

rd_heatmap_size <- function() {
    "Heatmap body width/height, can be a [unit][grid::unit] object"
}
