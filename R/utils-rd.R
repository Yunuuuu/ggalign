rd_values <- function(x, quote = TRUE, code = TRUE, sep = ", ", final = "and") {
    if (quote) x <- paste0("\"", x, "\"")
    if (code) x <- paste0("`", x, "`")
    oxford_comma(x, sep = sep, final = final)
}

rd_layout <- function() "[heatmap_layout()] or [stack_layout()] object"

rd_theme <- function() {
    paste(
        "One of:",
        "- [waiver()][ggplot2::waiver()]: will inherit from the parent layout.",
        "- `NULL`: Use the [default theme][theme_ggalign].",
        "- [theme()][ggplot2::theme]: will be added with the parent layout theme.",
        sep = "\n"
    )
}

rd_layout_theme <- function() {
    paste(
        "A [theme()][ggplot2::theme] object to rendering the guides",
        "title, subtitle, caption, margins and background.",
        sep = ", "
    )
}

rd_stack_what <- function() {
    paste(
        "Possible values are follows:",
        "* A single number or string of the plot elements in the stack layout.",
        "* `NULL`: remove any active context",
        sep = "\n"
    )
}

rd_guides <- function() {
    paste(
        "Which guide should be collected? A string containing one or more of",
        rd_values(.tlbr)
    )
}

rd_free_guides <- function() {
    paste(
        "One of:",
        "- [waiver()][ggplot2::waiver()]: Following the parent layout behaviour.",
        "- `NULL`: prevent the collection of guides for the plot.",
        paste(
            "- A string containing one or more of",
            rd_values(.tlbr),
            "indicates which guide should be collected for the plot."
        ),
        sep = "\n"
    )
}
