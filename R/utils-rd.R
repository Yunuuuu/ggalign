rd_values <- function(x, quote = TRUE, code = TRUE, sep = ",", final = "and") {
    if (quote) x <- paste0("\"", x, "\"")
    if (code) x <- paste0("`", x, "`")
    oxford_comma(x, sep = sep, final = final)
}

rd_theme <- function() {
    paste(
        "A [theme()][ggplot2::theme] object to rendering the guides",
        "title, subtitle, caption, margins and background.",
        sep = ", "
    )
}
