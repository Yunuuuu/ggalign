rd_values <- function(x, quote = TRUE, sep = ",", final = "and") {
    if (quote) x <- paste0("\"", x, "\"")
    x <- paste0("`", x, "`")
    oxford_comma(x, sep = sep, final = final)
}
