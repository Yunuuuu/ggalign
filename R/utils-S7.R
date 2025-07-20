#' @importFrom rlang is_na
position_prop <- function(...) {
    S7::new_property(
        validator = function(value) {
            if (is.waive(value) || is.null(value) || is_na(value)) {
                return(NULL)
            }
            if (!is.character(value) || length(value) != 1L) {
                return(sprintf(
                    "must be a single string containing only the characters: %s",
                    oxford_and(c(.tlbr, "i"))
                ))
            }
            if (grepl("[^tlbri]", value)) {
                return(sprintf(
                    "can only include the characters", oxford_and(c(.tlbr, "i"))
                ))
            }
        },
        ...,
        default = NA
    )
}
