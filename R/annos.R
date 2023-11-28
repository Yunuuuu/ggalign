AnnoList <- R6::R6Class("AnnoList",
    public = list(
        anno_list = list(),
        draw_row = function(data) {

        },
        draw_column = function(data) {

        },
        draw_fn = function(data) {
            cli::cli_abort("Not implemented yet")
        }
    )
)

annotation_list <- function() {
    AnnoList$new()
}
