htanno_dendro <- function() {

}

HtannoDendro <- ggplot2::ggproto("HtannoDendro", HtannoProto,
    setup_params = function(self, data, position, params) {
        if (nrow(data) != length(group <- .subset2(params, "group"))) {
            cli::cli_abort(paste(
                "{.arg group} of {.fn {snake_class(self)}} must be ",
                sprintf(
                    "the same length of heatmap {to_axis(position)} axis (%d)",
                    nrow(data)
                )
            ))
        }
        params$group <- factor(group)
        params
    },
    make_slice = function(data, position, statistics, old_group, group) {
        if (!is.null(old_group)) {
            cli::cli_abort(c(
                "{.fn {snake_class(self)}} cannot do sub-split",
                i = "group of heatmap {to_axis(position)} already exists"
            ))
        }
        group
    }
)
