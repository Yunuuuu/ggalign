patch_title <- function(border, label,
                        alignment = waiver(), element = waiver()) {
    border <- match(border, BORDERS)
    assert_string(label)
    alignment <- match.arg(alignment, c("plot", "panel"))
    assert_s3_class(element, "element_text")
    structure(
        list(
            border = border, label = label,
            alignment = alignment, element = element
        ),
        class = "patch_title"
    )
}

#' @importFrom ggplot2 ggplot_add merge_element
#' @export
ggplot_add.patch_title <- function(object, plot, object_name) {
    border <- .subset2(object, "border")
    label <- .subset2(object, "label")
    new <- .subset2(object, "element")
    old <- .subset2(.subset2(plot, "patch_titles"), border)
    plot$patch_titles[[border]] <- list(
        label = label,
        alignment = .subset2(object, "alignment"),
        element = merge_element(new, old)
    )
    plot
}

# element_render(
#     theme, "plot.title", plot$labels$title,
#     margin_y = TRUE, margin_x = TRUE
# )
# theme(plot.title = element_text())
