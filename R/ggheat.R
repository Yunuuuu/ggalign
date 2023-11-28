ggheat <- function(.matrix, .fn = NULL, ..., row_title = NULL, column_title = NULL, .blank = FALSE, .name = NULL) {
    matrix <- build_matrix(.matrix)
    name <- build_name(.name)
    draw_fn <- build_function(.fn)
    assert_bool(.blank)
    structure(list(
        name = name,
        row_title = row_title,
        column_title = column_title,
        matrix = matrix,
        draw_fn = draw_fn,
        draw_params = rlang::list2(...),
        blank = .blank,
        top = annotation_list(),
        bottom = annotation_list(),
        left = annotation_list(),
        right = annotation_list()
    ), class = c("ggheatmap", "ggheat"))
}
