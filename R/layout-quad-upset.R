#' Create an UpSet plot
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' `ggupset` is a specialized version of [`quad_discrete()`], which simplifies
#' the creation of Upset plot.
#'
#' @param data Data used to create the UpSet plot. [`fortify_matrix()`] will be
#' used to convert the data to a matrix. Currently, only
#' [`fortify_matrix.list_upset`] and [`fortify_matrix.matrix_upset`] are
#' suitable for creating an UpSet plot.
#' @param ... Additional arguments passed to [`fortify_matrix()`].
#' @inheritParams quad_layout
#' @param direction A string indicating the direction of the UpSet plot,
#' `"h"`(`horizontal`) or `"v"`(`vertical`). In a vertical UpSet plot, the
#' columns of the matrix correspond to the sets, and the rows correspond to the
#' intersections. By default, the horizontal UpSet plot is used, where the rows
#' of the matrix correspond to the sets and the columns correspond to the
#' intersections.
#' @param point A list of parameters passed to
#' [`geom_point()`][ggplot2::geom_point()].
#' @param line A list of parameters passed to
#' [`geom_line()`][ggplot2::geom_line()].
#' @param rect A list of parameters passed to
#' [`geom_rect()`][ggplot2::geom_rect()].
#'
#' @inheritSection heatmap_layout ggplot2 specification
#' @examples
#' set.seed(123)
#' lt <- list(
#'     a = sample(letters, 5),
#'     b = sample(letters, 10),
#'     c = sample(letters, 15)
#' )
#' ggupset(tune(lt)) +
#'     scale_fill_manual(values = c("#F0F0F0", "white"), guide = "none") +
#'     scale_color_manual(values = c("grey", "black"), guide = "none") +
#'     anno_top() +
#'     ggalign(data = function(d) ggalign_attr(d, "intersection_sizes")) +
#'     ggplot2::geom_bar(aes(y = .data$value), stat = "identity") +
#'     anno_right() +
#'     ggalign(data = function(d) ggalign_attr(d, "set_sizes")) +
#'     ggplot2::geom_bar(aes(x = .data$value),
#'         stat = "identity",
#'         orientation = "y"
#'     )
#' @importFrom ggplot2 aes
#' @export
ggupset <- function(data = NULL, mapping = aes(),
                    ...,
                    direction = "h",
                    point = NULL, line = NULL, rect = NULL,
                    width = NA, height = NA,
                    theme = NULL, active = NULL) {
    UseMethod("ggupset")
}

# Don't allow inherit from the parent layout, since data for upset plot is
# usually different with others
#' @export
ggupset.NULL <- function(data, ...) {
    cli_abort("{.arg data} must be provided to create upset plot")
}

#' @export
ggupset.waiver <- ggupset.NULL

#' @importFrom ggplot2 aes
#' @export
ggupset.default <- function(data = NULL, mapping = aes(),
                            ...,
                            direction = "h",
                            point = NULL, line = NULL, rect = NULL,
                            width = NA, height = NA,
                            theme = NULL, active = NULL) {
    direction <- check_direction(direction)
    # we need a matrix to melted into long formated data frame
    data <- fortify_matrix(data = data, ...)
    if (is_vertical(direction)) data <- ggalign_data_restore(t(data), data)
    ans <- new_quad_layout(
        name = "ggupset",
        data = data,
        mapping = mapping,
        theme = theme,
        active = active
    )
    ans@plot <- ggadd_default(ans@plot, mapping = aes(.data$.x, .data$.y)) +
        ggplot2::labs(x = NULL, y = NULL) +
        upset_rect(direction, rect) +
        upset_point(point) +
        upset_line(direction, line)
    ans
}

merge_mapping <- function(x, y) {
    if (is.null(x)) {
        return(y)
    }
    for (i in names(y)) {
        x[[i]] <- .subset2(y, i)
    }
    x
}

merge_data_fn <- function(plot_data, user_data) {
    if (is.null(user_data) || is_waiver(user_data)) {
        plot_data
    } else if (is.function(user_data <- allow_lambda(user_data))) {
        force(plot_data)
        function(data) user_data(plot_data(data))
    } else {
        user_data
    }
}

upset_rect <- function(direction, rect) {
    if (is_horizontal(direction)) {
        rect$mapping <- merge_mapping(rect$mapping, aes(
            ymin = .data$.ymin, ymax = .data$.ymax, fill = .data$rect_group
        ))
        rect$data <- merge_data_fn(function(data) {
            column <- c(
                ".y", ".panel_x", ".panel_y",
                ".row_index", ".row_names", ".discrete_y"
            )
            o <- vec_unique(data[vec_set_intersect(column, names(data))])
            o$rect_group <- (.subset2(o, ".y") %% 2L) == 0L
            o$.ymin <- o$.y - 0.5
            o$.ymax <- o$.y + 0.5
            o
        }, rect$data)
        inject(ggplot2::geom_rect(!!!rect,
            xmin = -Inf, xmax = Inf, inherit.aes = FALSE
        ))
    } else {
        rect$mapping <- merge_mapping(rect$mapping, aes(
            xmin = .data$.xmin, xmax = .data$.xmax,
            fill = .data$rect_group
        ))
        rect$data <- merge_data_fn(function(data) {
            column <- c(
                ".x", ".panel_x", ".panel_y",
                ".column_index", ".column_names", ".discrete_x"
            )
            o <- vec_unique(data[vec_set_intersect(column, names(data))])
            o$rect_group <- (.subset2(o, ".x") %% 2L) == 0L
            o$.xmin <- o$.x - 0.5
            o$.xmax <- o$.x + 0.5
            o
        }, rect$data)
        inject(ggplot2::geom_rect(!!!rect,
            ymin = -Inf, ymax = Inf, inherit.aes = FALSE
        ))
    }
}

upset_point <- function(point) {
    point$mapping <- merge_mapping(point$mapping, aes(
        x = .data$.x, y = .data$.y, color = .data$point_group
    ))
    point$data <- merge_data_fn(function(data) {
        data$point_group <- .subset2(data, "value")
        data
    }, point$data)
    inject(ggplot2::geom_point(!!!point, inherit.aes = FALSE))
}

upset_line <- function(direction, line) {
    line$mapping <- merge_mapping(line$mapping, switch_direction(
        direction,
        aes(.data$.x, .data$.y, group = paste(.data$.panel_x, .data$.x)),
        aes(.data$.x, .data$.y, group = paste(.data$.panel_y, .data$.y))
    ))
    line$data <- merge_data_fn(function(data) {
        if (is_horizontal(direction)) {
            dlist <- vec_split(data, data[c(".panel_x", ".x")])
            dlist <- lapply(.subset2(dlist, "val"), function(d) {
                o <- vec_slice(d, .subset2(d, "value"))
                if (vec_size(o) < 2L) {
                    return(NULL)
                }
                vec_slice(o, c(
                    which.min(.subset2(o, ".y")),
                    which.max(.subset2(o, ".y"))
                ))
            })
            vec_rbind(!!!dlist)
        } else {
            dlist <- vec_split(data, data[c(".panel_y", ".y")])
            dlist <- lapply(.subset2(dlist, "val"), function(d) {
                o <- vec_slice(d, .subset2(d, "value"))
                if (vec_size(o) < 2L) {
                    return(NULL)
                }
                vec_slice(o, c(
                    which.min(.subset2(o, ".x")),
                    which.max(.subset2(o, ".x"))
                ))
            })
            vec_rbind(!!!dlist)
        }
    }, line$data)
    inject(ggplot2::geom_line(!!!line, inherit.aes = FALSE))
}
