# We are removing the patchwork dependency by defining our own version of
# patchwork::area, as some desired features won't be merged (see this
# https://github.com/thomasp85/patchwork/issues/379). Therefore, ggalign will
# retain `alignpatch-*` scripts.
#' Define the plotting areas in `align_plots`
#' @inherit patchwork::area
#' @details
#' The grid that the areas are specified in reference to enumerate rows from top
#' to bottom, and coloumns from left to right. This means that `t` and `l`
#' should always be less or equal to `b` and `r` respectively. Instead of
#' specifying area placement with a combination of `area()` calls, it is
#' possible to instead pass in a single string
#'
#' ```
#' areas <- c(area(1, 1, 2, 1),
#'            area(2, 3, 3, 3))
#' ```
#'
#' is equivalent to
#'
#' ```
#' areas < -"A##
#'           A#B
#'           ##B"
#' ```
#' @return A `align_area` object.
#' @examples
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) +
#'     geom_bar(aes(gear)) +
#'     facet_wrap(~cyl)
#'
#' layout <- c(
#'     area(1, 1),
#'     area(1, 3, 3),
#'     area(3, 1, 3, 2)
#' )
#'
#' # Show the layout to make sure it looks as it should
#' plot(layout)
#'
#' # Apply it to a patchwork
#' align_plots(p1, p2, p3, design = layout)
#' @importFrom vctrs df_list vec_cast new_data_frame
#' @export
area <- function(t, l, b = t, r = l) {
    if (missing(t) || missing(l)) {
        one_area <- df_list(
            t = integer(0L),
            l = integer(0L),
            b = integer(0L),
            r = integer(0L)
        )
    } else {
        one_area <- df_list(
            t = vec_cast(t, integer()),
            l = vec_cast(l, integer()),
            b = vec_cast(b, integer()),
            r = vec_cast(r, integer())
        )
        if (any(.subset2(one_area, "t") > .subset2(one_area, "b"))) {
            cli::cli_abort("{.arg t} must be less than {.arg b}")
        }
        if (any(.subset2(one_area, "l") > .subset2(one_area, "r"))) {
            cli::cli_abort("{.arg l} must be less than {.arg r}")
        }
    }
    new_data_frame(one_area, class = c("align_area", "patch_area"))
}

as_areas <- function(x) UseMethod("as_areas")

#' @export
as_areas.default <- function(x) {
    cli::cli_abort("Cannot convert {.obj_type_friendly {x}} into a design area")
}

#' @export
as_areas.NULL <- function(x) NULL

#' @export
as_areas.align_area <- function(x) x

#' @importFrom vctrs vec_rbind
#' @export
as_areas.character <- function(x) {
    call <- current_call() # used for message only
    # split into rows
    x <- .subset2(strsplit(x, split = "\n"), 1L)
    x <- lapply(x, trimws)
    if (identical(x[[1L]], "")) x[[1L]] <- NULL
    if (identical(x[[length(x)]], "")) x[[length(x)]] <- NULL
    x <- lapply(x, function(x) .subset2(strsplit(x, split = ""), 1L))
    ncols <- lengths(x)
    ncol <- .subset(ncols, 1L)
    if (any(ncols != ncol)) {
        cli::cli_abort("character layout must be rectangular", call = call)
    }
    row <- rep(seq_along(x), each = ncol)
    col <- rep(seq_len(ncol), length(x))
    x <- unlist(x, use.names = FALSE)
    # here, area will be reordered by the levels of `x`
    area_list <- imap(split(seq_along(x), x), function(i, name) {
        if (identical(name, "#")) {
            return(area())
        }
        area_rows <- range(row[i])
        area_cols <- range(col[i])
        t <- .subset(area_rows, 1L)
        l <- .subset(area_cols, 1L)
        b <- .subset(area_rows, 2L)
        r <- .subset(area_cols, 2L)
        if (!all(x[row >= t & row <= b & col >= l & col <= r] ==
            x[.subset(i, 1L)])) {
            cli::cli_abort("Patch areas must be rectangular", call = call)
        }
        area(t = t, l = l, b = b, r = r)
    })
    do.call(vec_rbind, area_list)
}

#' @importFrom vctrs vec_rbind
#' @export
c.align_area <- function(...) vec_rbind(...)

#' @importFrom vctrs vec_slice
#' @export
`[.align_area` <- function(x, i) vec_slice(x, i)

# For area from patchwork
#' @export
as_areas.patch_area <- function(x) add_class(x, "align_area")

#' @export
length.align_area <- function(x) length(.subset2(x, "t"))

#' @importFrom vctrs new_data_frame
#' @export
print.align_area <- function(x, ...) {
    cat(
        length(x), "patch areas, spanning",
        max(x$r), "columns and", max(x$b), "rows\n\n"
    )
    print(new_data_frame(unclass(x), row.names = paste0(seq_along(x), ": ")))
    invisible(x)
}

#' @importFrom grid unit
#' @importFrom ggplot2 aes margin theme
#' @export
plot.align_area <- function(x, ...) {
    x$l <- x$l - 0.45
    x$r <- x$r + 0.45
    x$t <- x$t - 0.45
    x$b <- x$b + 0.45
    x$name <- as.factor(seq_len(nrow(x)))
    b_fun <- function(lim) {
        if (lim[1] < lim[2]) {
            lim <- seq(floor(lim[1]), ceiling(lim[2]), by = 1)
        } else {
            lim <- seq(ceiling(lim[1]), floor(lim[2]), by = -1)
        }
        lim[-c(1, length(lim))]
    }
    ggplot2::ggplot(x) +
        ggplot2::geom_rect(aes(
            xmin = .data$l, xmax = .data$r,
            ymin = .data$t, ymax = .data$b, fill = .data$name
        ), alpha = 0.3) +
        ggplot2::scale_y_reverse(breaks = b_fun, expand = c(0, 0.04)) +
        ggplot2::scale_x_continuous(
            breaks = b_fun, expand = c(0, 0.04), position = "top"
        ) +
        ggplot2::labs(fill = "Patch") +
        ggplot2::theme_void() +
        theme(
            panel.grid.minor = ggplot2::element_line(
                size = 0.5, colour = "grey"
            ),
            axis.text = ggplot2::element_text(),
            axis.ticks.length = unit(3, "mm"),
            plot.margin = margin(10, 10, 10, 10)
        )
}
