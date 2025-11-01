# We are removing the patchwork dependency by defining our own version of
# patchwork::area, as some desired features won't be merged (see this
# https://github.com/thomasp85/patchwork/issues/379). Therefore, ggalign will
# retain `alignpatch-*` scripts.

#' Define the plotting areas
#'
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
#' @return A `ggalign_area` object.
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
#' # Apply it to a alignpatches
#' align_plots(p1, p2, p3, design = layout)
#' @export
area <- function(t, l, b = t, r = l) {
    if (missing(t) || missing(l)) {
        one_area <- list(
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
            cli_abort("{.arg t} must be less than {.arg b}")
        }
        if (any(.subset2(one_area, "l") > .subset2(one_area, "r"))) {
            cli_abort("{.arg l} must be less than {.arg r}")
        }
    }
    new_areas(one_area)
}

#' @export
length.ggalign_area <- function(x) vec_size(x)

#' @export
print.ggalign_area <- function(x, ...) {
    vctrs::obj_print(x, ...) # skip patch_area method
    invisible(x)
}

# Define a custom S3 class `ggalign_area`, without using S7 directly
# Patchwork expects traditional S3 classes, and S7 classes are not fully
# interoperable with patchwork layouts.
S3_area <- S7::new_S3_class("ggalign_area")

new_areas <- function(x) new_rcrd(x, class = c("ggalign_area", "patch_area"))

#' @export
c.ggalign_area <- function(...) vec_c(...)

create_area <- function(ncol, nrow, byrow) {
    mat <- matrix(seq_len(ncol * nrow),
        nrow = nrow, ncol = ncol, byrow = byrow
    )
    ind <- as.vector(mat)
    ind <- match(seq_along(ind), ind)
    area(t = row(mat)[ind], l = col(mat)[ind])
}

#' @export
obj_print_data.ggalign_area <- function(x, ...) {
    if (vec_size(x) > 0) {
        data <- vec_data(x)
        data <- vec_set_names(data, paste0(vec_seq_along(data), ": "))
        print(x = data, ..., quote = FALSE)
    } else {
        cat("   ", "\n", sep = " ")
    }
}

#' @export
obj_print_footer.ggalign_area <- function(x, ...) {
    if (vec_size(x) == 0) {
        ncols <- 0
        nrows <- 0
    } else {
        ncols <- max(field(x, "r"))
        nrows <- max(field(x, "b"))
    }
    cat("\n<Spanning", ncols, "columns and", nrows, "rows>\n")
}

#' @export
vec_ptype_abbr.ggalign_area <- function(x, ...) "areas"

trim_area <- function(area) {
    area <- vec_data(area)
    w <- min(.subset2(area, "l"), .subset2(area, "r"))
    h <- min(.subset2(area, "t"), .subset2(area, "b"))
    area$l <- .subset2(area, "l") - w + 1L
    area$r <- .subset2(area, "r") - w + 1L
    area$t <- .subset2(area, "t") - h + 1L
    area$b <- .subset2(area, "b") - h + 1L
    new_areas(area)
}

#' Convert object into a design area
#' @keywords internal
#' @noRd
as_areas <- function(x) UseMethod("as_areas")

#' @export
as_areas.default <- function(x) {
    cli_abort("Cannot convert {.obj_type_friendly {x}} into a design area")
}

#' @export
as_areas.ggalign_area <- function(x) x

#' @export
as_areas.character <- function(x) {
    call <- current_call() # used for message only
    # split into rows
    x <- .subset2(strsplit(x, split = "\n"), 1L)
    x <- lapply(x, trimws)
    if (identical(x[[1L]], "")) x[[1L]] <- NULL
    if (identical(x[[length(x)]], "")) x[[length(x)]] <- NULL
    x <- lapply(x, function(x) .subset2(strsplit(x, split = ""), 1L))
    ncols <- list_sizes(x)
    ncol <- .subset(ncols, 1L)
    if (any(ncols != ncol)) {
        cli_abort("character layout must be rectangular", call = call)
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
            cli_abort("Design areas must be rectangular", call = call)
        }
        new_areas(list(t = t, l = l, b = b, r = r))
    })
    vec_c(!!!vec_set_names(area_list, NULL))
}

# For area from patchwork
#' @export
as_areas.patch_area <- function(x) new_areas(unclass(x))

#' @importFrom grid unit
#' @importFrom ggplot2 ggplot aes margin theme
#' @keywords internal
#' @export
ggplot.ggalign_area <- function(data, ...) {
    data <- vec_data(data)
    data$l <- data$l - 0.45
    data$r <- data$r + 0.45
    data$t <- data$t - 0.45
    data$b <- data$b + 0.45
    data$name <- as.factor(vec_seq_along(data))
    b_fun <- function(lim) {
        if (lim[1] < lim[2]) {
            lim <- seq(floor(lim[1]), ceiling(lim[2]), by = 1)
        } else {
            lim <- seq(ceiling(lim[1]), floor(lim[2]), by = -1)
        }
        lim[-c(1, length(lim))]
    }
    ggplot(data, ...) +
        ggplot2::geom_rect(
            aes(
                xmin = .data$l, xmax = .data$r,
                ymin = .data$t, ymax = .data$b,
                fill = .data$name
            ),
            alpha = 0.3
        ) +
        ggplot2::scale_y_reverse(breaks = b_fun, expand = c(0, 0.04)) +
        ggplot2::scale_x_continuous(
            breaks = b_fun, expand = c(0, 0.04), position = "top"
        ) +
        ggplot2::labs(fill = "Patch") +
        ggplot2::theme_void() +
        theme(
            panel.grid.minor = ggplot2::element_line(
                linewidth = 0.5, colour = "grey"
            ),
            axis.text = ggplot2::element_text(),
            axis.ticks.length = unit(3, "mm"),
            plot.margin = margin(10, 10, 10, 10)
        )
}

#' @export
plot.ggalign_area <- function(x, ...) plot(ggplot(data = x, ...))
