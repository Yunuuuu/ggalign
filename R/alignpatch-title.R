#' Annotate the whole layout
#'
#' @inheritParams ggplot2::labs
#' @return A `layout_title` object.
#' @examples
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) +
#'     geom_bar(aes(gear)) +
#'     facet_wrap(~cyl)
#' align_plots(p1, p2, p3) +
#'     layout_title(title = "I'm title")
#' @importFrom ggplot2 waiver
#' @include utils-ggplot.R
#' @export
layout_title <- S7::new_class("layout_title",
    properties = lapply(
        rlang::set_names(c("title", "subtitle", "caption")),
        function(type) {
            S7::new_property(
                S7::new_union(S3_waiver, NULL, S7::class_character),
                validator = function(value) {
                    if (!is_waiver(value) && !is.null(value) &&
                        length(value) != 1L) {
                        return("must be a single character string")
                    }
                },
                default = quote(waiver())
            )
        }
    )
)

#' @importFrom S7 prop prop<-
S7::method(init_object, layout_title) <- function(input) {
    prop(input, "title", check = FALSE) <- prop(input, "title") %|w|% NULL
    prop(input, "subtitle", check = FALSE) <- prop(input, "subtitle") %|w|% NULL
    prop(input, "caption", check = FALSE) <- prop(input, "caption") %|w|% NULL
    input
}

#' @importFrom ggplot2 is_waiver
#' @importFrom S7 props
local(
    S7::method(`+`, list(layout_title, layout_title)) <-
        function(e1, e2) {
            fields <- props(e2)
            fields <- fields[
                !vapply(fields, is_waiver, logical(1L), USE.NAMES = FALSE)
            ]
            if (length(fields)) props(e1) <- fields
            e1
        }
)

##########################################################
#' Add patch titles to plot borders
#'
#' This function extends ggplot2's title functionality, allowing you to add
#' titles to each border of the plot: top, left, bottom, and right.
#'
#' @details
#'
#' The appearance and alignment of these patch titles can be customized using
#' [theme()][ggplot2::theme]:
#' - `plot.patch_title`/`plot.patch_title.*`: Controls the text appearance of
#'   patch titles. By default, `plot.patch_title` inherit from `plot.title`, and
#'   settings for each border will inherit from `plot.patch_title`, with the
#'   exception of the `angle` property, which is not inherited.
#' - `plot.patch_title.position`/`plot.patch_title.position.*`: Determines the
#'   alignment of the patch titles. By default, `plot.patch_title.position`
#'   inherit from `plot.title.position`, and settings for each border will
#'   inherit from `plot.patch_title`. The value `"panel"` aligns the patch
#'   titles with the plot panels. Setting this to `"plot"` aligns the patch
#'   title with the entire plot (excluding margins and plot tags).
#'
#' @param top,left,bottom,right A string specifying the title to be added to the
#' top, left, bottom, and right border of the plot.
#' @return A [`labels`][ggplot2::labs] object to be added to ggplot.
#' @examples
#' ggplot(mtcars) +
#'     geom_point(aes(mpg, disp)) +
#'     patch_title(
#'         top = "I'm top patch title",
#'         left = "I'm left patch title",
#'         bottom = "I'm bottom patch title",
#'         right = "I'm right patch title"
#'     )
#' @importFrom ggplot2 waiver is_waiver
#' @export
patch_title <- S7::new_class(
    "patch_title",
    properties = lapply(
        rlang::set_names(c("top", "left", "bottom", "right")),
        function(position) {
            S7::new_property(
                S7::new_union(S3_waiver, NULL, S7::class_character),
                validator = function(value) {
                    if (!is_waiver(value) && !is.null(value) &&
                        length(value) != 1L) {
                        return("must be a single character string")
                    }
                },
                default = quote(waiver())
            )
        }
    )
)

#' @export
#' @usage NULL
#' @rdname patch_title
patch_titles <- function(...) {
    lifecycle::deprecate_soft(
        "1.1.0.9000",
        "patch_titles()",
        details = "Please use `patch_title()` instead"
    )
    patch_title(...)
}

#' @importFrom ggplot2 is_waiver
#' @importFrom S7 props
local(
    S7::method(`+`, list(patch_title, patch_title)) <-
        function(e1, e2) {
            fields <- props(e2)
            fields <- fields[
                !vapply(fields, is_waiver, logical(1L), USE.NAMES = FALSE)
            ]
            if (length(fields)) props(e1) <- fields
            e1
        }
)

#' @importFrom ggplot2 find_panel zeroGrob
#' @importFrom ggplot2 calc_element element_grob merge_element is_theme_element
#' @importFrom rlang arg_match0
#' @importFrom grid grobName
setup_patch_title <- function(table, patch_title, theme) {
    patch_title <- patch_title %||% patch_title()
    old_text <- calc_element("plot.title", theme)
    # always justification by center for patch title
    old_text$hjust <- 0.5
    if (is.null(text <- theme$plot.patch_title)) {
        text <- old_text
    } else if (is_theme_element(text, "text")) {
        text <- merge_element(text, old_text)
    } else {
        cli_abort(
            paste(
                "Theme element {.var plot.patch_title} must be a",
                "{.cls element_text}."
            ),
            call = quote(theme())
        )
    }
    # inherit from plot.title.position, default use "panel"
    position <- theme$plot.patch_title.position %||%
        theme$plot.title.position %||% "panel"

    for (border in .TLBR) {
        title <- prop(patch_title, border) %|w|% NULL
        panel_pos <- find_panel(table)
        name <- paste("plot.patch_title", border, sep = ".")
        if (is.null(title)) {
            title <- zeroGrob()
        } else {
            # set the default angle
            text$angle <- switch(border,
                top = 0L,
                left = 90L,
                bottom = 0L,
                right = -90L
            )
            # we merge the element with `plot.patch_title`
            if (is.null(el <- theme[[name]])) {
                el <- text
            } else if (is_theme_element(el, "text")) {
                el <- merge_element(el, text)
            } else {
                cli_abort(
                    paste(
                        "Theme element {.var {name}} must have",
                        "class {.cls element_text}."
                    ),
                    call = quote(theme())
                )
            }
            # render the patch title grob
            title <- element_grob(el, title, margin_y = TRUE, margin_x = TRUE)
            title$name <- grobName(title, name)
        }

        name <- paste("plot.patch_title.position", border, sep = ".")
        pos <- arg_match0(
            theme[[name]] %||% position,
            c("panel", "plot"),
            arg_nm = name,
            error_call = quote(theme())
        )
        if (border == "top") {
            height <- grobHeight(title)
            if (pos == "panel") {
                l <- panel_pos$l
                r <- panel_pos$r
            } else {
                l <- 1L
                r <- ncol(table)
            }
            h <- .subset2(panel_pos, "t") - 4L # above original xlab
            table <- gtable_add_rows(table, height, pos = h)
            table <- gtable_add_grob(table, title,
                name = "patch-title-top",
                t = h + 1L, b = h + 1L, l = l, r = r,
                clip = "off"
            )
        } else if (border == "left") {
            width <- grobWidth(title)
            if (pos == "panel") {
                t <- panel_pos$t
                b <- panel_pos$b
            } else {
                t <- 1L
                b <- nrow(table)
            }
            v <- .subset2(panel_pos, "l") - 4L # left of the ylab
            table <- gtable_add_cols(table, width, pos = v)
            table <- gtable_add_grob(table, title,
                name = "patch-title-left",
                t = t, b = b, l = v + 1L, r = v + 1L,
                clip = "off"
            )
        } else if (border == "bottom") {
            height <- grobHeight(title)
            if (pos == "panel") {
                l <- panel_pos$l
                r <- panel_pos$r
            } else {
                l <- 1L
                r <- ncol(table)
            }
            h <- .subset2(panel_pos, "b") + 3L # below original xlab
            table <- gtable_add_rows(table, height, pos = h)
            table <- gtable_add_grob(table, title,
                name = "patch-title-bottom",
                t = h + 1L, b = h + 1L, l = l, r = r,
                clip = "off"
            )
        } else if (border == "right") {
            width <- grobWidth(title)
            if (pos == "panel") {
                t <- panel_pos$t
                b <- panel_pos$b
            } else {
                t <- 1L
                b <- nrow(table)
            }
            v <- .subset2(panel_pos, "r") + 3L # right of the ylab
            table <- gtable_add_cols(table, width, pos = v)
            table <- gtable_add_grob(table, title,
                name = "patch-title-right",
                t = t, b = b, l = v + 1L, r = v + 1L,
                clip = "off"
            )
        }
    }
    table
}

#' @importFrom ggplot2 update_ggplot is_waiver
S7::method(update_ggplot, list(patch_title, ggplot2::class_ggplot)) <-
    function(object, plot, objectname, ...) {
        plot$ggalign_patch_title <- (plot$ggalign_patch_title %||%
            patch_title()) + object
        if (!inherits(plot, "patch_ggplot")) {
            plot <- add_class(plot, "patch_ggplot")
        }
        plot
    }
