#' Create a Grob That Spans Multiple Viewports
#'
#' This function initializes a communication channel to share location signals
#' across different viewports. It returns a `channelSenderGrob` object, which
#' can transmit multiple signals using its `$signal` method (see the "Signal"
#' section below for details). When drawn, all collected signals are passed to
#' the `make_content` function to generate the final [`grob`][grid::grob].
#'
#' @param make_content A function that accepts the list of signal locations and
#' returns a [`grob`][grid::grob].
#' @param ... Additional arguments passed to `make_content`.
#' @inheritParams grid::grob
#'
#' @section Signal:
#' A `channelSenderGrob` can emit multiple location signals using the `$signal`
#' method. This method accepts the following arguments:
#'
#'   - `x`: X-coordinate.
#'   - `y`: Y-coordinate.
#'   - `default.units`: The default units for `x` and `y`.
#'   - `tag`: A character string used to identify the location.
#'   - `name`: A name for the returned grob.
#'   - `vp`: A [`viewport`][grid::viewport] for the returned grob.
#'
#' The `$signal` method returns a `channelSignalGrob`.
#'
#' @return A `channelSenderGrob` object.
#' @examples
#' # we create a new channel, we will emit two singals
#' # here: we just add a line between the two signals
#' channel <- channelGrob(function(locations) {
#'     # you can also use `tag` to identify the locations
#'     loc1 <- .subset2(locations, 1L)
#'     loc2 <- .subset2(locations, 2L)
#'     grid::segmentsGrob(loc1$x, loc1$y, loc2$x, loc2$y)
#' })
#'
#' gt <- gtable::gtable(unit(1:2, c("cm")), unit(5, "cm"))
#' gt <- gtable::gtable_add_grob(
#'     gt,
#'     list(
#'         grid::rectGrob(gp = gpar(color = "black", fill = NA)),
#'         channel$signal(0.5, 0.5, "npc")
#'     ),
#'     t = 1, l = 1, name = c("rect1", "signal1")
#' )
#' gt <- gtable::gtable_add_grob(
#'     gt,
#'     list(
#'         grid::rectGrob(gp = gpar(color = "red", fill = NA)),
#'         channel$signal(0.5, 0.5, "npc")
#'     ),
#'     t = 1, l = 2, name = c("rect2", "signal2")
#' )
#' grid::grid.newpage()
#' grid::grid.draw(gt)
#' @importFrom grid unit is.unit
#' @importFrom rlang list2
#' @export
channelGrob <- function(make_content, ..., name = NULL, vp = NULL) {
    make_content <- allow_lambda(make_content)
    if (!is.function(make_content)) {
        cli_abort("{.arg make_content} must be a function")
    }
    # Used to communicate between different signals
    channel <- new.env(parent = emptyenv())
    channel$make_content <- make_content
    channel$dots <- list2(...)
    channel$n <- 0L # total number of signals
    grid::grob(
        channel = channel,
        name = name,
        vp = vp,

        # method used to release signal and retutn a new grob
        signal = function(self, x, y, default.units = "native",
                          tag = NULL, name = NULL, vp = NULL) {
            if (!is.unit(x)) {
                x <- unit(x, default.units)
            }
            if (!is.unit(y)) {
                y <- unit(y, default.units)
            }
            if (length(x) != length(y)) {
                cli_abort("{.arg x} and {.arg y} must have the same length")
            }
            assert_string(tag, allow_empty = FALSE, allow_null = TRUE)
            signal <- list(list(x = x, y = y))
            if (!is.null(tag)) names(signal) <- tag
            channel <- .subset2(self, "channel")
            channel$signals <- c(channel$signals, signal)
            i <- channel$n <- channel$n + 1L
            grid::grob(
                channel = channel,
                i = i,
                name = name,
                vp = vp,
                cl = c("channelSignalGrob", "channelGrob")
            )
        },
        # Grob used to send signals
        cl = c("channelSenderGrob", "channelGrob")
    )
}

#' @export
`$.channelGrob` <- function(self, name) {
    field <- .subset2(self, name)
    if (!is.function(field)) {
        return(field)
    }
    args <- formals(field)
    # is.null is a fast path for a common case; the %in% check is slower but
    # also catches the case where there's a `self = NULL` argument.
    has_self <- !is.null(args[["self"]]) || "self" %in% names(args)

    # We assign the method with its correct name and construct a call to it to
    # make errors reported as coming from the method name rather than `field()`
    assign(name, field, envir = environment())
    args <- list(quote(...))
    if (has_self) {
        args$self <- quote(self)
    }
    rlang::new_function(alist(... = ), rlang::call2(name, !!!args))
}

#' @export
`[[.channelGrob` <- `$.channelGrob`

# https://www.stat.auckland.ac.nz/~paul/Reports/CustomGrobs/custom-grob.html
# preDraw:
#  - makeContext
#  - pushvpgp
#  - preDrawDetails: by default, do noting
# makeContent:
# drawDetails:
# postDraw:
#  - postDrawDetails: by default, do noting
#  - popgrobvp
#' @importFrom grid makeContent drawDetails viewport is.grob gTree
#' @export
makeContent.channelGrob <- function(x) {
    channel <- .subset2(x, "channel")
    # If no signals, do nothing
    if (channel$n == 0L) return(x) # styler: off

    if (inherits(x, "channelSenderGrob")) {
        # we use the viewport from the signals sender
        channel$vp <- grid::current.viewport()
    } else if (inherits(x, "channelSignalGrob")) {
        if (is.null(channel$locations)) {
            channel$locations <- vector("list", channel$n)
            names(channel$locations) <- names(channel$signals)
        }

        # convert the viewport coordinates to the device coordinates
        i <- .subset2(x, "i")
        signal <- .subset2(channel$signals, i)
        channel$locations[[i]] <- grid::deviceLoc(signal$x, signal$y)
        if (!any(vapply(channel$locations, is.null, logical(1L), # styler: off
                        USE.NAMES = FALSE))) {                   # styler: off
            # When all locations have been prepared
            # we output the grob with all device locations
            x <- grid::grob(
                channel = channel,
                vp = .subset2(x, "vp"), # Don't change the viewport
                cl = c("channelReceiverGrob", "channelGrob")
            )
        }
    }
    x
}

#' @export
drawDetails.channelGrob <- function(x, recording) {
}

#' @importFrom grid grid.draw
#' @export
drawDetails.channelReceiverGrob <- function(x, recording) {
    # we always reset the locations after drawing
    old <- grid::current.viewport()$name
    on.exit(grid::seekViewport(old), add = TRUE)
    channel <- .subset2(x, "channel")
    locations <- channel$locations
    on.exit(channel$locations <- NULL, add = TRUE)
    if (!is.null(vp <- channel$vp)) {
        grid::seekViewport(vp$name)
        # convert the device cooridnates into the drawing viewport coordinates
        trans <- solve(grid::current.transform())
        locations <- lapply(locations, grid_solve_loc, trans = trans)
    } else { # If no viewport, we use the `ROOT` viewport
        grid::upViewport(0)
        grid::pushViewport(grid::viewport())
    }
    grob <- rlang::inject(channel$make_content(locations, !!!channel$dots))
    if (is.gList(grob)) grob <- gTree(children = grob)
    if (is.grob(grob)) grid.draw(grob, recording = recording)
}
