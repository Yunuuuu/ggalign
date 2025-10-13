#' Rasterize a grob object with magick
#'
#' @param grob A [`grob()`][grid::grob]. Use [`as_grob()`] to convert any
#' objects into a `grob`.
#' @param magick A function (purrr-style formula is accepted) that takes an
#' [`image_read()`][magick::image_read] object as input and returns an object
#' compatible with [`as.raster()`][grDevices::as.raster]. You can use any of
#' the `image_*()` functions from the **magick** package to process the raster
#' image.
#' @param res An integer sets the desired resolution in pixels.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams grid::rasterGrob
#' @return A `magickGrob` object.
#' @export
magickGrob <- function(grob, ...) {
    rlang::check_installed("magick", "to use `magickGrob()`")
    UseMethod("magickGrob")
}

#' @importFrom grid gTree
#' @export
#' @rdname magickGrob
magickGrob.grob <- function(grob, magick = NULL, ...,
                            res = NULL, interpolate = FALSE,
                            name = NULL, vp = NULL) {
    rlang::check_dots_empty()
    if (!is.null(magick) && !is.function(magick <- allow_lambda(magick))) {
        cli_abort("{.arg magick} must be a function")
    }
    assert_number_whole(res, min = 1, allow_null = TRUE)
    assert_bool(interpolate)
    gTree(
        grob = grob, magick = magick, res = res,
        interpolate = interpolate, name = name, vp = vp,
        cl = "magickGrob"
    )
}

#' @importFrom grid gTree
#' @export
magickGrob.gList <- function(grob, ...) {
    magickGrob(grob = gTree(children = grob), ...)
}

#' @importFrom grid editGrob
#' @importFrom rlang inject
#' @export
#' @rdname magickGrob
magickGrob.magickGrob <- function(grob, magick = waiver(), ...,
                                  res = waiver(), interpolate = waiver(),
                                  name = waiver(), vp = waiver()) {
    rlang::check_dots_empty()
    if (!is.waive(magick) &&
        !is.null(magick) &&
        !is.function(magick <- allow_lambda(magick))) {
        cli_abort("{.arg magick} must be a function")
    }
    if (!is.waive(magick)) assert_number_whole(res, min = 1, allow_null = TRUE)
    if (!is.waive(interpolate)) assert_bool(interpolate)
    params <- list(
        magick = magick, res = res,
        interpolate = interpolate, name = name, vp = vp
    )
    params <- params[!vapply(params, is.waive, logical(1L), USE.NAMES = FALSE)]
    inject(editGrob(grob, !!!params))
}

#' @export
magickGrob.default <- function(grob, ...) {
    cli_abort("{.arg grob} must be a {.cls grob} object")
}

# preDraw:
#  - makeContext
#  - pushvpgp
#  - preDrawDetails: by default, do noting
# makeContent:
# drawDetails:
# postDraw:
#  - postDrawDetails: by default, do noting
#  - popgrobvp
#' @importFrom grid makeContent unit convertHeight convertWidth viewport gList
#' @export
makeContent.magickGrob <- function(x) {
    # Grab viewport information
    width <- convertWidth(unit(1, "npc"), "pt", valueOnly = TRUE)
    height <- convertHeight(unit(1, "npc"), "pt", valueOnly = TRUE)

    # Grab grob metadata
    plot_res <- convertWidth(unit(1, "inch"), "pt", valueOnly = TRUE)
    res <- .subset2(x, "res") %||% plot_res

    magick <- .subset2(x, "magick")
    interpolate <- .subset2(x, "interpolate")

    # Track current device
    old_dev <- grDevices::dev.cur()

    # Reset current device upon function exit
    on.exit(grDevices::dev.set(old_dev), add = TRUE)

    # open the magick raster device
    image <- magick::image_graph(
        width = width * res / plot_res,
        height = height * res / plot_res,
        bg = NA_character_, res = res,
        clip = FALSE, antialias = FALSE
    )

    # Render the grob
    grid::pushViewport(viewport())

    # Clean up the grob for rendering
    grid::grid.draw(.subset2(x, "grob")) # should respect the viewport of `x`
    grid::popViewport()
    grDevices::dev.off()
    on.exit(magick::image_destroy(image), add = TRUE)

    # run `magick` when necessary
    if (!is.null(magick)) image <- magick(image)

    # Use native raster instead
    raster <- grDevices::as.raster(image, native = TRUE)

    # Forward raster grob
    setChildren(x, children = gList(
        grid::rasterGrob(
            raster, # should contain current area of full viewport
            x = 0.5, y = 0.5,
            height = unit(height, "pt"),
            width = unit(width, "pt"),
            default.units = "npc",
            just = "center",
            interpolate = interpolate
        )
    ))
}
