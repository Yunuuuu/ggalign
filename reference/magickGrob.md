# Rasterize a grob object with magick

Rasterize a grob object with magick

## Usage

``` r
magickGrob(grob, ...)

# S3 method for class 'grob'
magickGrob(
  grob,
  magick = NULL,
  ...,
  res = NULL,
  interpolate = FALSE,
  name = NULL,
  vp = NULL
)

# S3 method for class 'magickGrob'
magickGrob(
  grob,
  magick = waiver(),
  ...,
  res = waiver(),
  interpolate = waiver(),
  name = waiver(),
  vp = waiver()
)
```

## Arguments

- grob:

  A [`grob()`](https://rdrr.io/r/grid/grid-defunct.html). Use
  [`as_grob()`](https://yunuuuu.github.io/ggalign/reference/as_grob.md)
  to convert any objects into a `grob`.

- ...:

  These dots are for future extensions and must be empty.

- magick:

  A function (purrr-style formula is accepted) that takes an
  [`image_read()`](https://docs.ropensci.org/magick/reference/editing.html)
  object as input and returns an object compatible with
  [`as.raster()`](https://rdrr.io/r/grDevices/as.raster.html). You can
  use any of the `image_*()` functions from the **magick** package to
  process the raster image.

- res:

  An integer sets the desired resolution in pixels.

- interpolate:

  A logical value indicating whether to linearly interpolate the image
  (the alternative is to use nearest-neighbour interpolation, which
  gives a more blocky result).

- name:

  A character identifier.

- vp:

  A Grid viewport object (or NULL).

## Value

A `magickGrob` object.
