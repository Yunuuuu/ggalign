# Options for Patch

This class defines the options used by the `Patch` object. It includes:

## Usage

``` r
patch_options(theme = NULL, guides = NULL, tag = NULL)
```

## Details

- `theme`: The theme to be applied, which can be either `NULL` or a
  ggplot2 [theme](https://ggplot2.tidyverse.org/reference/theme.html)
  object.

- `guides`: The guides for the plot, which can be `NULL` or a character
  vector.

- `tag`: Can be `NULL` (no tag), a single string, or a `LayoutTagger`
  object that provides a `$tag()` method to generate a tag string for
  each plot. The `LayoutTagger` is used specifically by
  [`alignpatches()`](https://yunuuuu.github.io/ggalign/reference/alignpatches.md).
  For individual plots, you typically call the `$tag()` method of the
  `LayoutTagger` object to return a string, which triggers the internal
  `Patch$tag()` method to add a tag.
