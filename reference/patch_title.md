# Add patch titles to plot borders

This function extends ggplot2's title functionality, allowing you to add
titles to each border of the plot: top, left, bottom, and right.

## Usage

``` r
patch_title(
  top = waiver(),
  left = waiver(),
  bottom = waiver(),
  right = waiver()
)
```

## Arguments

- top, left, bottom, right:

  A string specifying the title to be added to the top, left, bottom,
  and right border of the plot.

## Value

A [`labels`](https://ggplot2.tidyverse.org/reference/labs.html) object
to be added to ggplot.

## Details

The appearance and alignment of these patch titles can be customized
using [theme()](https://ggplot2.tidyverse.org/reference/theme.html):

- `plot.patch_title`/`plot.patch_title.*`: Controls the text appearance
  of patch titles. By default, `plot.patch_title` inherit from
  `plot.title`, and settings for each border will inherit from
  `plot.patch_title`, with the exception of the `angle` property, which
  is not inherited.

- `plot.patch_title.position`/`plot.patch_title.position.*`: Determines
  the alignment of the patch titles. By default,
  `plot.patch_title.position` inherit from `plot.title.position`, and
  settings for each border will inherit from `plot.patch_title`. The
  value `"panel"` aligns the patch titles with the plot panels. Setting
  this to `"plot"` aligns the patch title with the entire plot
  (excluding margins and plot tags).

## Examples

``` r
ggplot(mtcars) +
    geom_point(aes(mpg, disp)) +
    patch_title(
        top = "I'm top patch title",
        left = "I'm left patch title",
        bottom = "I'm bottom patch title",
        right = "I'm right patch title"
    )
```
