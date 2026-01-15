# Annotate the whole layout

Annotate the whole layout

## Usage

``` r
layout_title(title = waiver(), subtitle = waiver(), caption = waiver())
```

## Arguments

- title:

  The text for the title.

- subtitle:

  The text for the subtitle for the plot which will be displayed below
  the title.

- caption:

  The text for the caption which will be displayed in the bottom-right
  of the plot by default.

## Value

A `layout_title` object.

## Examples

``` r
p1 <- ggplot(mtcars) +
    geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) +
    geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) +
    geom_bar(aes(gear)) +
    facet_wrap(~cyl)
align_plots(p1, p2, p3) +
    layout_title(title = "I'm title")
```
