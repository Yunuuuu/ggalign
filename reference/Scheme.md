# Abstract Scheme Class

`scheme` is an abstract base class that represents a configurable scheme
with a unique `key` (the first class name). Developers should create
subclasses of `scheme` to define specific schemes used in layouts or
plotting contexts.

## Usage

``` r
Scheme()
```

## Details

Developers should subclass `Scheme` to implement specific behaviors
(e.g., theme adjustments, alignment guides, layout spacings) and define
how those schemes are initialized, combined, and applied to plots.

## Developer Guide

When creating a new subclass of `Scheme`, you may optionally override
the following methods to customize its behavior:

- [`ggalign_init()`](https://yunuuuu.github.io/ggalign/reference/ggalign_init.md)
  *(optional)*: Initializes the scheme, often by assigning default
  values or computing derived properties.

  **Default behavior**: Returns the scheme unchanged.

- [`ggalign_update(x, object, ...)`](https://yunuuuu.github.io/ggalign/reference/ggalign_update.md)
  *(optional)*: Defines how to update a scheme by merging it with
  another of the same key.

  **Default behavior**: Replaces `x` entirely with `object`.

- [`ggalign_inherit(x, object)`](https://yunuuuu.github.io/ggalign/reference/ggalign_inherit.md)
  *(optional)*: Defines how a scheme inherits from a parent scheme
  (e.g., a layout template), typically merging instead of replacing.

  **Default behavior**: Inheritance is ignored; `x` is returned
  unchanged.

- [`ggalign_update(x, object, ...)`](https://yunuuuu.github.io/ggalign/reference/ggalign_update.md):
  Applies the scheme (`object`) to a plot (`x`) (usually a `ggplot`) by
  modifying the `x` components, theming, or annotations.
