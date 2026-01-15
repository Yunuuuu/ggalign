# Craftsman Object for Layout Management

The `Craftsman` is a virtual object used internally to manage
layout-specific behavior during the `ggalign` plot composition process.
It defines how a layout interacts with the domain data, sets up facets
and coordinates, and aligns scales or axis labels accordingly.

## Details

This object is used by layout constructors and should not be modified
directly.

## Methods

The following key methods are implemented:

- `setup_stack_facet()` / `setup_stack_coord()` / `setup_stack_plot()`

- `setup_circle_facet()` / `setup_circle_coord()` /
  `setup_circle_plot()`

- `build_plot()` / `finish_plot()` – finalize plot decorations

- [`summary()`](https://rdrr.io/r/base/summary.html) – print class info
