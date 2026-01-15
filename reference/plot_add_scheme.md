# Apply a Scheme to a plot

`plot_add_scheme()` is a generic used to apply a
[`Scheme`](https://yunuuuu.github.io/ggalign/reference/Scheme.md) (or a
[`Schemes`](https://yunuuuu.github.io/ggalign/reference/Schemes.md)
container) to a plot object. This allows schemes to update or modify the
plot's appearance or behavior according to their configuration.

## Usage

``` r
plot_add_scheme(x, object, ...)
```

## Arguments

- x:

  The object whose properties are being updated.

- object:

  The object from which the properties will be applied to `x`.

- ...:

  Arguments passed to methods.

## Details

By default When a
[`Schemes`](https://yunuuuu.github.io/ggalign/reference/Schemes.md)
object is passed, each individual
[`Scheme`](https://yunuuuu.github.io/ggalign/reference/Scheme.md) is
applied in sequence via its respective `plot_add_scheme()` method.
