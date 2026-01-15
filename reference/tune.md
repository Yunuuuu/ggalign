# Change the shape of the input object

Change the shape of the input object

## Usage

``` r
tune(data, shape = NULL)
```

## Arguments

- data:

  An R object.

- shape:

  Usually `NULL` or a string, specifying the new shape for the object.
  Refer to the detailed method for allowed values.

## Details

In most cases,
[`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
or
[`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)
provide full support for transforming objects. However, some objects may
require two completely different approaches to be fortified. The `tune`
function acts as a helper to create a new class tailored for these
objects.

## `tune` method collections

- [`tune.list()`](https://yunuuuu.github.io/ggalign/reference/tune.list.md)

- [`tune.MAF()`](https://yunuuuu.github.io/ggalign/reference/tune.MAF.md)

- [`tune.matrix()`](https://yunuuuu.github.io/ggalign/reference/tune.matrix.md)
