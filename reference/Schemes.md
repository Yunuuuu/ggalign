# A container for multiple layout schemes

`Schemes` is a container class that holds a list of `Scheme` objects,
each uniquely identified by their `key` property (usually derived from
the class name). It is used internally by the `ggalign` system to manage
sets of layout or rendering configurations that can be inherited,
updated, or applied during plot composition.

## Usage

``` r
Schemes(...)
```

## Arguments

- ...:

  \<[dyn-dots](https://rlang.r-lib.org/reference/dyn-dots.html)\> A list
  of [`Scheme`](https://yunuuuu.github.io/ggalign/reference/Scheme.md)
  object.

## Keys and Validation

- Each `Scheme` in the container must have a unique key.

- The key is used to reference and update individual schemes.

## Accessors

- `value`: The underlying list of `Scheme` objects.

- `entries`: A named list of schemes, where names are derived from their
  keys.

- `keys`: A character vector of all scheme keys.

## See also

[`Scheme`](https://yunuuuu.github.io/ggalign/reference/Scheme.md)
