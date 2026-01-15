# Inherit Properties from Another Object

This generic function allows one object to inherit properties or
configurations from another object. It is used when an object needs to
adopt the state of another object without directly modifying it.

## Usage

``` r
ggalign_inherit(x, object, ...)
```

## Arguments

- x:

  The object that will inherit properties.

- object:

  The object from which `x` will inherit properties.

- ...:

  Arguments passed to methods.

## Value

The `x` object with properties inherited from `object`.
