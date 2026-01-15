# Initialize the Default Properties of an Object

This helper function is used to initialize an object before use by
setting its properties or internal elements to their default values. It
is particularly useful when the object's internal defaults differ from
those required for proper usage, such as when internal defaults indicate
values not set by the user. The function ensures that the object is
properly configured for subsequent operations.

## Usage

``` r
ggalign_init(x)
```

## Arguments

- x:

  An object that needs initialization.

## Value

The initialized object, ready for use.
