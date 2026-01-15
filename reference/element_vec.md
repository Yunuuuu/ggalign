# Apply a function to the fields of an element object

For an [`element`](https://ggplot2.tidyverse.org/reference/element.html)
object, some fields are vectorized, while others are not. This function
allows you to apply a function to the vectorized fields.

The following helper functions are available:

- `element_vec_fields`: Identify which fields are vectorized. Developers
  should implement this when creating new element classes.

- `element_vec`: Apply a custom function `.fn` to vectorized fields.

- `element_rep`: Applies [`rep()`](https://rdrr.io/r/base/rep.html).

- `element_rep_len`: Applies
  [`rep_len()`](https://rdrr.io/r/base/rep.html).

- `element_vec_recycle`: Applies
  [`vec_recycle()`](https://vctrs.r-lib.org/reference/vec_recycle.html).

- `element_vec_rep`: Applies
  [`vec_rep()`](https://vctrs.r-lib.org/reference/vec-rep.html).

- `element_vec_rep_each`: Applies
  [`vec_rep_each()`](https://vctrs.r-lib.org/reference/vec-rep.html).

- `element_vec_slice`: Applies
  [`vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.html).

## Usage

``` r
element_vec_fields(.el, ...)

element_vec(.el, .fn, ...)

element_rep(.el, ...)

element_rep_len(.el, length.out, ...)

element_vec_recycle(.el, size, ...)

element_vec_rep(.el, times, ...)

element_vec_rep_each(.el, times, ...)

element_vec_slice(.el, i, ...)
```

## Arguments

- .el:

  An [`element`](https://ggplot2.tidyverse.org/reference/element.html)
  object.

- ...:

  Additional arguments passed on to `fn`.

- .fn:

  The function to be applied to the vectorized fields of the element
  object.

- length.out:

  Non-negative integer. The desired length of the output vector. Other
  inputs will be coerced to a double vector and the first element taken.
  Ignored if `NA` or invalid.

- size:

  Desired output size.

- times:

  For `vec_rep()`, a single integer for the number of times to repeat
  the entire vector.

  For `vec_rep_each()`, an integer vector of the number of times to
  repeat each element of `x`. `times` will be
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
  to the size of `x`.

- i:

  An integer, character or logical vector specifying the locations or
  names of the observations to get/set. Specify `TRUE` to index all
  elements (as in `x[]`), or `NULL`, `FALSE` or
  [`integer()`](https://rdrr.io/r/base/integer.html) to index none (as
  in `x[NULL]`).
