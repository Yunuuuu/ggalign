# Get Data from the Attribute Attached by ggalign

`ggalign_attr` retrieves supplementary information stored as attributes
during the layout rendering process. These attributes-typically added
during data transformation by functions such as
[`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
or
[`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)-may
contain filtered data, auxiliary metadata, or other context essential
for downstream operations.

Factor level information, stored as a separate attribute, can be
accessed via `ggalign_lvls`.

## Usage

``` r
ggalign_attr(x, field = NULL, check = TRUE)

ggalign_lvls(x)
```

## Arguments

- x:

  Data used, typically inherited from the layout
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)/[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md)
  or
  [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  object.

- field:

  A string specifying the particular data to retrieve from the attached
  attribute. If `NULL`, the entire attached attribute list will be
  returned.

- check:

  A boolean indicating whether to check if the `field` exists. If
  `TRUE`, an error will be raised if the specified `field` does not
  exist.

## Value

- `ggalign_attr`: The specified data from the attached supplementary
  data or `NULL` if it is unavailable.

- `ggalign_lvls`: The attached supplementary levels or `NULL` if it is
  unavailable.

## Details

Attributes attached to the data are especially useful when the input
data is transformed in ways that limit access to the complete dataset.
For example,
[`fortify_matrix.MAF()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.MAF.md)
might filter mutation data while adding attributes that retain important
context, such as the total number of observations, for detailed or
aggregated analyses. Additionally, it stores the levels of
`Variant_Classification` for further usage.
