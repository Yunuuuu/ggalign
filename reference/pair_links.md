# Helper function to create pairs of observation groups

[`ggmark()`](https://yunuuuu.github.io/ggalign/reference/ggmark.md) and
[`cross_link()`](https://yunuuuu.github.io/ggalign/reference/cross_link.md)
allow users to add links between observations. These functions help
define the linked observations. The selected pairs will either be linked
together, or each group in the pair will be linked separately to the
same plot area.

- `pair_links`: Helper function to create pairs of observation groups.

- `range_link`: Helper function to create a range of observations.

## Usage

``` r
pair_links(..., .handle_missing = "error", .reorder = NULL)

range_link(point1, point2)
```

## Arguments

- ...:

  \<[dyn-dots](https://rlang.r-lib.org/reference/dyn-dots.html)\> A list
  of formulas, where each side of the formula should be an `integer` or
  `character` index of the original data, or a `range_link()` object
  defining the linked observations. Use `NULL` to indicate no link on
  that side. You can also combine these by wrapping them into a single
  [`list()`](https://rdrr.io/r/base/list.html). If only the left-hand
  side of the formula exists, you can input it directly. For integer
  indices, wrap them with [`I()`](https://rdrr.io/r/base/AsIs.html) to
  use the ordering from the layout. You can also use
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html) to
  inherit values from the other group.

- .handle_missing:

  A string of `"error"` or `"remove"` indicates the action for handling
  missing observations.

- .reorder:

  A string of `"hand1"` or `"hand2"` indicating whether to reorder the
  input links to follow the specified layout ordering.

- point1, point2:

  A single integer or character index, defining the lower and higher
  bounds of the range. For integer indices, wrap them with
  [`I()`](https://rdrr.io/r/base/AsIs.html) to indicate the ordered
  index by the layout.

## Examples

``` r
x <- pair_links(
    # group on the left hand only
    c("a", "b"),
    # normally, integer index will be interpreted as the index of the
    # origianl data
    1:2,
    # wrapped with `I()` indicate` the integer index is ordering of the
    # layout
    I(1:2),
    range_link(1, 6),
    range_link("a", "b"),
    # group on the right hand only
    ~ 1:2,
    ~ c("a", "b"),
    ~ range_link(1, 6),
    # group on the both side
    range_link(1, 6) ~ c("a", "b"),
    # waiver() indicates the right hand is the same of the left hand
    range_link(1, 6) ~ waiver(),
    # the same for the left hand
    waiver() ~ 1:2,
    ~NULL # an empty link
)
x
#> <ggalign_pair_links>
#> A total of 12 pairs of link groups
#> 
#>                       hand1 ~ hand2                    
#>    1:           c("a", "b") ~                          
#>    2:                   1:2 ~                          
#>    3:                I(1:2) ~                          
#>    4:      range_link(1, 6) ~                          
#>    5:  range_link("a", "b") ~                          
#>    6:                       ~ 1:2                      
#>    7:                       ~ c("a", "b")              
#>    8:                       ~ range_link(1, 6)         
#>    9:      range_link(1, 6) ~ c("a", "b")              
#>   10:      range_link(1, 6) ~ waiver()                 
#>   11:              waiver() ~ 1:2                      
#>   12:                       ~                   <empty>
#> 
#> A total of 14 link groups

# we can modify it as usual list
x[[1]] <- NULL # remove the first link
x$a <- ~LETTERS
x
#> <ggalign_pair_links>
#> A total of 12 pairs of link groups
#> 
#>                       hand1 ~ hand2                    
#>    1:                   1:2 ~                          
#>    2:                I(1:2) ~                          
#>    3:      range_link(1, 6) ~                          
#>    4:  range_link("a", "b") ~                          
#>    5:                       ~ 1:2                      
#>    6:                       ~ c("a", "b")              
#>    7:                       ~ range_link(1, 6)         
#>    8:      range_link(1, 6) ~ c("a", "b")              
#>    9:      range_link(1, 6) ~ waiver()                 
#>   10:              waiver() ~ 1:2                      
#>   11:                       ~                   <empty>
#>    a:                       ~ c(A, B, ..., Z)          
#> 
#> A total of 14 link groups

# modify with a list
x[1:2] <- list(~ c("a", "b"), ~ range_link("a", "b"))
x
#> <ggalign_pair_links>
#> A total of 12 pairs of link groups
#> 
#>                       hand1 ~ hand2                        
#>    1:                       ~ c("a", "b")                  
#>    2:                       ~ range_link("a", "b")         
#>    3:      range_link(1, 6) ~                              
#>    4:  range_link("a", "b") ~                              
#>    5:                       ~ 1:2                          
#>    6:                       ~ c("a", "b")                  
#>    7:                       ~ range_link(1, 6)             
#>    8:      range_link(1, 6) ~ c("a", "b")                  
#>    9:      range_link(1, 6) ~ waiver()                     
#>   10:              waiver() ~ 1:2                          
#>   11:                       ~                       <empty>
#>    a:                       ~ c(A, B, ..., Z)              
#> 
#> A total of 14 link groups
```
