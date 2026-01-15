# Ordering Permutation

`order2` returns a permutation which rearranges its first argument into
ascending order.

## Usage

``` r
order2(x)

# S3 method for class 'hclust'
order2(x)

# S3 method for class 'dendrogram'
order2(x)

# S3 method for class 'ser_permutation_vector'
order2(x)

# S3 method for class 'ser_permutation'
order2(x)

# S3 method for class 'phylo'
order2(x)

# S3 method for class 'memo_weights'
order2(x)
```

## Arguments

- x:

  Any objects can be extracting ordering.

## Value

An integer vector unless any of the inputs has `2^31` or more elements,
when it is a double vector.

## Examples

``` r
order2(hclust2(matrix(rnorm(100L), nrow = 10L)))
#>  [1]  1  7  8 10  4  6  9  3  2  5
```
