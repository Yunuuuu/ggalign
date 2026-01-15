# Benchmarks

``` r
library(ComplexHeatmap)
#> Loading required package: grid
#> ========================================
#> ComplexHeatmap version 2.26.0
#> Bioconductor page: http://bioconductor.org/packages/ComplexHeatmap/
#> Github page: https://github.com/jokergoo/ComplexHeatmap
#> Documentation: http://jokergoo.github.io/ComplexHeatmap-reference
#> 
#> If you use it in published research, please cite either one:
#> - Gu, Z. Complex Heatmap Visualization. iMeta 2022.
#> - Gu, Z. Complex heatmaps reveal patterns and correlations in multidimensional 
#>     genomic data. Bioinformatics 2016.
#> 
#> 
#> The new InteractiveComplexHeatmap package can directly export static 
#> complex heatmaps into an interactive Shiny app with zero effort. Have a try!
#> 
#> This message can be suppressed by:
#>   suppressPackageStartupMessages(library(ComplexHeatmap))
#> ========================================
library(pheatmap)
#> 
#> Attaching package: 'pheatmap'
#> The following object is masked from 'package:ComplexHeatmap':
#> 
#>     pheatmap
library(gplots)
#> 
#> ---------------------
#> gplots 3.3.0 loaded:
#>   * Use citation('gplots') for citation info.
#>   * Homepage: https://talgalili.github.io/gplots/
#>   * Report issues: https://github.com/talgalili/gplots/issues
#>   * Ask questions: https://stackoverflow.com/questions/tagged/gplots
#>   * Suppress this message with: suppressPackageStartupMessages(library(gplots))
#> ---------------------
#> 
#> Attaching package: 'gplots'
#> The following object is masked from 'package:stats':
#> 
#>     lowess
library(ggalign)
#> Loading required package: ggplot2
#> ========================================
#> ggalign version 1.2.0.9000
#> 
#> If you use it in published research, please cite: 
#> Peng, Y.; Jiang, S.; Song, Y.; et al. ggalign: Bridging the Grammar of Graphics and Biological Multilayered Complexity. Advanced Science. 2025. doi:10.1002/advs.202507799
#> ========================================
```

``` r
set.seed(123)
n <- 1000
mat <- matrix(rnorm(n * n), nrow = n)
```

## Compared with other packages

A simple heatmap.

``` r
bench::mark(
    "heatmap()" = {
        pdf(NULL)
        heatmap(mat, Rowv = NA, Colv = NA)
        dev.off()
        NULL
    },
    "gplots::heatmap.2()" = {
        pdf(NULL)
        heatmap.2(mat, dendrogram = "none", trace = "none")
        dev.off()
        NULL
    },
    "ComplexHeatmap::Heatmap()" = {
        pdf(NULL)
        draw(Heatmap(mat,
            cluster_rows = FALSE, cluster_columns = FALSE,
            use_raster = TRUE
        ))
        dev.off()
        NULL
    },
    "pheatmap::pheatmap()" = {
        pdf(NULL)
        pheatmap(mat, cluster_rows = FALSE, cluster_cols = FALSE)
        dev.off()
        NULL
    },
    "ggalign()" = {
        pdf(NULL)
        print(ggheatmap(mat, filling = "raster"))
        dev.off()
        NULL
    },
    memory = FALSE
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 5 × 6
#>   expression                     min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 heatmap()                 149.27ms 152.93ms     6.50         NA    8.13 
#> 2 gplots::heatmap.2()          2.36s    2.36s     0.424        NA    0.847
#> 3 ComplexHeatmap::Heatmap()    4.59s    4.59s     0.218        NA    1.96 
#> 4 pheatmap::pheatmap()      554.66ms 554.66ms     1.80         NA    0    
#> 5 ggalign()                    2.05s    2.05s     0.488        NA    7.32
```

For heatmap with dendrogram

``` r
bench::mark(
    "heatmap()" = {
        pdf(NULL)
        heatmap(mat)
        dev.off()
        NULL
    },
    "gplots::heatmap.2()" = {
        pdf(NULL)
        heatmap.2(mat, trace = "none")
        dev.off()
        NULL
    },
    "ComplexHeatmap::Heatmap()" = {
        pdf(NULL)
        draw(Heatmap(mat,
            row_dend_reorder = FALSE, column_dend_reorder = FALSE,
            use_raster = TRUE
        ))
        dev.off()
        NULL
    },
    "pheatmap::pheatmap()" = {
        pdf(NULL)
        pheatmap(mat)
        dev.off()
        NULL
    },
    "ggalign()" = {
        pdf(NULL)
        print(ggheatmap(mat, filling = "raster") +
            anno_top() + align_dendro() +
            anno_right() + align_dendro())
        dev.off()
        NULL
    },
    memory = FALSE
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 5 × 6
#>   expression                     min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 heatmap()                    2.65s    2.65s     0.377        NA    1.51 
#> 2 gplots::heatmap.2()          2.82s    2.82s     0.355        NA    1.06 
#> 3 ComplexHeatmap::Heatmap()    5.56s    5.56s     0.180        NA    2.52 
#> 4 pheatmap::pheatmap()         2.22s    2.22s     0.451        NA    0.902
#> 5 ggalign()                    5.27s    5.27s     0.190        NA    3.98
```
