
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Installation

You can install the development version of `ggalign` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("Yunuuuu/ggalign")
```

This package now depends on some un-merged characteristics of patchwork.
Please see <https://github.com/thomasp85/patchwork/pull/373>. If you
want to try it, you should install patchwork with
`pak::pkg_install(Yunuuuu/patchwork@align_axis_title)`.

The documentation for `ggalign` is currently in progress.

# ggalign

This package extends ggplot2 and provides numerous benefits for
organizing and arranging plots. It is specifically designed to align a
specific axis of multiple ggplot objects in a consistent order. This
functionality is particularly useful for plots that require manipulation
of data order. A common plot combination that can be effectively
organized using this package includes a dendrogram and a heatmap.

``` r
library(ggalign)
#> Loading required package: ggplot2
```

## `Examples`

Following examples are all from
[ComplexHeatmap](https://jokergoo.github.io/ComplexHeatmap-reference/book/more-examples.html#add-more-information-for-gene-expression-matrix)
package.

### Add more information for gene expression matrix

``` r
expr <- readRDS(system.file(package = "ComplexHeatmap", "extdata", "gene_expression.rds"))
mat <- as.matrix(expr[, grep("cell", colnames(expr))])
base_mean <- rowMeans(mat)
mat_scaled <- t(apply(mat, 1, scale))
type <- gsub("s\\d+_", "", colnames(mat))

ggstack(data = mat_scaled) +
  align_kmeans(centers = 5L) +
  ggpanel(size = unit(1, "cm")) +
  geom_tile(aes(x = 1, fill = factor(.panel))) +
  scale_fill_brewer(palette = "Dark2", name = "Kmeans group") +
  scale_x_continuous(breaks = NULL, name = NULL) +
  (ggheatmap(mat_scaled) +
    scale_y_continuous(breaks = NULL) +
    scale_fill_viridis_c() +
    hmanno("t") +
    align_dendro() +
    ggalign(data = type, size = unit(1, "cm")) +
    geom_tile(aes(y = 1, fill = factor(value))) +
    scale_y_continuous(breaks = NULL, name = NULL) +
    scale_fill_brewer(palette = "Set1", name = "type") +
    hmanno(NULL)) +
  (ggheatmap(base_mean, width = unit(2, "cm")) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(name = "base mean", breaks = FALSE) +
    scale_fill_gradientn(colours = c("#2600D1FF", "white", "#EE3F3FFF")) +
    hmanno("t", size = unit(2, "cm")) +
    ggalign() +
    geom_boxplot(aes(y = value, fill = factor(.extra_panel))) +
    scale_x_continuous(expand = expansion(), breaks = NULL) +
    scale_fill_brewer(palette = "Dark2", guide = "none") +
    theme(axis.title.y = element_blank()) +
    hmanno(NULL)) +
  ggalign(data = expr$length, size = unit(2, "cm")) +
  geom_point(aes(x = value)) +
  labs(x = "length") +
  theme(
    panel.border = element_rect(fill = NA),
    axis.text.x = element_text(angle = -60, hjust = 0)
  ) +
  (ggheatmap(expr$type, width = unit(2, "cm")) +
    scale_fill_brewer(palette = "Set3", name = "gene type") +
    scale_x_continuous(breaks = NULL, name = "gene type") +
    hmanno("t") +
    ggalign(limits = FALSE) +
    geom_bar(
      aes(.extra_panel, fill = factor(value)),
      position = position_fill()
    ) +
    scale_x_discrete() +
    scale_y_continuous(expand = expansion()) +
    scale_fill_brewer(palette = "Set3", name = "gene type", guide = "none")) &
  theme(plot.margin = margin())
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

### The measles vaccine heatmap

``` r
mat <- readRDS(system.file("extdata", "measles.rds",
  package =
    "ComplexHeatmap"
))
ggheatmap(mat, filling = FALSE) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradientn(
    colours = c("white", "cornflowerblue", "yellow", "red"),
    values = scales::rescale(c(0, 800, 1000, 127000), c(0, 1))
  ) +
  theme(axis.text.x = element_text(angle = -60, hjust = 0)) +
  hmanno("r") +
  align_dendro(plot_dendrogram = FALSE) +
  hmanno("t", size = unit(2, "cm")) +
  ggalign(data = rowSums) +
  geom_bar(aes(y = value), fill = "#FFE200", stat = "identity") +
  scale_y_continuous(expand = expansion()) +
  ggtitle("Measles cases in US states 1930-2001\nVaccine introduced 1961") +
  theme(plot.title = element_text(hjust = 0.5)) +
  hmanno("r", size = unit(2, "cm")) +
  ggalign(data = rowSums) +
  geom_bar(aes(x = value),
    fill = "#FFE200", stat = "identity",
    orientation = "y"
  ) +
  scale_x_continuous(expand = expansion()) +
  theme(axis.text.x = element_text(angle = -60, hjust = 0))
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

## Session information

``` r
sessionInfo()
#> R version 4.4.0 (2024-04-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04 LTS
#> 
#> Matrix products: default
#> BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/libmkl_rt.so;  LAPACK version 3.8.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: Asia/Shanghai
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] ggalign_0.0.1 ggplot2_3.5.1
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.5         dplyr_1.1.4          compiler_4.4.0      
#>  [4] highr_0.11           tidyselect_1.2.1     tidyr_1.3.1         
#>  [7] scales_1.3.0         yaml_2.3.8           fastmap_1.2.0       
#> [10] ggh4x_0.2.8          R6_2.5.1             labeling_0.4.3      
#> [13] generics_0.1.3       patchwork_1.2.0.9000 knitr_1.47          
#> [16] tibble_3.2.1         munsell_0.5.1        pillar_1.9.0        
#> [19] RColorBrewer_1.1-3   rlang_1.1.4          utf8_1.2.4          
#> [22] xfun_0.45            viridisLite_0.4.2    cli_3.6.3           
#> [25] withr_3.0.0          magrittr_2.0.3       digest_0.6.36       
#> [28] grid_4.4.0           lifecycle_1.0.4      vctrs_0.6.5         
#> [31] evaluate_0.24.0      glue_1.7.0           farver_2.1.2        
#> [34] fansi_1.0.6          colorspace_2.1-0     rmarkdown_2.27      
#> [37] purrr_1.0.2          tools_4.4.0          pkgconfig_2.0.3     
#> [40] htmltools_0.5.8.1
```
