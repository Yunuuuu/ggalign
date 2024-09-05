## code to prepare `DATASET` dataset goes here

if (!dir.exists(odir <- file.path("inst/extdata"))) {
    dir.create(odir, recursive = TRUE)
}

expr <- readRDS(system.file(
    package = "ComplexHeatmap", "extdata", "gene_expression.rds"
))

saveRDS(expr, file.path(odir, "gene_expression.rds"), version = 2L)

mat <- readRDS(system.file("extdata", "measles.rds",
    package = "ComplexHeatmap"
))

saveRDS(mat, file.path(odir, "measles.rds"), version = 2L)
