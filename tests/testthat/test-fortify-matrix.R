test_that("`MAF` method calculate variant_weights", {
    skip_if_not_installed("maftools")
    laml.maf <- system.file("extdata", "tcga_laml.maf.gz", package = "maftools")
    laml.clin <- system.file("extdata", "tcga_laml_annot.tsv",
        package = "maftools"
    )
    laml <- maftools::read.maf(
        maf = laml.maf,
        clinicalData = laml.clin,
        verbose = FALSE
    )
    expect_equal(
        rowSums(ggalign_attr(fortify_matrix(laml), "variant_weights")[-1]),
        ggalign_attr(fortify_matrix(laml), "gene_summary")$AlteredSamples
    )
})
