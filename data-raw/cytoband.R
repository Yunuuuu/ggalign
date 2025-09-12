if (!dir.exists(odir <- file.path("inst/extdata"))) {
    dir.create(odir, recursive = TRUE)
}

anno_hub <- AnnotationHub::AnnotationHub(localHub = FALSE)
AnnotationHub::query(
    anno_hub, c("UCSC", "Homo sapiens", "cytoband"),
    ignore.case = TRUE
)
# AH53177 | UCSC cytoBand track for hg19
# AH53178 | UCSC cytoBand track for hg38

ref_cytoband_hg19 <- anno_hub[["AH53177"]]
ref_cytoband_hg19 <- GenomeInfoDb::keepSeqlevels(
    ref_cytoband_hg19,
    setdiff(standardChromosomes(ref_cytoband_hg19, species = NULL), "chrM"),
    pruning.mode = "coarse"
)

ref_cytoband_hg19 <- as.data.frame(ref_cytoband_hg19)
saveRDS(
    ref_cytoband_hg19, file.path(odir, "ref_cytoband_hg19.rds"),
    version = 2L
)

ref_cytoband_hg38 <- anno_hub[["AH53178"]]
ref_cytoband_hg38 <- GenomeInfoDb::keepSeqlevels(
    ref_cytoband_hg38,
    setdiff(standardChromosomes(ref_cytoband_hg38, species = NULL), "chrM"),
    pruning.mode = "coarse"
)
ref_cytoband_hg38 <- as.data.frame(ref_cytoband_hg38)
saveRDS(
    ref_cytoband_hg38, file.path(odir, "ref_cytoband_hg38.rds"),
    version = 2L
)
