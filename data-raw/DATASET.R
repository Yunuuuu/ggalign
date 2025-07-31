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

# setup package logo ----------------------------
if (!dir.exists(logo_dir <- file.path("man/figures"))) {
    dir.create(logo_dir, recursive = TRUE)
}
# <a href="https://www.flaticon.com/free-icons/aligned" title="aligned
# icons">Aligned icons created by Freepik - Flaticon</a>
hexSticker::sticker(
    "data-raw/aligned.png",
    package = "ggalign",
    p_size = 20, s_x = 0.95, s_y = 0.83, s_width = .5, p_y = 1.5,
    h_fill = "#fceeb9", h_color = "#5f7c4b", p_color = "#5f7c4b",
    spotlight = TRUE, l_x = 0.8, l_y = 1, l_alpha = 0.1,
    url = "https://github.com/Yunuuuu/ggalign",
    u_size = 4, u_color = "#aaa",
    filename = file.path(logo_dir, "logo.png")
)

# ** ref_cytoband ---------------------------------------------------------

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
