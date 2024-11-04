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
library(ggalign)
if (!dir.exists(logo_dir <- file.path("man/figures"))) {
    dir.create(logo_dir, recursive = TRUE)
}
expr <- read_example("gene_expression.rds")
mat <- as.matrix(expr[, grep("cell", colnames(expr))])
base_mean <- rowMeans(mat)
mat_scaled <- t(apply(mat, 1, scale))
type <- gsub("s\\d+_", "", colnames(mat))

logo <- ggstack(data = mat_scaled) +
    stack_active(sizes = c(0.2, 1, 1)) +
    # group stack rows into 5 groups
    align_kmeans(centers = 5L) +
    # add a block plot for each group in the stack
    ggalign(size = unit(1, "cm"), data = NULL) +
    geom_tile(aes(x = 1, fill = factor(.panel))) +
    scale_fill_brewer(palette = "Dark2", name = "Kmeans group") +
    scale_x_continuous(breaks = NULL, name = NULL) +
    # add a heatmap plot in the stack
    ggheatmap() +
    quad_active(free_spaces = "l") +
    scale_y_continuous(breaks = NULL) +
    scale_fill_viridis_c(option = "magma") +
    # add dendrogram for this heatmap
    anno_top() +
    align_dendro() +
    # add a block for the heatmap column
    ggalign(data = type, size = unit(1, "cm")) +
    geom_tile(aes(y = 1, fill = factor(value))) +
    scale_y_continuous(breaks = NULL, name = NULL) +
    scale_fill_brewer(palette = "Set1", name = "type") +
    # add another heatmap in the stack and set the heatmap body width
    ggheatmap(base_mean, .width = unit(2, "cm")) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(name = "base mean", breaks = FALSE) +
    scale_fill_gradientn(colours = c("#2600D1FF", "white", "#EE3F3FFF")) +
    # set the active context of the heatmap to the top
    # and set the size of the top stack
    anno_top(size = unit(4, "cm")) +
    # add box plot in the heatmap top
    ggalign() +
    geom_boxplot(aes(y = value, fill = factor(.extra_panel))) +
    scale_x_continuous(expand = expansion(), breaks = NULL) +
    scale_fill_brewer(palette = "Dark2", guide = "none") +
    theme(axis.title.y = element_blank()) +
    # we move into the stack layout
    stack_active() +
    # add a point plot
    ggalign(data = expr$length, size = unit(2, "cm")) +
    geom_point(aes(x = value)) +
    labs(x = "length") +
    theme(
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = -60, hjust = 0)
    ) +
    # add another heatmap and set the heatmap body width
    ggheatmap(expr$type, width = unit(2, "cm")) +
    scale_fill_brewer(palette = "Set3", name = "gene type") +
    scale_x_continuous(breaks = NULL, name = "gene type") +
    # add barplot in the top annotation, and remove the spaces in the y-axis
    anno_top(free_spaces = "lr") +
    ggalign(limits = FALSE) +
    geom_bar(
        aes(.extra_panel, fill = factor(value)),
        position = position_fill()
    ) +
    scale_x_discrete() +
    scale_y_continuous(expand = expansion()) +
    scale_fill_brewer(palette = "Set3", name = "gene type", guide = "none") -
    plot_theme(plot.margin = margin()) &
    theme(
        plot.background = element_blank(), panel.background = element_blank(),
        legend.background = element_blank()
    )
logo_tmp <- tempfile(fileext = ".png")
ggplot2::ggsave(logo_tmp, plot = logo, device = "png", dpi = 1000L)
hexSticker::sticker(
    logo_tmp,
    package = "ggalign", s_x = 1.02, s_y = 0.86,
    s_width = 0.72, s_height = 0.8,
    p_size = 80, p_y = 1.6, p_color = "#741140",
    h_fill = "white", h_color = "#db5d37", h_size = 0.8,
    filename = file.path(logo_dir, "logo.png"),
    dpi = 1200L
)
if (file.exists(logo_tmp)) file.remove(logo_tmp)
