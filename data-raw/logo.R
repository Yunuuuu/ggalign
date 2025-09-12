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
