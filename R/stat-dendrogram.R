ht_dendrogram <- function() {

}

grid.dendrogram <- function(dend, ...) {
    gb <- dendrogramGrob(dend = dend, ...)
    grid::grid.draw(gb)
}

# grid::grid.newpage()
# grid::pushViewport(grid::viewport())
# hc <- hclust(dist(USArrests), "ave")
# dend1 <- as.dendrogram(hc)
# grid.dendrogram(dend1, default.units = "mm")
dendrogramGrob <- function(dend, ...,
                           orientation = "vertical", reverse = FALSE,
                           center = FALSE, leaf_pos = NULL) {
    data <- dendrogram_coords(dend, center = center, leaf_pos = leaf_pos)
    orientation <- match.arg(orientation, c("horizontal", "vertical"))
    if (orientation == "horizontal") {
        data <- rename(data, structure(
            c("ymin", "ymax", "xmin", "xmax"),
            names = c("xmin", "xmax", "ymin", "ymax")
        ))
    }
    if (reverse) {
        data <- rename(
            data,
            structure(c("ymin", "ymax"), names = c("ymax", "ymin"))
        )
    }
    grid::segmentsGrob(
        x0 = .subset2(data, "xmin"),
        x1 = .subset2(data, "xmax"),
        y0 = .subset2(data, "ymin"),
        y1 = .subset2(data, "ymax"),
        ...
    )
}

dendrogram_coords <- function(dend, center = FALSE, leaf_pos = NULL) {
    i <- 0L
    leaf_pos <- leaf_pos %||% seq_len(stats::nobs(dend))
    .dendrogram_coords <- function(dend, ymax = NA_real_) {
        if (stats::is.leaf(dend)) { # base version
            index <- as.integer(dend)
            y <- attr(dend, "height")
            label <- attr(dend, "label")
            i <<- i + 1L # Next leaf
            x <- .subset(leaf_pos, i)
            list(
                x_node = x, x_branch = x,
                # vertical segments
                coord = tibble0(
                    label = label, index = index,
                    xmin = x, xmax = x,
                    ymin = y, ymax = ymax %||% y,
                    leaf = TRUE
                )
            )
        } else if (inherits(dend, "dendrogram")) { # recursive version
            # x <- attr(dend, "midpoint")
            y <- attr(dend, "height")
            out <- transpose(lapply(dend, .dendrogram_coords, ymax = y))
            coord <- do.call(rbind, .subset2(out, "coord"))
            x_node <- unlist(.subset2(out, "x_node"),
                recursive = FALSE, use.names = FALSE
            )
            x_branch <- unlist(.subset2(out, "x_branch"), # the branch point
                recursive = FALSE, use.names = FALSE
            )
            xmin <- min(x_branch)
            xmax <- max(x_branch)
            x_branch <- sum(range(x_node)) / 2L
            coord <- tibble::add_row(coord,
                # horizontal and vertical segments
                xmin = c(xmin, x_branch),
                xmax = c(xmax, x_branch),
                ymin = rep_len(y, 2L),
                ymax = c(y, ymax),
                leaf = rep_len(FALSE, 2L)
            )
            if (center) {
                # x_node contain all the nodes for current tree
                list(x_node = x_node, x_branch = x_branch, coord = coord)
            } else {
                # x_branch only contain the median point of current horizontal
                # line
                list(x_node = x_branch, x_branch = x_branch, coord = coord)
            }
        } else {
            cli::cli_abort("{.arg dend} must be a {.cls dendrogram} object")
        }
    }
    coord <- .subset2(.dendrogram_coords(dend), "coord")
    coord[!is.na(.subset2(coord, "ymax")), ]
}
