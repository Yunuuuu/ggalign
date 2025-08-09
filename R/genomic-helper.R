#' Calculate inter-region distances for genomic rainfall plots
#'
#' This function computes distances between adjacent genomic regions, grouped by
#' chromosome. Useful for visualizing clustering or dispersion of genomic
#' features.
#'
#' @param region A data frame with at least 3 columns: chromosome, start, and
#' end.
#' @param mode How to assign distance for intermediate regions: one of `"min"`,
#'   `"max"`, `"mean"`, `"left"`, or `"right"`.
#' @details
#' The distance between two adjacent regions is calculated as the number of
#' bases between the **end position of the upstream region** and the
#' **start position of the downstream region**. If two regions overlap or are
#' adjacent (<=1 bp apart), the distance is set to `0`. The resulting distance
#' is assigned to each region according to the selected `mode`:
#'
#' - `"left"`: assign the distance to the upstream region
#' - `"right"`: assign to the downstream region
#' - `"min"` / `"max"` / `"mean"`: for intermediate regions, calculate the
#'   minimum, maximum, or average of the distances to neighboring regions
#'
#' @return A data frame with an additional `dist` column.
#' @export
genomic_dist <- function(region, mode = NULL) {
    # Check input validity
    if (is.null(mode)) {
        mode <- "min"
    } else {
        mode <- arg_match0(mode, c("min", "max", "mean", "left", "right"))
    }
    if (!is.data.frame(region) || ncol(region) < 3L) {
        cli_abort("{.arg region} must have at least 3 columns: chromosome, start, and end")
    }
    if (anyNA(region[[1L]]) || anyNA(region[[2L]]) || anyNA(region[[3L]])) {
        cli_abort("Columns 1, 2, and 3 of {.arg region} must not contain missing values")
    }
    if (!is.numeric(region[[2L]]) || !is.numeric(region[[3L]])) {
        cli_abort("Columns 2 and 3 of {.arg region} must be numeric (start and end positions)")
    }
    if (any(region[[2L]] > region[[3L]])) {
        cli_abort("Column 2 (start) must not be greater than column 3 (end) in any row of {.arg region}")
    }

    # Split the region data frame by chromosome (first column)
    groups <- vec_split(region, .subset2(region, 1L))
    dist_list <- lapply(.subset2(groups, "val"), function(d) {
        n <- nrow(d)
        if (n < 2L) {
            d$dist <- NA_real_
            return(d)
        }

        # Sort regions by start, then end positions
        ordering <- order(.subset2(d, 2L), .subset2(d, 3L))

        # Compute distances between adjacent regions
        dists <- vapply(seq_len(n - 1L), function(i) {
            first <- vec_slice(d, ordering[i])
            second <- vec_slice(d, ordering[i + 1])
            out <- .subset2(second, 2L) - .subset2(first, 3L)
            # If overlapping or adjacent (<=1 bp), treat distance as 0
            if (out <= 1L) out <- 0L else out <- as.integer(out)
            out
        }, integer(1L), USE.NAMES = FALSE)

        # If only two regions, just repeat the distance
        if (n == 2L) {
            d$dist <- rep_len(dists, n)
            return(d)
        }

        # For intermediate regions (not first or last), choose how to assign
        # distance
        d1 <- dists[seq_len(n - 2L)]
        d2 <- dists[2:(n - 1L)]
        if (mode == "min") {
            body_dists <- pmin(d1, d2)
        } else if (mode == "max") {
            body_dists <- pmax(d1, d2)
        } else if (mode == "mean") {
            body_dists <- (d1 + d2) / 2L
        } else if (mode == "left") {
            body_dists <- d1
        } else if (mode == "right") {
            body_dists <- d2
        }

        # Combine distances: head, body, and tail
        d$dist <- c(dists[1L], body_dists, dists[n - 1L])[order(ordering)]
        d
    })
    vec_rbind(!!!dist_list)
}

#' Calculate Genomic Region Density
#'
#' Computes the density or count of genomic regions in sliding or fixed windows
#' across the genome. The density can be reported as the percentage of uncovered
#' bases or the number of overlapping regions within each window.
#'
#' This function splits the input by chromosome and tiles the genomic space
#' into windows, optionally overlapping. For each window, it calculates:
#'
#' - the number of regions that overlap it (if `mode = "count"`), or
#' - the fraction of bases covered by any region (if `mode = "percent"`).
#'
#' @param region A data frame with at least 3 columns: chromosome, start, and
#' end.
#'   - Column 1: character or factor, chromosome name.
#'   - Column 2: numeric, start position (must be <= end).
#'   - Column 3: numeric, end position.
#' @param window_size Numeric, the width of each window (default is `1e+07`).
#'   Ignored if `n_window` is specified.
#' @param n_window Integer, the number of windows per chromosome. If provided,
#'   overrides `window_size` and evenly splits the chromosome into `n_window`
#'   (non-overlapping) or `2*n_window - 1` (overlapping) windows.
#' @param overlap Logical, whether to use overlapping windows (default `TRUE`).
#'   Overlapping windows are spaced by half the window size.
#' @param mode Character, either `"coverage"` or `"count"`:
#'   - `"count"`: reports the number of regions overlapping each window.
#'   - `"coverage"`: reports the fraction of each window covered by regions.
#' @param seqlengths Optional named vector of chromosome lengths. If missing,
#'   the maximum `end` value in the input is used as the chromosome length.
#'
#' @return A data frame containing the first three columns from `region`,
#'   plus a fourth column `density`, which represents either the region count
#'   or the coverage percentage, depending on `mode`.
#'
#' @examples
#' region <- data.frame(
#'     chr = rep("chr1", 3),
#'     start = c(100, 5000000, 15000000),
#'     end = c(2000000, 7000000, 17000000)
#' )
#' genomic_density(region, window_size = 1e7, mode = "count")
#' genomic_density(region, n_window = 3, overlap = FALSE, mode = "coverage")
#'
#' @export
genomic_density <- function(region, window_size = 1e+07, n_window = NULL,
                            overlap = TRUE, mode = c("coverage", "count"),
                            seqlengths = NULL) {
    # Check input validity
    assert_number_whole(window_size, allow_null = TRUE, min = 1)
    assert_number_whole(n_window, allow_null = TRUE, min = 1)
    if (is.null(window_size) && is.null(n_window)) {
        cli_abort(c(
            "Both {.arg window_size} and {.arg n_window} are {.val NULL}.",
            "i" = "You must supply at least one of these arguments to define the binning strategy."
        ))
    }
    assert_bool(overlap)
    if (is.null(mode)) {
        mode <- "coverage"
    } else {
        mode <- arg_match0(mode, c("coverage", "count"))
    }
    if (!is.data.frame(region) || ncol(region) < 3L) {
        cli_abort("{.arg region} must have at least 3 columns: chromosome, start, and end")
    }
    if (anyNA(region[[1L]]) || anyNA(region[[2L]]) || anyNA(region[[3L]])) {
        cli_abort("Columns 1, 2, and 3 of {.arg region} must not contain missing values")
    }
    if (!is.numeric(region[[2L]]) || !is.numeric(region[[3L]])) {
        cli_abort("Columns 2 and 3 of {.arg region} must be numeric (start and end positions)")
    }
    if (any(region[[2L]] > region[[3L]])) {
        cli_abort("Column 2 (start) must not be greater than column 3 (end) in any row of {.arg region}")
    }

    # Split the region data frame by chromosome (first column)
    groups <- vec_split(region, .subset2(region, 1L))
    density_list <- lapply(.subset2(groups, "val"), function(d) {
        chr <- .subset2(d, 1L)[1L]
        if (is.null(seqlengths)) {
            max_pos <- max(.subset2(region, 3L))
        } else {
            max_pos <- seqlengths[chr]
            if (is.na(max_pos)) {
                cli_abort("Chromosome {.val {chr}} is not found in {.arg seqlengths}. Please check that all chromosomes in {.arg region} have corresponding sequence lengths.")
            }
        }
        if (overlap) {
            if (is.null(n_window)) {
                # Half-step sliding windows: start at 1, step = half window size
                b <- seq(0L, max_pos, by = window_size %/% 2L)
                s <- b[-length(b)]
                s <- s[-length(s)] + 1L
                e <- s + window_size - 1L
            } else {
                b <- seq(0L, max_pos, length.out = 2L * n_window + 1L)
                s <- b[-length(b)]
                s <- s[-length(s)] + 1L
                e <- s + b[3L] - b[1L] - 1L
            }
        } else {
            if (is.null(n_window)) {
                b <- seq(0L, max_pos, by = window_size)
                s <- b[-length(b)] + 1L
                e <- s + window_size - 1L
            } else {
                b <- seq(0, max_pos, length.out = n_window + 1L)
                s <- b[-length(b)] + 1L
                e <- s + b[2L] - b[1L]
            }
        }
        s <- as.integer(s)
        e <- as.integer(e)
        region_s <- .subset2(d, 2L)
        region_e <- .subset2(d, 3L)
        if (mode == "count") {
            # For each window, count how many regions overlap with it
            density <- vapply(seq_along(s), function(i) {
                si <- s[i]
                ei <- e[i]
                sum(!(region_s > ei | region_e < si))
            }, integer(1L), USE.NAMES = FALSE)
        } else {
            region_s <- as.integer(ceiling(region_s))
            region_e <- as.integer(floor(region_e))
            # For each window, calculate the proportion of bases covered by
            # regions
            density <- vapply(seq_along(s), function(i) {
                si <- s[i]
                ei <- e[i]
                # Keep regions that overlap with current window
                keep <- !(region_s > ei | region_e < si)

                # Clip regions to fit within the current window
                region_s_used <- pmax(region_s[keep], si)
                region_e_used <- pmin(region_e[keep], ei)

                # If no overlapping region, coverage is zero
                if (length(region_s_used) == 0L) {
                    return(0)
                }

                # Sort intervals by start, then end
                ordering <- order(region_s_used, region_e_used)
                region_s_used <- region_s_used[ordering]
                region_e_used <- region_e_used[ordering]

                # Initialize with the first interval
                start <- region_s_used[1L]
                end <- region_e_used[1L]

                # If there's only one interval, just compute its coverage
                if (length(region_s_used) == 1L) {
                    cov <- end - start + 1L
                    return(cov / (ei - si + 1L))
                }
                # Merge overlapping/adjacent intervals and compute total
                # coverage
                cov <- 0L
                for (j in 2:length(region_s_used)) {
                    if (region_s_used[j] - end <= 1L) {
                        end <- max(end, region_e_used[j])
                    } else {
                        cov <- cov + (end - start + 1L)
                        start <- region_s_used[j]
                        end <- region_e_used[j]
                    }
                }
                cov <- cov + (end - start + 1L)
                cov / (ei - si + 1L)
            }, numeric(1L), USE.NAMES = FALSE)
        }
        data_frame0(seqnames = chr, start = s, end = e, density = density)
    })
    out <- vec_rbind(!!!density_list)
    names(out)[1:3] <- names(region)[1:3]
    out
}
