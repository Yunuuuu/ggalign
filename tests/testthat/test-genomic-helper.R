test_that("genomic_dist computes distances correctly", {
  region <- data.frame(
    chr = c("chr1", "chr1", "chr1", "chr1", "chr2"),
    start = c(10, 30, 55, 80, 5),
    end = c(20, 40, 65, 90, 15)
  )

  # Expected distances for each mode
  dist_left <- c(10, 10, 15, 15, NA)
  dist_right <- c(10, 15, 15, 15, NA)
  dist_min <- c(10, 10, 15, 15, NA)
  dist_max <- c(10, 15, 15, 15, NA)
  dist_mean <- c(10, 12.5, 15, 15, NA)

  expect_equal(genomic_dist(region, "left")$dist, dist_left)
  expect_equal(genomic_dist(region, "right")$dist, dist_right)
  expect_equal(genomic_dist(region, "min")$dist, dist_min)
  expect_equal(genomic_dist(region, "max")$dist, dist_max)
  expect_equal(genomic_dist(region, "mean")$dist, dist_mean)
})

test_that("genomic_dist handles overlapping and adjacent regions as 0", {
  region <- data.frame(
    chr = rep("chr1", 3),
    start = c(10, 15, 20),
    end = c(20, 20, 25)
  )

  result <- genomic_dist(region, "right")
  expect_equal(result$dist, c(0, 0, 0))
})

test_that("genomic_dist validates input", {
  df_invalid <- data.frame(chr = "chr1", start = 10, end = 5)
  expect_error(genomic_dist(df_invalid), "start.*greater than.*end")

  df_missing <- data.frame(chr = NA, start = 1, end = 2)
  expect_error(genomic_dist(df_missing), "must not contain missing")

  df_non_numeric <- data.frame(chr = "chr1", start = "a", end = "b")
  expect_error(genomic_dist(df_non_numeric), "must be numeric")
})

test_that("genomic_density returns expected result in count mode", {
  region <- data.frame(
    chr = rep("chr1", 3),
    start = c(1, 5, 15), end = c(3, 10, 20)
  )
  result <- genomic_density(region,
    window_size = 10,
    overlap = FALSE, mode = "count"
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("seqnames", "start", "end", "density"))
  expect_equal(result$density, c(2, 1)) # regions 1 and 2 in first window, region 3 in second
})

test_that("genomic_density returns expected result in coverage mode", {
  region <- data.frame(chr = rep("chr1", 2), start = c(1, 20), end = c(10, 30))
  result <- genomic_density(region,
    window_size = 15,
    overlap = FALSE, mode = "coverage"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(result$density, c(10 / 15, 11 / 15))
})
