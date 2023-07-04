# calc_proportions() --------------------------------------------------------------------

test_that("success rate curve: bad inputs", {
  load(system.file("examples/model_output_gradcurvpca.Rdata", package = "TerrainWorksUtils"))

  expect_error(calc_proportions(NULL), "No data supplied")
  expect_error(calc_proportions(predictions, prob_col = "not_here"), "prob_col must exist in data or an alternate column name must be specified.")
  expect_error(calc_proportions(predictions, plot = -1), "Plot variable must be TRUE/FALSE.")
  expect_error(calc_proportions(tibble(prob.pos = c(0, 1)), bins = seq(0, 1, length = 10)), "Number of bins is greater than the data provided.")
  expect_error(calc_proportions(tibble(prob.pos = c("words"))), "Data must be numeric.")
})

test_that("success rate curve: small tibble", {
  input <- tibble(
    prob.pos = c(1, 0, 0.5, .2, .2)
  )
  res <- calc_proportions(input, plot = FALSE)
  expect_true(is_tibble(res))

  # check that cumulative sums make sense
  expect_equal(max(res[["prob_cumul"]]), sum(input[["prob.pos"]]))
  expect_equal(max(res[["area_cumul"]]), nrow(input))
})

test_that("success rate curve: test with sample dem", {
  load(system.file("examples/model_output_gradcurvpca.Rdata", package = "TerrainWorksUtils"))

  tic()
  expect_warning(res <- calc_proportions(predictions, plot = FALSE))
  t <- toc(quiet = TRUE)

  # check that runtime isn't too long
  expect_lt((t$toc - t$tic), 3)

  # check return type
  expect_true(is_tibble(res))

  # check that proportions range from 0 to 1
  expect_true(min(res[["prob_prop"]]) < 0.001)
  expect_true((1 - max(res[["prob_prop"]])) < 0.001)
  expect_true(min(res[["area_prop"]]) < 0.001)
  expect_true((1 - max(res[["area_prop"]])) < 0.001)
})

# combine_proportions() ------------------------------------------------------------

test_that("srcurve combine: bad inputs", {
  expect_error(combine_proportions(), "No data provided.")
  expect_error(combine_proportions(NULL), "All curves must have columns \"prob_cumul\" and \"area_cumul\".")
  expect_error(combine_proportions(tibble(x = c(0, 1))), "All curves must have columns \"prob_cumul\" and \"area_cumul\".")
  expect_error(combine_proportions(calc_proportions(tibble(prob.pos = c(1))), plot = -1), "Plot variable must be TRUE/FALSE.")
})

test_that("srcurve combine: non-matching curves", {
  samp1 <- calc_proportions(data = tibble(prob.pos = c(0, 0.1, 0.12, 0.91, 0.92)),
                   bins = NULL, plot = FALSE)
  samp2 <- calc_proportions(data = tibble(prob.pos = c(.497, .498, .499, .51, .512, .514)),
                   bins = NULL, plot = FALSE)

  expect_error(combine_proportions(samp1, samp2, plot = FALSE), "All curves must be the same length.")
})

test_that("srcurve combine: one curve", {
  samp1 <- calc_proportions(data = tibble(prob.pos = c(0, 0.1, 0.12, 0.91, 0.92)),
                   bins = NULL, plot = FALSE)
  expect_equal(samp1, combine_proportions(samp1, plot = FALSE))
})

test_that("srcurve combine: small inputs", {
  samp1 <- calc_proportions(data = tibble(prob.pos = c(0, 0.1, 0.12, 0.91, 0.92, .93)),
                   bins = NULL, plot = FALSE)
  samp2 <- calc_proportions(data = tibble(prob.pos = c(.497, .498, .499, .51, .512, .514)),
                   bins = NULL, plot = FALSE)

  res <- combine_proportions(samp1, samp2, plot = FALSE)

  expect_true(is_tibble(res))
  expect_equal(nrow(res), nrow(samp1))
})

# srcurve_auc() ----------------------------------------------------------------

test_that("srcurve_auc: bad inputs", {
  input <- tibble(
    prob_prop = c(0, 1),
    area_prop = c(0, 1)
  )

  bad_input <- tibble(
    prob_prop = c(0, 1)
  )

  expect_error(srcurve_auc(NULL), "No data provided.")
  expect_error(srcurve_auc(bad_input), "Curve must be a success rate curve with area and probability proportions.")
  expect_error(srcurve_auc(input, integral_type = "none"), "Integral type must be \"left\", \"right\", or \"trap\"")
})

test_that("srcurve_auc: two point integrals", {
  input <- tibble(
    prob_prop = c(0, 1),
    area_prop = c(0, 1)
  )

  expect_equal(srcurve_auc(input, integral_type = "left"), 0)
  expect_equal(srcurve_auc(input, integral_type = "right"), 1)
  expect_equal(srcurve_auc(input, integral_type = "trap"), 0.5)
})

test_that("srcurve_auc: three point integrals", {
  input <- tibble(
    prob_prop = c(0, .5, 1),
    area_prop = c(0, .5, 1)
  )

  expect_equal(srcurve_auc(input, integral_type = "left"), .25)
  expect_equal(srcurve_auc(input, integral_type = "right"), .75)
  expect_equal(srcurve_auc(input, integral_type = "trap"), 0.5)
})

test_that("srcurve_auc: many point integrals", {
  input <- tibble(
    prob_prop = seq(0, 1, length = 100),
    area_prop = seq(0, 1, length = 100)
  )

  # with enough data points, all of these values should converge to .5
  expect_lt(abs(srcurve_auc(input, integral_type = "left") - .5), .01)
  expect_lt(abs(srcurve_auc(input, integral_type = "right") - .5), .01)
  expect_lt(abs(srcurve_auc(input, integral_type = "trap") - .5), .01)
})

test_that("srcurve_auc: many point integrals", {
  input <- tibble(
    prob_prop = seq(0, 1, length = 100),
    area_prop = seq(0, 1, length = 100)
  )

  # with enough data points, all of these values should converge to .5
  expect_lt(abs(srcurve_auc(input, integral_type = "left") - .5), .01)
  expect_lt(abs(srcurve_auc(input, integral_type = "right") - .5), .01)
  expect_lt(abs(srcurve_auc(input, integral_type = "trap") - .5), .01)
})

test_that("srcurve_auc: sample dem", {
  load(system.file("examples/model_output_gradcurvpca.Rdata", package = "TerrainWorksUtils"))

  curve <- calc_proportions(predictions, bins = seq(0, 1, length = 1000), plot = FALSE)
  curve_auc <- srcurve_auc(curve, integral_type = "trap")

  expect_true(is.numeric(curve_auc))
  expect_gt(curve_auc, 0.5)
  expect_lt(curve_auc, 1)

})

# combined tests ---------------------------------------------------------------

# Lets check that all of these functions behave in combination with each other
# in the way that we would expect.
#
# We will divide a DEM in half and check that using srcurve_combine gives the
# same curve as calling srcurve on the entire DEM.
# The AUC calculation should be the average value of the individual curves.
test_that("srcurve combining two dems", {
  load(system.file("examples/model_output_gradcurvpca.Rdata", package = "TerrainWorksUtils"))

  half1 <- predictions[1:floor(nrow(predictions) / 2), ]
  half2 <- predictions[ceiling(nrow(predictions) / 2):nrow(predictions), ]

  expect_warning(calc_proportions(half1, plot = FALSE))
  expect_warning(calc_proportions(half2, plot = FALSE))

  b <- seq(0, 1, length = 1000)
  half1_curve <- calc_proportions(half1, plot = FALSE, bins = b)
  half2_curve <- calc_proportions(half2, plot = FALSE, bins = b)
  full_curve <- calc_proportions(predictions, plot = FALSE, bins = b)

  comb_curve <- combine_proportions(half1_curve, half2_curve, plot = TRUE)

  # allow for some rounding error, but otherwise the curves should be the same
  expect_true(all((full_curve[["prob_prop"]] - comb_curve[["prob_prop"]]) < 0.001))
  expect_true(all((full_curve[["area_prop"]] - comb_curve[["area_prop"]]) < 0.001))

  expect_lt(srcurve_auc(full_curve) - srcurve_auc(comb_curve), 0.001)
  expect_lt(srcurve_auc(full_curve) - mean(srcurve_auc(half1_curve),
                                           srcurve_auc(half2_curve)),
               0.001)
  expect_lt(srcurve_auc(comb_curve) - mean(srcurve_auc(half1_curve),
                                           srcurve_auc(half2_curve)),
            0.001)

})

test_that("srcurve combining two dems, uneven split", {
  load(system.file("examples/model_output_gradcurvpca.Rdata", package = "TerrainWorksUtils"))

  half1 <- predictions[1:floor(nrow(predictions) / 4), ]
  half2 <- predictions[ceiling(nrow(predictions) / 4):nrow(predictions), ]

  expect_warning(calc_proportions(half1, plot = FALSE))
  expect_warning(calc_proportions(half2, plot = FALSE))

  b <- seq(0, 1, length = 1000)
  half1_curve <- calc_proportions(half1, plot = FALSE, bins = b)
  half2_curve <- calc_proportions(half2, plot = FALSE, bins = b)
  full_curve <- calc_proportions(predictions, plot = FALSE, bins = b)

  comb_curve <- combine_proportions(half1_curve, half2_curve, plot = FALSE)

  # allow for some rounding error, but otherwise the curves should be the same
  expect_true(all((full_curve[["prob_prop"]] - comb_curve[["prob_prop"]]) < 0.001))
  expect_true(all((full_curve[["area_prop"]] - comb_curve[["area_prop"]]) < 0.001))

  expect_lt(srcurve_auc(full_curve) - srcurve_auc(comb_curve), 0.001)
  expect_lt(srcurve_auc(full_curve) - mean(srcurve_auc(half1_curve),
                                           srcurve_auc(half2_curve)),
            0.001)
  expect_lt(srcurve_auc(comb_curve) - mean(srcurve_auc(half1_curve),
                                           srcurve_auc(half2_curve)),
            0.001)

})
