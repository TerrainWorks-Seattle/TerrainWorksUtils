test_that("success rate curve: bad inputs", {
  expect_error(srcurve(NULL), "No data supplied")
  expect_error(srcurve(predictions, prob_col = "not_here"), "prob_col must exist in data or an alternate column name must be specified.")
  expect_error(srcurve(predictions, plot = -1), "Plot variable must be TRUE/FALSE.")
})

test_that("success rate curve: small tibble", {
  input <- tibble(
    row_ids = c(1, 2, 3, 4, 5),
    prob.pos = c(1, 0, 0.5, .2, .2)
  )
  res <- srcurve(input, plot = FALSE)
  expect_true(is_tibble(res))

  # check that cumulative sums make sense
  expect_equal(max(res[["prob_cumul"]]), sum(input[["prob.pos"]]))
  expect_equal(max(res[["area_cumul"]]), max(input[["row_ids"]]))
})

test_that("success rate curve: test with sample dem", {
  res <- srcurve(predictions, plot = FALSE)
  expect_true(is_tibble(res))

  # check that proportions range from 0 to 1
  expect_true(min(res[["prob_prop"]]) < 0.001)
  expect_true((1 - max(res[["prob_prop"]])) < 0.001)
  expect_true(min(res[["area_prop"]]) < 0.001)
  expect_true((1 - max(res[["area_prop"]])) < 0.001)
})

test_that("srcurve combine: bad inputs", {

})

test_that("srcurve combine: sum is right", {

})

