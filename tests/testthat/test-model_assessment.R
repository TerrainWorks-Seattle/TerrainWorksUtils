test_that("success rate curve: bad inputs", {
  expect_error(success_rate_curve(NULL), "No data supplied")
  expect_error(success_rate_curve(predictions, prob_col = "not_here"), "prob_col must exist in data.")
  expect_error(success_rate_curve(predictions, plot = 0), "Plot variable must be TRUE/FALSE.")
})

test_that("success rate curve: returns a tibble", {
  res <- success_rate_curve(predictions)
  # IN PROG --- currently, the predictions i am importing are generally very low,
  # producing an almost right angle success rate curve. I may want to use a different model
  # as an example.
  expect_true(is_tibble(res))

  # check that proportions range from 0 to 1
  expect_true(min(res[["prob_prop"]]) < 0.001)
  expect_true((1 - max(res[["prob_prop"]])) < 0.001)
  expect_true(min(res[["area_prop"]]) < 0.001)
  expect_true((1 - max(res[["area_prop"]])) < 0.001)
})

