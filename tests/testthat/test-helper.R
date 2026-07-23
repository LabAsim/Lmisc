##############################
# modify_parameter_estimates #
##############################
test_that("correct output", {
  test <- data.frame(
    pvalue = c(0, 1, 0.001, 0.1, 0.5, 0.0001)
  )
  testit <- modify_parameter_estimates(df = test)

  correct_output <- data.frame(
    pvalue = c(0, 1, 0.001, 0.1, 0.5, 0),
    pvalue_str = c("<0.001", "=1", "=0.001", "=0.1", "=0.5", "<0.001")
  )

  expect_equal(correct_output, testit)
})


test_that("correct output with characters", {
  test <- data.frame(
    pvalue = c("0", "1", 0.001, 0.1, 0.5, 0.0001)
  )
  testit <- modify_parameter_estimates(df = test)

  correct_output <- data.frame(
    pvalue = c(0, 1, 0.001, 0.1, 0.5, 0.0001),
    pvalue_str = c("<0.001", "=1", "=0.001", "=0.1", "=0.5", "<0.001")
  )

  expect_equal(correct_output, testit)
})


test_that("when add_equal_sign is False", {
  test <- data.frame(
    pvalue = c("0", "1", 0.001, 0.1, 0.5, 0.0001)
  )
  testit <- modify_parameter_estimates(df = test, add_equal_sign = F)

  correct_output <- data.frame(
    pvalue = c(0, 1, 0.001, 0.1, 0.5, 0.0001),
    pvalue_str = c("<0.001", "1", "0.001", "0.1", "0.5", "<0.001")
  )

  expect_equal(correct_output, testit)
})
