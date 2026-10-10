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


test_that("correct output with characters that are not numbers", {
  test <- data.frame(
    pvalue = c("0", "1", 0.001, 0.1, 0.5, 0.0001),
    extra = c("a", "b", "c", "d", "e", "d")
  )
  testit <- modify_parameter_estimates(df = test)

  correct_output <- data.frame(
    pvalue = c(0, 1, 0.001, 0.1, 0.5, 0.0001),
    extra = c("a", "b", "c", "d", "e", "d"),
    pvalue_str = c("<0.001", "=1", "=0.001", "=0.1", "=0.5", "<0.001")
  )

  expect_equal(correct_output, testit)
})


test_that("correct output with mixed types", {
  test <- data.frame(
    pvalue = c("0", "1", 0.001, 0.1, 0.5, 0.0001),
    extra = c("a", "b", "c", "d", "2", "1")
  )
  testit <- modify_parameter_estimates(df = test)
  correct_output <- data.frame(
    pvalue = c(0, 1, 0.001, 0.1, 0.5, 0.0001),
    extra = c("a", "b", "c", "d", "2", "1"),
    pvalue_str = c("<0.001", "=1", "=0.001", "=0.1", "=0.5", "<0.001")
  )

  expect_equal(correct_output, testit)
})

####################
# shrink_endpoints #
####################

test_that("negative factor throws error", {
  expect_error(
    shrink_endpoints(
      x1 = 0,
      y1 = 0,
      x2 = 10,
      y2 = 10,
      factor = -0.5
    )
  )
})


test_that("factor greater than 1 throws error", {
  expect_error(
    shrink_endpoints(
      x1 = 0,
      y1 = 0,
      x2 = 10,
      y2 = 10,
      factor = 1.5
    )
  )
})


test_that("factor 1 leaves endpoints unchanged", {
  result <- shrink_endpoints(
    x1 = 2,
    y1 = 3,
    x2 = 8,
    y2 = 9,
    factor = 1
  )

  correct_output <- data.frame(
    x1 = 2,
    y1 = 3,
    x2 = 8,
    y2 = 9
  )

  expect_equal(result, correct_output)
})


test_that("factor close to zero produces short line", {
  result <- shrink_endpoints(
    x1 = 0,
    y1 = 0,
    x2 = 10,
    y2 = 20,
    factor = 0.01
  )

  correct_output <- data.frame(
    x1 = 4.95,
    y1 = 9.9,
    x2 = 5.05,
    y2 = 10.1
  )

  expect_equal(result, correct_output)
})
