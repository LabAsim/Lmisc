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


describe("shrink_endpoints", {
  it("preserves endpoints when factor is 1", {
    result <- shrink_endpoints(0, 0, 10, 10, factor = 1)

    expect_equal(
      unname(as.numeric(result[1, ])),
      c(0, 0, 10, 10)
    )
  })

  it("collapses endpoints to the midpoint when factor is 0", {
    result <- shrink_endpoints(0, 0, 10, 10, factor = 0)

    expect_equal(
      unname(as.numeric(result[1, ])),
      c(5, 5, 5, 5)
    )
  })

  it("shrinks a horizontal line symmetrically", {
    result <- shrink_endpoints(0, 0, 10, 0, factor = 0.5)

    expect_equal(
      unname(as.numeric(result[1, ])),
      c(2.5, 0, 7.5, 0)
    )
  })

  it("shrinks a vertical line symmetrically", {
    result <- shrink_endpoints(0, 0, 0, 10, factor = 0.5)

    expect_equal(
      unname(as.numeric(result[1, ])),
      c(0, 2.5, 0, 7.5)
    )
  })

  it("shrinks a diagonal line symmetrically", {
    result <- shrink_endpoints(0, 0, 8, 4, factor = 0.5)

    expect_equal(
      unname(as.numeric(result[1, ])),
      c(2, 1, 6, 3)
    )
  })

  it("preserves the midpoint of the original line", {
    result <- shrink_endpoints(2, 4, 12, 14, factor = 0.3)

    expect_equal(
      (result$x1 + result$x2) / 2,
      7
    )

    expect_equal(
      (result$y1 + result$y2) / 2,
      9
    )
  })

  it("reduces line length by the specified factor", {
    result <- shrink_endpoints(0, 0, 3, 4, factor = 0.4)

    new_length <- sqrt(
      (result$x2 - result$x1)^2 +
        (result$y2 - result$y1)^2
    )

    expect_equal(new_length, 5 * 0.4)
  })

  it("rejects factors outside the interval [0, 1]", {
    expect_error(
      shrink_endpoints(0, 0, 10, 10, factor = -0.1)
    )

    expect_error(
      shrink_endpoints(0, 0, 10, 10, factor = 1.1)
    )
  })

  it("returns the expected data frame structure", {
    result <- shrink_endpoints(0, 0, 10, 10)

    expect_s3_class(result, "data.frame")
    expect_named(result, c("x1", "y1", "x2", "y2"))
    expect_equal(nrow(result), 1L)
  })
})
