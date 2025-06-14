# test shadow s3 classes

test_that("new_shadow creates valid shadow objects", {
  # test basic shadow creation
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = "x",
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  expect_s3_class(shadow, "margot_shadow")
  expect_s3_class(shadow, "measurement_error_shadow")
  expect_equal(shadow$type, "measurement_error")
  expect_equal(shadow$params$sigma, 0.5)
})

test_that("new_shadow validates shadow types", {
  # test invalid type
  expect_error(
    new_shadow(type = "invalid_type", params = list()),
    "Invalid shadow type"
  )
})

test_that("shadow validation works for measurement error", {
  # test missing required params
  expect_error(
    new_shadow(
      type = "measurement_error",
      params = list()
    ),
    "requires 'variables' parameter"
  )
  
  expect_error(
    new_shadow(
      type = "measurement_error", 
      params = list(variables = "x")
    ),
    "requires 'error_type' parameter"
  )
  
  # test invalid error type
  expect_error(
    new_shadow(
      type = "measurement_error",
      params = list(
        variables = "x",
        error_type = "invalid"
      )
    ),
    "Invalid error_type"
  )
  
  # test missing sigma for classical error
  expect_error(
    new_shadow(
      type = "measurement_error",
      params = list(
        variables = "x",
        error_type = "classical"
      )
    ),
    "requires numeric sigma"
  )
  
  # test misclassification parameters
  expect_error(
    new_shadow(
      type = "measurement_error",
      params = list(
        variables = "x",
        error_type = "misclassification"
      )
    ),
    "requires 'sensitivity' and 'specificity'"
  )
})

test_that("shadow validation works for missing data", {
  # test missing mechanism
  expect_error(
    new_shadow(
      type = "missing_data",
      params = list(prob = 0.2)
    ),
    "requires 'mechanism' parameter"
  )
  
  # test invalid mechanism
  expect_error(
    new_shadow(
      type = "missing_data",
      params = list(
        mechanism = "invalid",
        prob = 0.2
      )
    ),
    "Invalid mechanism"
  )
  
  # test missing prob
  expect_error(
    new_shadow(
      type = "missing_data",
      params = list(mechanism = "MCAR")
    ),
    "requires 'prob' between 0 and 1"
  )
  
  # test invalid prob
  expect_error(
    new_shadow(
      type = "missing_data",
      params = list(
        mechanism = "MCAR",
        prob = 1.5
      )
    ),
    "between 0 and 1"
  )
})

test_that("shadow validation works for truncation", {
  # test missing bounds
  expect_error(
    new_shadow(
      type = "truncation",
      params = list(variables = "x")
    ),
    "at least one of 'lower' or 'upper' bounds"
  )
  
  # test invalid bounds
  expect_error(
    new_shadow(
      type = "truncation",
      params = list(
        variables = "x",
        lower = 5,
        upper = 2
      )
    ),
    "lower bound must be less than upper bound"
  )
  
  # valid truncation shadow
  shadow <- new_shadow(
    type = "truncation",
    params = list(
      variables = "x",
      lower = 0
    )
  )
  expect_s3_class(shadow, "truncation_shadow")
})

test_that("print methods work correctly", {
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = c("x", "y", "z"),
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  # capture print output
  output <- capture.output(print(shadow))
  expect_match(output[1], "<margot_shadow>")
  expect_match(output[2], "Type: measurement_error")
  
  # test summary
  summ <- summary(shadow)
  expect_s3_class(summ, "summary.margot_shadow")
  expect_equal(summ$type, "measurement_error")
  expect_equal(summ$n_params, 3)
})

test_that("shadow helper functions work", {
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = "x",
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  # test is_shadow
  expect_true(is_shadow(shadow))
  expect_false(is_shadow(list(type = "measurement_error")))
  
  # test as_shadow
  list_shadow <- list(
    type = "missing_data",
    params = list(
      mechanism = "MCAR",
      prob = 0.2
    )
  )
  
  converted <- as_shadow(list_shadow)
  expect_s3_class(converted, "margot_shadow")
  expect_s3_class(converted, "missing_data_shadow")
})

test_that("shadow_list class works correctly", {
  shadow1 <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = "x",
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  shadow2 <- new_shadow(
    type = "missing_data",
    params = list(
      mechanism = "MAR",
      prob = 0.3
    )
  )
  
  # create shadow list
  shadows <- new_shadow_list(shadow1, shadow2)
  expect_s3_class(shadows, "shadow_list")
  expect_length(shadows, 2)
  
  # test print
  output <- capture.output(print(shadows))
  expect_match(output[1], "<shadow_list> with 2 shadows")
  
  # test combining
  shadow3 <- new_shadow(
    type = "truncation",
    params = list(
      variables = "y",
      lower = 0
    )
  )
  
  combined <- c(shadows, shadow3)
  expect_s3_class(combined, "shadow_list")
  expect_length(combined, 3)
  
  # test extraction
  extracted <- shadows[1]
  expect_s3_class(extracted, "margot_shadow")
})

test_that("as.data.frame methods work", {
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = "x",
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  df <- as.data.frame(shadow)
  expect_s3_class(df, "data.frame")
  expect_equal(df$type, "measurement_error")
  expect_equal(df$variables, "x")
  expect_equal(df$sigma, 0.5)
})