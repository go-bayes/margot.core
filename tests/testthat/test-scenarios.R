# test scenario s3 classes

test_that("new_scenario creates valid scenario objects", {
  # create test shadow
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = "x",
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  # test basic scenario creation
  scenario <- new_scenario(
    name = "Test Scenario",
    description = "A test scenario",
    shadows = shadow
  )
  
  expect_s3_class(scenario, "margot_scenario")
  expect_equal(scenario$name, "Test Scenario")
  expect_equal(scenario$description, "A test scenario")
  expect_equal(length(scenario$shadows), 1)
  expect_equal(scenario$n_shadows, 1)
})

test_that("new_scenario handles multiple shadows", {
  shadows <- list(
    new_shadow(
      type = "measurement_error",
      params = list(
        variables = "x",
        error_type = "classical",
        sigma = 0.5
      )
    ),
    new_shadow(
      type = "missing_data",
      params = list(
        mechanism = "MAR",
        prob = 0.2
      )
    )
  )
  
  scenario <- new_scenario(
    name = "Multi-shadow scenario",
    shadows = shadows
  )
  
  expect_equal(length(scenario$shadows), 2)
  expect_equal(scenario$n_shadows, 2)
})

test_that("scenario validation works", {
  # test invalid name
  expect_error(
    new_scenario(
      name = 123,
      shadows = list()
    ),
    "name must be a single character string"
  )
  
  # test invalid shadows
  expect_error(
    new_scenario(
      name = "Test",
      shadows = "not a list"
    ),
    "must be a list"
  )
  
  # test non-shadow in list
  expect_error(
    new_scenario(
      name = "Test",
      shadows = list(
        list(type = "invalid")
      )
    ),
    "is not a margot_shadow object"
  )
})

test_that("scenario print method works", {
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
      mechanism = "MCAR",
      prob = 0.1
    )
  )
  
  scenario <- new_scenario(
    name = "Complex Study",
    description = "Testing multiple biases",
    shadows = list(shadow1, shadow2),
    justification = "Based on prior validation studies",
    references = c("Smith 2020", "Jones 2021")
  )
  
  output <- capture.output(print(scenario))
  expect_match(output[1], "<margot_scenario>")
  expect_match(output[2], "Name: Complex Study")
  expect_true(any(grepl("Description:", output)))
  expect_true(any(grepl("Justification:", output)))
  expect_true(any(grepl("References:", output)))
  expect_true(any(grepl("Number of shadows: 2", output)))
})

test_that("scenario combination works", {
  scenario1 <- new_scenario(
    name = "Scenario 1",
    shadows = new_shadow(
      type = "measurement_error",
      params = list(
        variables = "x",
        error_type = "classical",
        sigma = 0.5
      )
    )
  )
  
  scenario2 <- new_scenario(
    name = "Scenario 2",
    shadows = new_shadow(
      type = "missing_data",
      params = list(
        mechanism = "MAR",
        prob = 0.2
      )
    )
  )
  
  combined <- c(scenario1, scenario2)
  expect_s3_class(combined, "margot_scenario")
  expect_equal(combined$name, "Scenario 1 + Scenario 2")
  expect_equal(length(combined$shadows), 2)
})

test_that("scenario extraction works", {
  shadows <- list(
    new_shadow(
      type = "measurement_error",
      params = list(
        variables = "x",
        error_type = "classical",
        sigma = 0.5
      )
    ),
    new_shadow(
      type = "missing_data",
      params = list(
        mechanism = "MAR",
        prob = 0.2
      )
    ),
    new_shadow(
      type = "truncation",
      params = list(
        variables = "y",
        lower = 0
      )
    )
  )
  
  scenario <- new_scenario(
    name = "Multi-shadow",
    shadows = shadows
  )
  
  # extract single shadow
  shadow1 <- scenario[1]
  expect_s3_class(shadow1, "margot_shadow")
  expect_equal(shadow1$type, "measurement_error")
  
  # extract multiple shadows
  subset <- scenario[c(1, 3)]
  expect_s3_class(subset, "margot_scenario")
  expect_equal(length(subset$shadows), 2)
  expect_match(subset$name, "subset")
})

test_that("scenario helper functions work", {
  scenario <- new_scenario(
    name = "Test",
    shadows = list(
      new_shadow(
        type = "measurement_error",
        params = list(
          variables = "x",
          error_type = "classical",
          sigma = 0.5
        )
      ),
      new_shadow(
        type = "missing_data",
        params = list(
          mechanism = "MAR",
          prob = 0.2
        )
      )
    )
  )
  
  # test length
  expect_equal(length(scenario), 2)
  
  # test names
  expect_equal(names(scenario), c("measurement_error", "missing_data"))
  
  # test is_scenario
  expect_true(is_scenario(scenario))
  expect_false(is_scenario(list(name = "Test")))
  
  # test as_scenario
  shadow <- new_shadow(
    type = "truncation",
    params = list(
      variables = "y",
      lower = 0
    )
  )
  
  scenario_from_shadow <- as_scenario(shadow, name = "Single shadow scenario")
  expect_s3_class(scenario_from_shadow, "margot_scenario")
  expect_equal(length(scenario_from_shadow$shadows), 1)
})

test_that("as.data.frame.margot_scenario works", {
  scenario <- new_scenario(
    name = "Test Scenario",
    shadows = list(
      new_shadow(
        type = "measurement_error",
        params = list(
          variables = c("x", "y"),
          error_type = "classical",
          sigma = 0.5
        )
      ),
      new_shadow(
        type = "missing_data",
        params = list(
          mechanism = "MAR",
          prob = 0.3
        )
      )
    )
  )
  
  df <- as.data.frame(scenario)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_equal(df$scenario[1], "Test Scenario")
  expect_equal(df$shadow_type, c("measurement_error", "missing_data"))
  expect_true("variables" %in% names(df))
  expect_true("mechanism" %in% names(df))
})