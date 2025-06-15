# Unit tests for temporal measurement error S3 class in margot.core

test_that("new_temporal_measurement_error creates valid objects", {
  # retrospective measurement error
  tme1 <- new_temporal_measurement_error(
    error_type = "retrospective",
    affected_variables = c("t1_a", "t2_a"),
    window_size = 30,
    window_type = "fixed",
    aggregation = "any"
  )
  
  expect_s3_class(tme1, "temporal_measurement_error")
  expect_s3_class(tme1, "margot_shadow")
  expect_equal(tme1$type, "temporal_measurement_error")
  expect_equal(tme1$params$error_type, "retrospective")
  expect_equal(tme1$params$window_size, 30)
  expect_equal(tme1$params$aggregation, "any")
  
  # delayed measurement error
  tme2 <- new_temporal_measurement_error(
    error_type = "delayed",
    affected_variables = c("t1_a", "t2_a"),
    delay = 1,
    carry_forward = TRUE
  )
  
  expect_equal(tme2$params$error_type, "delayed")
  expect_equal(tme2$params$delay, 1)
  expect_true(tme2$params$carry_forward)
  
  # anticipatory measurement error
  tme3 <- new_temporal_measurement_error(
    error_type = "anticipatory",
    affected_variables = "t2_a",
    lead_time = 7,
    probability = 0.8
  )
  
  expect_equal(tme3$params$error_type, "anticipatory")
  expect_equal(tme3$params$lead_time, 7)
  expect_equal(tme3$params$probability, 0.8)
})

test_that("new_temporal_measurement_error validates inputs", {
  # missing affected_variables
  expect_error(
    new_temporal_measurement_error(
      error_type = "retrospective",
      window_size = 30
    ),
    "affected_variables is required"
  )
  
  # invalid error_type
  expect_error(
    new_temporal_measurement_error(
      error_type = "invalid",
      affected_variables = "t1_a"
    ),
    "should be one of"
  )
  
  # retrospective without window_size
  expect_error(
    new_temporal_measurement_error(
      error_type = "retrospective",
      affected_variables = "t1_a"
    ),
    "window_size is required for retrospective"
  )
  
  # delayed without delay parameter
  expect_error(
    new_temporal_measurement_error(
      error_type = "delayed",
      affected_variables = "t1_a"
    ),
    "delay is required for delayed"
  )
  
  # anticipatory without lead_time
  expect_error(
    new_temporal_measurement_error(
      error_type = "anticipatory",
      affected_variables = "t1_a"
    ),
    "lead_time is required for anticipatory"
  )
  
  # invalid probability
  expect_error(
    new_temporal_measurement_error(
      error_type = "anticipatory",
      affected_variables = "t1_a",
      lead_time = 7,
      probability = 1.5
    ),
    "probability must be between 0 and 1"
  )
  
  expect_error(
    new_temporal_measurement_error(
      error_type = "anticipatory",
      affected_variables = "t1_a",
      lead_time = 7,
      probability = -0.1
    ),
    "probability must be between 0 and 1"
  )
})

test_that("temporal measurement error integrates with timeline", {
  # create timeline
  tl <- new_timeline(
    n_time_points = 5,
    start_time = 0,
    time_spacing = 7
  )
  
  # create temporal measurement error with timeline
  tme <- new_temporal_measurement_error(
    error_type = "retrospective",
    affected_variables = c("t1_a", "t2_a", "t3_a"),
    timeline = tl,
    window_size = 14,  # 2 weeks
    aggregation = "any"
  )
  
  expect_equal(tme$params$timeline, tl)
  
  # window size should make sense given timeline spacing
  expect_equal(tme$params$window_size / tl$time_spacing, 2)
})

test_that("validate_shadow.temporal_measurement_error_shadow works", {
  # create a valid shadow
  tme <- new_temporal_measurement_error(
    error_type = "retrospective",
    affected_variables = c("t1_a", "t2_a"),
    window_size = 30,
    aggregation = "any"
  )
  
  # add the temporal_measurement_error_shadow class
  class(tme) <- c("temporal_measurement_error_shadow", class(tme))
  
  # should validate successfully
  expect_true(validate_shadow(tme))
  
  # create invalid shadow - missing error_type
  bad_tme <- tme
  bad_tme$params$error_type <- NULL
  
  expect_error(
    validate_shadow(bad_tme),
    "error_type parameter required"
  )
  
  # missing variables
  bad_tme2 <- tme
  bad_tme2$params$variables <- NULL
  bad_tme2$params$affected_variables <- NULL
  
  expect_error(
    validate_shadow(bad_tme2),
    "variables parameter required"
  )
  
  # retrospective specific validation
  bad_retro <- tme
  bad_retro$params$window_size <- NULL
  
  expect_error(
    validate_shadow(bad_retro),
    "retrospective errors require window_size"
  )
  
  # delayed specific validation
  delay_tme <- new_temporal_measurement_error(
    error_type = "delayed",
    affected_variables = "t1_a",
    delay = 1
  )
  class(delay_tme) <- c("temporal_measurement_error_shadow", class(delay_tme))
  
  bad_delay <- delay_tme
  bad_delay$params$delay <- NULL
  
  expect_error(
    validate_shadow(bad_delay),
    "delayed measurement requires delay"
  )
  
  # anticipatory specific validation
  anticip_tme <- new_temporal_measurement_error(
    error_type = "anticipatory",
    affected_variables = "t1_a",
    lead_time = 7,
    probability = 0.8
  )
  class(anticip_tme) <- c("temporal_measurement_error_shadow", class(anticip_tme))
  
  bad_anticip <- anticip_tme
  bad_anticip$params$lead_time <- NULL
  
  expect_error(
    validate_shadow(bad_anticip),
    "anticipatory measurement requires lead_time"
  )
})

test_that("print.temporal_measurement_error_shadow displays correctly", {
  # retrospective
  tme1 <- new_temporal_measurement_error(
    error_type = "retrospective",
    affected_variables = c("t1_a", "t2_a"),
    window_size = 30,
    window_type = "fixed",
    aggregation = "any"
  )
  class(tme1) <- c("temporal_measurement_error_shadow", class(tme1))
  
  output1 <- capture.output(print(tme1))
  expect_true(any(grepl("Temporal Measurement Error Shadow", output1)))
  expect_true(any(grepl("Type: retrospective", output1)))
  expect_true(any(grepl("Variables: t1_a, t2_a", output1)))
  expect_true(any(grepl("Window: 30 fixed", output1)))
  expect_true(any(grepl("Aggregation: any", output1)))
  
  # delayed
  tme2 <- new_temporal_measurement_error(
    error_type = "delayed",
    affected_variables = "t1_a",
    delay = 2,
    carry_forward = TRUE
  )
  class(tme2) <- c("temporal_measurement_error_shadow", class(tme2))
  
  output2 <- capture.output(print(tme2))
  expect_true(any(grepl("Type: delayed", output2)))
  expect_true(any(grepl("Delay: 2 periods", output2)))
  expect_true(any(grepl("Carry forward: TRUE", output2)))
  
  # anticipatory
  tme3 <- new_temporal_measurement_error(
    error_type = "anticipatory",
    affected_variables = "t2_a",
    lead_time = 7,
    probability = 0.75
  )
  class(tme3) <- c("temporal_measurement_error_shadow", class(tme3))
  
  output3 <- capture.output(print(tme3))
  expect_true(any(grepl("Type: anticipatory", output3)))
  expect_true(any(grepl("Lead time: 7", output3)))
  expect_true(any(grepl("Follow-through: 0.75", output3)))
})

test_that("temporal measurement error parameters are complete", {
  # check all parameter types are properly handled
  
  # retrospective with all parameters
  tme_retro_full <- new_temporal_measurement_error(
    error_type = "retrospective",
    affected_variables = c("t1_a", "t2_a"),
    window_size = 30,
    window_type = "variable",
    aggregation = "mean",
    recall_decay = 0.1,
    reference_point = "visit_date"
  )
  
  expect_equal(tme_retro_full$params$window_type, "variable")
  expect_equal(tme_retro_full$params$aggregation, "mean")
  expect_equal(tme_retro_full$params$recall_decay, 0.1)
  
  # delayed with all parameters
  tme_delay_full <- new_temporal_measurement_error(
    error_type = "delayed",
    affected_variables = c("t1_a", "t2_a"),
    delay = 1,
    carry_forward = FALSE,
    reference_point = "previous_visit"
  )
  
  expect_false(tme_delay_full$params$carry_forward)
  expect_equal(tme_delay_full$params$reference_point, "previous_visit")
  
  # aggregated type (for future implementation)
  tme_agg <- new_temporal_measurement_error(
    error_type = "aggregated",
    affected_variables = c("t1_y", "t2_y"),
    aggregation_period = 30,
    aggregation_method = "mean"
  )
  
  expect_equal(tme_agg$params$error_type, "aggregated")
  expect_equal(tme_agg$params$aggregation_period, 30)
})

test_that("affected_variables vs variables parameter handling", {
  # should accept either affected_variables or variables
  tme1 <- new_temporal_measurement_error(
    error_type = "retrospective",
    affected_variables = c("t1_a", "t2_a"),
    window_size = 30
  )
  
  expect_equal(tme1$params$variables, c("t1_a", "t2_a"))
  
  # using variables parameter directly
  tme2 <- new_temporal_measurement_error(
    error_type = "retrospective",
    variables = c("t1_a", "t2_a"),
    window_size = 30
  )
  
  expect_equal(tme2$params$variables, c("t1_a", "t2_a"))
  
  # both specified - affected_variables takes precedence
  tme3 <- new_temporal_measurement_error(
    error_type = "retrospective",
    affected_variables = c("t1_a"),
    variables = c("t2_a"),
    window_size = 30
  )
  
  expect_equal(tme3$params$variables, c("t1_a"))
})