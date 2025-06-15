# Unit tests for margot_timeline S3 class

test_that("new_timeline creates valid timeline objects", {
  # basic timeline
  tl1 <- new_timeline(n_time_points = 5)
  
  expect_s3_class(tl1, "margot_timeline")
  expect_equal(tl1$n_time_points, 5)
  expect_equal(tl1$start_time, 0)
  expect_equal(tl1$time_spacing, 1)
  expect_equal(tl1$time_points, 0:4)
  expect_equal(length(tl1$observation_schedule), 5)
  
  # custom parameters
  tl2 <- new_timeline(
    n_time_points = 3,
    start_time = 100,
    time_spacing = 7
  )
  
  expect_equal(tl2$time_points, c(100, 107, 114))
  expect_equal(tl2$time_spacing, 7)
  
  # misaligned observation schedule
  tl3 <- new_timeline(
    n_time_points = 4,
    observation_schedule = "misaligned"
  )
  
  expect_true(all(tl3$observation_schedule != tl3$time_points))
})

test_that("new_timeline validates inputs correctly", {
  # invalid n_time_points
  expect_error(
    new_timeline(n_time_points = -1),
    "n_time_points must be a positive integer"
  )
  
  expect_error(
    new_timeline(n_time_points = 2.5),
    "n_time_points must be a positive integer"
  )
  
  expect_error(
    new_timeline(n_time_points = 0),
    "n_time_points must be a positive integer"
  )
  
  # invalid time_spacing
  expect_error(
    new_timeline(n_time_points = 5, time_spacing = -1),
    "time_spacing must be positive"
  )
  
  expect_error(
    new_timeline(n_time_points = 5, time_spacing = 0),
    "time_spacing must be positive"
  )
  
  # invalid observation_schedule
  expect_error(
    new_timeline(n_time_points = 5, observation_schedule = "invalid"),
    "observation_schedule must be one of"
  )
  
  expect_error(
    new_timeline(n_time_points = 5, observation_schedule = 1:3),
    "observation_schedule vector must have length"
  )
})

test_that("validate_timeline works correctly", {
  # valid timeline
  tl <- new_timeline(n_time_points = 5)
  expect_silent(validate_timeline(tl))
  
  # invalid structure
  bad_tl <- list(n_time_points = 5)
  class(bad_tl) <- "margot_timeline"
  expect_error(validate_timeline(bad_tl), "required elements")
  
  # inconsistent time points
  bad_tl2 <- tl
  bad_tl2$time_points <- 1:3  # wrong length
  expect_error(validate_timeline(bad_tl2), "inconsistent with n_time_points")
  
  # non-numeric time points
  bad_tl3 <- tl
  bad_tl3$time_points <- letters[1:5]
  expect_error(validate_timeline(bad_tl3), "must be numeric")
})

test_that("print.margot_timeline displays correctly", {
  tl <- new_timeline(
    n_time_points = 5,
    start_time = 10,
    time_spacing = 2
  )
  
  output <- capture.output(print(tl))
  
  expect_true(any(grepl("margot_timeline", output)))
  expect_true(any(grepl("Time points: 5", output)))
  expect_true(any(grepl("Start: 10", output)))
  expect_true(any(grepl("Spacing: 2", output)))
  expect_true(any(grepl("Schedule: aligned", output)))
})

test_that("timeline helper functions work correctly", {
  # count_time_points
  expect_equal(count_time_points(0:4), 5)
  expect_equal(count_time_points(c(0, 7, 14, 21)), 4)
  expect_equal(count_time_points(numeric(0)), 0)
  expect_equal(count_time_points(100), 1)
  
  # validate_temporal_events
  events <- data.frame(
    time = c(0, 1, 2, 3),
    event = c("A", "B", "C", "D")
  )
  
  tl <- new_timeline(n_time_points = 5)
  
  # valid events
  expect_silent(validate_temporal_events(events, tl))
  
  # events outside timeline
  bad_events <- data.frame(
    time = c(0, 5, 10),  # 5 and 10 are outside
    event = c("A", "B", "C")
  )
  
  expect_warning(
    validate_temporal_events(bad_events, tl),
    "Events found outside timeline"
  )
  
  # missing time column
  no_time <- data.frame(event = c("A", "B"))
  expect_error(
    validate_temporal_events(no_time, tl),
    "Events data frame must have a 'time' column"
  )
})

test_that("check_temporal_requirements works", {
  tl1 <- new_timeline(n_time_points = 5)
  tl2 <- new_timeline(n_time_points = 3)
  
  # sufficient time points
  req1 <- list(min_time_points = 3)
  expect_true(check_temporal_requirements(tl1, req1))
  expect_true(check_temporal_requirements(tl2, req1))
  
  # insufficient time points
  req2 <- list(min_time_points = 10)
  expect_false(check_temporal_requirements(tl1, req2))
  expect_false(check_temporal_requirements(tl2, req2))
  
  # specific spacing requirement
  req3 <- list(required_spacing = 1)
  expect_true(check_temporal_requirements(tl1, req3))
  
  tl_weekly <- new_timeline(n_time_points = 4, time_spacing = 7)
  req4 <- list(required_spacing = 7)
  expect_true(check_temporal_requirements(tl_weekly, req4))
  expect_false(check_temporal_requirements(tl1, req4))
  
  # observation type requirement
  req5 := list(observation_type = "aligned")
  expect_true(check_temporal_requirements(tl1, req5))
  
  tl_misaligned <- new_timeline(
    n_time_points = 4,
    observation_schedule = "misaligned"
  )
  expect_false(check_temporal_requirements(tl_misaligned, req5))
})

test_that("timeline metadata is preserved", {
  meta <- list(
    study = "Example Study",
    version = "1.0",
    notes = "Test timeline"
  )
  
  tl <- new_timeline(
    n_time_points = 5,
    metadata = meta
  )
  
  expect_equal(tl$metadata$study, "Example Study")
  expect_equal(tl$metadata$version, "1.0")
  expect_true("created" %in% names(tl$metadata))
  expect_s3_class(tl$metadata$created, "POSIXct")
})

test_that("timeline with custom observation schedule works", {
  # provide specific observation times
  obs_times <- c(0, 3, 7, 14)
  
  tl <- new_timeline(
    n_time_points = 4,
    observation_schedule = obs_times
  )
  
  expect_equal(tl$observation_schedule, obs_times)
  expect_equal(tl$n_time_points, 4)
  
  # jittered schedule
  set.seed(123)
  tl_jitter <- new_timeline(
    n_time_points = 5,
    observation_schedule = "jittered",
    start_time = 0,
    time_spacing = 10
  )
  
  # observations should be near but not exactly at time points
  diffs <- abs(tl_jitter$observation_schedule - tl_jitter$time_points)
  expect_true(all(diffs > 0))
  expect_true(all(diffs < 5))  # jitter should be reasonable
})