# test margot_panel class

test_that("margot_panel creates valid panel objects", {
  # create test data
  df <- data.frame(
    id = rep(1:3, each = 3),
    wave = rep(1:3, 3),
    outcome = rnorm(9),
    treatment = rbinom(9, 1, 0.5)
  )
  
  # create panel
  panel <- margot_panel(df, id = "id", time = "wave")
  
  expect_s3_class(panel, "margot_panel")
  expect_s3_class(panel, "data.frame")
  expect_equal(attr(panel, "id_col"), "id")
  expect_equal(attr(panel, "time_col"), "wave")
})

test_that("margot_panel validates inputs", {
  df <- data.frame(x = 1:5, y = 1:5)
  
  # not a data frame
  expect_error(
    margot_panel(list(x = 1:5), id = "x", time = "y"),
    "must be a data.frame"
  )
  
  # id not character
  expect_error(
    margot_panel(df, id = 1, time = "y"),
    "id must be a single character string"
  )
  
  # time not character
  expect_error(
    margot_panel(df, id = "x", time = 2),
    "time must be a single character string"
  )
  
  # missing id column
  expect_error(
    margot_panel(df, id = "missing", time = "y"),
    "id column 'missing' not found"
  )
  
  # missing time column
  expect_error(
    margot_panel(df, id = "x", time = "missing"),
    "time column 'missing' not found"
  )
})

test_that("print method displays panel information", {
  # balanced panel
  df_balanced <- data.frame(
    id = rep(1:3, each = 3),
    wave = rep(1:3, 3),
    outcome = rnorm(9)
  )
  
  panel <- margot_panel(df_balanced, id = "id", time = "wave")
  
  output <- capture.output(print(panel))
  expect_match(output[1], "<margot_panel>")
  expect_true(any(grepl("Rows: 9", output)))
  expect_true(any(grepl("Units: 3", output)))
  expect_true(any(grepl("Time periods: 3", output)))
  expect_true(any(grepl("Panel: balanced", output)))
  
  # unbalanced panel
  df_unbalanced <- df_balanced[-c(2, 5), ]  # remove some observations
  panel_unbal <- margot_panel(df_unbalanced, id = "id", time = "wave")
  
  output_unbal <- capture.output(print(panel_unbal))
  expect_true(any(grepl("Panel: unbalanced", output_unbal)))
  expect_true(any(grepl("77.8% complete", output_unbal)))
})

test_that("summary method provides detailed information", {
  df <- data.frame(
    id = rep(1:4, times = c(3, 2, 3, 1)),
    time = c(1:3, 1:2, 1:3, 1),
    value = rnorm(9)
  )
  
  panel <- margot_panel(df, id = "id", time = "time")
  summ <- summary(panel)
  
  expect_s3_class(summ, "summary.margot_panel")
  expect_equal(summ$n_units, 4)
  expect_equal(summ$n_periods, 3)
  expect_equal(summ$n_obs, 9)
  expect_false(summ$is_balanced)
  expect_equal(summ$missing_combinations, 3)
  expect_equal(summ$time_range, c(1, 3))
  
  # check print
  output <- capture.output(print(summ))
  expect_match(output[1], "Panel Data Summary")
  expect_true(any(grepl("Units: 4", output)))
  expect_true(any(grepl("Panel type: Unbalanced", output)))
})

test_that("is_margot_panel works correctly", {
  df <- data.frame(id = 1:3, time = 1:3)
  panel <- margot_panel(df, id = "id", time = "time")
  
  expect_true(is_margot_panel(panel))
  expect_false(is_margot_panel(df))
  expect_false(is_margot_panel(list()))
})

test_that("panel_attrs extracts attributes", {
  df <- data.frame(id = 1:3, time = 1:3, x = 1:3)
  panel <- margot_panel(df, id = "id", time = "time")
  
  attrs <- panel_attrs(panel)
  expect_equal(attrs$id_col, "id")
  expect_equal(attrs$time_col, "time")
  
  # error on non-panel
  expect_error(
    panel_attrs(df),
    "must be a margot_panel object"
  )
})

test_that("as_wide.margot_panel converts to wide format", {
  df <- data.frame(
    id = rep(1:2, each = 3),
    time = rep(1:3, 2),
    outcome = 1:6,
    treatment = c(0, 1, 1, 1, 0, 1)
  )
  
  panel <- margot_panel(df, id = "id", time = "time")
  wide <- as_wide.margot_panel(panel)
  
  expect_s3_class(wide, "data.frame")
  expect_equal(nrow(wide), 2)  # 2 units
  expect_true("outcome_1" %in% names(wide))
  expect_true("outcome_2" %in% names(wide))
  expect_true("outcome_3" %in% names(wide))
  expect_true("treatment_1" %in% names(wide))
  
  # test with custom separator
  wide_dot <- as_wide.margot_panel(panel, sep = ".")
  expect_true("outcome.1" %in% names(wide_dot))
  
  # test with no value columns
  df_no_values <- data.frame(id = 1:3, time = 1:3)
  panel_no_values <- margot_panel(df_no_values, id = "id", time = "time")
  expect_warning(
    wide_no_values <- as_wide.margot_panel(panel_no_values),
    "No value columns found"
  )
})

test_that("subsetting preserves panel structure when appropriate", {
  df <- data.frame(
    id = rep(1:3, each = 3),
    wave = rep(1:3, 3),
    outcome = rnorm(9),
    treatment = rbinom(9, 1, 0.5)
  )
  
  panel <- margot_panel(df, id = "id", time = "wave")
  
  # row subsetting preserves panel
  subset1 <- panel[1:6, ]
  expect_s3_class(subset1, "margot_panel")
  expect_equal(nrow(subset1), 6)
  
  # column subsetting that keeps id and time preserves panel
  subset2 <- panel[, c("id", "wave", "outcome")]
  expect_s3_class(subset2, "margot_panel")
  
  # column subsetting that drops id loses panel class
  subset3 <- panel[, c("wave", "outcome")]
  expect_false(inherits(subset3, "margot_panel"))
  
  # single column with drop = TRUE returns vector
  outcome_vec <- panel[, "outcome", drop = TRUE]
  expect_type(outcome_vec, "double")
  expect_false(inherits(outcome_vec, "margot_panel"))
  
  # single column with drop = FALSE returns data frame
  outcome_df <- panel[, "outcome", drop = FALSE]
  expect_s3_class(outcome_df, "data.frame")
  expect_false(inherits(outcome_df, "margot_panel"))
})