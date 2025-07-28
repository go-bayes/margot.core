# test margot.core utility functions

test_that(".margot_core_classes returns correct class names", {
  classes <- .margot_core_classes()
  
  expect_type(classes, "character")
  expect_true(length(classes) > 0)
  
  # check for expected core classes
  expect_true("margot_shadow" %in% classes)
  expect_true("margot_scenario" %in% classes)
  expect_true("margot_panel" %in% classes)
  expect_true("shadow_list" %in% classes)
})

test_that(".margot_core_generics returns correct generic names", {
  generics <- .margot_core_generics()
  
  expect_type(generics, "character")
  expect_true(length(generics) > 0)
  
  # check for expected generics
  expect_true("validate_shadow" %in% generics)
  expect_true("apply_shadow" %in% generics)
})

test_that("margot_options works correctly", {
  # save current options
  original_opts <- options()
  
  # test getting all options
  all_opts <- margot_options()
  expect_type(all_opts, "list")
  
  # test setting an option
  margot_options(margot.verbose = TRUE)
  expect_true(getOption("margot.verbose"))
  
  # test getting single option
  verbose_opt <- margot_options("margot.verbose")
  expect_true(verbose_opt$margot.verbose)
  
  # test setting multiple options
  margot_options(
    margot.verbose = FALSE,
    margot.debug = TRUE
  )
  expect_false(getOption("margot.verbose"))
  expect_true(getOption("margot.debug"))
  
  # restore original options
  options(original_opts)
})

test_that("margot_option_defaults returns expected defaults", {
  defaults <- margot_option_defaults()
  
  expect_type(defaults, "list")
  expect_true(length(defaults) > 0)
  
  # check for expected default options
  expect_true("margot.verbose" %in% names(defaults))
  expect_true("margot.parallel" %in% names(defaults))
  
  # verify default values
  expect_false(defaults$margot.verbose)  # default should be FALSE
  expect_false(defaults$margot.parallel)  # default should be FALSE
})