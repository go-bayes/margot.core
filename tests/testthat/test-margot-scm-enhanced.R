# Test enhanced SCM functionality
library(testthat)

test_that("margot_scm_new creates valid objects with mandatory exogenous", {
  nodes <- c("X", "Y", "Z")
  parents <- list(X = character(), Y = "X", Z = c("X", "Y"))
  exogenous <- list(X = "U_X", Y = "U_Y", Z = "U_Z")
  
  scm <- margot_scm_new(nodes, parents, exogenous)
  
  expect_s3_class(scm, "margot_scm")
  expect_s3_class(scm, "scm")  # also inherits from base scm
  expect_equal(scm$nodes, nodes)
  expect_equal(scm$pa, parents)
  expect_equal(scm$U, exogenous)
  expect_equal(length(scm$k), length(nodes))
  expect_true(all(is.na(scm$k)))  # no time indices by default
})

test_that("margot_scm_new enforces mandatory exogenous mapping", {
  nodes <- c("X", "Y")
  parents <- list(X = character(), Y = "X")
  
  # missing exogenous mapping should fail
  expect_error(
    margot_scm_new(nodes, parents, list(X = "U_X")),
    "every node must have an exogenous error term"
  )
  
  # correct mapping should work
  exogenous <- list(X = "U_X", Y = "U_Y")
  scm <- margot_scm_new(nodes, parents, exogenous)
  expect_equal(scm$U$X, "U_X")
  expect_equal(scm$U$Y, "U_Y")
})

test_that("margot_scm_new handles time indices", {
  nodes <- c("t0_b", "t1_a", "t2_y")
  parents <- list(t0_b = character(), t1_a = "t0_b", t2_y = c("t0_b", "t1_a"))
  exogenous <- list(t0_b = "U_b", t1_a = "U_a", t2_y = "U_y")
  time_index <- c(1, 2, 3)
  
  scm <- margot_scm_new(nodes, parents, exogenous, time_index = time_index)
  
  expect_equal(scm$k, time_index)
})

test_that("margot_scm_from_df creates SCM with automatic exogenous", {
  df <- data.frame(
    t0_b = rnorm(100),
    t0_l = rnorm(100),
    t1_l = rnorm(100),
    t1_a = rbinom(100, 1, 0.5),
    t2_y = rnorm(100)
  )
  
  scm <- margot_scm_from_df(df)
  
  expect_s3_class(scm, "margot_scm")
  expect_equal(length(scm$nodes), 5)
  
  # check automatic exogenous mapping
  expect_equal(scm$U$t0_b, "U_t0_b")
  expect_equal(scm$U$t1_a, "U_t1_a")
  expect_equal(scm$U$t2_y, "U_t2_y")
  
  # check time indices
  expect_equal(scm$k[scm$nodes == "t0_b"], 1)
  expect_equal(scm$k[scm$nodes == "t1_a"], 2)
  expect_equal(scm$k[scm$nodes == "t2_y"], 3)
})

test_that("margot_scm_from_dsl parses timeline DSL correctly", {
  dsl <- "
  U  = U_b, U_l, U_a, U_y
  k1 = t0_b ~ U_b
  k2 = t1_l ~ t0_b + U_l
  k3 = t2_a ~ t0_b + t1_l + U_a
  k4 = t3_y ~ t2_a + t1_l + U_y
  "
  
  scm <- margot_scm_from_dsl(dsl)
  
  expect_s3_class(scm, "margot_scm")
  expect_equal(scm$nodes, c("t0_b", "t1_l", "t2_a", "t3_y"))
  
  # check parents (U terms should be separate, not in parents)
  expect_equal(scm$pa$t0_b, character())  # no endogenous parents
  expect_equal(scm$pa$t1_l, "t0_b")
  expect_equal(scm$pa$t2_a, c("t0_b", "t1_l"))
  
  # check exogenous mapping
  expect_equal(scm$U$t0_b, "U_b")
  expect_equal(scm$U$t1_l, "U_l")
  expect_equal(scm$U$t2_a, "U_a")
  expect_equal(scm$U$t3_y, "U_y")
  
  # check time indices
  expect_equal(scm$k, c(1, 2, 3, 4))
})

test_that("margot_scm_from_dsl validates DSL syntax", {
  # missing U declaration
  expect_error(
    margot_scm_from_dsl("k1 = x ~ y"),
    "exactly one line starting with 'U =' is required"
  )
  
  # invalid k-line format
  expect_error(
    margot_scm_from_dsl("U = U_x\nx = y"),
    "lines must start with k#"
  )
  
  # missing exogenous in U list
  expect_error(
    margot_scm_from_dsl("U = U_x\nk1 = y ~ U_y"),
    "no exogenous error listed in U = ... for node y"
  )
})

test_that("margot_scm_add_shift adds interventions correctly", {
  scm <- margot_scm_from_dsl("
  U = U_a, U_y
  k1 = a ~ U_a
  k2 = y ~ a + U_y
  ")
  
  # add shift intervention
  shift_fun <- function(a, ...) pmax(a, 0)
  scm2 <- margot_scm_add_shift(scm, "a", "positive", shift_fun)
  
  expect_true("a" %in% names(scm2$shift))
  expect_true("positive" %in% names(scm2$shift$a))
  expect_identical(scm2$shift$a$positive, shift_fun)
})

test_that("margot_scm_add_shift validates inputs", {
  scm <- margot_scm_new("X", list(X = character()), list(X = "U_X"))
  
  # invalid node
  expect_error(
    margot_scm_add_shift(scm, "Y", "shift1", function(x) x),
    "node 'Y' not found"
  )
  
  # non-function shift
  expect_error(
    margot_scm_add_shift(scm, "X", "shift1", "not a function"),
    "fun must be a function"
  )
})

test_that("multiple shifts can be added to same node", {
  scm <- margot_scm_new("A", list(A = character()), list(A = "U_A"))
  
  scm <- margot_scm_add_shift(scm, "A", "increase", function(a) a + 1)
  scm <- margot_scm_add_shift(scm, "A", "decrease", function(a) a - 1)
  scm <- margot_scm_add_shift(scm, "A", "double", function(a) a * 2)
  
  expect_equal(length(scm$shift$A), 3)
  expect_equal(names(scm$shift$A), c("increase", "decrease", "double"))
})

test_that("print.margot_scm shows all components", {
  dsl <- "
  U = U_a, U_y
  k1 = a ~ U_a
  k2 = y ~ a + U_y
  "
  scm <- margot_scm_from_dsl(dsl)
  scm <- margot_scm_add_shift(scm, "a", "increase", function(a) a + 1)
  
  output <- capture.output(print(scm))
  
  # check structure
  expect_true(any(grepl("structural causal model", output)))
  # check for key components with flexible spacing
  expect_true(any(grepl("a := f_a\\(U_a\\)", output)))
  expect_true(any(grepl("y := f_y\\(a, U_y\\)", output)))
  expect_true(any(grepl("\\[k1\\]", output)))
  expect_true(any(grepl("\\[k2\\]", output)))
  
  # check shift section
  expect_true(any(grepl("shift interventions:", output)))
  expect_true(any(grepl("a\\s*:\\s*increase", output)))
})

test_that("margot_scm_as_latex produces correct LaTeX", {
  scm <- margot_scm_new(
    nodes = c("X", "Y"),
    parents = list(X = character(), Y = "X"),
    exogenous = list(X = "U_X", Y = "U_Y")
  )
  
  latex <- margot_scm_as_latex(scm)
  
  expect_true(grepl("\\\\begin\\{align\\*\\}", latex))
  expect_true(grepl("\\\\end\\{align\\*\\}", latex))
  expect_true(grepl("X &:= f_\\{X\\}\\(U_X\\)", latex))
  expect_true(grepl("Y &:= f_\\{Y\\}\\(X, U_Y\\)", latex))
})

test_that("margot_scm_get_shifts retrieves shift functions", {
  scm <- margot_scm_new("A", list(A = character()), list(A = "U_A"))
  shift1 <- function(a) a + 1
  shift2 <- function(a) a * 2
  
  scm <- margot_scm_add_shift(scm, "A", "s1", shift1)
  scm <- margot_scm_add_shift(scm, "A", "s2", shift2)
  
  # get all shifts
  all_shifts <- margot_scm_get_shifts(scm)
  expect_equal(length(all_shifts), 1)
  expect_equal(names(all_shifts), "A")
  
  # get shifts for specific node
  a_shifts <- margot_scm_get_shifts(scm, "A")
  expect_equal(length(a_shifts), 2)
  expect_identical(a_shifts$s1, shift1)
  expect_identical(a_shifts$s2, shift2)
})

test_that("margot_scm_time_indices extracts time information", {
  scm <- margot_scm_from_dsl("
  U = U_x, U_y
  k5 = x ~ U_x
  k10 = y ~ x + U_y
  ")
  
  indices <- margot_scm_time_indices(scm)
  expect_equal(indices, c(x = 5, y = 10))
})

test_that("is_margot_scm correctly identifies objects", {
  scm1 <- margot_scm_new("X", list(X = character()), list(X = "U_X"))
  scm2 <- new_scm("X", list(X = character()))  # old style
  
  expect_true(is_margot_scm(scm1))
  expect_false(is_margot_scm(scm2))
  expect_false(is_margot_scm(list()))
})

test_that("complex DSL with multiple blocks per time", {
  dsl <- "
  U = U_b1, U_b2, U_l, U_a, U_y1, U_y2
  k1 = b1 ~ U_b1
  k1 = b2 ~ U_b2
  k2 = l ~ b1 + b2 + U_l
  k2 = a ~ b1 + l + U_a
  k3 = y1 ~ a + l + U_y1
  k3 = y2 ~ a + b2 + U_y2
  "
  
  scm <- margot_scm_from_dsl(dsl)
  
  expect_equal(length(scm$nodes), 6)
  expect_equal(sum(scm$k == 1), 2)  # two at k=1
  expect_equal(sum(scm$k == 2), 2)  # two at k=2
  expect_equal(sum(scm$k == 3), 2)  # two at k=3
})

test_that("integration with lmtp workflow", {
  # create SCM with shift
  scm <- margot_scm_from_dsl("
  U = U_l, U_a, U_y
  k1 = l ~ U_l
  k2 = a ~ l + U_a
  k3 = y ~ a + l + U_y
  ")
  
  # add realistic shift
  scm <- margot_scm_add_shift(
    scm, 
    node = "a",
    name = "increase_low",
    fun = function(a, data, time) {
      # increase treatment for those with low l
      ifelse(data[["l"]] < median(data[["l"]]), pmin(a + 0.5, 1), a)
    }
  )
  
  shifts <- margot_scm_get_shifts(scm, "a")
  expect_true("increase_low" %in% names(shifts))
})