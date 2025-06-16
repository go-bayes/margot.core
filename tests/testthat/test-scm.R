# Test SCM functionality
library(testthat)

test_that("new_scm creates valid SCM objects", {
  # simple scm
  nodes <- c("X", "Y")
  parents <- list(X = character(), Y = "X")
  exog <- c("U_X", "U_Y")
  
  scm <- new_scm(nodes, parents, exogenous = exog)
  
  expect_s3_class(scm, "scm")
  expect_equal(scm$nodes, nodes)
  expect_equal(scm$pa, parents)
  expect_equal(scm$U, exog)
  expect_null(scm$f)
})

test_that("new_scm validates inputs", {
  # invalid nodes
  expect_error(new_scm(NULL, list()), "nodes must be a non-empty character vector")
  expect_error(new_scm(character(), list()), "nodes must be a non-empty character vector")
  expect_error(new_scm(1:3, list()), "nodes must be a non-empty character vector")
  
  # invalid parents
  expect_error(new_scm("X", "Y"), "parents must be a list")
  
  # mismatched names
  expect_error(
    new_scm(c("X", "Y"), list(X = character())),
    "names of parents list must match nodes exactly"
  )
  
  # invalid exogenous
  expect_error(new_scm("X", list(X = character()), exogenous = 1:3), 
               "exogenous must be a character vector")
})

test_that("scm_from_df works with LMTP naming", {
  # create test data
  df <- data.frame(
    t0_b1 = rnorm(100),
    t0_b2 = rnorm(100),
    t0_l = rnorm(100),
    t0_a = rbinom(100, 1, 0.5),
    t1_l = rnorm(100),
    t1_a = rbinom(100, 1, 0.5),
    t1_y = rnorm(100),
    t2_l = rnorm(100),
    t2_a = rbinom(100, 1, 0.5),
    t2_y = rnorm(100)
  )
  
  scm <- scm_from_df(df)
  
  expect_s3_class(scm, "scm")
  expect_equal(length(scm$nodes), 10)
  
  # check baseline nodes have no parents
  expect_equal(scm$pa$t0_b1, character())
  expect_equal(scm$pa$t0_b2, character())
  expect_equal(scm$pa$t0_l, character())
  expect_equal(scm$pa$t0_a, character())
  
  # check t1_l depends on all t0 variables
  expect_true(all(c("t0_b1", "t0_b2", "t0_l", "t0_a") %in% scm$pa$t1_l))
  
  # check t1_a depends on t0 variables and t1_l
  expect_true(all(c("t0_b1", "t0_b2", "t0_l", "t0_a", "t1_l") %in% scm$pa$t1_a))
  
  # check t1_y depends on all b, l, a up to t1
  expected_parents_y1 <- c("t0_b1", "t0_b2", "t0_l", "t0_a", "t1_l", "t1_a")
  expect_true(all(expected_parents_y1 %in% scm$pa$t1_y))
  
  # check exogenous variables
  expect_equal(length(scm$U), 10)
  expect_true(all(grepl("^u_", scm$U)))
})

test_that("scm_from_df validates minimum waves", {
  # too few waves
  df_short <- data.frame(
    t0_b = rnorm(10),
    t1_y = rnorm(10)
  )
  
  expect_error(scm_from_df(df_short), "need at least 3 time points; found 2")
  
  # can override minimum
  scm <- scm_from_df(df_short, K_min = 2)
  expect_s3_class(scm, "scm")
})

test_that("scm_from_df handles missing blocks", {
  # data without some blocks
  df <- data.frame(
    t0_b = rnorm(100),
    t1_a = rbinom(100, 1, 0.5),
    t2_y = rnorm(100)
  )
  
  scm <- scm_from_df(df)
  
  expect_equal(length(scm$nodes), 3)
  expect_equal(scm$pa$t0_b, character())
  expect_true("t0_b" %in% scm$pa$t1_a)
  expect_true(all(c("t0_b", "t1_a") %in% scm$pa$t2_y))
})

test_that("scm_from_spec parses equations correctly", {
  # simple spec
  spec <- "X ~ U1; Y ~ X + U2"
  scm <- scm_from_spec(spec)
  
  expect_s3_class(scm, "scm")
  expect_equal(scm$nodes, c("X", "Y"))
  expect_equal(scm$pa$X, "U1")
  expect_equal(scm$pa$Y, c("X", "U2"))
  expect_equal(sort(scm$U), c("U1", "U2"))
})

test_that("scm_from_spec handles complex equations", {
  # more complex spec
  spec <- "t0_b1 ~ U1 + U2; t0_b2 ~ U1; t1_l ~ t0_b1 + t0_b2; t1_a ~ t1_l + U3; t1_y ~ t1_a + t1_l"
  scm <- scm_from_spec(spec)
  
  expect_equal(length(scm$nodes), 5)
  expect_equal(scm$pa$t0_b1, c("U1", "U2"))
  expect_equal(scm$pa$t1_l, c("t0_b1", "t0_b2"))
  expect_equal(scm$pa$t1_y, c("t1_a", "t1_l"))
  expect_equal(sort(scm$U), c("U1", "U2", "U3"))
})

test_that("scm_from_spec handles empty parents", {
  spec <- "X ~ ; Y ~ X"
  scm <- scm_from_spec(spec)
  
  expect_equal(scm$pa$X, character())
  expect_equal(scm$pa$Y, "X")
})

test_that("scm_from_spec validates input", {
  # not a single string
  expect_error(scm_from_spec(c("X ~ U1", "Y ~ X")), 
               "spec must be a single character string")
  
  # missing ~
  expect_error(scm_from_spec("X = U1; Y ~ X"), 
               "each equation needs a '~'")
})

test_that("print.scm produces correct output", {
  scm <- scm_from_spec("X ~ U1; Y ~ X + U2")
  
  output <- capture.output(print(scm))
  
  expect_true(any(grepl("structural causal model", output)))
  expect_true(any(grepl("X := f_X\\(U1, u_X\\)", output)))
  expect_true(any(grepl("Y := f_Y\\(X, U2, u_Y\\)", output)))
})

test_that("print.scm handles nodes with no parents", {
  scm <- new_scm("X", list(X = character()), exogenous = "U_X")
  
  output <- capture.output(print(scm))
  expect_true(any(grepl("X := f_X\\(u_X\\)", output)))
})

test_that("print.scm wraps long lines", {
  # create scm with many parents
  nodes <- c("X", "Y")
  parents <- list(
    X = character(),
    Y = paste0("V", 1:20)  # 20 parents
  )
  scm <- new_scm(nodes, parents)
  
  output <- capture.output(print(scm, width = 40))
  
  # should have wrapped lines
  expect_true(length(output) > 4)  # header + separator + 2 nodes
})

test_that("as_latex.scm produces valid LaTeX", {
  scm <- scm_from_spec("X ~ U1; Y ~ X + U2")
  
  latex <- as_latex(scm)
  
  expect_true(grepl("\\\\begin\\{align\\*\\}", latex))
  expect_true(grepl("\\\\end\\{align\\*\\}", latex))
  expect_true(grepl("X &:= f_\\{X\\}", latex))
  expect_true(grepl("Y &:= f_\\{Y\\}", latex))
  expect_true(grepl("X, U2,", latex))  # check parent formatting
})

test_that("as_latex.scm handles empty parents", {
  scm <- new_scm("X", list(X = character()))
  
  latex <- as_latex(scm)
  expect_true(grepl("X &:= f_\\{X\\}\\(u_\\{X\\}\\)", latex))
})

test_that("is_scm works correctly", {
  scm <- new_scm("X", list(X = character()))
  
  expect_true(is_scm(scm))
  expect_false(is_scm(list(nodes = "X")))
  expect_false(is_scm("not an scm"))
  expect_false(is_scm(NULL))
})

test_that("summary.scm provides useful information", {
  df <- data.frame(
    t0_b1 = rnorm(10),
    t0_b2 = rnorm(10),
    t0_l = rnorm(10),
    t1_l = rnorm(10),
    t1_a = rbinom(10, 1, 0.5),
    t1_y = rnorm(10),
    t2_l = rnorm(10),
    t2_a = rbinom(10, 1, 0.5),
    t2_y = rnorm(10)
  )
  
  scm <- scm_from_df(df)
  
  output <- capture.output(summary(scm))
  
  expect_true(any(grepl("total nodes: 9", output)))
  expect_true(any(grepl("node types:", output)))
  expect_true(any(grepl("dependency structure:", output)))
  
  # check returned values
  res <- invisible(summary(scm))
  expect_equal(res$n_nodes, 9)
  expect_true(res$avg_in_degree > 0)
})

test_that("integration: scm_from_df and scm_from_spec produce compatible objects", {
  # create equivalent scms
  df <- data.frame(
    t0_b = rnorm(10),
    t1_a = rbinom(10, 1, 0.5),
    t1_y = rnorm(10),
    t2_y = rnorm(10)  # need t2 to meet K_min = 3
  )
  
  scm1 <- scm_from_df(df)
  
  # manually specify the same structure
  spec <- "t0_b ~ ; t1_a ~ t0_b; t1_y ~ t0_b + t1_a; t2_y ~ t0_b + t1_a + t1_y"
  scm2 <- scm_from_spec(spec)
  
  # both should be valid scms
  expect_true(is_scm(scm1))
  expect_true(is_scm(scm2))
  
  # same nodes
  expect_equal(sort(scm1$nodes), sort(scm2$nodes))
  
  # same parent structure
  expect_equal(scm1$pa$t0_b, scm2$pa$t0_b)
  expect_equal(scm1$pa$t1_a, scm2$pa$t1_a)
  expect_equal(sort(scm1$pa$t1_y), sort(scm2$pa$t1_y))
})

test_that("scm_from_df handles waves with gaps", {
  # waves t0, t2, t4 (missing t1, t3) - but still 3 waves total
  df <- data.frame(
    t0_b = rnorm(10),
    t2_l = rnorm(10),
    t2_a = rbinom(10, 1, 0.5),
    t4_y = rnorm(10)
  )
  
  scm <- scm_from_df(df)
  
  expect_equal(length(scm$nodes), 4)
  # t2_l should depend on t0_b
  expect_true("t0_b" %in% scm$pa$t2_l)
  # t4_y should depend on all prior variables
  expect_true(all(c("t0_b", "t2_l", "t2_a") %in% scm$pa$t4_y))
})

test_that("scm objects can be subset", {
  scm <- scm_from_spec("X ~ U1; Y ~ X + U2; Z ~ Y + U3")
  
  # extract subgraph
  sub_nodes <- c("X", "Y")
  sub_pa <- scm$pa[sub_nodes]
  sub_scm <- new_scm(sub_nodes, sub_pa, exogenous = c("U1", "U2"))
  
  expect_equal(length(sub_scm$nodes), 2)
  expect_equal(sub_scm$pa$Y, c("X", "U2"))
})