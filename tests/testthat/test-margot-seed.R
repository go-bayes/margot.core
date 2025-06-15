test_that("with_seed preserves RNG state", {
  # set initial seed
  set.seed(123)
  x1 <- runif(1)
  
  # run with different seed
  result <- with_seed(456, {
    runif(5)
  })
  
  # check state restored
  x2 <- runif(1)
  
  # x2 should be what we would have gotten without the interruption
  set.seed(123)
  x1_check <- runif(1)
  x2_check <- runif(1)
  
  expect_equal(x2, x2_check)
})

test_that("with_seed returns expression value", {
  result <- with_seed(123, {
    x <- runif(5)
    mean(x)
  })
  
  # verify by running again
  set.seed(123)
  x <- runif(5)
  expected <- mean(x)
  
  expect_equal(result, expected)
})

test_that("preserve_seed and restore_seed_state work", {
  # set known state
  set.seed(999)
  state1 <- preserve_seed()
  
  # change state
  set.seed(111)
  x1 <- runif(1)
  
  # restore
  restore_seed_state(state1)
  
  # verify restoration
  state2 <- preserve_seed()
  expect_true(check_seed_state(state1, state2))
})

test_that("local_seed works within functions", {
  test_function <- function() {
    local_seed(123)
    runif(3)
  }
  
  # set different seed outside
  set.seed(999)
  x1 <- runif(1)
  
  # call function twice - should get same results
  result1 <- test_function()
  result2 <- test_function()
  expect_equal(result1, result2)
  
  # check outer state preserved
  x2 <- runif(1)
  set.seed(999)
  x1_check <- runif(1)
  x2_check <- runif(1)
  expect_equal(x2, x2_check)
})

test_that("generate_seed_sequence produces unique seeds", {
  seeds1 <- generate_seed_sequence(10, base_seed = 123)
  expect_length(seeds1, 10)
  expect_equal(length(unique(seeds1)), 10)  # all unique
  
  # reproducible
  seeds2 <- generate_seed_sequence(10, base_seed = 123)
  expect_equal(seeds1, seeds2)
  
  # different base gives different sequence
  seeds3 <- generate_seed_sequence(10, base_seed = 456)
  expect_false(any(seeds1 == seeds3))
})

test_that("with_seed handles errors properly", {
  # state should be restored even if error occurs
  set.seed(123)
  state1 <- preserve_seed()
  
  expect_error({
    with_seed(456, {
      x <- runif(5)
      stop("test error")
    })
  })
  
  state2 <- preserve_seed()
  expect_true(check_seed_state(state1, state2))
})

test_that("nested with_seed calls work", {
  result <- with_seed(123, {
    x1 <- runif(1)
    y <- with_seed(456, {
      runif(1)
    })
    x2 <- runif(1)
    c(x1, y, x2)
  })
  
  # verify inner seed doesn't affect outer
  set.seed(123)
  x1_check <- runif(1)
  x2_check <- runif(1)
  
  set.seed(456)
  y_check <- runif(1)
  
  expect_equal(result[1], x1_check)
  expect_equal(result[2], y_check)
  expect_equal(result[3], x2_check)
})

test_that("RNG kind is preserved", {
  # set specific RNG kind
  old_kind <- RNGkind("Mersenne-Twister")[1]
  state1 <- preserve_seed()
  
  # change kind inside with_seed
  with_seed(123, {
    # nothing needed
  }, .rng_kind = "L'Ecuyer-CMRG")
  
  # check kind restored
  expect_equal(RNGkind()[1], "Mersenne-Twister")
  
  # restore original
  RNGkind(old_kind)
})

test_that("uninitialized RNG is handled", {
  # remove .Random.seed if it exists
  if (exists(".Random.seed", envir = .GlobalEnv)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  
  # should still work
  state <- preserve_seed()
  expect_true(is.list(state))
  expect_true("seed" %in% names(state))
  
  # with_seed should work too
  result <- with_seed(123, runif(5))
  expect_length(result, 5)
})