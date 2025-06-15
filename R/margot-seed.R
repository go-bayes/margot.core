#' Seed Management for margot.core
#'
#' @description
#' Core functions for managing random seeds in a way that preserves
#' user state and ensures reproducibility. These functions form the
#' foundation for seed management across the margotsphere.
#'
#' @details
#' The functions in this file follow R best practices:
#' - Always restore the user's RNG state
#' - Use on.exit() for guaranteed cleanup
#' - No global side effects
#' - Simple function interface (no S3 classes needed)

#' Run Expression with Temporary Seed
#'
#' Temporarily sets a random seed for reproducible execution without
#' affecting the global RNG state. This is the recommended way to
#' ensure reproducibility in margot workflows.
#'
#' @param seed Integer seed value
#' @param expr Expression to evaluate
#' @param .rng_kind Optional RNG kind (e.g., "L'Ecuyer-CMRG" for parallel)
#' @param .envir Environment to evaluate expression in
#'
#' @return The result of evaluating expr
#' @export
#'
#' @examples
#' # Original RNG state is preserved
#' set.seed(123)
#' x1 <- runif(1)
#' 
#' # Run with different seed
#' y <- with_seed(456, runif(5))
#' 
#' # Original state restored
#' x2 <- runif(1)
#' identical(x1, x2)  # Would be FALSE without state preservation
with_seed <- function(seed, expr, .rng_kind = NULL, .envir = parent.frame()) {
  # capture expression
  expr <- substitute(expr)
  
  # preserve current state
  old_state <- preserve_seed()
  on.exit(restore_seed_state(old_state), add = TRUE)
  
  # set new RNG kind if specified
  if (!is.null(.rng_kind)) {
    RNGkind(.rng_kind)
  }
  
  # set seed and evaluate
  set.seed(seed)
  eval(expr, envir = .envir)
}

#' Preserve Current RNG State
#'
#' Captures the current RNG state for later restoration. This includes
#' both the seed and the RNG kind.
#'
#' @return A list containing the RNG state
#' @export
#'
#' @examples
#' # Save state before potentially disruptive operations
#' state <- preserve_seed()
#' set.seed(123)
#' # ... do work ...
#' restore_seed_state(state)
preserve_seed <- function() {
  # ensure RNG is initialized
  if (!exists(".Random.seed", envir = .GlobalEnv)) {
    set.seed(NULL)
  }
  
  list(
    seed = get(".Random.seed", envir = .GlobalEnv),
    kind = RNGkind()[1],
    normal_kind = RNGkind()[2],
    sample_kind = RNGkind()[3]
  )
}

#' Restore RNG State
#'
#' Restores a previously saved RNG state.
#'
#' @param state RNG state from preserve_seed()
#' @return NULL (invisibly)
#' @export
restore_seed_state <- function(state) {
  if (!is.list(state) || !all(c("seed", "kind") %in% names(state))) {
    stop("Invalid RNG state object")
  }
  
  # restore RNG kind
  RNGkind(state$kind, state$normal_kind, state$sample_kind)
  
  # restore seed
  assign(".Random.seed", state$seed, envir = .GlobalEnv)
  
  invisible(NULL)
}

#' Local Seed Setting
#'
#' Sets a seed that will be automatically restored when the calling
#' function exits. Useful inside functions that need reproducibility.
#'
#' @param seed Integer seed value
#' @param .local_envir Environment to attach exit handler to
#' @param .rng_kind Optional RNG kind
#'
#' @return The seed value (invisibly)
#' @export
#'
#' @examples
#' my_function <- function() {
#'   local_seed(123)
#'   runif(5)  # Always gives same values
#' }
#' 
#' # RNG state restored after function exits
#' my_function()
#' runif(1)  # Not affected by seed inside function
local_seed <- function(seed, .local_envir = parent.frame(), .rng_kind = NULL) {
  # preserve state in calling environment
  old_state <- preserve_seed()
  
  # need to inject the old_state into parent environment
  .local_envir$`.margot_old_state` <- old_state
  
  # use do.call to properly handle the on.exit in parent frame
  do.call(
    on.exit,
    list(quote(restore_seed_state(`.margot_old_state`)), add = TRUE),
    envir = .local_envir
  )
  
  # set new RNG kind if specified
  if (!is.null(.rng_kind)) {
    RNGkind(.rng_kind)
  }
  
  # set seed
  set.seed(seed)
  invisible(seed)
}

#' Check Seed State
#'
#' Utility to check if RNG state has been modified. Useful for
#' testing that functions properly restore state.
#'
#' @param state1 First RNG state (from preserve_seed)
#' @param state2 Second RNG state (from preserve_seed)
#'
#' @return Logical indicating if states are identical
#' @export
#'
#' @examples
#' state1 <- preserve_seed()
#' set.seed(123)
#' state2 <- preserve_seed()
#' check_seed_state(state1, state2)  # FALSE
#' 
#' restore_seed_state(state1)
#' state3 <- preserve_seed()
#' check_seed_state(state1, state3)  # TRUE
check_seed_state <- function(state1, state2) {
  identical(state1$seed, state2$seed) &&
    identical(state1$kind, state2$kind) &&
    identical(state1$normal_kind, state2$normal_kind) &&
    identical(state1$sample_kind, state2$sample_kind)
}

#' Generate Seed Sequence
#'
#' Creates a sequence of well-separated seeds for independent runs.
#' This is useful for Monte Carlo simulations where each replication
#' needs an independent seed.
#'
#' @param n Number of seeds to generate
#' @param base_seed Starting seed (NULL for random)
#' @param method Method for generating seeds ("spaced" or "digest")
#'
#' @return Integer vector of seeds
#' @export
#'
#' @examples
#' # Generate 10 seeds for independent simulations
#' seeds <- generate_seed_sequence(10, base_seed = 123)
#' 
#' # Use in parallel computations
#' results <- lapply(seeds, function(s) {
#'   with_seed(s, {
#'     # simulation code here
#'   })
#' })
generate_seed_sequence <- function(n, base_seed = NULL, method = c("spaced", "digest")) {
  method <- match.arg(method)
  
  if (is.null(base_seed)) {
    base_seed <- as.integer(Sys.time())
  }
  
  if (method == "spaced") {
    # use spacing method for speed
    with_seed(base_seed, {
      seeds <- integer(n)
      for (i in seq_len(n)) {
        # create seed based on position and some random values
        # this ensures uniqueness and avoids overflow
        seeds[i] <- as.integer((base_seed + i * 1000 + sample.int(100000, 1)) %% 2147483647)
      }
      seeds
    })
  } else {
    # use digest method for guaranteed independence
    if (!requireNamespace("digest", quietly = TRUE)) {
      stop("digest package required for method='digest'")
    }
    seeds <- integer(n)
    for (i in seq_len(n)) {
      # create unique string and hash it
      unique_string <- paste(base_seed, i, sep = "_")
      hash <- digest::digest(unique_string, algo = "xxhash32")
      # ensure positive integer in valid range
      seeds[i] <- abs(strtoi(substr(hash, 1, 8), base = 16L)) %% 2147483647
    }
    seeds
  }
}