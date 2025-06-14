#' Core Utilities for margot.core
#'
#' @description
#' This file contains core utility functions used across the margotsphere
#' ecosystem. These functions provide common functionality without heavy
#' dependencies.

# list of S3 classes in margot.core --------------------------

#' Get list of S3 classes in margot.core
#'
#' @return Character vector of S3 class names defined in margot.core
#' @export
.margot_core_classes <- function() {
  c(
    # shadow classes
    "margot_shadow",
    "measurement_error_shadow",
    "missing_data_shadow", 
    "censoring_shadow",
    "truncation_shadow",
    "selection_shadow",
    "positivity_shadow",
    "mode_effects_shadow",
    "coarsening_shadow",
    "item_missingness_shadow",
    "shadow_list",
    
    # scenario classes
    "margot_scenario",
    
    # panel data class
    "margot_panel"
  )
}

#' Get list of generic functions in margot.core
#'
#' @return Character vector of generic function names
#' @export
.margot_core_generics <- function() {
  c(
    # constructors
    "new_shadow",
    "new_scenario", 
    "new_shadow_list",
    "margot_panel",
    
    # validators
    "validate_shadow",
    "validate_scenario",
    
    # coercion
    "as_shadow",
    "as_scenario",
    
    # type checking
    "is_shadow",
    "is_scenario",
    "is_margot_panel"
  )
}

# option management ---------------------------------------------------

#' Standard margotverse options
#'
#' @description
#' Get or set standard options used across margotverse packages
#'
#' @param ... Named options to set
#' @param .list List of named options to set
#' @return If no arguments, returns list of current margot options
#' @export
#' @examples
#' # get all margot options
#' margot_options()
#' 
#' # set specific options
#' margot_options(verbose = TRUE, parallel = FALSE)
#' 
#' # reset to defaults
#' margot_options(.list = margot_option_defaults())
margot_options <- function(..., .list = NULL) {
  opts <- list(...)
  if (!is.null(.list)) {
    opts <- c(opts, .list)
  }
  
  if (length(opts) == 0) {
    # return current options
    current <- options()
    margot_opts <- current[grep("^margot\\.", names(current))]
    return(margot_opts)
  }
  
  # set options with margot prefix
  opts_to_set <- stats::setNames(
    opts,
    paste0("margot.", names(opts))
  )
  
  options(opts_to_set)
  invisible(opts_to_set)
}

#' Get default margotverse options
#'
#' @return List of default option values
#' @export
margot_option_defaults <- function() {
  list(
    verbose = FALSE,
    parallel = FALSE,
    seed = 123,
    cache_dir = tempdir(),
    progress = interactive(),
    validate = TRUE,
    max_memory = "4GB"
  )
}