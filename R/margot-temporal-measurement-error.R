#' Temporal Measurement Error S3 Class
#'
#' This class represents temporal measurement errors where the timing of
#' measurement differs from the timing of the actual exposure or outcome.
#' Common in longitudinal studies where exposures occur continuously but
#' are measured at discrete visits.
#'
#' @details
#' Temporal measurement errors can arise from:
#' - Retrospective reporting (e.g., "in the past month...")
#' - Prospective reporting (e.g., "do you plan to...")
#' - Measurement delays (exposure at t, measured at t+1)
#' - Aggregation over intervals (continuous exposure, periodic measurement)
#' - Recall bias affecting timing accuracy
#'
#' @examples
#' # Example 1: Retrospective measurement with 30-day recall window
#' tme_retro <- new_temporal_measurement_error(
#'   error_type = "retrospective",
#'   affected_variables = c("t1_a", "t2_a", "t3_a"),
#'   window_size = 30,  # days
#'   window_type = "fixed",
#'   aggregation = "any",  # any exposure in window → 1
#'   reference_point = "visit_date"
#' )
#'
#' # Example 2: Measurement delay - exposure measured at next visit
#' tme_delay <- new_temporal_measurement_error(
#'   error_type = "delayed",
#'   affected_variables = c("t1_a", "t2_a"),
#'   delay = 1,  # measured 1 time point later
#'   carry_forward = TRUE  # exposure value persists
#' )
#'
#' # Example 3: Anticipatory measurement
#' tme_anticip <- new_temporal_measurement_error(
#'   error_type = "anticipatory",
#'   affected_variables = "t2_a",
#'   lead_time = 7,  # recorded 7 days before actual start
#'   probability = 0.8  # 80% follow through with planned exposure
#' )
#'
#' @export
new_temporal_measurement_error <- function(error_type = c("retrospective", "delayed", 
                                                         "anticipatory", "aggregated"),
                                          affected_variables,
                                          timeline = NULL,
                                          # retrospective parameters
                                          window_size = NULL,
                                          window_type = c("fixed", "variable", "decaying"),
                                          aggregation = c("any", "mean", "max", "last", "mode"),
                                          recall_decay = NULL,
                                          # delay parameters
                                          delay = NULL,
                                          carry_forward = FALSE,
                                          # anticipatory parameters
                                          lead_time = NULL,
                                          probability = 1,
                                          # general parameters
                                          reference_point = c("visit_date", "previous_visit", 
                                                            "exposure_onset", "custom"),
                                          metadata = NULL) {
  
  error_type <- match.arg(error_type)
  window_type <- match.arg(window_type)
  aggregation <- match.arg(aggregation)
  reference_point <- match.arg(reference_point)
  
  # validate affected variables
  if (!is.character(affected_variables) || length(affected_variables) == 0) {
    stop("affected_variables must be a non-empty character vector")
  }
  
  # type-specific validation
  if (error_type == "retrospective") {
    if (is.null(window_size) || !is.numeric(window_size) || window_size <= 0) {
      stop("window_size must be positive for retrospective errors")
    }
    if (!is.null(recall_decay) && (!is.numeric(recall_decay) || recall_decay < 0 || recall_decay > 1)) {
      stop("recall_decay must be between 0 and 1")
    }
  } else if (error_type == "delayed") {
    if (is.null(delay) || !is.numeric(delay) || delay < 0) {
      stop("delay must be non-negative for delayed measurement")
    }
    if (!is.logical(carry_forward)) {
      stop("carry_forward must be logical")
    }
  } else if (error_type == "anticipatory") {
    if (is.null(lead_time) || !is.numeric(lead_time) || lead_time <= 0) {
      stop("lead_time must be positive for anticipatory measurement")
    }
    if (!is.numeric(probability) || probability < 0 || probability > 1) {
      stop("probability must be between 0 and 1")
    }
  }
  
  # construct object
  tme <- structure(
    list(
      error_type = error_type,
      affected_variables = affected_variables,
      timeline = timeline,
      # retrospective params
      window_size = window_size,
      window_type = window_type,
      aggregation = aggregation,
      recall_decay = recall_decay,
      # delay params
      delay = delay,
      carry_forward = carry_forward,
      # anticipatory params
      lead_time = lead_time,
      probability = probability,
      # general params
      reference_point = reference_point,
      metadata = metadata %||% list(
        created = Sys.time(),
        version = utils::packageVersion("margot.core")
      )
    ),
    class = c("margot_temporal_measurement_error", "margot_measurement_error")
  )
  
  validate_temporal_measurement_error(tme)
  tme
}

#' Validate temporal measurement error object
#'
#' @param x A margot_temporal_measurement_error object
#' @export
validate_temporal_measurement_error <- function(x) {
  if (!inherits(x, "margot_temporal_measurement_error")) {
    stop("Object must be of class 'margot_temporal_measurement_error'")
  }
  
  # check for required fields based on error type
  required_fields <- c("error_type", "affected_variables")
  
  if (x$error_type == "retrospective") {
    required_fields <- c(required_fields, "window_size", "window_type", "aggregation")
  } else if (x$error_type == "delayed") {
    required_fields <- c(required_fields, "delay", "carry_forward")
  } else if (x$error_type == "anticipatory") {
    required_fields <- c(required_fields, "lead_time", "probability")
  }
  
  missing <- setdiff(required_fields, names(x))
  if (length(missing) > 0) {
    stop("Missing required fields: ", paste(missing, collapse = ", "))
  }
  
  invisible(TRUE)
}

#' Apply temporal measurement error to data
#'
#' This function applies temporal measurement error to longitudinal data,
#' simulating the misalignment between when exposures occur and when
#' they are measured.
#'
#' @param data Data frame with temporal variables
#' @param tme Temporal measurement error object
#' @param true_data Optional data frame with true exposure timings
#' @return Data frame with temporal measurement errors applied
#' @export
apply_temporal_measurement_error <- function(data, tme, true_data = NULL) {
  validate_temporal_measurement_error(tme)
  
  # create copy to avoid modifying original
  result <- data
  
  # extract time structure from variable names
  time_vars <- grep("^t[0-9]+_", names(data), value = TRUE)
  if (length(time_vars) == 0) {
    warning("No temporal variables found in data")
    return(result)
  }
  
  # apply error based on type
  if (tme$error_type == "retrospective") {
    result <- apply_retrospective_error(result, tme, true_data)
  } else if (tme$error_type == "delayed") {
    result <- apply_delayed_measurement(result, tme)
  } else if (tme$error_type == "anticipatory") {
    result <- apply_anticipatory_measurement(result, tme)
  } else if (tme$error_type == "aggregated") {
    result <- apply_aggregated_measurement(result, tme, true_data)
  }
  
  result
}

#' Helper: Apply retrospective measurement error
#' @keywords internal
apply_retrospective_error <- function(data, tme, true_data = NULL) {
  # for each affected variable, aggregate exposure over recall window
  for (var in tme$affected_variables) {
    if (!(var %in% names(data))) {
      warning(sprintf("Variable %s not found in data", var))
      next
    }
    
    # extract time index
    time_match <- regmatches(var, regexpr("^t([0-9]+)_", var))
    if (length(time_match) == 0) next
    
    time_idx <- as.numeric(sub("^t([0-9]+)_.*", "\\1", var))
    var_stem <- sub("^t[0-9]+_", "", var)
    
    # for retrospective, we would aggregate over previous time points
    # this is simplified - full implementation would use timeline object
    if (time_idx > 0 && tme$aggregation == "any") {
      # check if any exposure in previous time period
      prev_var <- sprintf("t%d_%s", time_idx - 1, var_stem)
      if (prev_var %in% names(data)) {
        # simple "any exposure" aggregation
        data[[var]] <- pmax(data[[var]], data[[prev_var]], na.rm = TRUE)
      }
    }
    
    # apply recall decay if specified
    if (!is.null(tme$recall_decay) && is.numeric(data[[var]])) {
      noise <- rnorm(nrow(data), mean = 0, sd = tme$recall_decay)
      data[[var]] <- data[[var]] + noise
    }
  }
  
  data
}

#' Helper: Apply delayed measurement
#' @keywords internal  
apply_delayed_measurement <- function(data, tme) {
  # shift measurements by specified delay
  for (var in tme$affected_variables) {
    if (!(var %in% names(data))) next
    
    # extract time structure
    time_match <- regmatches(var, regexpr("^t([0-9]+)_", var))
    if (length(time_match) == 0) next
    
    time_idx <- as.numeric(sub("^t([0-9]+)_.*", "\\1", var))
    var_stem <- sub("^t[0-9]+_", "", var)
    
    # create delayed variable name
    delayed_time <- time_idx + tme$delay
    delayed_var <- sprintf("t%d_%s", delayed_time, var_stem)
    
    # if target time exists, shift the measurement
    if (delayed_var %in% names(data)) {
      if (tme$carry_forward) {
        # exposure persists until measured
        data[[delayed_var]] <- pmax(data[[var]], data[[delayed_var]], na.rm = TRUE)
      } else {
        # simple shift
        data[[delayed_var]] <- data[[var]]
      }
      # original measurement is now missing/uncertain
      data[[var]] <- NA
    }
  }
  
  data
}

#' Helper: Apply anticipatory measurement  
#' @keywords internal
apply_anticipatory_measurement <- function(data, tme) {
  # measurement occurs before actual exposure
  for (var in tme$affected_variables) {
    if (!(var %in% names(data))) next
    
    # for binary exposures, apply probability of follow-through
    if (all(data[[var]] %in% c(0, 1, NA))) {
      planned <- data[[var]]
      actual <- rbinom(length(planned), 1, 
                      prob = ifelse(planned == 1, tme$probability, 0))
      data[[var]] <- actual
    }
  }
  
  data
}

#' Print method for temporal measurement error
#' @export
print.margot_temporal_measurement_error <- function(x, ...) {
  cat("Temporal Measurement Error\n")
  cat("Type:", x$error_type, "\n")
  cat("Affected variables:", paste(x$affected_variables, collapse = ", "), "\n")
  
  if (x$error_type == "retrospective") {
    cat("Window size:", x$window_size, "\n")
    cat("Window type:", x$window_type, "\n")
    cat("Aggregation:", x$aggregation, "\n")
    if (!is.null(x$recall_decay)) {
      cat("Recall decay:", x$recall_decay, "\n")
    }
  } else if (x$error_type == "delayed") {
    cat("Delay:", x$delay, "time units\n")
    cat("Carry forward:", x$carry_forward, "\n")
  } else if (x$error_type == "anticipatory") {
    cat("Lead time:", x$lead_time, "\n")
    cat("Follow-through probability:", x$probability, "\n")
  }
  
  invisible(x)
}

# internal helper
`%||%` <- function(x, y) if (is.null(x)) y else x