#' Create a margot timeline object
#'
#' @description
#' Creates a timeline object representing the true causal process across time points.
#' The timeline distinguishes between causal time (when events actually occur) and
#' observation time (when measurements are taken), providing infrastructure for
#' temporal analysis in causal inference.
#'
#' @param n_time_points Integer number of time points in the causal process
#' @param start_time Starting time point (default = 0)
#' @param time_spacing Numeric spacing between time points (default = 1)
#' @param observation_schedule Optional character string or numeric vector specifying
#'   when observations occur. Can be "aligned" (default), "irregular", or a numeric
#'   vector of specific observation times
#' @param metadata Optional list of additional timeline metadata
#'
#' @return An object of class "margot_timeline" containing:
#'   - time_points: Numeric vector of causal time points
#'   - observation_schedule: When observations occur
#'   - n_time_points: Number of time points
#'   - metadata: Additional timeline information
#'
#' @export
#' @examples
#' # create simple timeline with 3 time points
#' tl <- new_timeline(n_time_points = 3)
#' print(tl)
#' 
#' # create timeline with irregular observations
#' tl_irregular <- new_timeline(
#'   n_time_points = 5,
#'   observation_schedule = c(0, 1, 3, 5)
#' )
new_timeline <- function(n_time_points,
                        start_time = 0,
                        time_spacing = 1,
                        observation_schedule = "aligned",
                        metadata = NULL) {
  
  # validate inputs
  if (!is.numeric(n_time_points) || length(n_time_points) != 1 || 
      n_time_points < 1 || n_time_points != round(n_time_points)) {
    stop("n_time_points must be a single positive integer", call. = FALSE)
  }
  
  if (!is.numeric(start_time) || length(start_time) != 1) {
    stop("start_time must be a single numeric value", call. = FALSE)
  }
  
  if (!is.numeric(time_spacing) || length(time_spacing) != 1 || time_spacing <= 0) {
    stop("time_spacing must be a single positive numeric value", call. = FALSE)
  }
  
  # create time points
  time_points <- start_time + (0:(n_time_points - 1)) * time_spacing
  
  # handle observation schedule
  if (is.character(observation_schedule)) {
    if (observation_schedule == "aligned") {
      obs_schedule <- time_points
    } else if (observation_schedule == "irregular") {
      # placeholder for future implementation
      stop("Irregular observation schedules not yet implemented", call. = FALSE)
    } else {
      stop("Invalid observation_schedule. Must be 'aligned', 'irregular', or numeric vector", 
           call. = FALSE)
    }
  } else if (is.numeric(observation_schedule)) {
    obs_schedule <- observation_schedule
  } else {
    stop("observation_schedule must be character or numeric", call. = FALSE)
  }
  
  # create timeline object
  timeline <- structure(
    list(
      time_points = time_points,
      observation_schedule = obs_schedule,
      n_time_points = n_time_points,
      start_time = start_time,
      time_spacing = time_spacing,
      metadata = metadata %||% list(
        created = Sys.time(),
        version = utils::packageVersion("margot.core")
      )
    ),
    class = "margot_timeline"
  )
  
  # validate
  validate_timeline(timeline)
  
  timeline
}

#' Validate a timeline object
#'
#' @param x A timeline object to validate
#' @param ... Additional arguments (unused)
#'
#' @return The validated timeline object (invisibly)
#' @export
validate_timeline <- function(x, ...) {
  if (!inherits(x, "margot_timeline")) {
    stop("Object is not a margot_timeline", call. = FALSE)
  }
  
  # check required components
  required_names <- c("time_points", "observation_schedule", "n_time_points")
  missing_names <- setdiff(required_names, names(x))
  if (length(missing_names) > 0) {
    stop("Timeline missing required components: ", 
         paste(missing_names, collapse = ", "), call. = FALSE)
  }
  
  # validate time points
  if (!is.numeric(x$time_points)) {
    stop("time_points must be numeric", call. = FALSE)
  }
  
  if (length(x$time_points) != x$n_time_points) {
    stop("Length of time_points does not match n_time_points", call. = FALSE)
  }
  
  # check time points are ordered
  if (is.unsorted(x$time_points)) {
    stop("time_points must be in ascending order", call. = FALSE)
  }
  
  # validate observation schedule
  if (!is.numeric(x$observation_schedule)) {
    stop("observation_schedule must be numeric", call. = FALSE)
  }
  
  # check observations are within timeline bounds
  obs_range <- range(x$observation_schedule)
  time_range <- range(x$time_points)
  if (obs_range[1] < time_range[1] || obs_range[2] > time_range[2]) {
    warning("Some observations fall outside the timeline bounds", call. = FALSE)
  }
  
  invisible(x)
}

#' Print method for margot_timeline objects
#'
#' @param x A margot_timeline object
#' @param ... Additional arguments passed to print
#' @return The object invisibly
#' @export
print.margot_timeline <- function(x, ...) {
  cat("<margot_timeline>\n")
  cat("  Time points:", x$n_time_points, "\n")
  cat("  Time range: [", x$time_points[1], ", ", 
      x$time_points[length(x$time_points)], "]\n", sep = "")
  cat("  Time spacing:", x$time_spacing, "\n")
  
  # check observation alignment
  if (identical(x$time_points, x$observation_schedule)) {
    cat("  Observations: aligned with time points\n")
  } else {
    cat("  Observations:", length(x$observation_schedule), "at times",
        paste(utils::head(x$observation_schedule, 3), collapse = ", "))
    if (length(x$observation_schedule) > 3) cat("...")
    cat("\n")
  }
  
  invisible(x)
}

#' Validate temporal events against requirements
#'
#' @description
#' Checks that a set of temporal events meets specified requirements
#' such as proper ordering, required time points, or minimum spacing.
#'
#' @param events Numeric vector of event times
#' @param requirements List of requirements to check, which may include:
#'   - min_spacing: Minimum spacing between consecutive events
#'   - required_times: Specific times that must be included
#'   - max_duration: Maximum allowed duration from first to last event
#'
#' @return Logical indicating whether all requirements are met. If FALSE,
#'   attributes contain details about which requirements failed.
#' @export
#' @examples
#' # check events have minimum spacing of 7 days
#' events <- c(0, 7, 14, 21)
#' validate_temporal_events(events, list(min_spacing = 7))
#' 
#' # check multiple requirements
#' validate_temporal_events(
#'   events,
#'   list(min_spacing = 5, required_times = c(0, 21))
#' )
validate_temporal_events <- function(events, requirements = list()) {
  if (!is.numeric(events)) {
    stop("events must be numeric", call. = FALSE)
  }
  
  if (!is.list(requirements)) {
    stop("requirements must be a list", call. = FALSE)
  }
  
  # track validation results
  valid <- TRUE
  failures <- list()
  
  # check minimum spacing
  if (!is.null(requirements$min_spacing)) {
    if (length(events) > 1) {
      spacings <- diff(sort(events))
      if (any(spacings < requirements$min_spacing)) {
        valid <- FALSE
        failures$min_spacing <- paste(
          "Some events are closer than minimum spacing of",
          requirements$min_spacing
        )
      }
    }
  }
  
  # check required times
  if (!is.null(requirements$required_times)) {
    missing_times <- setdiff(requirements$required_times, events)
    if (length(missing_times) > 0) {
      valid <- FALSE
      failures$required_times <- paste(
        "Missing required times:",
        paste(missing_times, collapse = ", ")
      )
    }
  }
  
  # check maximum duration
  if (!is.null(requirements$max_duration)) {
    duration <- max(events) - min(events)
    if (duration > requirements$max_duration) {
      valid <- FALSE
      failures$max_duration <- paste(
        "Duration", duration, "exceeds maximum of",
        requirements$max_duration
      )
    }
  }
  
  # return result with details
  if (!valid) {
    attr(valid, "failures") <- failures
  }
  
  valid
}

#' Count time points in data
#'
#' @description
#' Counts the number of unique time points in a dataset, with options
#' for handling different data structures and time representations.
#'
#' @param data A data frame, matrix, or vector containing temporal data
#' @param time_col Character string naming the time column (for data frames)
#' @param na.rm Logical, whether to remove NA values before counting
#'
#' @return Integer count of unique time points
#' @export
#' @examples
#' # count time points in a vector
#' count_time_points(c(0, 1, 1, 2, 2, 2))
#' 
#' # count time points in a data frame
#' df <- data.frame(
#'   id = rep(1:3, each = 3),
#'   time = rep(0:2, 3),
#'   value = rnorm(9)
#' )
#' count_time_points(df, time_col = "time")
count_time_points <- function(data, time_col = NULL, na.rm = TRUE) {
  # extract time values based on data type
  if (is.data.frame(data)) {
    if (is.null(time_col)) {
      stop("time_col must be specified for data frames", call. = FALSE)
    }
    if (!time_col %in% names(data)) {
      stop("Column '", time_col, "' not found in data", call. = FALSE)
    }
    time_values <- data[[time_col]]
  } else if (is.matrix(data)) {
    if (!is.null(time_col)) {
      if (!time_col %in% colnames(data)) {
        stop("Column '", time_col, "' not found in matrix", call. = FALSE)
      }
      time_values <- data[, time_col]
    } else {
      stop("time_col must be specified for matrices", call. = FALSE)
    }
  } else if (is.numeric(data)) {
    time_values <- data
  } else {
    stop("data must be a data frame, matrix, or numeric vector", call. = FALSE)
  }
  
  # remove NAs if requested
  if (na.rm) {
    time_values <- time_values[!is.na(time_values)]
  }
  
  # count unique values
  length(unique(time_values))
}

#' Check temporal requirements for causal inference
#'
#' @description
#' Verifies that data meets temporal requirements for causal inference,
#' such as having baseline measurements before treatment and outcomes
#' measured after treatment.
#'
#' @param baseline_time Time of baseline measurements
#' @param treatment_times Vector of treatment times
#' @param outcome_times Vector of outcome measurement times
#' @param requirements List of specific requirements to check
#'
#' @return List with logical 'valid' and character vector 'messages'
#'   describing any violations
#' @export
#' @examples
#' # check basic temporal ordering
#' check_temporal_requirements(
#'   baseline_time = 0,
#'   treatment_times = c(1, 2),
#'   outcome_times = c(3, 4)
#' )
#' 
#' # check with specific requirements
#' check_temporal_requirements(
#'   baseline_time = 0,
#'   treatment_times = 1,
#'   outcome_times = c(2, 3),
#'   requirements = list(min_followup = 1)
#' )
check_temporal_requirements <- function(baseline_time,
                                      treatment_times,
                                      outcome_times,
                                      requirements = list()) {
  
  # validate inputs
  if (!is.numeric(baseline_time) || length(baseline_time) != 1) {
    stop("baseline_time must be a single numeric value", call. = FALSE)
  }
  
  if (!is.numeric(treatment_times)) {
    stop("treatment_times must be numeric", call. = FALSE)
  }
  
  if (!is.numeric(outcome_times)) {
    stop("outcome_times must be numeric", call. = FALSE)
  }
  
  # initialise results
  valid <- TRUE
  messages <- character()
  
  # check baseline precedes treatment
  if (any(treatment_times <= baseline_time)) {
    valid <- FALSE
    messages <- c(messages, "Some treatments occur at or before baseline")
  }
  
  # check treatment precedes outcome
  min_outcome_time <- min(outcome_times)
  if (any(treatment_times >= min_outcome_time)) {
    valid <- FALSE
    messages <- c(messages, "Some treatments occur at or after earliest outcome")
  }
  
  # check minimum follow-up if specified
  if (!is.null(requirements$min_followup)) {
    max_treatment_time <- max(treatment_times)
    followup_duration <- min(outcome_times) - max_treatment_time
    if (followup_duration < requirements$min_followup) {
      valid <- FALSE
      messages <- c(messages, paste(
        "Insufficient follow-up time:",
        round(followup_duration, 2),
        "< required",
        requirements$min_followup
      ))
    }
  }
  
  # check for concurrent treatments if specified
  if (!is.null(requirements$no_concurrent) && requirements$no_concurrent) {
    if (length(unique(treatment_times)) < length(treatment_times)) {
      valid <- FALSE
      messages <- c(messages, "Concurrent treatments detected")
    }
  }
  
  # return results
  list(
    valid = valid,
    messages = if (length(messages) > 0) messages else "All requirements met"
  )
}