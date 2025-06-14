#' Create a margot panel object
#'
#' @description
#' Creates a panel data object with standardised structure for use
#' across the margotsphere ecosystem. This S3 class ensures consistent
#' handling of longitudinal data.
#'
#' @param data A data.frame or tibble containing panel data
#' @param id Character string naming the unit identifier column
#' @param time Character string naming the time period column
#'
#' @return An object of class "margot_panel" which inherits from the
#'   original data class
#' @export
#' @examples
#' # create example panel data
#' df <- data.frame(
#'   id = rep(1:3, each = 3),
#'   wave = rep(1:3, 3),
#'   outcome = rnorm(9),
#'   treatment = rbinom(9, 1, 0.5)
#' )
#' 
#' # create panel object
#' panel <- margot_panel(df, id = "id", time = "wave")
#' print(panel)
margot_panel <- function(data, id, time) {
  # validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data.frame or tibble", call. = FALSE)
  }
  
  if (!is.character(id) || length(id) != 1) {
    stop("id must be a single character string", call. = FALSE)
  }
  
  if (!is.character(time) || length(time) != 1) {
    stop("time must be a single character string", call. = FALSE)
  }
  
  # check columns exist
  if (!id %in% names(data)) {
    stop("id column '", id, "' not found in data", call. = FALSE)
  }
  
  if (!time %in% names(data)) {
    stop("time column '", time, "' not found in data", call. = FALSE)
  }
  
  # create panel object
  structure(
    data,
    id_col = id,
    time_col = time,
    class = c("margot_panel", class(data))
  )
}

#' Print method for margot_panel objects
#'
#' @param x A margot_panel object
#' @param ... Additional arguments passed to print methods
#' @return The object invisibly
#' @export
print.margot_panel <- function(x, ...) {
  cat("<margot_panel>\n")
  
  # basic info
  n_rows <- nrow(x)
  n_ids <- length(unique(x[[attr(x, "id_col")]]))
  n_times <- length(unique(x[[attr(x, "time_col")]]))
  
  cat("  Rows:", n_rows, "\n")
  cat("  Units:", n_ids, "\n")
  cat("  Time periods:", n_times, "\n")
  cat("  ID column:", attr(x, "id_col"), "\n")
  cat("  Time column:", attr(x, "time_col"), "\n")
  
  # check balance
  expected_rows <- n_ids * n_times
  if (n_rows == expected_rows) {
    cat("  Panel: balanced\n")
  } else {
    cat("  Panel: unbalanced (", 
        round(n_rows / expected_rows * 100, 1), 
        "% complete)\n", sep = "")
  }
  
  cat("\n")
  
  # print underlying data
  NextMethod("print", x)
  
  invisible(x)
}

#' Summary method for margot_panel objects
#'
#' @param object A margot_panel object
#' @param ... Additional arguments (unused)
#' @return Summary information about the panel
#' @export
summary.margot_panel <- function(object, ...) {
  id_col <- attr(object, "id_col")
  time_col <- attr(object, "time_col")
  
  # get unique counts
  ids <- unique(object[[id_col]])
  times <- unique(object[[time_col]])
  
  # check for balance
  panel_table <- table(object[[id_col]], object[[time_col]])
  is_balanced <- all(panel_table > 0)
  
  # count observations per unit
  obs_per_unit <- table(object[[id_col]])
  
  # create summary
  structure(
    list(
      n_units = length(ids),
      n_periods = length(times),
      n_obs = nrow(object),
      is_balanced = is_balanced,
      id_column = id_col,
      time_column = time_col,
      time_range = range(times),
      obs_per_unit_summary = summary(as.numeric(obs_per_unit)),
      missing_combinations = sum(panel_table == 0)
    ),
    class = "summary.margot_panel"
  )
}

#' @export
print.summary.margot_panel <- function(x, ...) {
  cat("Panel Data Summary\n")
  cat("==================\n")
  cat("Units:", x$n_units, "\n")
  cat("Time periods:", x$n_periods, "\n")
  cat("Total observations:", x$n_obs, "\n")
  cat("Panel type:", ifelse(x$is_balanced, "Balanced", "Unbalanced"), "\n")
  
  if (!x$is_balanced) {
    cat("Missing combinations:", x$missing_combinations, "\n")
  }
  
  cat("\nTime range:", x$time_range[1], "to", x$time_range[2], "\n")
  cat("\nObservations per unit:\n")
  print(x$obs_per_unit_summary)
  
  invisible(x)
}

#' Check if object is a margot_panel
#'
#' @param x Object to test
#' @return Logical
#' @export
is_margot_panel <- function(x) {
  inherits(x, "margot_panel")
}

#' Extract panel attributes
#'
#' @param x A margot_panel object
#' @return List with id_col and time_col
#' @export
panel_attrs <- function(x) {
  if (!is_margot_panel(x)) {
    stop("x must be a margot_panel object", call. = FALSE)
  }
  
  list(
    id_col = attr(x, "id_col"),
    time_col = attr(x, "time_col")
  )
}

#' Convert margot_panel to wide format
#'
#' @param x A margot_panel object
#' @param sep Character separator for column names (default: "_")
#' @param ... Additional arguments passed to reshape
#' @return Wide format data frame
#' @export
as_wide.margot_panel <- function(x, sep = "_", ...) {
  if (!is_margot_panel(x)) {
    stop("x must be a margot_panel object", call. = FALSE)
  }
  
  id_col <- attr(x, "id_col")
  time_col <- attr(x, "time_col")
  
  # get non-id, non-time columns
  value_cols <- setdiff(names(x), c(id_col, time_col))
  
  if (length(value_cols) == 0) {
    warning("No value columns found to reshape", call. = FALSE)
    return(x)
  }
  
  # use stats::reshape for base r compatibility
  wide <- stats::reshape(
    as.data.frame(x),
    idvar = id_col,
    timevar = time_col,
    direction = "wide",
    sep = sep,
    ...
  )
  
  # clean up row names
  rownames(wide) <- NULL
  
  wide
}

#' Subset margot_panel objects
#'
#' @param x A margot_panel object
#' @param i Row indices
#' @param j Column indices or names
#' @param ... Additional arguments
#' @param drop If TRUE, drop panel attributes when subsetting to single column
#' @return Subsetted margot_panel or vector if single column with drop=TRUE
#' @export
`[.margot_panel` <- function(x, i, j, ..., drop = FALSE) {
  # get attributes before subsetting
  id_col <- attr(x, "id_col")
  time_col <- attr(x, "time_col")
  
  # remove margot_panel class to use default subsetting
  class(x) <- setdiff(class(x), "margot_panel")
  
  # use default subsetting with missing argument handling
  if (missing(i) && missing(j)) {
    result <- x[, , ..., drop = drop]
  } else if (missing(i)) {
    result <- x[, j, ..., drop = drop]
  } else if (missing(j)) {
    result <- x[i, , ..., drop = drop]
  } else {
    result <- x[i, j, ..., drop = drop]
  }
  
  # only preserve panel class if:
  # 1. result is still a data frame
  # 2. contains BOTH required columns (id AND time)
  # 3. not dropping dimensions
  if (is.data.frame(result) && 
      id_col %in% names(result) && 
      time_col %in% names(result) &&
      !drop) {
    result <- margot_panel(result, id = id_col, time = time_col)
  }
  
  result
}