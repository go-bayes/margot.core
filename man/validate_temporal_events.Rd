% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot-timeline.R
\name{validate_temporal_events}
\alias{validate_temporal_events}
\title{Validate temporal events against requirements}
\usage{
validate_temporal_events(events, requirements = list())
}
\arguments{
\item{events}{Numeric vector of event times}

\item{requirements}{List of requirements to check, which may include:
\itemize{
\item min_spacing: Minimum spacing between consecutive events
\item required_times: Specific times that must be included
\item max_duration: Maximum allowed duration from first to last event
}}
}
\value{
Logical indicating whether all requirements are met. If FALSE,
attributes contain details about which requirements failed.
}
\description{
Checks that a set of temporal events meets specified requirements
such as proper ordering, required time points, or minimum spacing.
}
\examples{
# check events have minimum spacing of 7 days
events <- c(0, 7, 14, 21)
validate_temporal_events(events, list(min_spacing = 7))

# check multiple requirements
validate_temporal_events(
  events,
  list(min_spacing = 5, required_times = c(0, 21))
)
}
