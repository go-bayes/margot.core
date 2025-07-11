% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot-panel.R
\name{[.margot_panel}
\alias{[.margot_panel}
\title{Subset margot_panel objects}
\usage{
\method{[}{margot_panel}(x, i, j, ..., drop = FALSE)
}
\arguments{
\item{x}{A margot_panel object}

\item{i}{Row indices}

\item{j}{Column indices or names}

\item{...}{Additional arguments}

\item{drop}{If TRUE, drop panel attributes when subsetting to single column}
}
\value{
Subsetted margot_panel or vector if single column with drop=TRUE
}
\description{
Subset margot_panel objects
}
