% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot-scm.R
\name{as_latex}
\alias{as_latex}
\title{Convert SCM to LaTeX representation}
\usage{
as_latex(x)
}
\arguments{
\item{x}{An scm object}
}
\value{
Character string with LaTeX align environment
}
\description{
Convert SCM to LaTeX representation
}
\examples{
scm <- scm_from_spec("X ~ U1; Y ~ X + U2")
cat(as_latex(scm))
}
