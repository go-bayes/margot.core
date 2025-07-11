% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot-scm.R
\name{new_scm}
\alias{new_scm}
\title{Structural Causal Model (SCM) S3 Class for Margotsphere}
\usage{
new_scm(nodes, parents, funcs = NULL, exogenous = character())
}
\arguments{
\item{nodes}{Character vector of node names}

\item{parents}{Named list; each element is the parent set of the node}

\item{funcs}{Named list of functional forms (optionally NULL)}

\item{exogenous}{Character vector of exogenous (U-) terms}
}
\value{
Object of class \code{scm}
}
\description{
Provides flexible helpers for creating and printing Structural Causal Models
in the LMTP style (Diaz et al.). Can create SCMs from wide data frames
following LMTP naming conventions or from explicit equation specifications.
Create an SCM object
}
\examples{
# simple scm
nodes <- c("X", "Y")
parents <- list(X = character(), Y = "X")
scm <- new_scm(nodes, parents, exogenous = c("U_X", "U_Y"))
print(scm)
}
