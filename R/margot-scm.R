#' Structural Causal Model (SCM) S3 Class for Margotsphere
#'
#' @description
#' Provides flexible helpers for creating and printing Structural Causal Models
#' in the LMTP style (Diaz et al.). Can create SCMs from wide data frames
#' following LMTP naming conventions or from explicit equation specifications.

# constructor ------------------------------------------------------------------

#' Create an SCM object
#'
#' @param nodes Character vector of node names
#' @param parents Named list; each element is the parent set of the node
#' @param funcs Named list of functional forms (optionally NULL)
#' @param exogenous Character vector of exogenous (U-) terms
#' @return Object of class `scm`
#' @export
#' @examples
#' # simple scm
#' nodes <- c("X", "Y")
#' parents <- list(X = character(), Y = "X")
#' scm <- new_scm(nodes, parents, exogenous = c("U_X", "U_Y"))
#' print(scm)
new_scm <- function(nodes, parents, funcs = NULL, exogenous = character()) {
  # validate inputs
  if (!is.character(nodes) || length(nodes) == 0) {
    stop("nodes must be a non-empty character vector")
  }
  
  if (!is.list(parents)) {
    stop("parents must be a list")
  }
  
  if (!setequal(names(parents), nodes)) {
    stop("names of parents list must match nodes exactly")
  }
  
  # ensure all parent entries are character vectors
  parents <- lapply(parents, function(p) {
    if (is.null(p)) character() else as.character(p)
  })
  
  if (!is.null(funcs) && !is.list(funcs)) {
    stop("funcs must be NULL or a list")
  }
  
  if (!is.character(exogenous)) {
    stop("exogenous must be a character vector")
  }
  
  structure(
    list(
      nodes = nodes,
      pa = parents,
      f = funcs,
      U = exogenous
    ),
    class = "scm"
  )
}

# from data frame --------------------------------------------------------------

#' Build an SCM from a wide data frame
#'
#' @description
#' Creates an SCM object from a wide data frame that follows the LMTP naming
#' convention (t0_*, t1_*, ..., t{K-1}_*).
#'
#' @param data A data.frame or tibble with columns following LMTP naming
#' @param K_min Minimum number of waves (default 3)
#' @return Object of class `scm`
#' @export
#' @examples
#' # create example data
#' df <- data.frame(
#'   t0_b = rnorm(100),
#'   t0_l = rnorm(100),
#'   t1_l = rnorm(100),
#'   t1_a = rbinom(100, 1, 0.5),
#'   t1_y = rnorm(100),
#'   t2_l = rnorm(100),
#'   t2_a = rbinom(100, 1, 0.5),
#'   t2_y = rnorm(100)
#' )
#' scm <- scm_from_df(df)
#' print(scm)
scm_from_df <- function(data, K_min = 3) {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  
  # extract waves
  pats <- grep("^t[0-9]+_", names(data), value = TRUE)
  if (length(pats) == 0) {
    stop("No columns matching LMTP pattern (t0_*, t1_*, ...) found")
  }
  
  waves <- unique(sub("^(t[0-9]+)_.*", "\\1", pats))
  # sort waves numerically
  wave_nums <- as.numeric(sub("^t", "", waves))
  waves <- waves[order(wave_nums)]
  K <- length(waves)
  
  if (K < K_min) {
    stop("need at least ", K_min, " time points; found ", K)
  }
  
  # helper to get variables for a block
  get_block <- function(w, blk) {
    pattern <- paste0("^", w, "_", blk)
    names(data)[grepl(pattern, names(data), perl = TRUE)]
  }
  
  # build parent lists
  pa <- list()
  nodes <- character()
  
  # baseline (t0)
  for (blk in c("b", "l", "a", "y")) {
    for (v in get_block("t0", blk)) {
      nodes <- c(nodes, v)
      pa[[v]] <- character()  # baseline has no observed parents
    }
  }
  
  # post-baseline waves
  for (k_idx in 2:K) {
    wk <- waves[k_idx]
    k_num <- as.numeric(sub("^t", "", wk))
    
    # l-block: depends on all prior history
    for (v in get_block(wk, "l")) {
      nodes <- c(nodes, v)
      history <- character()
      # get all waves before current one
      for (j_idx in 1:(k_idx-1)) {
        wj <- waves[j_idx]
        for (blk in c("b", "l", "a", "y")) {
          history <- c(history, get_block(wj, blk))
        }
      }
      pa[[v]] <- history
    }
    
    # a-block: depends on all prior history + current l
    for (v in get_block(wk, "a")) {
      nodes <- c(nodes, v)
      history <- character()
      # get all waves before current one
      for (j_idx in 1:(k_idx-1)) {
        wj <- waves[j_idx]
        for (blk in c("b", "l", "a", "y")) {
          history <- c(history, get_block(wj, blk))
        }
      }
      # add current wave l
      history <- c(history, get_block(wk, "l"))
      pa[[v]] <- history
    }
    
    # y-block: depends on all b, l, a up to current wave
    for (v in get_block(wk, "y")) {
      nodes <- c(nodes, v)
      history <- character()
      # get all waves up to and including current one
      for (j_idx in 1:k_idx) {
        wj <- waves[j_idx]
        for (blk in c("b", "l", "a")) {
          history <- c(history, get_block(wj, blk))
        }
      }
      pa[[v]] <- history
    }
  }
  
  # exogenous errors - one per observed node
  U <- paste0("u_", nodes)
  
  new_scm(nodes, pa, exogenous = U)
}

# from specification -----------------------------------------------------------

#' Build an SCM from equation specifications
#'
#' @description
#' Creates an SCM object from a semicolon-separated string of equations.
#'
#' @param spec A single character string with equations separated by semicolons
#' @return Object of class `scm`
#' @export
#' @examples
#' spec <- "t0_b ~ U1 + U2; t1_a ~ U1; t2_y ~ U2"
#' scm <- scm_from_spec(spec)
#' print(scm)
scm_from_spec <- function(spec) {
  if (length(spec) != 1 || !is.character(spec)) {
    stop("spec must be a single character string")
  }
  
  eqs <- trimws(strsplit(spec, ";")[[1]])
  pa <- list()
  nodes <- character()
  U <- character()
  
  for (eq in eqs) {
    if (!grepl("~", eq, fixed = TRUE)) {
      stop("each equation needs a '~': ", eq)
    }
    
    parts <- strsplit(eq, "~", fixed = TRUE)[[1]]
    
    # handle different cases
    if (length(parts) == 0) {
      stop("invalid equation format: ", eq)
    } else if (length(parts) == 1) {
      # case like "X ~" with no RHS
      lhs <- trimws(parts[1])
      rhs <- ""
    } else {
      # normal case
      lhs <- trimws(parts[1])
      rhs <- trimws(parts[2])
    }
    
    # parse parents from rhs
    parents <- if (nchar(rhs) == 0 || rhs == "") {
      character()
    } else {
      # split by + and trim whitespace
      parent_list <- trimws(strsplit(rhs, "[+]")[[1]])
      # remove any empty strings
      parent_list[nchar(parent_list) > 0]
    }
    
    nodes <- c(nodes, lhs)
    pa[[lhs]] <- parents
    
    # collect any explicit u-nodes on the rhs
    U <- union(U, parents[grepl("^U[0-9]+$", parents)])
  }
  
  new_scm(nodes, pa, exogenous = U)
}

# print methods ----------------------------------------------------------------

#' Print method for SCM objects
#'
#' @param x An scm object
#' @param indent Indentation string (default "  ")
#' @param width Maximum line width (default from options)
#' @param ... Additional arguments (unused)
#' @return Invisibly returns the SCM object
#' @export
print.scm <- function(x, indent = "  ", width = getOption("width", 80), ...) {
  cat("structural causal model (diaz-style lmtp notation)\n",
      paste(rep("-", 53), collapse = ""), "\n", sep = "")
  
  wrap <- function(s) {
    paste(strwrap(s, width = width, exdent = nchar(indent)), 
          collapse = "\n")
  }
  
  for (n in x$nodes) {
    rhs <- if (length(x$pa[[n]]) == 0) {
      paste0("f_", n, "(u_", n, ")")
    } else {
      sprintf("f_%s(%s, u_%s)",
              n,
              paste0(x$pa[[n]], collapse = ", "),
              n)
    }
    cat(indent, n, " := ", wrap(rhs), "\n", sep = "")
  }
  
  invisible(x)
}

#' Convert SCM to LaTeX representation
#'
#' @param x An scm object
#' @return Character string with LaTeX align environment
#' @export
#' @examples
#' scm <- scm_from_spec("X ~ U1; Y ~ X + U2")
#' cat(as_latex(scm))
as_latex <- function(x) {
  UseMethod("as_latex")
}

#' @export
as_latex.scm <- function(x) {
  lines <- vapply(x$nodes,
                  function(n) {
                    pa <- x$pa[[n]]
                    if (length(pa)) {
                      pa_hist <- paste0(pa, collapse = ", ")
                      sprintf("%s &:= f_{%s}(%s, u_{%s}) \\\\",
                              n, n, pa_hist, n)
                    } else {
                      sprintf("%s &:= f_{%s}(u_{%s}) \\\\",
                              n, n, n)
                    }
                  },
                  character(1))
  paste0("\\begin{align*}\n", paste(lines, collapse = "\n"), "\n\\end{align*}")
}

# validation -------------------------------------------------------------------

#' Validate SCM object
#'
#' @param x An scm object
#' @return TRUE if valid, error otherwise
#' @keywords internal
validate_scm <- function(x) {
  if (!inherits(x, "scm")) {
    stop("not an scm object")
  }
  
  if (!all(c("nodes", "pa", "f", "U") %in% names(x))) {
    stop("scm object missing required components")
  }
  
  if (!is.character(x$nodes)) {
    stop("nodes must be character vector")
  }
  
  if (!is.list(x$pa)) {
    stop("pa must be a list")
  }
  
  if (!setequal(names(x$pa), x$nodes)) {
    stop("pa names must match nodes")
  }
  
  TRUE
}

# type checking ----------------------------------------------------------------

#' Check if object is an SCM
#'
#' @param x Object to check
#' @return Logical indicating if x is an scm object
#' @export
is_scm <- function(x) {
  inherits(x, "scm")
}

# summary method ---------------------------------------------------------------

#' Summary method for SCM objects
#'
#' @param object An scm object
#' @param ... Additional arguments (unused)
#' @return Summary information about the SCM
#' @export
summary.scm <- function(object, ...) {
  n_nodes <- length(object$nodes)
  n_exog <- length(object$U)
  
  # count nodes by type (if lmtp naming)
  node_types <- table(sub("^t[0-9]+_([a-z]+).*", "\\1", object$nodes))
  
  # compute average in-degree
  in_degrees <- sapply(object$pa, length)
  avg_in_degree <- mean(in_degrees)
  max_in_degree <- max(in_degrees)
  
  cat("structural causal model summary\n")
  cat("-------------------------------\n")
  cat("total nodes:", n_nodes, "\n")
  cat("exogenous variables:", n_exog, "\n")
  
  if (length(node_types) > 1) {
    cat("\nnode types:\n")
    for (type in names(node_types)) {
      cat("  ", type, ":", node_types[type], "\n")
    }
  }
  
  cat("\ndependency structure:\n")
  cat("  average in-degree:", round(avg_in_degree, 2), "\n")
  cat("  maximum in-degree:", max_in_degree, "\n")
  cat("  nodes with no parents:", sum(in_degrees == 0), "\n")
  
  invisible(list(
    n_nodes = n_nodes,
    n_exogenous = n_exog,
    node_types = node_types,
    avg_in_degree = avg_in_degree,
    max_in_degree = max_in_degree
  ))
}