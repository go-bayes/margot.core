#' Enhanced Structural Causal Model (SCM) with Interventions
#'
#' @description
#' Enhanced SCM functionality for the margotsphere ecosystem with:
#' - Mandatory exogenous errors for every endogenous node
#' - Timeline DSL for user-friendly specification
#' - Built-in shift interventions
#' - Proper margot_ naming conventions


# enhanced constructor ---------------------------------------------------------

#' Create an enhanced SCM object
#'
#' @param nodes Character vector of node names
#' @param parents Named list; each element is the parent set of the node
#' @param exogenous Named list mapping each node to its exogenous error term
#' @param funcs Named list of functional forms (optional)
#' @param time_index Integer vector indicating time period for each node
#' @param shifts List of shift intervention functions
#' @return Object of class `margot_scm`
#' @export
#' @examples
#' nodes <- c("t0_b", "t1_a", "t1_y")
#' parents <- list(t0_b = character(), t1_a = "t0_b", t1_y = c("t0_b", "t1_a"))
#' exogenous <- list(t0_b = "U_b", t1_a = "U_a", t1_y = "U_y")
#' scm <- margot_scm_new(nodes, parents, exogenous, time_index = c(1, 2, 2))
margot_scm_new <- function(nodes,
                          parents,
                          exogenous,
                          funcs = NULL,
                          time_index = NULL,
                          shifts = NULL) {

  # validate inputs
  if (!is.character(nodes) || length(nodes) == 0) {
    stop("nodes must be a non-empty character vector")
  }

  if (!is.list(parents) || !is.list(exogenous)) {
    stop("parents and exogenous must be lists")
  }

  # check all nodes have parents and exogenous specified
  if (!setequal(names(parents), nodes)) {
    stop("names of parents list must match nodes exactly")
  }

  if (!setequal(names(exogenous), nodes)) {
    stop("every node must have an exogenous error term specified")
  }

  # validate optional arguments
  if (!is.null(funcs) && !setequal(names(funcs), nodes)) {
    stop("if provided, funcs must have entries for all nodes")
  }

  if (!is.null(shifts) && !is.list(shifts)) {
    stop("shifts must be NULL or a list")
  }

  if (!is.null(time_index) && length(time_index) != length(nodes)) {
    stop("time_index must have same length as nodes")
  }

  # ensure parent entries are character vectors
  parents <- lapply(parents, function(p) {
    if (is.null(p)) character() else as.character(p)
  })

  structure(
    list(
      nodes = nodes,
      pa = parents,
      U = exogenous,
      f = funcs %||% vector("list", length(nodes)),
      k = time_index %||% rep(NA_integer_, length(nodes)),
      shift = shifts %||% list()
    ),
    class = c("margot_scm", "scm")
  )
}

# from data frame with enhanced features ---------------------------------------

#' Build SCM from data frame with automatic exogenous mapping
#'
#' @param data Data frame with LMTP naming convention
#' @param K_min Minimum number of time periods required
#' @return Object of class `margot_scm`
#' @export
margot_scm_from_df <- function(data, K_min = 3) {

  # identify waves
  pats <- grep("^t[0-9]+_", names(data), value = TRUE)
  if (length(pats) == 0) {
    stop("No columns matching LMTP pattern (t0_*, t1_*, ...) found")
  }

  waves <- unique(sub("^(t[0-9]+)_.*", "\\1", pats))
  wave_nums <- as.numeric(sub("^t", "", waves))
  waves <- waves[order(wave_nums)]
  K <- length(waves)

  if (K < K_min) {
    stop("need at least ", K_min, " time points; found ", K)
  }

  # helper function
  block_vars <- function(w, blk) {
    names(data)[grepl(paste0("^", w, "_", blk), names(data), perl = TRUE)]
  }

  # initialize containers
  nodes <- character()
  pa <- list()
  Umap <- list()
  kvec <- integer()

  # helper to add node
  add_node <- function(v, parents, k) {
    nodes <<- c(nodes, v)
    pa[[v]] <<- parents
    Umap[[v]] <<- paste0("U_", v)
    kvec <<- c(kvec, k)
  }

  # baseline
  for (v in block_vars("t0", "b")) {
    add_node(v, character(), 1)
  }

  # add baseline l, a, y if they exist
  for (blk in c("l", "a", "y")) {
    for (v in block_vars("t0", blk)) {
      add_node(v, character(), 1)
    }
  }

  # post-baseline waves
  for (k_idx in 2:K) {
    wk <- waves[k_idx]
    k_num <- as.numeric(sub("^t", "", wk))

    # history helper
    hist_nodes <- function(upto_idx) {
      nodes_list <- character()
      for (j_idx in 1:upto_idx) {
        wj <- waves[j_idx]
        for (blk in c("b", "l", "a", "y")) {
          nodes_list <- c(nodes_list, block_vars(wj, blk))
        }
      }
      nodes_list
    }

    # L block
    for (v in block_vars(wk, "l")) {
      add_node(v, hist_nodes(k_idx - 1), k_num + 1)
    }

    # A block
    for (v in block_vars(wk, "a")) {
      parents <- c(hist_nodes(k_idx - 1), block_vars(wk, "l"))
      add_node(v, parents, k_num + 1)
    }

    # Y block
    for (v in block_vars(wk, "y")) {
      parents <- character()
      for (j_idx in 1:k_idx) {
        wj <- waves[j_idx]
        for (blk in c("b", "l", "a")) {
          parents <- c(parents, block_vars(wj, blk))
        }
      }
      add_node(v, parents, k_num + 1)
    }
  }

  margot_scm_new(nodes, pa, Umap, time_index = kvec)
}

# DSL parser -------------------------------------------------------------------

#' Parse SCM from domain-specific language
#'
#' @param text Character string with SCM specification
#' @return Object of class `margot_scm`
#' @export
#' @examples
#' dsl <- "
#' U  = U_l, U_a, U_y
#' k1 = t0_l ~ U_l
#' k2 = t1_a ~ t0_l + U_a
#' k3 = t2_y ~ t1_a + t0_l + U_y
#' "
#' scm <- margot_scm_from_dsl(dsl)
margot_scm_from_dsl <- function(text) {

  txt <- trimws(strsplit(text, "\n")[[1]])
  txt <- txt[nzchar(txt) & !grepl("^#", txt)]  # drop blanks and comments

  # get U universe
  Uline <- grep("^U\\s*=", txt, value = TRUE)
  if (length(Uline) != 1) {
    stop("exactly one line starting with 'U =' is required")
  }

  # handle potential comments in U line
  Uline_clean <- gsub("#.*$", "", Uline)  # remove comments
  U_all <- trimws(strsplit(sub("^U\\s*=\\s*", "", Uline_clean), ",")[[1]])
  txt <- setdiff(txt, Uline)

  # process each k-block
  nodes <- character()
  pa <- list()
  Umap <- list()
  kvec <- integer()

  for (line in txt) {
    # remove comments
    line_clean <- gsub("#.*$", "", line)
    line_clean <- trimws(line_clean)

    if (!grepl("^k[0-9]+\\s*=", line_clean)) {
      stop("lines must start with k#, e.g. 'k2 = var ~ ...'")
    }

    # split "k# = lhs ~ rhs"
    split1 <- strsplit(line_clean, "=", fixed = TRUE)[[1]]
    if (length(split1) != 2) {
      stop("invalid format in line: ", line)
    }

    k <- as.integer(sub("^k", "", trimws(split1[1])))
    rhs_whole <- trimws(split1[2])

    # split equation
    if (!grepl("~", rhs_whole)) {
      stop("missing ~ in equation: ", rhs_whole)
    }

    split2 <- strsplit(rhs_whole, "~", fixed = TRUE)[[1]]
    lhs <- trimws(split2[1])
    rhs <- if (length(split2) > 1) trimws(split2[2]) else ""

    # parse all terms from rhs
    all_terms <- if (nchar(rhs)) {
      trimws(strsplit(rhs, "\\+")[[1]])
    } else {
      character()
    }

    # separate exogenous and endogenous parents
    u_terms <- intersect(U_all, all_terms)
    if (length(u_terms) == 0) {
      stop("no exogenous error listed in U = ... for node ", lhs)
    }
    u_term <- u_terms[1]  # use first U term

    # parents are all terms except the U term
    parents <- setdiff(all_terms, U_all)

    # store
    nodes <- c(nodes, lhs)
    pa[[lhs]] <- parents
    Umap[[lhs]] <- u_term
    kvec <- c(kvec, k)
  }

  margot_scm_new(nodes, pa, Umap, time_index = kvec)
}

# shift interventions ----------------------------------------------------------

#' Add shift intervention to SCM
#'
#' @param scm A margot_scm object
#' @param node Name of node to intervene on
#' @param name Name for this intervention
#' @param fun Shift function
#' @return Updated margot_scm object
#' @export
#' @examples
#' scm <- margot_scm_from_dsl("
#' U = U_a, U_y
#' k1 = a ~ U_a
#' k2 = y ~ a + U_y
#' ")
#' scm <- margot_scm_add_shift(scm, "a", "increase", function(a, ...) a + 1)
margot_scm_add_shift <- function(scm, node, name, fun) {
  if (!inherits(scm, "margot_scm")) {
    stop("scm must be a margot_scm object")
  }

  if (!(node %in% scm$nodes)) {
    stop("node '", node, "' not found in SCM")
  }

  if (!is.function(fun)) {
    stop("fun must be a function")
  }

  # store as nested list
  if (is.null(scm$shift[[node]])) {
    scm$shift[[node]] <- list()
  }
  scm$shift[[node]][[name]] <- fun

  scm
}

# print methods ----------------------------------------------------------------

#' Print method for margot_scm objects
#'
#' @param x A margot_scm object
#' @param ... Additional arguments
#' @param indent Indentation string
#' @param width Line width
#' @return Invisibly returns the object
#' @export
print.margot_scm <- function(x, ..., indent = "  ",
                            width = getOption("width", 80)) {

  cat("structural causal model\n",
      paste(rep("-", 27), collapse = ""), "\n", sep = "")

  wrap <- function(s) {
    paste(strwrap(s, width, exdent = nchar(indent)), collapse = "\n")
  }

  for (n in x$nodes) {
    # build function arguments
    args <- character()
    if (length(x$pa[[n]]) > 0) {
      args <- c(args, paste(x$pa[[n]], collapse = ", "))
    }
    args <- c(args, x$U[[n]])

    rhs <- sprintf("f_%s(%s)", n, paste(args, collapse = ", "))

    # add time index if available
    time_str <- ""
    if (!is.null(x$k) && !is.na(x$k[which(x$nodes == n)])) {
      time_str <- sprintf(" [k%d]", x$k[which(x$nodes == n)])
    }

    cat(indent, n, " := ", wrap(rhs), time_str, "\n", sep = "")
  }

  # show shift interventions if any
  if (length(x$shift) > 0) {
    cat("\nshift interventions:\n",
        paste(rep("-", 22), collapse = ""), "\n", sep = "")
    for (nd in names(x$shift)) {
      cat(indent, nd, ": ", paste(names(x$shift[[nd]]), collapse = ", "), "\n")
    }
  }

  invisible(x)
}

# latex method -----------------------------------------------------------------

#' Convert margot_scm to LaTeX
#'
#' @param x A margot_scm object
#' @return LaTeX string
#' @export
margot_scm_as_latex <- function(x) {
  lines <- vapply(x$nodes, function(n) {
    # build parent list
    pa_str <- if (length(x$pa[[n]]) > 0) {
      paste(x$pa[[n]], collapse = ", ")
    } else {
      ""
    }

    # combine with exogenous
    args <- if (nchar(pa_str) > 0) {
      paste0(pa_str, ", ", x$U[[n]])
    } else {
      x$U[[n]]
    }

    sprintf("%s &:= f_{%s}(%s) \\\\", n, n, args)
  }, character(1))

  paste0("\\begin{align*}\n",
         paste(lines, collapse = "\n"),
         "\n\\end{align*}")
}

# S3 method registration
#' @export
as_latex.margot_scm <- function(x) {
  margot_scm_as_latex(x)
}

# utility functions ------------------------------------------------------------

#' Check if object is a margot_scm
#'
#' @param x Object to check
#' @return Logical
#' @export
is_margot_scm <- function(x) {
  inherits(x, "margot_scm")
}

#' Extract shift functions from SCM
#'
#' @param scm A margot_scm object
#' @param node Node name (optional, returns all if NULL)
#' @return List of shift functions
#' @export
margot_scm_get_shifts <- function(scm, node = NULL) {
  if (!inherits(scm, "margot_scm")) {
    stop("scm must be a margot_scm object")
  }

  if (is.null(node)) {
    return(scm$shift)
  }

  if (!(node %in% scm$nodes)) {
    stop("node '", node, "' not found in SCM")
  }

  scm$shift[[node]] %||% list()
}

#' Get time indices from SCM
#'
#' @param scm A margot_scm object
#' @return Named vector of time indices
#' @export
margot_scm_time_indices <- function(scm) {
  if (!inherits(scm, "margot_scm")) {
    stop("scm must be a margot_scm object")
  }

  if (is.null(scm$k)) {
    return(NULL)
  }

  setNames(scm$k, scm$nodes)
}
