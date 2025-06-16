#' Shadow Dependency Management System
#'
#' @description
#' Implements a dependency system for shadows to handle interactions
#' between different types of observational distortions. For example,
#' truncation changes the variance which affects measurement error.
#'
#' @details
#' The system uses a directed acyclic graph (DAG) to represent dependencies
#' and automatically reorders shadows for correct application sequence.
#' 
#' TODO: Enhanced dependency resolution for time-varying confounding scenarios
#' is under development. Current implementation handles basic dependencies
#' with explicit ordering.

# dependency definitions ----------------------------------------------

#' Get shadow dependency definitions
#'
#' @description
#' Returns the dependency relationships between shadow types.
#' Each shadow type can declare which aspects of the data it modifies
#' and which aspects it depends on.
#'
#' @return List of dependency definitions
#' @keywords internal
get_shadow_dependencies <- function() {
  list(
    measurement_error = list(
      depends_on = c("distribution"),  # needs accurate variance
      modifies = c("values"),
      priority = 5  # higher number = apply later
    ),
    
    truncation = list(
      depends_on = c(),  # no dependencies
      modifies = c("distribution", "sample_size"),
      priority = 1  # apply early
    ),
    
    censoring = list(
      depends_on = c(),
      modifies = c("values", "completeness"),
      priority = 2
    ),
    
    selection = list(
      depends_on = c("values"),  # depends on variable values
      modifies = c("sample_size"),
      priority = 3
    ),
    
    positivity = list(
      depends_on = c("values"),
      modifies = c("sample_size"),
      priority = 3
    ),
    
    missing_data = list(
      depends_on = c("values"),
      modifies = c("completeness"),
      priority = 4
    ),
    
    item_missingness = list(
      depends_on = c("values"),
      modifies = c("completeness"),
      priority = 4
    ),
    
    coarsening = list(
      depends_on = c("values"),
      modifies = c("values", "precision"),
      priority = 6
    ),
    
    mode_effects = list(
      depends_on = c("values"),
      modifies = c("values"),
      priority = 6
    ),
    
    misclassification = list(
      depends_on = c("values"),
      modifies = c("values"),
      priority = 6
    )
  )
}

#' Detect cycles in shadow dependency graph
#' 
#' @description
#' Uses depth-first search (DFS) to detect cycles in the dependency graph.
#' This is critical for preventing infinite loops in shadow application.
#' 
#' @param shadows List of shadow objects
#' @return List with cycle detection results
#' @export
detect_cycles <- function(shadows) {
  if (length(shadows) <= 1) {
    return(list(
      has_cycles = FALSE,
      cycles = list(),
      message = "No cycles possible with 0 or 1 shadow"
    ))
  }
  
  # build adjacency list from shadow dependencies
  deps <- get_shadow_dependencies()
  shadow_types <- sapply(shadows, function(s) {
    if (inherits(s, "margot_shadow")) s$type else s$type
  })
  
  # create adjacency list
  adj_list <- list()
  for (i in seq_along(shadow_types)) {
    adj_list[[shadow_types[i]]] <- character(0)
  }
  
  # build edges based on dependencies
  for (i in seq_along(shadow_types)) {
    type_i <- shadow_types[i]
    dep_info_i <- deps[[type_i]]
    
    if (!is.null(dep_info_i)) {
      # what this shadow modifies
      modifies_i <- dep_info_i$modifies
      
      # check which other shadows depend on these modifications
      for (j in seq_along(shadow_types)) {
        if (i == j) next
        type_j <- shadow_types[j]
        dep_info_j <- deps[[type_j]]
        
        if (!is.null(dep_info_j)) {
          # does shadow j depend on what shadow i modifies?
          if (any(dep_info_j$depends_on %in% modifies_i)) {
            # add edge from i to j
            adj_list[[type_i]] <- c(adj_list[[type_i]], type_j)
          }
        }
      }
    }
  }
  
  # perform dfs to detect cycles
  visited <- list()
  rec_stack <- list()
  cycles <- list()
  
  # initialise all nodes as not visited
  for (node in names(adj_list)) {
    visited[[node]] <- FALSE
    rec_stack[[node]] <- FALSE
  }
  
  # dfs helper function
  dfs <- function(node, path = character(0)) {
    visited[[node]] <<- TRUE
    rec_stack[[node]] <<- TRUE
    path <- c(path, node)
    
    # check all adjacent nodes
    for (neighbour in adj_list[[node]]) {
      if (!visited[[neighbour]]) {
        # recurse on unvisited neighbour
        result <- dfs(neighbour, path)
        if (!is.null(result)) return(result)
      } else if (rec_stack[[neighbour]]) {
        # found a cycle
        cycle_start <- which(path == neighbour)
        if (length(cycle_start) > 0) {
          cycle <- path[cycle_start[1]:length(path)]
          cycles <<- append(cycles, list(c(cycle, neighbour)))
        }
        return(TRUE)
      }
    }
    
    rec_stack[[node]] <<- FALSE
    return(NULL)
  }
  
  # check each component
  for (node in names(adj_list)) {
    if (!visited[[node]]) {
      dfs(node)
    }
  }
  
  # format results
  if (length(cycles) > 0) {
    cycle_descriptions <- sapply(cycles, function(cycle) {
      paste(cycle, collapse = " -> ")
    })
    
    return(list(
      has_cycles = TRUE,
      cycles = cycles,
      cycle_descriptions = cycle_descriptions,
      message = paste("Found", length(cycles), "cycle(s) in shadow dependencies:",
                     paste(cycle_descriptions, collapse = "; "))
    ))
  } else {
    return(list(
      has_cycles = FALSE,
      cycles = list(),
      message = "No cycles detected in shadow dependencies"
    ))
  }
}

#' Check shadow ordering for dependencies
#'
#' @description
#' Verifies that shadows are ordered correctly based on their dependencies.
#' Also checks for cycles using detect_cycles().
#'
#' @param shadows List of shadow objects
#' @return List with validation results
#' @export
check_shadow_ordering <- function(shadows) {
  if (length(shadows) <= 1) {
    return(list(
      valid = TRUE,
      issues = character(0),
      suggested_order = shadows
    ))
  }
  
  # first check for cycles
  cycle_check <- detect_cycles(shadows)
  if (cycle_check$has_cycles) {
    return(list(
      valid = FALSE,
      issues = c(
        paste("Cycle detected:", cycle_check$message),
        "Cannot determine valid ordering with circular dependencies"
      ),
      suggested_order = shadows,
      has_cycles = TRUE,
      cycles = cycle_check$cycles
    ))
  }
  
  deps <- get_shadow_dependencies()
  
  # extract shadow types
  shadow_types <- sapply(shadows, function(s) {
    if (inherits(s, "margot_shadow")) {
      s$type
    } else if (is.list(s) && !is.null(s$type)) {
      s$type
    } else {
      "unknown"
    }
  })
  
  issues <- character(0)
  
  # check each shadow's dependencies
  for (i in seq_along(shadows)) {
    current_type <- shadow_types[i]
    if (current_type == "unknown") next
    
    current_deps <- deps[[current_type]]
    if (is.null(current_deps)) next
    
    # check what this shadow depends on
    depends_on <- current_deps$depends_on
    
    # check if any earlier shadows modify what this depends on
    if (i > 1 && length(depends_on) > 0) {
      for (j in 1:(i-1)) {
        earlier_type <- shadow_types[j]
        if (earlier_type == "unknown") next
        
        earlier_deps <- deps[[earlier_type]]
        if (is.null(earlier_deps)) next
        
        # check if earlier shadow modifies what current shadow depends on
        if (any(depends_on %in% earlier_deps$modifies)) {
          # this is good - dependency is satisfied
          next
        }
      }
      
      # now check if any dependency is NOT satisfied by earlier shadows
      for (dep in depends_on) {
        satisfied <- FALSE
        for (j in 1:(i-1)) {
          earlier_type <- shadow_types[j]
          if (earlier_type == "unknown") next
          
          earlier_deps <- deps[[earlier_type]]
          if (!is.null(earlier_deps) && dep %in% earlier_deps$modifies) {
            satisfied <- TRUE
            break
          }
        }
        
        if (!satisfied) {
          # check if any later shadow provides this dependency
          for (j in (i+1):length(shadows)) {
            if (j > length(shadows)) break
            later_type <- shadow_types[j]
            if (later_type == "unknown") next
            
            later_deps <- deps[[later_type]]
            if (!is.null(later_deps) && dep %in% later_deps$modifies) {
              issues <- c(issues, sprintf(
                "%s shadow at position %d depends on '%s', which is modified by %s shadow at position %d (should come earlier)",
                current_type, i, dep, later_type, j
              ))
              break
            }
          }
        }
      }
    }
  }
  
  # suggest reordering if issues found
  suggested_order <- if (length(issues) > 0) {
    reorder_shadows(shadows)
  } else {
    shadows
  }
  
  list(
    valid = length(issues) == 0,
    issues = issues,
    suggested_order = suggested_order,
    has_cycles = FALSE
  )
}

#' Reorder shadows based on dependencies
#'
#' @description
#' Automatically reorders shadows to respect dependency relationships.
#' Uses topological sorting based on the dependency DAG.
#'
#' @param shadows List of shadow objects
#' @return Reordered list of shadows
#' @export
reorder_shadows <- function(shadows) {
  if (length(shadows) <= 1) return(shadows)
  
  # check for cycles first
  cycle_check <- detect_cycles(shadows)
  if (cycle_check$has_cycles) {
    warning("Cannot reorder shadows with circular dependencies: ", 
            cycle_check$message)
    return(shadows)
  }
  
  deps <- get_shadow_dependencies()
  
  # extract shadow info
  shadow_info <- lapply(seq_along(shadows), function(i) {
    s <- shadows[[i]]
    type <- if (inherits(s, "margot_shadow")) {
      s$type
    } else if (is.list(s) && !is.null(s$type)) {
      s$type
    } else {
      "unknown"
    }
    
    priority <- if (!is.null(deps[[type]]$priority)) {
      deps[[type]]$priority
    } else {
      5  # default middle priority
    }
    
    list(
      index = i,
      shadow = s,
      type = type,
      priority = priority
    )
  })
  
  # sort by priority (lower priority = apply earlier)
  sorted_info <- shadow_info[order(sapply(shadow_info, function(x) x$priority))]
  
  # extract reordered shadows
  lapply(sorted_info, function(x) x$shadow)
}