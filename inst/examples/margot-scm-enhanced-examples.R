# Examples of enhanced SCM functionality with proper margot_ naming

library(devtools)
load_all()

# Example 1: Create SCM from DSL with mandatory exogenous errors
# -------------------------------------------------------------

cat("Example 1: Timeline DSL with mandatory exogenous errors\n")
cat("======================================================\n\n")

dsl <- "
U  = U_l, U_a, U_y
k1 = t0_l ~ U_l
k2 = t1_a ~ t0_l + U_a
k3 = t2_y ~ t1_a + t0_l + U_y
"

scm <- margot_scm_from_dsl(dsl)
print(scm)

# Example 2: Add shift interventions
# ----------------------------------

cat("\n\nExample 2: Adding shift interventions\n")
cat("=====================================\n\n")

# Add multiple shift interventions to treatment node
scm <- margot_scm_add_shift(
  scm,
  node = "t1_a",
  name = "increase_by_1",
  fun = function(a, ...) a + 1
)

scm <- margot_scm_add_shift(
  scm,
  node = "t1_a", 
  name = "threshold_4",
  fun = function(a, ...) ifelse(a < 4, 4, a)
)

scm <- margot_scm_add_shift(
  scm,
  node = "t1_a",
  name = "dynamic_shift",
  fun = function(a, data, ...) {
    # shift based on confounder value
    ifelse(data[["t0_l"]] > 0, a + 0.5, a - 0.5)
  }
)

print(scm)

# Example 3: Create SCM from data frame
# -------------------------------------

cat("\n\nExample 3: Automatic SCM from data frame\n")
cat("========================================\n\n")

# simulate some data
set.seed(123)
n <- 1000
df <- data.frame(
  # baseline
  t0_b1 = rnorm(n),
  t0_b2 = rbinom(n, 1, 0.3),
  
  # time 1
  t1_l = rnorm(n),
  t1_a = rbinom(n, 1, 0.5),
  
  # time 2
  t2_l = rnorm(n),
  t2_a = rbinom(n, 1, 0.5),
  t2_y = rnorm(n)
)

scm_auto <- margot_scm_from_df(df)
print(scm_auto)

# show automatic exogenous mapping
cat("\nAutomatic exogenous mapping:\n")
for (node in scm_auto$nodes) {
  cat(sprintf("  %s -> %s\n", node, scm_auto$U[[node]]))
}

# Example 4: Complex model with feedback
# --------------------------------------

cat("\n\nExample 4: Complex model with treatment-confounder feedback\n")
cat("===========================================================\n\n")

feedback_dsl <- "
U = U_l, U_a, U_y
k1 = t0_l ~ U_l
k1 = t0_a ~ t0_l + U_a
k1 = t0_y ~ t0_a + t0_l + U_y
k2 = t1_l ~ t0_y + t0_a + U_l         # y affects future l (feedback)
k2 = t1_a ~ t1_l + t0_y + U_a         # y affects future a
k2 = t1_y ~ t1_a + t1_l + t0_y + U_y
"

scm_feedback <- margot_scm_from_dsl(feedback_dsl)
print(scm_feedback)

# Example 5: Extract information for analysis
# -------------------------------------------

cat("\n\nExample 5: Extracting information for analysis\n")
cat("==============================================\n\n")

# get time indices
time_indices <- margot_scm_time_indices(scm_feedback)
cat("Time indices:\n")
print(time_indices)

# get shifts for specific node
shifts <- margot_scm_get_shifts(scm, "t1_a")
cat("\nAvailable shifts for t1_a:\n")
print(names(shifts))

# Example 6: LaTeX output for manuscript
# --------------------------------------

cat("\n\nExample 6: LaTeX output for manuscript\n")
cat("======================================\n\n")

cat(margot_scm_as_latex(scm))

# Example 7: Integration with LMTP workflow
# -----------------------------------------

cat("\n\nExample 7: Integration with LMTP workflow\n")
cat("=========================================\n\n")

# create SCM for LMTP analysis
lmtp_dsl <- "
U = U_w, U_a, U_y
k1 = w1 ~ U_w
k1 = w2 ~ U_w  
k2 = a ~ w1 + w2 + U_a
k3 = y ~ a + w1 + w2 + U_y
"

scm_lmtp <- margot_scm_from_dsl(lmtp_dsl)

# add LMTP-style shift
scm_lmtp <- margot_scm_add_shift(
  scm_lmtp,
  node = "a",
  name = "modified_treatment_policy",
  fun = function(a, data, ...) {
    # example: increase treatment by 0.2 for those below median of w1
    w1_median <- median(data[["w1"]], na.rm = TRUE)
    ifelse(data[["w1"]] < w1_median, pmin(a + 0.2, 1), a)
  }
)

cat("SCM ready for LMTP analysis:\n")
print(scm_lmtp)

# extract shift function for use with lmtp package
mtp_shift <- margot_scm_get_shifts(scm_lmtp, "a")$modified_treatment_policy
cat("\nShift function extracted and ready for lmtp::lmtp_tmle()\n")

# Example 8: Create reporting-ready SCM
# -------------------------------------

cat("\n\nExample 8: Reporting-ready SCM output\n")
cat("=====================================\n\n")

# wrapper function for clean reporting
margot_report_scm <- function(scm, title = NULL) {
  if (!is.null(title)) {
    cat(title, "\n")
    cat(paste(rep("=", nchar(title)), collapse = ""), "\n\n")
  }
  
  # basic info
  cat("Number of nodes:", length(scm$nodes), "\n")
  cat("Time periods:", paste(sort(unique(scm$k)), collapse = ", "), "\n")
  
  # count interventions
  n_shifts <- sum(sapply(scm$shift, length))
  if (n_shifts > 0) {
    cat("Interventions defined:", n_shifts, "\n")
  }
  
  cat("\n")
  print(scm)
  
  # return LaTeX for easy inclusion
  invisible(margot_scm_as_latex(scm))
}

# use the reporting function
latex_code <- margot_report_scm(scm, "Final SCM for Analysis")

cat("\n\nLaTeX code saved for manuscript inclusion\n")