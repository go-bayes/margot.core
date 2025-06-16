# Examples of using the SCM functionality in margot.core

library(margot.core)

# Example 1: Automatic mode from data frame
# ------------------------------------------

# create example data following lmtp naming convention
set.seed(123)
fake <- data.frame(
  # baseline
  t0_b = rbinom(100, 1, 0.5),
  t0_l = rnorm(100),
  
  # time 1
  t1_l = rnorm(100),
  t1_a = rbinom(100, 1, 0.4),
  
  # time 2  
  t2_y = rnorm(100)
)

# create scm from data structure
scm1 <- scm_from_df(fake)
print(scm1)

# get latex representation
cat("\nLaTeX representation:\n")
cat(as_latex(scm1))
cat("\n\n")

# Example 2: Explicit equations
# -----------------------------

spec <- "t0_b ~ U1 + U2; t1_a ~ U1; t2_y ~ U2"
scm2 <- scm_from_spec(spec)
print(scm2)

# Example 3: More complex structure
# ---------------------------------

# create data with multiple baseline variables and outcomes
complex_data <- data.frame(
  # baseline confounders
  t0_b1 = rnorm(100),
  t0_b2 = rnorm(100),
  t0_b3 = rbinom(100, 1, 0.3),
  
  # baseline exposure and confounder
  t0_l = rnorm(100),
  t0_a = rbinom(100, 1, 0.5),
  
  # time 1
  t1_l = rnorm(100),
  t1_a = rbinom(100, 1, 0.5),
  t1_y1 = rnorm(100),
  t1_y2 = rnorm(100),
  
  # time 2
  t2_l = rnorm(100),
  t2_a = rbinom(100, 1, 0.5),
  t2_y1 = rnorm(100),
  t2_y2 = rnorm(100)
)

scm3 <- scm_from_df(complex_data)
summary(scm3)

# Example 4: Custom dependency structure via equations
# ----------------------------------------------------

# m-bias structure
m_bias_spec <- paste(
  "U1 ~ ;",
  "U2 ~ ;", 
  "X ~ U1;",
  "Y ~ U2;",
  "M ~ U1 + U2",
  sep = " "
)

scm_mbias <- scm_from_spec(m_bias_spec)
print(scm_mbias)

# Example 5: Time-varying confounding with feedback
# -------------------------------------------------

feedback_spec <- paste(
  "t0_l ~ U_l;",
  "t0_a ~ t0_l + U_a;",
  "t0_y ~ t0_a + t0_l + U_y;",
  "t1_l ~ t0_y + t0_a + U_l;",  # feedback from y to l
  "t1_a ~ t1_l + t0_y + U_a;",   # feedback from y to a
  "t1_y ~ t1_a + t1_l + t0_y + U_y",
  sep = " "
)

scm_feedback <- scm_from_spec(feedback_spec)
print(scm_feedback)

# show latex for manuscript
cat("\nLaTeX for feedback model:\n")
cat(as_latex(scm_feedback))

# Example 6: Working with SCM objects
# ------------------------------------

# check if object is an scm
is_scm(scm1)  # TRUE
is_scm(fake)  # FALSE

# get summary statistics
summary(scm3)

# extract specific information
n_nodes <- length(scm3$nodes)
cat("\nNumber of nodes:", n_nodes, "\n")

# find nodes with no parents (exogenous)
exogenous_nodes <- names(which(sapply(scm3$pa, length) == 0))
cat("Exogenous nodes:", paste(exogenous_nodes, collapse = ", "), "\n")

# find most connected node
in_degrees <- sapply(scm3$pa, length)
most_connected <- names(which.max(in_degrees))
cat("Most connected node:", most_connected, 
    "with", max(in_degrees), "parents\n")