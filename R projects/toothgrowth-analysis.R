# ToothGrowth Analysis
# ====================
# This script analyzes the ToothGrowth dataset using t-tests, linear models,
# estimated marginal means, and visualizations.

# Load required libraries
library(readr)     # for reading CSV files
library(dplyr)     # for data manipulation
library(ggplot2)   # for visualization
library(emmeans)   # for estimated marginal means

# ---------------------------------------------------------
# Load and view the dataset
# ---------------------------------------------------------

# Read data from CSV
tooth.df <- read_csv("ToothGrowth.csv")

# Optional: View the dataset in a spreadsheet-style window
View(tooth.df)

# ---------------------------------------------------------
# One-sample t-test for overall mean tooth length
# ---------------------------------------------------------

# Run a one-sample t-test on len (vs. population mean = 0)
tooth.t <- t.test(len ~ 1, data = tooth.df)

# Display structure of the test result (for learning purposes)
tooth.t %>% str()

# Summarize t-test results for plotting
tooth.toPlot <- tibble(
  xbar = tooth.t$estimate,
  se = tooth.t$stderr,
  lwr = tooth.t$conf.int[1],
  upr = tooth.t$conf.int[2]
)

# Plot mean with standard error bar
ggplot(tooth.toPlot, aes(x = 1, y = xbar, ymin = xbar - se, ymax = xbar + se)) +
  geom_point(size = 3) +
  geom_errorbar(linewidth = 1, width = 0.1) +
  xlim(0, 2) +
  labs(title = "Mean Tooth Length with SE", y = "Length", x = "") +
  theme_minimal()

# ---------------------------------------------------------
# Compare mean tooth length by supplement type
# ---------------------------------------------------------

# Fit a linear model with supplement type as predictor
supp_model <- lm(len ~ supp, data = tooth.df)

# Get estimated marginal means by supplement type
supp.emm <- emmeans(supp_model, ~supp, level = 0.95)

# Convert emmeans output to data frame for plotting
supp.out <- as.data.frame(supp.emm)

# Plot mean estimates by supplement with confidence intervals
ggplot(supp.out, aes(x = supp, y = emmean, ymin = lower.CL, ymax = upper.CL)) +
  geom_point(size = 3) +
  geom_errorbar(linewidth = 1, width = 0.1, colour = "blue") +
  labs(title = "Tooth Length by Supplement Type",
       y = "Estimated Mean Tooth Length", x = "Supplement Type") +
  theme_minimal()

# ---------------------------------------------------------
# Compare mean tooth length by dose level
# ---------------------------------------------------------

# Convert dose to a factor (categorical)
tooth.df <- mutate(tooth.df, dose = as.factor(dose))

# Fit a linear model with dose as categorical predictor
dose_model <- lm(len ~ dose, data = tooth.df)

# Get estimated marginal means and pairwise comparisons between doses
dose.out <- dose_model %>%
  emmeans(~dose, level = 0.95) %>%
  pairs(infer = TRUE) %>%
  as_tibble()

# Plot pairwise comparisons of dose levels with confidence intervals
ggplot(dose.out, aes(y = contrast, x = estimate,
                     xmin = lower.CL, xmax = upper.CL)) +
  geom_point(size = 3) +
  geom_errorbar(linewidth = 1, width = 0.1) +
  labs(title = "Pairwise Differences in Tooth Length by Dose",
       x = "Difference in Means", y = "Dose Contrast") +
  theme_minimal()
