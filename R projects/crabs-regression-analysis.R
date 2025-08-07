# -----------------------------
# 🦀 Dungeness Crab Premolt Analysis
# -----------------------------
# This analysis explores the relationship between premolt and postmolt weights
# in Dungeness crabs using regression modeling and visualization.
# Techniques used: scatter plots, confidence interval prediction, linear modeling.
# -----------------------------

# --- Load Required Libraries ---
library(readr)      # For reading CSV files
library(ggplot2)    # For plotting
library(dplyr)      # For data manipulation
library(tibble)     # For working with tibbles
library(magrittr)   # For using the pipe (%>%) operator

# --- Load Dataset ---
crabs.df <- read_csv("dungeness-crabs.csv")
View(crabs.df)  # View the full dataset

# --- Initial Scatter Plot with Regression Line ---
ggplot(crabs.df, aes(x = Postmolt, y = Premolt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Premolt vs Postmolt Mass") +
  xlab("Postmolt Mass") +
  ylab("Premolt Mass")

# --- Basic Summary Statistics ---
nrow(crabs.df)       # Number of observations
summary(crabs.df)    # Summary statistics for each variable

# --- Fit Linear Model ---
crabs.fit <- lm(Premolt ~ Postmolt, data = crabs.df)

# --- Predict Premolt Mass for Specific Postmolt Values ---
to.predict <- tibble(Postmolt = c(125, 130))
predict(crabs.fit, newdata = to.predict, interval = "confidence", level = 0.95)

# --- Visualize Regression Line with Confidence Interval ---
ggplot(crabs.df, aes(x = Postmolt, y = Premolt)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.95, se = TRUE) +
  ggtitle("Linear Regression with 95% CI")

# --- Display R-squared Value ---
summary(crabs.fit)$r.squared

# --- Create a Sequence of Postmolt Values for Prediction ---
to.predict <- summarise(crabs.df, min = min(Postmolt), max = max(Postmolt)) %>%
  summarise(Postmolt = seq(min, max, length.out = 100))

# --- Generate Confidence Intervals from Model Predictions ---
crabs.predict <- predict(crabs.fit, newdata = to.predict, interval = "confidence") %>%
  bind_cols(to.predict)

# --- Final Visualization: Regression Line with Ribbon ---
ggplot(crabs.df, aes(x = Postmolt, y = Premolt)) +
  geom_point() +
  geom_ribbon(
    data = crabs.predict,
    inherit.aes = FALSE,  # Prevent inheriting y = Premolt from the main ggplot
    aes(x = Postmolt, ymin = lwr, ymax = upr),
    fill = "lightblue", alpha = 0.4
  ) +
  geom_line(
    data = crabs.predict,
    aes(x = Postmolt, y = fit),
    color = "red", size = 1
  ) +
  ggtitle("Fitted Regression Line with 95% Confidence Interval") +
  xlab("Postmolt Mass") +
  ylab("Premolt Mass")
