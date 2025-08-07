# -----------------------------
# 🐧 Penguin Flipper Length Analysis
# -----------------------------
# This analysis investigates how flipper lengths vary among different penguin species
# using data visualization and statistical inference in R.
# Techniques used: histograms, density plots, boxplots, and ANOVA.

# --- Load and Prepare the Data ---
library(readr)

# Load the penguin dataset
penguins.df <- read_csv("penguins.csv")

# Optional: View the dataframe in the RStudio viewer
View(penguins.df)

# Correct encoding: standardize species name "Adelie" to "Adélie" for display clarity
penguins.df$species <- ifelse(penguins.df$species == "Adelie", "Adélie", penguins.df$species)


# --- Explore Distribution of Flipper Lengths ---
library(ggplot2)

# Basic histogram of flipper lengths
ggplot(penguins.df, aes(x = flipper_length_mm)) +
  geom_histogram(fill = "skyblue", colour = "black") +
  labs(title = "Histogram of Flipper Lengths",
       x = "Flipper Length (mm)",
       y = "Count")

# Histogram with more bins for better granularity
ggplot(penguins.df, aes(x = flipper_length_mm)) + 
  geom_histogram(bins = 25, fill = "skyblue", colour = "black") + 
  labs(title = "Histogram of Flipper Lengths (25 Bins)",
       x = "Flipper Length (mm)",
       y = "Count")

# Density plot showing the smooth distribution curve
ggplot(penguins.df, aes(x = flipper_length_mm)) + 
  geom_density() +
  labs(title = "Density Plot of Flipper Lengths",
       x = "Flipper Length (mm)",
       y = "Density")


# --- Combine Histogram and Density Plots ---

# Histogram + Density overlay (with different y-scales)
ggplot(penguins.df, aes(x = flipper_length_mm)) +
  geom_histogram(aes(y = ..count..), fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1.2) +
  labs(title = "Histogram + Density (Separate Scales)",
       x = "Flipper Length (mm)",
       y = "Count / Density")

# Histogram + Density (normalized histogram to match density scale)
ggplot(penguins.df, aes(x = flipper_length_mm)) +
  geom_histogram(aes(y = after_stat(density)), fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1.2) +
  labs(title = "Histogram + Density (Same Scale)",
       x = "Flipper Length (mm)",
       y = "Density")

# Improved version: control number of bins and alignment
ggplot(penguins.df, aes(x = flipper_length_mm)) +
  geom_histogram(aes(y = after_stat(density)), bins = 8, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1.2) +
  labs(title = "Flipper Lengths with Matching Histogram and Density",
       x = "Flipper Length (mm)",
       y = "Density")

# 💡 Note:
# Originally, overlaying raw count histograms with density curves resulted in poor visuals.
# Switching to `after_stat(density)` normalizes the histogram and aligns both plots to the same y-scale.


# --- Calculate Group Means for Boxplot Overlay ---
grp.means <- aggregate(flipper_length_mm ~ species, data = penguins.df, FUN = mean)
print(grp.means)


# --- Boxplots by Species ---

# Basic boxplot showing flipper length distribution by species
ggplot(penguins.df, aes(x = species, y = flipper_length_mm)) + 
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of Flipper Lengths by Species",
       x = "Species",
       y = "Flipper Length (mm)")

# Enhanced boxplot with red triangle markers for group means
ggplot(penguins.df, aes(x = species, y = flipper_length_mm)) + 
  geom_boxplot(fill = "lightgreen") +
  geom_point(data = grp.means, aes(x = species, y = flipper_length_mm),
             colour = "red", size = 3, pch = 17) +
  labs(title = "Boxplot with Mean Flipper Lengths Highlighted",
       x = "Species",
       y = "Flipper Length (mm)")


# --- Statistical Inference: ANOVA ---

# Fit ANOVA model to test if species means are significantly different
anova.model <- aov(flipper_length_mm ~ species, data = penguins.df)

# Display ANOVA summary results
summary(anova.model)

# 📊 Interpretation:
# At a 5% significance level, we reject the null hypothesis that all penguin species
#
