# Install and load required packages
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Create data frame with concentration and absorbance values
concentration <- c(0.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.5)  # From substrate concentration in first table
absorbance <- c(0.104, 0.034, 0.114, 0.236, 0.288, 0.422, 0.516, 0.652)  # From second table, excluding blank

# Create the data frame
data <- data.frame(
  Concentration = concentration,
  Absorbance = absorbance
)

# Fit linear regression model
model <- lm(Absorbance ~ Concentration, data = data)

# Get R-squared value and equation coefficients
r_squared <- summary(model)$r.squared
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Create calibration curve plot
calibration_plot <- ggplot(data, aes(x = Concentration, y = Absorbance)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Calibration Curve for Amylase Reaction",
    x = "Concentration (mg/mL)",
    y = "Absorbance (540 nm)"
  ) +
  theme_minimal() +
  annotate(
    "text",
    x = max(concentration) * 0.3,
    y = max(absorbance) * 0.8,
    label = sprintf("y = %.4fx + %.4f\nR² = %.4f", slope, intercept, r_squared)
  )

# Print summary statistics
print(summary(model))

# Display the plot
print(calibration_plot)

# Create results table
results <- data.frame(
  "Concentration (mg/mL)" = concentration,
  "Absorbance (540 nm)" = absorbance,
  "Predicted_Absorbance" = predict(model)
)

# Print results table
print(results)