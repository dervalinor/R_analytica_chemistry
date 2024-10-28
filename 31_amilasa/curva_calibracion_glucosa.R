# Load required libraries
library(ggplot2)

# Organize the data into a data frame
glucose_data <- data.frame(
  Volume = c(0.25, 0.50, 0.75, 1.00, 1.25, 1.50),
  Absorbance = c(0.104, 0.034, 0.114, 0.236, 0.288, 0.422)
)

# Calculate the concentration for each sample
glucose_data$Concentration <- glucose_data$Volume * 4 / 5  # 4 g/L is the stock concentration

glucose_data$Concentration

# Fit the linear regression model
model <- lm(Absorbance ~ Concentration, data = glucose_data)

# Calculate R-squared
r_squared <- summary(model)$r.squared

# Get the coefficients of the linear equation
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Create the calibration curve plot
calibration_plot <- ggplot(glucose_data, aes(x = Concentration, y = Absorbance)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Glucose Calibration Curve",
    x = "Concentration (g/L)",
    y = "Absorbance (540 nm)"
  ) +
  theme_minimal() +
  annotate(
    "text",
    x = max(glucose_data$Concentration) * 0.3,
    y = max(glucose_data$Absorbance) * 0.8,
    label = sprintf("y = %.4fx + %.4f\nR² = %.4f", slope, intercept, r_squared)
  )

# Display the plot
print(calibration_plot)

# Print the summary statistics
print(summary(model))
