# Load necessary library
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Data for concentrations of each species over time
time <- seq(0, 5, by = 0.5)  # Time in seconds

# Approximate concentration values based on the image for each substance
H2 <- c(3, 1.8, 1.2, 1, 1, 1, 1, 1, 1, 1, 1)     # Reactant H2
N2 <- c(1.5, 0.9, 0.6, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)  # Reactant N2
NH3 <- c(0, 0.6, 1.2, 1.5, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8)   # Product NH3

# Create a data frame
data <- data.frame(
  Time = rep(time, 3),
  Concentration = c(H2, N2, NH3),
  Species = factor(rep(c("H2", "N2", "NH3"), each = length(time)))
)

# Plot the data
ggplot(data, aes(x = Time, y = Concentration, color = Species, linetype = Species)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("H2" = "black", "N2" = "gray", "NH3" = "darkgray")) +
  labs(
    title = expression("Concentration vs. Time for the Reaction " ~ 3*H[2] + N[2] %->% 2*NH[3]),
    x = "Time (s)",
    y = "Concentration (moles/L)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "top"
  )
