# Install and load required packages
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Data input
S <- c(5, 10, 20, 50, 100, 200)  # Substrate concentration
V <- c(22, 39, 65, 102, 120, 135)  # Initial velocity

# Create data frame
data <- data.frame(S = S, V = V)

# 1. Michaelis-Menten Plot with equation
# Fit non-linear model
mm_model <- nls(V ~ (Vm * S)/(Km + S), 
                data = data,
                start = list(Vm = 150, Km = 30))

Vm_mm <- coef(mm_model)["Vm"]
Km_mm <- coef(mm_model)["Km"]

mm_eq <- sprintf("V = %.1f × [S] / (%.1f + [S])", Vm_mm, Km_mm)

mm_plot <- ggplot(data, aes(x = S, y = V)) +
  geom_point(size = 3) +
  geom_smooth(method = "nls", 
              formula = y ~ (Vm * x)/(Km + x), 
              method.args = list(start = list(Vm = 150, Km = 30)),
              se = FALSE) +
  labs(title = "Michaelis-Menten Plot",
       x = "Substrate Concentration [S] (µmol/L)",
       y = "Velocity V (µmol/L·min⁻¹)") +
  annotate("text", x = max(S)/2, y = max(V), 
           label = mm_eq, size = 4) +
  theme_minimal()

#show Michaelis-Menten Plot
print(mm_plot)

# 2. Lineweaver-Burk Plot with equation
data$inv_S <- 1/data$S
data$inv_V <- 1/data$V

lb_model <- lm(inv_V ~ inv_S, data = data)
slope_lb <- coef(lb_model)[2]
intercept_lb <- coef(lb_model)[1]

Km_lb <- slope_lb/intercept_lb
Vmax_lb <- 1/intercept_lb

lb_eq <- sprintf("1/V = %.4f × (1/[S]) + %.4f", slope_lb, intercept_lb)

lb_plot <- ggplot(data, aes(x = inv_S, y = inv_V)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Lineweaver-Burk Plot",
       x = "1/[S] (L/µmol)",
       y = "1/V (L·min/µmol)") +
  annotate("text", x = mean(data$inv_S), y = max(data$inv_V), 
           label = lb_eq, size = 4) +
  theme_minimal()

#Show Lineweaver-Burk Plot
print(lb_plot)

# 3. Eadie-Hofstee Plot with equation
data$V_S <- data$V/data$S

eh_model <- lm(V ~ V_S, data = data)
slope_eh <- coef(eh_model)[2]
intercept_eh <- coef(eh_model)[1]

Km_eh <- -slope_eh
Vmax_eh <- intercept_eh

eh_eq <- sprintf("V = %.1f - %.1f × (V/[S])", intercept_eh, -slope_eh)

eh_plot <- ggplot(data, aes(x = V_S, y = V)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Eadie-Hofstee Plot",
       x = "V/[S]",
       y = "V (µmol/L·min⁻¹)") +
  annotate("text", x = mean(data$V_S), y = max(data$V), 
           label = eh_eq, size = 4) +
  theme_minimal()

#Show Eadie-Hofstee Plot 
print(eh_plot)

# Print results with equations
cat("Regression Equations and Parameters:\n\n")

cat("1. Michaelis-Menten Equation:\n")
cat(mm_eq, "\n")
cat("Vmax =", round(Vm_mm, 2), "µmol/L·min⁻¹\n")
cat("Km =", round(Km_mm, 2), "µmol/L\n\n")

cat("2. Lineweaver-Burk Equation:\n")
cat(lb_eq, "\n")
cat("Vmax =", round(Vmax_lb, 2), "µmol/L·min⁻¹\n")
cat("Km =", round(Km_lb, 2), "µmol/L\n\n")

cat("3. Eadie-Hofstee Equation:\n")
cat(eh_eq, "\n")
cat("Vmax =", round(Vmax_eh, 2), "µmol/L·min⁻¹\n")
cat("Km =", round(Km_eh, 2), "µmol/L\n")