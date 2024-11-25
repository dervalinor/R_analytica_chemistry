library(ggplot2)
library(patchwork)

# Generar datos de tiempo
time <- seq(0, 10, by = 0.1)

# Crear las gráficas A, B, C y D

# Gráfica A
X_A <- 0.5 * exp(-0.3 * time)     # Decrecimiento de X
W_A <- 0.1 * (1 - exp(-0.5 * time)) # Incremento de W
T_A <- 0.01 * time                # Incremento lineal de T

data_A <- data.frame(time, X = X_A, W = W_A, T = T_A)
plot_A <- ggplot(data_A, aes(x = time)) +
  geom_line(aes(y = X, color = "X")) +
  geom_line(aes(y = W, color = "W")) +
  geom_line(aes(y = T, color = "T")) +
  labs(title = "Gráfica A", y = "Concentración (M)", x = "Tiempo (t)") +
  scale_color_manual(values = c("X" = "black", "W" = "blue", "T" = "red")) +
  theme_minimal()

# Gráfica B
X_B <- 0.1 * exp(-0.5 * time)     # Decrecimiento rápido de X
W_B <- 0.1 * (1 - exp(-0.3 * time)) # Incremento lento de W
T_B <- 0.01 + 0.05 * (1 - exp(-0.2 * time)) # T alcanza un plateau

data_B <- data.frame(time, X = X_B, W = W_B, T = T_B)
plot_B <- ggplot(data_B, aes(x = time)) +
  geom_line(aes(y = X, color = "X")) +
  geom_line(aes(y = W, color = "W")) +
  geom_line(aes(y = T, color = "T")) +
  labs(title = "Gráfica B", y = "Concentración (M)", x = "Tiempo (t)") +
  scale_color_manual(values = c("X" = "black", "W" = "blue", "T" = "red")) +
  theme_minimal()

# Gráfica C
X_C <- 0.1 * exp(-0.4 * time)     # Decrecimiento rápido de X
W_C <- 0.5 * (1 - exp(-0.1 * time)) # Incremento lento de W
T_C <- 0.01 * (1 - exp(-0.6 * time)) # T aumenta rápidamente y luego se estabiliza

data_C <- data.frame(time, X = X_C, W = W_C, T = T_C)
plot_C <- ggplot(data_C, aes(x = time)) +
  geom_line(aes(y = X, color = "X")) +
  geom_line(aes(y = W, color = "W")) +
  geom_line(aes(y = T, color = "T")) +
  labs(title = "Gráfica C", y = "Concentración (M)", x = "Tiempo (t)") +
  scale_color_manual(values = c("X" = "black", "W" = "blue", "T" = "red")) +
  theme_minimal()

# Gráfica D
X_D <- 0.1 * exp(-0.6 * time)     # Decrecimiento rápido de X
W_D <- 0.5 * (1 - exp(-0.05 * time)) # Incremento lento de W
T_D <- 0.01 * exp(-0.1 * time)    # Decaimiento leve de T

data_D <- data.frame(time, X = X_D, W = W_D, T = T_D)
plot_D <- ggplot(data_D, aes(x = time)) +
  geom_line(aes(y = X, color = "X")) +
  geom_line(aes(y = W, color = "W")) +
  geom_line(aes(y = T, color = "T")) +
  labs(title = "Gráfica D", y = "Concentración (M)", x = "Tiempo (t)") +
  scale_color_manual(values = c("X" = "black", "W" = "blue", "T" = "red")) +
  theme_minimal()

# Organizar las gráficas en una cuadrícula 2x2
(plot_A | plot_B) / (plot_C | plot_D)

