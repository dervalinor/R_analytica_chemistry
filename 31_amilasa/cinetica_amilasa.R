# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Datos de concentración de glucosa (g/L) y absorbancia promedio
glucose_conc <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2)
absorbance <- c(0.001, 0.104, 0.034, 0.114, 0.236, 0.288, 0.422)

# 1. Ajustar el modelo de regresión lineal para la curva de calibración
calibration_model <- lm(absorbance ~ glucose_conc)
summary(calibration_model)

# Extraer los coeficientes del modelo
intercept_calib <- coef(calibration_model)[1]
slope_calib <- coef(calibration_model)[2]
r_squared <- summary(calibration_model)$r.squared

# Graficar la curva de calibración
glucose_calibration_plot <- ggplot(data.frame(glucose_conc, absorbance), aes(x = glucose_conc, y = absorbance)) +
  geom_point(size = 3, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Curva de Calibración para Glucosa",
       x = "Concentración de Glucosa (g/L)",
       y = "Absorbancia a 540 nm") +
  annotate("text", x = 0.8, y = 0.35, 
           label = paste("y =", round(slope_calib, 4), "x +", round(intercept_calib, 4), 
                         "\nR^2 =", round(r_squared, 4)), color = "red")
print(glucose_calibration_plot)

# Datos de absorbancia para las muestras de amilasa salival (mediciones duplicadas)
absorbance_amilase <- c(0.001, 0.104, 0.034, 0.114, 0.236, 0.288, 0.422, 0.516, 0.652)

# 2. Convertir absorbancia a concentración de glucosa usando la ecuación de calibración
glucose_concentration_amilase <- (absorbance_amilase - intercept_calib) / slope_calib

# Tabla de resultados: Tubos, absorbancia y concentración de glucosa
resultados <- data.frame(
  Tubo = 1:9,
  Absorbancia = absorbance_amilase,
  Concentracion_Glucosa = glucose_concentration_amilase
)
print(resultados)

# Tiempos correspondientes a cada lectura en minutos
time <- c(0, 5, 10, 15, 20, 25, 30, 35, 40)

# 3. Calcular la velocidad de reacción inicial (diferencia de concentración en intervalos de tiempo)
reaction_rate <- diff(glucose_concentration_amilase) / diff(time)

# 4. Crear una tabla de concentraciones de sustrato y velocidades para análisis de Lineweaver-Burk
substrate_conc <- glucose_concentration_amilase[-1]  # Excluye el primer valor para los cálculos de velocidad
reciprocal_conc <- 1 / substrate_conc
reciprocal_rate <- 1 / reaction_rate

# 5. Ajustar el modelo Lineweaver-Burk (regresión lineal de 1/[S] vs. 1/v)
lb_model <- lm(reciprocal_rate ~ reciprocal_conc)
summary(lb_model)

# Extraer Vmax, Km y R²
intercept_lb <- coef(lb_model)[1]
slope_lb <- coef(lb_model)[2]
Vmax <- 1 / intercept_lb
Km <- slope_lb * Vmax
r_squared_lb <- summary(lb_model)$r.squared  # Calcular R² del modelo Lineweaver-Burk

# Mostrar resultados de Vmax y Km
cat("Vmax:", round(Vmax, 4), "\nKm:", round(Km, 4), "\n")

# Graficar el gráfico de Lineweaver-Burk con R² incluido
lineweaver_burk_plot <- ggplot(data.frame(reciprocal_conc, reciprocal_rate), 
                               aes(x = reciprocal_conc, y = reciprocal_rate)) +
  geom_point(size = 4, color = "#1f78b4") +  # Cambiar tamaño y color de puntos
  geom_smooth(method = "lm", color = "#e31a1c", se = FALSE, linetype = "dashed") +  # Línea ajustada con estilo
  labs(title = "Gráfico de Lineweaver-Burk",
       subtitle = "Representación Lineal de la Ecuación de Michaelis-Menten",
       x = expression("1/[S] (1/(g/L))"), 
       y = expression("1/v (1/(velocidad de reacción))")) +
  theme_minimal(base_size = 15) +  # Tema minimalista y ajuste de tamaño
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 12, color = "grey30"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = 0.002, y = 0.05,
           label = paste("y =", round(slope_lb, 4), "x +", round(intercept_lb, 4),
                         "\nR² =", round(r_squared_lb, 4)), color = "#e31a1c", size = 5, hjust = -0.1) +
  geom_hline(yintercept = 0, color = "grey50", linetype = "dotted") +  # Línea horizontal en el origen
  geom_vline(xintercept = 0, color = "grey50", linetype = "dotted")    # Línea vertical en el origen

# Mostrar el gráfico
print(lineweaver_burk_plot)

# Resultado final de Km y Vmax
list(Vmax = Vmax, Km = Km)
