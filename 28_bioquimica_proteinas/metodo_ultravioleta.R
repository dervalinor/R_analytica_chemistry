# Cargar bibliotecas necesarias
library(ggplot2)

# Datos de volumen y absorbancia

volumenes <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2)
absorbancias <- c(0.0007, 0.0598, 0.0570, 0.0580, 0.0582, 0.0620, 0.0553)

# Calcular concentraciones (mg/ml)
concentracion_patron <- 1  # mg/ml
concentraciones <- (volumenes / 5) * concentracion_patron

# Crear dataframe
datos <- data.frame(
  Concentracion = concentraciones,
  Absorbancia = absorbancias
)

# Crear modelo lineal
modelo <- lm(Absorbancia ~ Concentracion, data = datos)

# Obtener coeficientes y R²
pendiente <- coef(modelo)[2]
intercepto <- coef(modelo)[1]
r_cuadrado <- summary(modelo)$r.squared

# Función para crear la ecuación como texto
crear_ecuacion <- function(pendiente, intercepto, r_cuadrado) {
  paste0("y = ", round(pendiente, 4), "x + ", round(intercepto, 4),
         "\nR² = ", round(r_cuadrado, 4))
}

# Crear gráfico con ggplot2
grafico <- ggplot(datos, aes(x = Concentracion, y = Absorbancia)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Método de absorción en el ultravioleta",
       x = "Concentración (mg/ml)",
       y = "Absorbancia") +
  theme_minimal() +
  annotate("text", x = max(concentraciones) * 0.7, y = min(absorbancias),
           label = crear_ecuacion(pendiente, intercepto, r_cuadrado),
           hjust = 0, vjust = 0)

# Mostrar el gráfico
print(grafico)

# Función para calcular concentración a partir de absorbancia
calcular_concentracion <- function(absorbancia) {
  (absorbancia - intercepto) / pendiente
}

# Calcular concentraciones de las muestras problema
absorbancia_7 <- 0.0502
absorbancia_8 <- 0.0582

concentracion_7 <- calcular_concentracion(absorbancia_7)
concentracion_8 <- calcular_concentracion(absorbancia_8)

# Ajustar por el volumen usado (0.5 ml para muestra 7, 1.0 ml para muestra 8)
concentracion_7_ajustada <- concentracion_7 * (5 / 0.5)
concentracion_8_ajustada <- concentracion_8 * (5 / 1.0)

print(paste("Concentración de la muestra 7:", round(concentracion_7_ajustada, 4), "mg/ml"))
print(paste("Concentración de la muestra 8:", round(concentracion_8_ajustada, 4), "mg/ml"))