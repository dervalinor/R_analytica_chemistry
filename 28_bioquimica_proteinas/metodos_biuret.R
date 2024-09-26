# Cargar las bibliotecas necesarias
library(ggplot2)

# Paso 1: Definir los datos
volumenes <- c(0, 0.1, 0.3, 0.5, 0.7, 0.9, 1.2, 1.5)
absorbancias <- c(0, 0.001, 0.014, 0.005, -0.002, 0.047, 0.009, 0.003)

# Paso 2: Calcular las concentraciones
concentracion_patron <- 5 # mg/ml
concentraciones <- (volumenes * concentracion_patron) / 2 # Dividimos por 2 porque el volumen total es 2 ml

# Paso 3: Crear el dataframe para la curva de calibración
datos_calibracion <- data.frame(
  Concentracion = concentraciones,
  Absorbancia = absorbancias
)

# Paso 4: Ajustar el modelo lineal
modelo <- lm(Absorbancia ~ Concentracion, data = datos_calibracion)

# Paso 5: Obtener R² y coeficientes para la ecuación
r_cuadrado <- format(summary(modelo)$r.squared, digits = 4)
intercepto <- format(coef(modelo)[1], digits = 4, scientific = FALSE)
pendiente <- format(coef(modelo)[2], digits = 4, scientific = FALSE)

# Crear la ecuación como texto
ecuacion <- paste("y =", pendiente, "x +", intercepto)
r2_texto <- paste("R² =", r_cuadrado)

# Paso 6: Crear la gráfica de la curva de calibración con la ecuación y R²
grafica <- ggplot(datos_calibracion, aes(x = Concentracion, y = Absorbancia)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Curva de Calibración - Método de Biuret",
       x = "Concentración (mg/ml)",
       y = "Absorbancia") +
  theme_minimal() +
  annotate("text", x = max(concentraciones) * 0.7, y = max(absorbancias) * 0.9, 
           label = ecuacion, hjust = 0) +
  annotate("text", x = max(concentraciones) * 0.7, y = max(absorbancias) * 0.8, 
           label = r2_texto, hjust = 0)

# Mostrar la gráfica
print(grafica)

# Paso 7: Imprimir los coeficientes del modelo
cat("Intercepto:", intercepto, "\n")
cat("Pendiente:", pendiente, "\n")
cat("R²:", r_cuadrado, "\n")

# Paso 8: Calcular la concentración de las muestras problema
absorbancia_muestra1 <- -0.014
absorbancia_muestra2 <- 0.005

concentracion_muestra1 <- (absorbancia_muestra1 - as.numeric(intercepto)) / as.numeric(pendiente)
concentracion_muestra2 <- (absorbancia_muestra2 - as.numeric(intercepto)) / as.numeric(pendiente)

cat("Concentración de la muestra 1:", round(concentracion_muestra1, 4), "mg/ml\n")
cat("Concentración de la muestra 2:", round(concentracion_muestra2, 4), "mg/ml\n")

# Paso 9: Calcular la concentración en la solución original
volumen_muestra1 <- 0.5
volumen_muestra2 <- 1.0
factor_dilucion1 <- 2 / volumen_muestra1
factor_dilucion2 <- 2 / volumen_muestra2

concentracion_original1 <- concentracion_muestra1 * factor_dilucion1
concentracion_original2 <- concentracion_muestra2 * factor_dilucion2

cat("Concentración en la solución original 1:", round(concentracion_original1, 4), "mg/ml\n")
cat("Concentración en la solución original 2:", round(concentracion_original2, 4), "mg/ml\n")