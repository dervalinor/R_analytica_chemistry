# Importar datos
datos <- data.frame(
  Tratamiento = rep(c("Droga A", "Droga B", "Placebo"), times = 7),
  Bloque = rep(1:7, each = 3),
  Respuesta = c(6.0, 6.1, 5.4, 4.8, 6.9, 4.0, 6.9, 6.5, 7.0, 6.4, 5.6, 5.8, 5.5, 3.9, 3.5, 9.0, 7.0, 7.6, 6.8, 5.4, 5.5)
)

# Ajustar el modelo ANOVA
modelo <- aov(Respuesta ~ Tratamiento + Bloque, data = datos)

# Evaluación de supuestos
# 1. Normalidad
par(mfrow = c(1, 2))  # Dividir la ventana gráfica en dos paneles
plot(density(residuals(modelo)))  # Gráfico de densidad de residuos
qqnorm(residuals(modelo))  # Gráfico de cuantiles normales
qqline(residuals(modelo), col = "red")  # Línea de referencia
shapiro.test(residuals(modelo))  # Prueba de Shapiro-Wilk

# 2. Homocedasticidad (homogeneidad de varianzas)
plot(modelo, which = 1)  # Gráfico de residuos vs. valores ajustados
bartlett.test(Respuesta ~ Tratamiento, data = datos)  # Prueba de Levene
#ver que caso se usa el Bartlett ya que no se viola el principio de 
#normalidad

# 3. Independencia
# Asumimos que las observaciones son independientes debido al diseño experimental

# Diagnóstico general
par(mfrow = c(2, 2))  # Dividir la ventana gráfica en 4 paneles
plot(modelo)  # Gráficos de diagnóstico

# Interpretación de los resultados
# Analizar los gráficos y las pruebas estadísticas para determinar si los supuestos se cumplen o no