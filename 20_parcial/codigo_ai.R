# Cargar librerías necesarias
library(car)
library(agricolae)
library(daewr)
library(dplyr)
library(ggplot2)

# Ingresar los datos
estados <- rep(c("Prefloración", "Floración", "Grano lechoso"), each = 8)
variedades <- rep(rep(c("Bacatá", "Cajicá", "Coyuse", "Nehuen"), each = 2), 3)
rendimientos <- c(9.55, 10.32, 11.76, 11.60, 7.16, 7.23, 10.68, 11.26,
                  8.61, 8.06, 11.05, 9.80, 6.92, 6.69, 6.96, 10.80,
                  6.77, 6.02, 7.26, 8.72, 6.68, 4.53, 9.92, 13.58)

# Crear el data frame
datos_avena <- data.frame(Estados = estados, Variedades = variedades, Rendimientos = rendimientos)

# a) Análisis descriptivo de los datos
# Estadísticas descriptivas por combinación de factores
descriptivo <- datos_avena %>%
  group_by(Estados, Variedades) %>%
  summarise(
    Media = mean(Rendimientos),
    Desviación_Estándar = sd(Rendimientos),
    Mínimo = min(Rendimientos),
    Máximo = max(Rendimientos)
  )

print(descriptivo)

# Análisis descriptivo visual
ggplot(datos_avena, aes(x = Variedades, y = Rendimientos, fill = Estados)) +
  geom_boxplot() +
  labs(title = "Rendimientos proteínicos por variedad y estado fisiológico",
       x = "Variedad de la avena",
       y = "Rendimientos proteínicos (%)") +
  theme_minimal()

# b) Modelo matemático y hipótesis del problema
# (Ya descrito anteriormente en el texto)

# c) ANOVA de dos vías y evaluación de supuestos
modelo_anova <- aov(Rendimientos ~ Estados * Variedades, data = datos_avena)
summary(modelo_anova)

# Evaluación de supuestos
residuos <- residuals(modelo_anova)

#Normalidad


# Prueba de normalidad de Shapiro-Wilk
shapiro_test <- shapiro.test(residuos)
print(shapiro_test)

# Prueba de homocedasticidad de Levene
levene_test <- leveneTest(Rendimientos ~ Estados * Variedades, data = datos_avena)
print(levene_test)

# Gráficos de residuos
par(mfrow = c(2, 2))
plot(modelo_anova)
par(mfrow = c(1, 1))

# d) Calcular las medias por tratamiento y crear nueva base de datos
medias_tratamiento <- datos_avena %>%
  group_by(Estados, Variedades) %>%
  summarise(Media = mean(Rendimientos))

print(medias_tratamiento)

# e) Crear gráfico de interacción
ggplot(medias_tratamiento, aes(x = Variedades, y = Media, group = Estados, color = Estados)) +
  geom_line() +
  geom_point() +
  labs(title = "Gráfico de Interacción de Rendimientos Proteínicos",
       x = "Variedad de la avena",
       y = "Rendimiento Proteínico (%)") +
  theme_minimal()
