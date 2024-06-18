# Paso 1: Definir los datos del experimento

# Velocidad de consumo de oxígeno
velocidad <- c(13.6, 8.9, 10.4, 5.5, 18.8, 9.7, 13.4, 14.5)

# Factores
concentracion <- factor(rep(c("100%", "50%"), each=4))
especie <- factor(rep(c("A.s", "A.d"), each=2, times=2))

# Crear un data frame
data <- data.frame(velocidad, concentracion, especie)

# Mostrar el data frame
print(data)

# Paso 2: Realizar el análisis de varianza (ANOVA)
anova_result <- aov(velocidad ~ concentracion * especie, data=data)
summary(anova_result)

# Paso 3: Calcular los efectos principales y la interacción
# Convertir los factores a -1 y 1 para cálculos de efectos
concentracion_num <- ifelse(concentracion == "100%", 1, -1)
especie_num <- ifelse(especie == "A.s", 1, -1)
interaccion_num <- concentracion_num * especie_num

# Efecto principal de la concentración
Aeff <- mean(velocidad * concentracion_num) / 2
# Efecto principal de la especie
Beff <- mean(velocidad * especie_num) / 2
# Efecto de la interacción
ABeff <- mean(velocidad * interaccion_num) / 2

# Mostrar los efectos
cat("Efecto de la concentración (A):", Aeff, "\n")
cat("Efecto de la especie (B):", Beff, "\n")
cat("Efecto de la interacción (AB):", ABeff, "\n")

# Paso 4: Calcular la contribución porcentual de los efectos
SST <- sum((velocidad - mean(velocidad))^2)
SSA <- sum((Aeff * concentracion_num)^2)
SSB <- sum((Beff * especie_num)^2)
SSAB <- sum((ABeff * interaccion_num)^2)

# Contribución porcentual
contrib_A <- SSA / SST * 100
contrib_B <- SSB / SST * 100
contrib_AB <- SSAB / SST * 100

# Mostrar la contribución porcentual
cat("Contribución porcentual de A:", contrib_A, "%\n")
cat("Contribución porcentual de B:", contrib_B, "%\n")
cat("Contribución porcentual de AB:", contrib_AB, "%\n")

# Paso 5: Crear un gráfico de interacciones usando ggplot2
library(ggplot2)
ggplot(data, aes(x=concentracion, y=velocidad, color=especie, group=especie)) +
  geom_point() +
  geom_line() +
  labs(title="Gráfico de Interacciones", x="Concentración", y="Velocidad de consumo de oxígeno (mg/L)") +
  theme_minimal()
