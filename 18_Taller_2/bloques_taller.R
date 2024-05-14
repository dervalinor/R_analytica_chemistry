library(car)
library(agricolae)
library(daewr)

# Importar datos

Tratamiento = factor(rep(c("Droga A", "Droga B", "Placebo"), times = 7))
Bloque = factor(rep(1:7, each = 3))
Respuesta = c(6.0, 6.1, 5.4, 4.8, 6.9, 4.0, 6.9, 6.5, 7.0, 6.4, 5.6, 5.8, 5.5, 3.9, 3.5, 9.0, 7.0, 7.6, 6.8, 5.4, 5.5)

datos <- data.frame(Respuesta, Tratamiento, Bloque)

attach(datos)

# Ajustar el modelo ANOVA

#Prueba de aditividad
#Es necesario verificar si existen interacciones entre 
#los bloques y los tratamientos, para asi saber si elegir una diseño de 
#bloques al azar o un diseño factorial

#El modelo matemático adecuado para evaluar la interacción entre los bloques y los tratamientos sería el siguiente:
#Y_ijk = μ + τ_i + β_j + (τβ)_ij + ε_ijk
#Donde:
  
#Y_ijk es la respuesta (número de linfocitos) para el tratamiento i, bloque j y réplica k.
#μ es la media general.
#τ_i es el efecto del tratamiento i (Droga A, Droga B, Placebo).
#β_j es el efecto del bloque j.
#(τβ)_ij es el efecto de la interacción entre el tratamiento i y el bloque j.
#ε_ijk es el error aleatorio.

#Las hipótesis para evaluar la interacción entre los bloques y los tratamientos serían:

#  Hipótesis nula (H0):
#  No existe interacción entre los bloques y los tratamientos.
#(τβ)_ij = 0 para todos i, j

#Hipótesis alternativa (H1):
#  Existe interacción entre los bloques y los tratamientos.
#(τβ)_ij ≠ 0 para al menos un par i, j

prueba_aditividad = Tukey1df(datos)
resumen_aditividad = summary(prueba_aditividad)
resumen_aditividad

p_valor_no_aditividad <- resumen_aditividad$"Pr>F"[4]

modelo <- aov(Respuesta ~ Tratamiento + Bloque, data = datos)
resumen_modelo = summary(modelo)
resumen_modelo

p_valor_aov = resumen_modelo[[1]][["Pr(>F)"]][1]

if(p_valor_aov >= 0.05){
  cat("Se acepta la hipotesis nula, NO existen diferencias significativas
      entre los tratamientos: ", p_valor_aov)
} else {
  cat("Se rechaza la hipotesis nula, exista al menos un tratamiento con 
      diferencias significativas: ", p_valor_aov)
}

# Evaluación de supuestos
# 1. Normalidad
par(mfrow = c(1, 2))  # Dividir la ventana gráfica en dos paneles
plot(density(residuals(modelo)))  # Gráfico de densidad de residuos
qqnorm(residuals(modelo))  # Gráfico de cuantiles normales
qqline(residuals(modelo), col = "red")  # Línea de referencia

resultado_shapiro = shapiro.test(residuals(modelo))  # Prueba de Shapiro-Wilk

p_valor_shapiro = resultado_shapiro$p.value

if(p_valor_shapiro >= 0.05){
  print(paste("Se acepta la hipotesis nula, entonces existe normalidad en el modelo con un p valor (p > 0.05): ", p_valor_shapiro))
} else {
  print(paste("Se rechaza la hipotesis nula, entonces no existe normalidad con un p valor (p < 0.05): ", p_valor_shapiro))
}

dev.off()

# 2. Homocedasticidad (homogeneidad de varianzas)
plot(modelo, which = 1)  # Gráfico de residuos vs. valores ajustados
resultado_bartlett = bartlett.test(Respuesta ~ Tratamiento, data = datos)  # Prueba de Levene
#ver que caso se usa el Bartlett ya que no se viola el principio de 
#normalidad

p_valor_bartlett = resultado_bartlett$p.value

if(p_valor_bartlett >= 0.05){
  print(paste("Se cumple el supuesto de homocedasticidad con un p valor (p >= 0.05): ", p_valor_bartlett))
} else {
  print(paste("No se cumple el supuesto de homocedasticidad con un p valor de: ", p_valor_bartlett))
}

# 3. Independencia
# Debe existe un patron aleatorio
plot(1:length(Respuesta),residuals(modelo),pch=9)

# Diagnóstico general
#par(mfrow = c(2, 2))  # Dividir la ventana gráfica en 4 paneles
#plot(modelo)  # Gráficos de diagnóstico

# Interpretación de los resultados
# Analizar los gráficos y las pruebas estadísticas para determinar si los supuestos se cumplen o no