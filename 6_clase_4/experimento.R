#Para comparar cuatro dietas D 1 , D 2 , D 3 y D 4 , respecto a su influencia
#en el tiempo de coagulación de la sangre, se seleccionaron 24 animales y
#cada un recibió aleatoriamente una de las dietas. Los datos se muestran a
#continuación

#Hacer un diseño totalmente aleotarizado y Annova para comprobar hipotesis

# D1 = {61, 60, 63, 59}
# D2 = {63, 67, 71, 64, 65, 66}
# D3 = {68, 66, 71, 67, 68, 68}
# D4 = {56, 62, 60, 63, 63, 64, 63, 59}

#tratamientos 
tratamientos = rep(c("D1", "D2", "D3", "D4"), c(4, 6, 6, 8))
tratamientos

#longitud del vector
length(tratamientos)

# Respuestas de experimento
respuestas = c(61, 60, 63, 59, 
               63, 67, 71, 64, 65, 66,
               68, 66, 71, 67, 68, 68,
               56, 62, 60, 63, 63, 64, 63, 59
            )

#ver longitud de un vector
length(respuestas)

#unir datos
datos_dietas = data.frame(tratamientos, respuestas)
datos_dietas

attach(datos_dietas)

#hacer analisis exploratorio
# Calculando estadísticas descriptivas
summary(datos_dietas)

# Visualización de los datos
boxplot(respuestas ~ tratamientos, data = datos_dietas,
        xlab = "Tipo de Dieta",
        ylab = "Tiempo de Coagulación",
        main = "Boxplot de Tiempo de Coagulación por Tipo de Dieta") # no es suficiente para ver diferencias
 
#chatGPT:

# Realizar la prueba ANOVA
anova_result <- aov(respuestas ~ tratamientos, data = datos_dietas)

# Obtener el valor p del ANOVA
p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]

# Nivel de significancia
alpha <- 0.05

# Condición para determinar la conclusión del experimento, Qe significado tiene matematico y fisico de esto 
if (p_value < alpha) {
  cat("Dado el valor p de", p_value, "se concluye que hay diferencias significativas entre al menos dos grupos de dieta. Por lo tanto, el tipo de dieta tiene un efecto significativo en el tiempo de coagulación de la sangre.")
} else {
  cat("Dado el valor p de", p_value, "no se encontraron diferencias significativas entre los grupos de dieta. Por lo tanto, no se puede concluir que el tipo de dieta tenga un efecto significativo en el tiempo de coagulación de la sangre.")
}

#Evaluar los supuestos: normalidad

residuales = residuals(anova_result)
residuales

#graficos
#hacer un dos graficos
par(mfrow = c(1, 2))
plot(density(residuales))
plot(anova_result, which = 2)

shapiro.test(residuales)

#quitar para evitar, quitar graficos
dev.off()

#evaluar homocedasticidad 

#modo grafica
plot(anova_result, which = 1)

# Cargar el paquete necesario
library(car)

# Realizar la prueba de bartlett para evaluar la homocedasticidad
# Realizar la prueba de Bartlett para evaluar la homocedasticidad
bartlett_test <- bartlett.test(respuestas ~ tratamientos, data = datos_dietas)
bartlett_test

# Realizar la prueba de Bartlett para evaluar la homocedasticidad
bartlett_test <- bartlett.test(respuestas ~ tratamientos, data = datos_dietas)

# Obtener el valor p del test de Bartlett
p_value_bartlett <- bartlett_test$p.value

# Nivel de significancia
alpha <- 0.05

# Condición para determinar la hipótesis nula de Bartlett
if (p_value_bartlett < alpha) {
  cat("La hipótesis nula de Bartlett (H0) es que las varianzas de las muestras provienen de poblaciones con la misma varianza.\n")
} else {
  cat("La hipótesis nula de Bartlett (H0) no puede ser rechazada. Las varianzas de las muestras pueden provenir de poblaciones con la misma varianza.\n")
}

#test  de independencia
plot(1:24, residuales, pch = 19) #no deben existir patrones

#interpretacion ANNOVA
summary(anova_result)

#esto no es blanco y negro, no es asi, ver que consecuencias de esto en un experimento, ver si puede replicar, siempre planeas
#balaceado

if (p_value < alpha) {
  cat("Dado el valor p de", p_value, "se concluye que hay diferencias significativas entre al menos dos grupos de dieta. Por lo tanto, el tipo de dieta tiene un efecto significativo en el tiempo de coagulación de la sangre.")
} else {
  cat("Dado el valor p de", p_value, "no se encontraron diferencias significativas entre los grupos de dieta. Por lo tanto, no se puede concluir que el tipo de dieta tenga un efecto significativo en el tiempo de coagulación de la sangre.")
}