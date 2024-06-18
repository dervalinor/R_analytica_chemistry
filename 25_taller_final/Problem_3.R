#se realizó un experimento de laboratorio para estudiar la
#influencia del fósforo, el nitrógeno y el carbono en la regulación del
#crecimiento del alga Microcystis aeruginosa. La respuesta fué la po-
#blación algal medida espectrofotométricamente en unidades de absor-
#bancia. Se hicieron dos repeticiones del experimento completamente
#al azar. Los datos se reportan en la siguientes tablas

# Datos de alturas de plantas
alturas <- c(33.0, 33.5, 38.5, 33.5, 40.0, 43.5, 42.5, 40.5,
             40.0, 39.5, 35.0, 32.0, 38.5, 30.0, 36.0, 39.0)

# Crear la matriz de datos
alturas_mat <- matrix(alturas, nrow = 4, byrow = TRUE)
dimnames(alturas_mat) <- list(c("(1)", "a", "b", "ab"), # Nombres de filas
                              c("Rep1", "Rep2", "Rep3", "Rep4")) # Nombres de columnas
alturas_mat

# Asignar niveles de factores
rhizobium <- rep(c("Sin inóculo", "Con inóculo"), each = 4)
micorrizas <- rep(c("Sin inóculo", "Con inóculo"), times = 4)

# Calcular totales por tratamiento
totales <- apply(alturas_mat, 1, sum)
totales

# Estimar efectos
A <- rep(c(-1, 1), 2) # Signos de A (rhizobium)
B <- c(-1, -1, 1, 1) # Signos de B (micorrizas)
AB <- A * B # Signos de AB (interacción)

r <- 4 # Número de réplicas

# Efectos promedio
A_efecto <- (totales %*% A) / (2 * r)
B_efecto <- (totales %*% B) / (2 * r)
AB_efecto <- (totales %*% AB) / (2 * r)

# Resumen de efectos
efectos <- t(totales) %*% cbind(A, B, AB) / (2 * r)
resumen <- rbind(cbind(A, B, AB), efectos)
dimnames(resumen)[[1]] <- c(dimnames(alturas_mat)[[1]], "efecto")
resumen

# Ajuste del modelo ANOVA
alturas_vec <- c(t(alturas_mat)) # Vector de alturas
Af <- rep(as.factor(A), rep(r, 4)) # Factor A (rhizobium)
Bf <- rep(as.factor(B), rep(r, 4)) # Factor B (micorrizas)
options(contrasts = c("contr.sum", "contr.poly")) # Contraste de suma a cero y polinomial

# Modelo ANOVA
modelo <- aov(alturas_vec ~ Af * Bf) # Modelo con interacción
summary(modelo) # Resumen del modelo ANOVA

# Pruebas de comparaciones múltiples
TukeyHSD(modelo, 'Af') # Comparaciones múltiples para Rhizobium
TukeyHSD(modelo, 'Bf') # Comparaciones múltiples para Micorrizas
TukeyHSD(modelo, 'Af:Bf') # Comparaciones múltiples para la interacción

# Gráficos de interacción
interaction.plot(x.factor = Af, trace.factor = Bf,
                 response = alturas_vec, fun = mean,
                 xlab = "Rhizobium", ylab = "Altura de plantas",
                 trace.label = "Micorrizas", col = c("red", "blue"))

# Gráfico de residuos
plot(modelo, which = 1:2) # Gráficos de residuos vs. ajustados y normal Q-Q

# Supuestos del modelo
shapiro.test(residuals(modelo)) # Prueba de normalidad de residuos
leveneTest(alturas_vec ~ Af * Bf) # Prueba de homogeneidad de varianzas