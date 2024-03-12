
library(agricolae) 
tratamientos <- c("T1", "T2", "T3", "T4") 


tratamientos


replicas = c(5, 5, 5, 5)
replicas


?design.crd 
design.crd(tratamientos, replicas) 



design.crd(tratamientos, replicas, seed = 20)

mi_tabla <- design.crd(tratamientos, replicas, seed = 20)
mi_tabla

 
tabla_diseno <- mi_tabla$book



set.seed(1010)


respuestas <- rnorm(20, mean = 5, sd = 2) 
respuestas



plot(density(respuestas))
tabla_diseno$respuestas = respuestas 
tabla_diseno

attach(tabla_diseno)

#analisis de un variable cualitativa vs variable cuantitativa
boxplot(respuestas ~ tratamientos, data = tabla_diseno, 
        col = c("#2F8FE7", "#B849E0", "#49E0C6", "#70E049"), 
        xlab = "Tratamientos", 
        ylab = "Respuestas",
        main = "Experimento simulado")

#pero necesitamos que nuestras conclusiones sean validas . usa la estadistica inferencial
#los tratamientos afectan la variable respuesta
#Respueta observada = media general + efecto tratamiento + Error

#hipotesis en la estadistica, existe varias tecnicas como p-valor (permite dicidir H oscila 0 < p < 1) y existe un nivel de significancia
#que indica la probabilidad de cometer un error, el nivel de significancia sea pequeño lo mas pequeño posible, es decir hacer
# el mayor numero de replicas

# Realizar la prueba ANOVA

names(tabla_diseno) = c("unidades experimentales", "Replica", "Tratamientos", "Respuestas") #cambiar nombre de las columna de las 
#tablas

attach(tabla_diseno) #no olvidar esto !!!!!

#ANOVA de un via, ya que hay un solo factor de juego es decir tratamientos
modelo <- aov(Respuestas ~ Tratamientos, data = tabla_diseno)

#resumen del anova
resumen <- summary(modelo)
resumen

#ver modelos que predicen datos
fitted(modelo)
tabla_diseno

#calcular residuos del modelo
residuos = residuals(modelo)
residuos #cuidado si no simetrica entonces es un fracaso en este analsis

residuos2 = plot(modelo, which = 2)

#evaluar normalidad

plot(density(residuos))

# Obtener el valor p
valor_p <- resumen[[1]][["Pr(>F)"]][1]

# Nivel de significancia
nivel_significancia <- 0.05

#repasar que es el modelo ANOVA - Es importante ver los supuestos de este modelo
# Evaluar la hipótesis nula
#se rechaza H cuando el nivel p < niveles de significancia en caso contrario no rechazo H
if (valor_p < 0.05) {
  cat("Se rechaza la hipótesis nula. Al menos una media es diferente.\n")
} else {
  cat("No se puede rechazar la hipótesis nula. Las medias son iguales.\n")
}

#prueba de shapiro residuos normales y residuos no normales
shapiro.test(residuos) #prueba de normalidad

#homocedasticidad
plot(modelo, which = 1) #en que caso hay homocedasticidad

#evaluar homocedasticidad

bartlett.test(Respuestas ~ Tratamientos, data = tabla_diseno) #hipotesis: existe homocedasticidad y A: existe heterocedasticidad

#test de Leven - se usa cuando no hay normalidad - buscar pruebas robustas
