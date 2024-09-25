library(car)
library(agricolae)
library(gmodels)
library(mvtnorm)
library(multcomp)
library(survival)
library(ggplot2)

#Los datos de la siguiente tabla muestran los rendimientos de cinco variedades de cebada en
#un experimento de bloques completos al azar realizado en Minnesota, publicado en The
#Journal of the American Society of Agronomy por Immer et al. (1934).

bloque <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
lugar <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)
ano <- c(1931, 1932, 1931, 1932, 1931, 1932, 1931, 1932, 1931, 1932, 1931, 1932)
manchuria <- c(81.0, 80.7, 146.6, 100.4, 82.3, 103.1, 119.8, 98.9, 98.9, 66.4, 86.9, 67.7)
svansota <- c(105.4, 82.3, 142.0, 115.5, 77.3, 105.1, 121.4, 61.9, 89.0, 49.9, 77.1, 66.7)
velvet <- c(119.7, 80.4, 150.7, 112.2, 78.4, 116.5, 124.0, 96.2, 69.1, 96.7, 78.9, 67.4)
trebi <- c(109.7, 87.2, 191.5, 147.7, 131.3, 139.9, 140.8, 125.5, 89.3, 61.9, 101.8, 91.8)
peatland <- c(98.3, 84.2, 145.7, 108.1, 89.6, 129.6, 124.8, 75.7, 104.1, 80.3, 96.0, 94.1)

#Marco de datos

variedades = rep(c("Manchuria", "Svansota", "Velvet", "Trebi", "Peatland"), each = 12)
variedades = factor(variedades)
length(variedades)

bloque = rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 5)
bloque = factor(bloque)
length(bloque)

rendimiento = c(manchuria, svansota, velvet, trebi, peatland)
length(rendimiento)

marco_datos = data.frame(rendimiento, variedades, bloque)

attach(marco_datos)

#Prueba de aditivodad
Tukey1df(marco_datos)


#Anova 
modelo_cedaba = aov(rendimiento ~ variedades + bloque, data = marco_datos)
summary(modelo_cedaba)

#Evalucacion de supuestos de la ANOVA

residuos_cb = residuals(modelo_cedaba)

#dividir el marco visual en dos
par(mfrow=c(1,2))

#Normalidad
plot(density(residuos_cb))
plot(modelo_cedaba, which = 2)

#test para normalidad: existe normalidad
shapiro.test(residuos_cb)

#limpiar ambiente grafico
dev.off()

#Homocedasticidad 
plot(modelo_cedaba, which = 1)
bartlett.test(rendimiento ~ variedades, data = marco_datos)

#independencia
plot(1:length(rendimiento),residuos_cb,pch=9)

#Analisis de eficiencia del diseño del bloques al azar vs un diseño completamente
#aleotarizado

#crear ANOVA del diseño completamenta aleotarizado

marco_DCA = data.frame(rendimiento, variedades)
attach(marco_DCA)

modelo_DCA = aov(rendimiento ~ variedades, data = marco_DCA)
summary(modelo_DCA)

#Ahora calculamos la eficiencia

#EFICIENCIA

resumen_modelo_dba
resumen_modelo

#Para BDA 
DF_DBA = resumen_modelo_dba[[1]][["Df"]][3]
DF_DBA
sigma_2_DBA = resumen_modelo_dba[[1]][["Mean Sq"]][3]
sigma_2_DBA

#Para DCA
DF_DCA = resumen_modelo[[1]][["Df"]][1]
DF_DCA
sigma_2_DCA = resumen_modelo[[1]][["Mean Sq"]][2]
sigma_2_DCA

Eficiencia_formal = 
  ((DF_DBA+1)*(DF_DCA + 3)*sigma_2_DCA)/((DF_DBA+3)*(DF_DCA + 1)*sigma_2_DBA)
cat("La eficiencia del diseño de bloques al azar en comparacion con el diseño
    completamente aleatorizado es: ", Eficiencia_formal)
