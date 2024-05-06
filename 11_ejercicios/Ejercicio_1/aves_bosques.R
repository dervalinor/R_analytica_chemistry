#librerias
library(car)
library(agricolae)
library(gmodels)
library(mvtnorm)
library(multcomp)
library(survival)

#Madrigal y Serna(1978) compararon la población de aves en cuatro áreas de
#bosques diferentes : un rodal de ciprés (RC), un bosque secundario al noroeste
#(BSN), una plantación de pinos patula (PP) y un bosque secundario aislado (BSA)
#, localizadas en piedras blancas, departamento de Antioquia, Colombia. El total
#de especies observadas durante 10 dias fue el siguiente:

RC = c(4, 2, 5, 2, 2, 1, 2, 3, 2, 4)
BSN = c(10, 10, 12, 11, 10, 12, 14, 12, 12, 11)
PP = c(1, 1, 2, 3, 1, 1, 1, 2, 2, 3)
BSA = c(8, 9, 9, 5, 7, 8, 7, 4, 12, 9)

#a) Especificar el modelo y construir la tabla del ANOVA.
#b) Estimar las medias para cada uno de los bosques.

#Tipos de bosque donde se realizaron las observaciones
#y cuantas observaciones se realizaron - Variable independiente
bosques_aves = rep(c("RC", "BSN", "PP", "BSA"), each = 10)

#Aves observadas en cada bosque - variable dependiente
especies_aves = c(RC, BSN, PP, BSA)

#crear marco de trabajo
datos_aves_bosques = data.frame(bosques_aves, especies_aves)

#Permitir entender cada columna como un variable del marco de datos
attach(datos_aves_bosques)

#Analisis descriptivo antes de ANOVA por medio de un boxplot

boxplot(especies_aves ~ bosques_aves, data = datos_aves_bosques, col = c("orange", "red", "blue", "yellow"), 
        xlab = "Tipos de Bosques",
        ylab = "Especies de aves observadas",
        main = "Especies de aves observadas en distintos bosques")
#en el bosque secuendario al noroeste se observa una mayor observacion de especies de aves en un rango mas alto
#pero tiene algunos datos atipicos superior a la valores comunmente observados

#Analisis de varianzas ANOVA 

modelo_aves = aov(especies_aves ~ bosques_aves, data = datos_aves_bosques)
resumen_anova = summary(modelo_aves)
resumen_anova

#ver Supuestos

nivel_significancia = 0.05 #para todas la pruebas
nivel_significancia
#Normalidad

#residuos del modelo
residuos = residuals(modelo_aves)

#dividir el entorno grafico en dos para ver dos graficos
par(mfrow = c(1,2))

#Grafica de densidad de los residuos
plot(density(residuos))

#Grafico de cuartil-cuartil para verificar tambien normalidad esto se ve si hay linealidad en la mayoria
#de puntos

plot(modelo_aves, which = 2)

#Observando las dos graficas la normalidad no es la mas optima tienendo discrepancias de lis ideal
#por lo cual la conclusiones del ANOVA no pueden ser muy confiables

#verificar la normalidad por el metodo de shapiro
resultado_normalidad = shapiro.test(residuos)
resultado_normalidad

#Obtener p valor
p_valor_norm = resultado_normalidad$p.value

#eligiendo un nivel de significancia de 0.05

if(p_valor_norm >= nivel_significancia){
  cat("Se acepta la hipotesis nula: Existe normalidad con p valor (p >= 0.05)", p_valor_norm, " \n")
} else {
  cat("No se acepta la hipotesis nula: No existe normalidad con un p valor p < 0.05", p_valor_norm, "\n")
}

#Pero el test de shapiro no dice que existe normalidad por lo cual se acepta el supuesto de normalidad 

#Limpiar entorno grafico
dev.off()

#Homocedasticidad

library(car)

#Grafico para ver si existe un distribucion continua de los errores las predicciones del modelo

plot(modelo_aves, which = 1) #parece no existir embudos lo cual indica un buen indicio de homocedasticidad

#Prueba de Bartlett para verificar homocedasticidad

prueba_homocedasticidad = bartlett.test(especies_aves ~ bosques_aves, data = datos_aves_bosques)
prueba_homocedasticidad

#obtener el valor p

p_valor_hm = prueba_homocedasticidad$p.value

if(p_valor_hm >= nivel_significancia){
  cat("Existe homocedasticidad con un p valor", p_valor_hm, " \n")
} else {
  cat("No existe homocedasticidad", p_valor_hm, " \n")
}

#Aqui hay un problema no existe homocedasticidad

#ver independencia

plot(1:length(especies_aves), residuos) #hay independencia



#Hacer un transformacion para mejor
respuestas_trans = log(especies_aves + 1)
respuestas_trans

datos_trans = data.frame(bosques_aves, respuestas_trans)

attach(datos_trans)

prueba_anova_trans = aov(respuestas_trans ~ bosques_aves, data = datos_trans)
resumen_aov_trans = summary(prueba_anova_trans)
resumen_aov_trans

p_valor_aov_trans = resumen_aov_trans[[1]][["Pr(>F)"]][1]
p_valor_aov_trans

#ver supuestos

#Normalidad 

par(mfrow = c(1,2))

residuos_trans = residuals(prueba_anova_trans)
plot(density(residuos_trans))
plot(prueba_anova_trans, which = 2)

#Homocedasticidad
plot(prueba_anova_trans, which = 1)
prueba_hm_trans = bartlett.test(respuestas_trans ~ bosques_aves, data = datos_trans)
p_valor_hm_trans = prueba_hm_trans$p.value
p_valor_hm_trans

#independencia

plot(1:length(respuestas_trans), residuos_trans) #existe independencias

#Los resultados del ANOVA no son confiables ya que no existe homocedasticidad
#usar prueba de Krushka wills

Prueba_kruskal_aves = kruskal.test(especies_aves ~ bosques_aves, data = datos_aves_bosques)
Prueba_kruskal_aves

p_valor_kruskal = Prueba_kruskal_aves$p.value

p_valor_kruskal

#Prueba de bartlett por que existe normalidad

#Hacer un transformacion para solucionar esto

#Estimacion de medias de cada bosques

#declarar variables
lista_especies_aves = list(RC, BSN, PP, BSA)
medias = numeric(length(lista_especies_aves))

for(i in 1:length(lista_especies_aves)){ #para ver lista elementos y tomar este numero para loop
  start = (i-1)*10 + 1
  end = i*10
  medias[i] = mean(especies_aves[start:end]) #luego recorrer todo el vector de especies de aves
}

print(medias)

mean(RC)
mean(BSN)
mean(PP)
mean(BSA)
