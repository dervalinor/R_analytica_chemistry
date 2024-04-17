library(car)
library(agricolae)
library(gmodels)
library(mvtnorm)
library(multcomp)
library(survival)

#(Valor 1,0) Se diseñó un experimento para comparar cinco medios de
#cultivo con respecto a la capacidad de crecimiento de celulas fibroblásticas de tejidos de ratones. 
#Para cada medio se tomaron cinco frascos
#con igual número de células depositadas en cada uno; y después de
#siete días se determinó la proteina celular total. Los resultados (en
#microgramos de proteina nitrogenada) se dan en la siguiente tabla

M1 = c(102, 101, 100, 105, 101)
M2 = c(103, 105, 100, 108, 102)
M3 = c(103, 107, 105, 105, 106)
M4 = c(108, 101, 104, 106, 104)
M5 = c(113, 117, 106, 115, 116)

#El medio M1 es un control, los medios M2, M3, M4 tienen compuestos
#similares y el medio M5 es un control alterno.
#a) (Valor 1,0) Realice un análisis descriptivo para estudiar el efecto
#de los tratamientos sobre la variable respuesta. Interprete.
#b) (Valor 1,0) Confirme mediante un test de hipótesis adecuado lo
#analizado en el item previo. Recuerde interpretar sus resultados
#siempre y cuando estos sean válidos. No olvide especificar el modelo y/o las pruebas 
#de hipótesis apropiadas para este estudio.

#Contrastes a analizar:

#1) Comparar los dos controles - Contraste 1
#2) Comparar el promedio de los controles con el promedio de los otros medios - Contraste 2
#3) Comparación que usted desee y que sea ortogonal a las comparaciones previas. - Contraste 3

tratamientos_ratones = rep(c("M1", "M2", "M3", "M4", "M5"), each = 5)

respuestas_ratones = c(M1, M2, M3, M4, M5)

#Creando marco de trabajo
datos_marco = data.frame(tratamientos_ratones, respuestas_ratones)
attach(datos_marco)


#modelo 
boxplot(respuestas_ratones ~ tratamientos_ratones, data = datos_marco, col = c("#30C9DB","#E247E0","#2DCC90","#C2DE48", "orange"),
        xlab="Medios de cultivo", ylab="Concentracion de proteina celular", main="Crecimiento de celulas en ratones")


#ANOVA

nivel_significancia_ratones = 0.05

modelo_ratones = aov(respuestas_ratones ~ tratamientos_ratones, data = datos_marco)
resumen_aov_ratones = summary(modelo_ratones)
resumen_aov_ratones


#Ver supuestos

residuales_ratones = residuals(modelo_ratones)

#Normalidad

par(mfrow=c(1,2))
plot(density(residuales_ratones))
plot(modelo_ratones, which = 2)
dev.off() 

#Ver que dice la prueba de shapiro
normalidad_prueba_shapiro = shapiro.test(residuales_ratones)
normalidad_prueba_shapiro #hay normalidad en el modelo

if(normalidad_prueba_shapiro$p.value >= nivel_significancia_ratones){
  cat("Existe normalidad con un p valor", normalidad_prueba_shapiro$p.value, "\n")
} else {
  cat("No existe normalidad", normalidad_prueba_shapiro$p.value, " \n")
}

#Homocedasticidad

plot(modelo_ratones, which = 1)

#ver si existe homocedasticidad
prueba_hm_ratones = bartlett.test(respuestas_ratones ~ tratamientos_ratones, data = datos_marco)
prueba_hm_ratones #Existe homocedasticidad
#ver p valor 
p_valor_hm_ratones = prueba_hm_ratones$p.value
p_valor_hm_ratones

if(p_valor_hm_ratones >= nivel_significancia_ratones){
  cat("Existe homocedasticidad con un p valor", p_valor_hm_ratones, " \n")
} else {
  cat("No existe homocedasticidad con p valor ", p_valor_hm_ratones, " \n")
}

#independencia
plot(1:length(respuestas_ratones),residuales_ratones,pch=9)

#Como se cumplen los supuestos de normalidad, homocedasticidad y independencia entonces los resultados del anova son
#confiables

resumen_aov_ratones #existen diferencias significativas entre los medios de cultivo para evaluar la capacidad 
#de crecimiento de las celulas fibroblasticas de tejidos de ratones.

#ver si acepta o se rechaza la hipotesis nula
#Hipotesis nula: No hay diferencicas significativas entre los medios de cultivo

p_valor_anova_ratones = resumen_aov_ratones[[1]][["Pr(>F)"]][1]

if(p_valor_anova_ratones >= nivel_significancia_ratones){
  cat("Se ACEPTA la hipotesis nula, no hay diferencias significativas entre los medios con un p valor: ", p_valor_anova_ratones, " \n")
} else {
  cat("Se RECHAZA la hipotesis nula, hay diferencias significativas entre los medios con un p valor: ", p_valor_anova_ratones, " \n")
}

#Matriz de contrastes

#Contrastes a analizar:

#1) Comparar los dos controles - Contraste 1

#2) Comparar el promedio de los controles con el promedio de los otros medios - Contraste 2

#3) Comparación que usted desee y que sea ortogonal a las comparaciones previas. - Contraste 3
#comparacion entre M2 y el promedio M3 y M4

q1=c(1,0,0,0,-1)
#contraste 2
q2=c(3,-2,-2,-2,3)
#contraste 3
q3<-c(0,1,-1,0,0) 
#se debe comparar si son ortogonales
sum(q1*q2)
sum(q1*q3)
sum(q2*q3)
#se genera la matriz de contrastes 
L=matrix(c(q1,q2,q3),nrow=3,ncol = 5,byrow=TRUE)
L
rownames(L)=c("Q1","Q2","Q3")
L

#analisis de contrastes por medio de condicionales
tratamientos_ratones = factor(tratamientos_ratones)
contrastes_resultados = fit.contrast(modelo_ratones,"tratamientos_ratones",L)
contrastes_resultados

p_valores_contrastes <- unlist(contrastes_resultados[, "Pr(>|t|)"])

for (i in 1:length(p_valores_contrastes)) {
  if (p_valores_contrastes[i] < nivel_significancia_ratones) {
    cat("Contraste", i, ": Se RECHAZA la hipótesis nula, SI hay diferencias significativas entre tratamientos.\n")
  } else {
    cat("Contraste", i, ": Se ACEPTA la hipótesis nula, NO hay diferencias significativas entre tratamientos.\n")
  }
}

#Contraste 4: El contraste que compara los primeros cuatro medios
#con el último es ortogonal con los contrastes del item anterior?
#Verifique de forma manual.
q4=c(1,1,1,1,-4)

sum(q1*q4)
sum(q2*q4)
sum(q3*q4)