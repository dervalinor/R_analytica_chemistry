BHA = rep(c("Tratado 1", "Control"), 8)
Bloques = rep(rep(c("b1", "b2"), each = 2), 4)
cepa = rep(c("c1", "c2", "c3", "c4"), each = 4)


# Combinar los datos en un vector
respuesta <- c(18.7, 7.7, 16.7, 6.4,
               17.9, 8.4, 14.4, 6.7,
               19.2, 9.8, 12.6, 8.1,
               26.3, 9.7, 19.8, 6.0)

datos <- data.frame(
  BHA = factor(BHA),
  cepa = factor(cepa),
  Bloques = factor(Bloques),
  respuesta
)

#debe dar una interaccion entre las cepas y la respuesta por lo cual 
#interaccion.

modelo = aov(respuesta ~ BHA*cepa + Bloques, data = datos)
summary(modelo)

#Evaluacion de supuestos:

#Normalidad

residualesd=residuals(modelo)

par(mfrow=c(1,2))
plot(density(residualesd)) 

plot(modelo,which=2)

#Homocedasticidad

dev.off() 

resultado_test <- shapiro.test(residualesd)

summary(resultado_test) #ver resultado del test

#obtener p-valor
p_valor <- resultado_test$p.value

#vamos a ver si acepta o se rechaza la hipotesis nula

if(p_valor >= 0.05) {
  print("Se acepta la hipotesis nula, entonces existe normalidad en el modelo")
} else {
  print("Se rechaza la hipotesis nula, entonces no existe normalidad")
}

#Supuesto de Homocedasticidad
#Gráfico
plot(modelo,which=1)

prueba_bartlett <- bartlett.test(respuesta~BHA,data=datos)

p_valor_b = prueba_bartlett$p.value

if(p_valor_b >= 0.05){
  print("Existe homocedasticidad, varianzas de los dietas similares")
} else {
  print("No existe homocedasticidad, varianzas son diferentes")
}

#Independencia (Supuesto)
plot(1:length(actividad),residualesd,pch=8) 

#Hacer pruebas de aditividad 

#Hacer el anova sin bloques para evaluar si fue buena idea los bloques
#evaluar efectividad

