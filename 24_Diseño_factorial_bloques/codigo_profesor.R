library(car)
library(agricolae)
library(gmodels)
library(mvtnorm)
library(multcomp)
library(survival)
library(ggplot2)

#Para prueba de aditividad
library(daewr)

#El bloque es mas efectivo en un diseño factorial

#Ejemplo: 
#Un experimento fue realizado para determinar si el BHA (Un antioxidante comun)
#usado en los alimento procesados inducia la actividad de la enzima hepatica 
#EROD en ratones y si esta actividad era independiente de la cepa de los ratones

#Este experimento fue parte de un estudio mas amplio para determinar si los 
#antioxidantes ayudan a proteger contra el cancer.

#Los factores de este experimento fueron:

#A: Factor que permite identificar si un raton fue tratado con BHA o no. Otro 
#factor de interes que denominaremos B fue la cepa del raton un ensayo de este
#experimento consistia en seleccionar un raton de una cepa especifica luego 
#incoorporar BHA (por un periodo de tres semanas o no dependiendo de lo que fue
#especificado) y finalmente sacrificar el raton y realizar un autosia para 
#determinar el nivel de actividad de la enzima EROD en el higado.

#Dado que los resultados de los experimentos como este pueden variar sustancial-
#mente en diferentes momentos y en el mismo laboratorio debido a diferencias en
#los reactivos usados para el analisis de enzima la calibracion de intrumentos 
#y factores ambientales en la cria de los animales, los experimentos fueron 
#bloqueados en el tiempo. Se seleccionaron 2 rotones de cada de cuatro cepas, 
#uno fue elegido al azar para recibir BHA en la dieta y se realizaron 8 ensayos
#simultaneamente eston representa un bloque. Dado que existen dos niveles del 
#fator A y 4 niveles de factor cepa, existieron 8 unidades experimentales por
#los resultodos del experimento se muestran a continuacion:

#datos

#Importante bloquear en el laboratorio para evitar ese error

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

#Convertir graficas a ggplot2

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
plot(1:length(respuesta),residualesd,pch=8) 

#Evalucion de aditividad
#intentar hacer anova de interaccion entre BHA*Bloques

# Evaluation of additivity using interaction terms
# Ensure the data frame is in the correct format for linear model analysis
datos_ad = data.frame(respuesta, BHA = factor(BHA), Bloques = factor(Bloques), cepa = factor(cepa))

# Fit a linear model with interaction terms
modelo_interaccion = aov(respuesta ~ BHA * Bloques, data = datos_ad)

# Perform ANOVA to evaluate interaction terms
anova_interaccion = anova(modelo_interaccion)
anova_interaccion

# Extract p-value for interaction term
p_valor_interaccion = anova_interaccion["BHA:Bloques", "Pr(>F)"]

cat("Resultados de la prueba de aditividad: \n")

if(p_valor_interaccion >= 0.05) {
  cat("No hay interacción significativa entre los tratamientos y las bloques. La aditividad se mantiene.\n")
} else {
  cat("Hay interacción significativa entre los tratamientos y las bloques. La aditividad no se mantiene.\n")
}

#Ver como esto se relaciona con el p valor - ESTO SE VE EN LA EFECTVIDAD !!!

#The significant F value for the blocks 
#indicate that including blocks in the design was a good decision. 
#This significance means that the variability due to blocks is non-negligible 
#and accounting for it improves the model's ability to explain 
#the variability in the response variable.

#By including blocks, you effectively reduce the residual error and 
#increase the power of the statistical tests for the factors of interest 
#(BHA and cepa). In summary, the analysis supports the conclusion that 
#blocking has helped in controlling variability and should be considered 
#a beneficial aspect of the experimental design.