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

library(car)
library(agricolae)
library(daewr)
library(ggplot2)

#bloques 
Bloque_1 = c(tratado_1, control_1)
Bloque_2 = c(tratado_2, control_2)

#Inteligencia Artificial:

# Datos
cepas <- rep(c("A/J", "129/Ola", "NIH", "BALB/5"), 2)
tratamiento <- rep(c("Tratado", "Control"), each = 4)
bloques <- rep(c("Bloque_1", "Bloque_2"), each = 8)

tratado_1 <- c(18.7, 17.9, 19.2, 26.3)
tratado_2 <- c(16.7, 14.4, 12.0, 19.8)
control_1 <- c(7.7, 8.4, 9.8, 9.7)
control_2 <- c(6.4, 6.7, 8.1, 6.0)

# Combinar los datos en un vector
actividad <- c(tratado_1, tratado_2, control_1, control_2)

# Crear un data frame
datos <- data.frame(
  Cepa = factor(rep(cepas, 2)),
  Tratamiento = factor(tratamiento, levels = c("Control", "Tratado")),
  Bloque = factor(bloques),
  Actividad_EROD = actividad
)

# Ver los datos
print(datos)

# Análisis ANOVA
modelo <- aov(Actividad_EROD ~ Tratamiento * Cepa + Bloque, data = datos)

# Mostrar el resumen del modelo ANOVA
summary(modelo)
#como F es significativo  19.875 por lo cual hay un indicio 
#que fue buena idea usar bloques


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

prueba_bartlett <- bartlett.test(actividad~tratamiento,data=datos)

p_valor_b = prueba_bartlett$p.value

if(p_valor_b >= 0.05){
  print("Existe homocedasticidad, varianzas de los dietas similares")
} else {
  print("No existe homocedasticidad, varianzas son diferentes")
}

#Independencia (Supuesto)
plot(1:length(actividad),residualesd,pch=8) 

#*Hipótesis nula (\(H_{0AB}\))**: No hay interacción entre el tratamiento con BHA y la cepa del ratón en la actividad de la enzima EROD.
#\[
#H_{0AB}: (\alpha\beta)_{11} = (\alpha\beta)_{12} = \ldots = (\alpha\beta)_{24} = 0
#\]

#- **Hipótesis alternativa (\(H_{1AB}\))**: Existe una interacción significativa entre el tratamiento con BHA y la cepa del ratón en la actividad de la enzima EROD.
#\[
#  H_{1AB}: (\alpha\beta)_{ij} \neq 0 \text{ para al menos una combinación } i,j
# \]



