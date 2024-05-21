library(car)
library(agricolae)
library(gmodels)
library(mvtnorm)
library(multcomp)
library(survival)
library(ggplot2)

#Para prueba de aditividad
library(daewr)

#en cada bloque las caracteristicas son homogeneas la variable 
#que no es de interes es constante en cada bloque se aplica cada
#tratamiento
#El DCA es un problema particular de un solo bloque
#no es completamente aleatorio

#Consideremos los datos de la tabla siguiente lo cuales provienen de Lim
#and Wolfe(1997), y fueron modificados parcialmente de Heffner et al.
#(1974). El efecto de un fármaco particular en el comportamiento
#de ratas fué el bojetivo del experimento.

#el tipo de rata y dosis afecta en comportamiento de farmaco sobre
#este

R1 <- c(0.60, 0.80, 0.82, 0.81, 0.50)
R2 <- c(0.51, 0.61, 0.79, 0.78, 0.77)
R3 <- c(0.62, 0.82, 0.83, 0.80, 0.52)
R4 <- c(0.60, 0.95, 0.91, 0.95, 0.70)
R5 <- c(0.92, 0.82, 1.04, 1.13, 1.03)
R6 <- c(0.63, 0.93, 1.02, 0.96, 0.63)
R7 <- c(0.84, 0.74, 0.98, 0.98, 1.00)
R8 <- c(0.96, 1.24, 1.27, 1.20, 1.06)
R9 <- c(1.01, 1.23, 1.30, 1.25, 1.24)
R10 <- c(0.95, 1.20, 1.18,1.23, 1.05)

#tratamientos

Tratamientos_rt = rep(c("T1", "T2", "T3", "T4", "T5"), 10)
length(Tratamientos_rt)

#creacion de bloque segun el tipo de rata
Bloques_rt = rep(c("b1", "b2", "b3", "b4", 
                   "b5", "b6", "b7", "b8", "b9", "b10"), each = 5)
length(Bloques_rt)

Respuestas_rt = c(R1, R2, R3, R4, R5, R6, R7, R8, R9, R10)
length(Respuestas_rt)

#Marco de trabajo

datos_ratas = data.frame(Respuestas_rt, Tratamientos_rt, Bloques_rt)
attach(datos_ratas)

#bloques definicion matermatica: yij = ui + Bi + eij
#no se puede estudiar los efectos de cada tipo de animal (Bloques) 
#sobre la variable respuesta ya que los bloques no son de nuestro interes
#Hipotesis: los efectos son cero, las medias de tratamientos son iguales

#bloxplot de respuesta-tratamientos 

boxplot(Respuestas_rt ~ Tratamientos_rt, col = c("#2FD9B7", "#B72FD9", 
                                                 "#D92F42", "#6FD92F", "#D9B02F"))

#bloxplot de respuesta-bloques

#No puedeo hacer analisis
boxplot(Respuestas_rt ~ Bloques_rt, col = c("#2FD9B7", "#B72FD9", "#D92F42", 
                                            "#6FD92F", "#D9B02F", 
                                            "#64C8C5", "#6485C8", "#64C88D", 
                                            "#C89664", "#64C866"))

modelo_rt = aov(Respuestas_rt ~ Tratamientos_rt + Bloques_rt, data = datos_ratas)
resumen_aov_DBA = summary(modelo_rt)
resumen_aov_DBA

#solo se intrepretan el primer p valor que corresponde a los tratamientos
#el segundo valor p no se interpreta ya que los bloques no son nuestro interes
#sino los tratamientos

p_valor_DBA = resumen_aov_DBA[[1]][["Pr(>F)"]][1]

cat("Resultados ANOVA en DBA: \n")

if(p_valor_DBA >= 0.05){
  cat("No hay diferencias significativas entre los tratamientos")  
} else {
  cat("Hay diferencias significativas entre los tratamientos")
}

#Intrepretacion:

#primer p valor: el tratamientos - si hay un efecto en los tratamientos

#Segundo p valor: el de bloques - No se puede intrepretar que hay efecto de la
#las especies de la ratas por que no hay aleotorizacion!!!!! si F valor es alto
#indica que fue buena idea ya que si es pequeño no sirvio hacer bloques.
#Quitar el segundo p valor

#Hacer un diagnostico de normalidad, homocedastcidad y independencia !!!!!

#Primero encontramos los valores de calcula el cuadrado medio del error (Mean Square Error, MSE) para el Diseño Compuesto Aumentado (DCA), denotado por σ^2_DCA.
#Esta fórmula se compone de dos sumandos:
  
#SCB (Suma de Cuadrados de Bloques): Esta suma se obtiene del análisis de varianza (ANOVA) del Diseño de Bloques al Azar (DBA), que es un componente del DCA.
#SCE (Suma de Cuadrados del Error): También se obtiene del ANOVA del DBA.

#Estos dos componentes se suman y se dividen por k(b-1), donde:
  
#k es el número de bloques del DBA
#b es el número de unidades experimentales por bloque

#Efectividad relativa: cuando efectivo es DCA vs DBA
#Esto se hace a partir de error experimental

#Consideremos como un DCA
modelo_DCA_rt = aov(Respuestas_rt ~ Tratamientos_rt, data = datos_ratas)
resument_aov_DCA = summary(modelo_DCA_rt) #ver el mean Sq estes 
#es varianza DCA = 0.04337
resument_aov_DCA

p_valor_DCA = resument_aov_DCA[[1]][["Pr(>F)"]][1]
p_valor_DCA

cat("Resultados del ANOVA en DCA: \n")

if(p_valor_DCA >= 0.05){
  cat("No hay diferencias significativas entre los tratamientos")  
} else {
  cat("Hay diferencias significativas entre los tratamientos")
}

#si varianza de DBA es menor a la varianza de DCA 
#entonces fue buena idea usar bloques
#ya que reduce la varianza del error, para esta caso es cierto 
#DCA 0.04337 > DBA 0.00824 fue buena idea hace bloques se redujo 
#el error

#Eficiencia es 5 indica que 5 veces si usa DCA en comparacion DBA es decir 
#para obtener los mismo resultados que los bloques con un DCA. Es mejor 
#trabajar en 10 ratas que con 50 ratas.

#Suma de cuadros y df de DBA

Mean_sq_DBA = resumen_aov_DBA[[1]][["Mean Sq"]][3]
Mean_sq_DBA

Df_DBA = resumen_aov_DBA[[1]][["Df"]][3]
Df_DBA #grados de libertad de los tratamientos

#suma de cuadrados y grados de libertad de DCA
Mean_sq_DCA = resument_aov_DCA[[1]][["Mean Sq"]][2]
Mean_sq_DCA #accediendo a la suma de los cuadrados de los
#residuos

#accediendo a los grados de libertad de residuos
Df_DCA = resument_aov_DCA[[1]][["Df"]][2]
Df_DCA

#eficiencia 

M1 = (Df_DBA+1)*(Df_DCA+3)*Mean_sq_DCA
M2= (Df_DBA+3)*(Df_DCA+1)*Mean_sq_DBA

Eficiencia = M1/M2
Eficiencia
#un valor de eficiencia mayor a 1 indica que es DBA es mejor que DCA para obtener
#mejores resultados, el valor 5 representa que se necesita 5 veces hacer DCA
#para obtener resultados igual de validos que el DBA.


Porcentaje_reduccion = (Mean_sq_DBA- Mean_sq_DCA )*100/Mean_sq_DCA 
#reduccion al 80% del 
#error
Porcentaje_reduccion

#Hacer analsis de contrastes !!!!


#HACER DIAGNOSTICO DE DBA - PRUEBA DE ADITIVIDAD


#install.packages("daewr")
library(daewr)

#Prueba de de aditividad - sirve para saber si existe interaccion entre los 
#bloaues y los tratamientos ya que el modelo de bloques supone que no existe
#tal interaccion, en este caso la hipotesis nula es que no hay interaccion
#entre los bloques y los tratamientos

#Hipotesis nula: y_{ij} = μ + τ_{i} + β_{j} + ε_{ij}
#Hipotesis alternativa: y_{ij} = μ + τ_{i} + β_{j} + (τβ)_{ij} + ε_{ij}

#donde:
  
#y_{ij} es la observación en el tratamiento i y el bloque j
#μ es la media general
#τ_{i} es el efecto del tratamiento i
#β_{j} es el efecto del bloque j
#ε_{ij} es el error aleatorio
#(τβ)_{ij} representa el término de interacción entre el tratamiento i y el bloque j.

?Tukey1df 

#Todos en el primer parametro debe ser la respuesta, luego tratamiento y bloques
#para estos tratamientos y bloques deben ser factores
Tratamientos_rt = factor(Tratamientos_rt)
Bloques_rt = factor(Bloques_rt)

datos_ratas = data.frame(Respuestas_rt, Tratamientos_rt, Bloques_rt)

attach(datos_ratas)

Tukey1df(datos_ratas) #la hipotesis nula no se puede rechazar, 
#es decir hay adividad

#otra forma de hacer la prueba de aditividad

#install.packages("asbio")- Solucionar error !!!!!
#library(asbio)
#tukey.add.test(datos_ratas) #prueba de adividad

#otra forma de hacer la prueba de aditividad


#Yo: intentando otra forma de calcular la eficiencia

var_error_DBA = deviance(modelo_rt)/df.residual(modelo_rt)
var_error_DCA = deviance(modelo_DCA_rt)/df.residual(modelo_DCA_rt)

eficiencia_nueva = var_error_DCA/var_error_DBA
eficiencia_nueva #Si funcionaaaa!!!!!!


#Comparando con lo que aparece en la diapositiva del profesor
#Ahora vemos la formula que nos enseño el profesor en clase.


#calcula el cuadrado medio del error (Mean Square Error, MSE) para el Diseño Compuesto Aumentado (DCA), denotado por σ^2_DCA.
#Esta fórmula se compone de dos sumandos:

#SCB (Suma de Cuadrados de Bloques): Esta suma se obtiene del análisis de varianza (ANOVA) del Diseño de Bloques al Azar (DBA), que es un componente del DCA.
#SCE (Suma de Cuadrados del Error): También se obtiene del ANOVA del DBA.

#Estos dos componentes se suman y se dividen por k(b-1), donde:

#b es el número de bloques del DBA
#k es el número de unidades experimentales por bloque

#Obtener valores del anova de DBA

#suma de cuadrdos de bloques
resumen_aov_DBA
SCB = resumen_aov_DBA[[1]][["Sum Sq"]][2]
SCB

#Suma de cuadrados del Error
SCE = resumen_aov_DBA[[1]][["Sum Sq"]][3]
SCE

#Numero de bloques
#length(unique(datos$bloques))  # Número de unidades experimentales por bloque
b = length(unique(datos_ratas$Bloques_rt))
b

#Numero de unidades experimentales de cada bloque
grados_li = resumen_aov_DBA[[1]][["Df"]][1]
k = grados_li + 1
k

sigma2_DBA <- (SCB + SCE) / (k * (b - 1))
sigma2_DBA #si es correcto !!!!!

#aunque tambien se puede hacer
resument_aov_DCA
sigma2_DBA = resument_aov_DCA[[1]][["Mean Sq"]][2]
sigma2_DBA

#calculo de cuadrados de DCA

sigma2_DCA = resumen_aov_DBA[[1]][["Mean Sq"]][3]
sigma2_DCA #si es correcto!!!!

Eficiency = ((Df_DBA+1)*(Df_DCA+3)*sigma2_DBA)/((Df_DBA+3)*(Df_DCA+1)*sigma2_DCA)
Eficiency