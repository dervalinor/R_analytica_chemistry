#Existen estudios
#absolutos: Los estudios absolutos se centran en examinar una sola población o grupo sin compararlos con otros grupos.
#Comparativos: Los estudios comparativos implican la comparación de dos o más grupos para evaluar diferencias en una o más 
#variables de interés.
#comparar con variables de respuesta, es experimental (control) y observacional (no se tiene control)
#siempre hay un error experimental
#IMPORTANTE: ver que la maquinas esta bien calibradas !!!!
#las unidades experimentales sean homogéneas lo mas posible, para evitar sesgos en los resultados - usar Bloques

#Bloque: grupo de unidades experimentales con características similares
#Existen experimentos que donde la variable respuesta es afectada por muchas variables - multifactorial: analizar de forma
#conjunta

#IMPORTANTE: Aleotarizar y replicar para evitar sesgos y obtener precisión
#leer el material !!!!

# 1. Diseño completamente aleatorizado (DCA)

#Como construir un DCA en Rstudio usar paquete agricolae
#install.packages("agricolae")

#verificar paquetes
#require(agricolae)
library(agricolae) #importante cargar esto para correr el codigo
tratamientos <- c("T1", "T2", "T3", "T4") #vector en R, c indicar un vector
#VER VARIABLES EN R

#cargar variable
tratamientos

#replicas de experimentos
replicas = c(5, 5, 5, 5)
replicas

#diseño completamente aleatorizado
?design.crd #ver manual de esta función
design.crd(tratamientos, replicas) #nos da tratamientos y replicas aleatorias 

#impotante guardar la tabla usar seed como id
#importante guardar matrix de diseño
design.crd(tratamientos, replicas, seed = 20)

#leer numeros: 101 - repolicar 1 del tratamiento 1, 103 replica 1 del tratamiento 3

#Ahora debemos tomar observaciones
#TRAER UN EXPERIMENTO DE UNA CLASE !!!!

#Guardar tabla
mi_tabla <- design.crd(tratamientos, replicas, seed = 20)
mi_tabla

#$ indica salidas atomicas como vectores
#Guardar tabla 
tabla_diseno <- mi_tabla$book

#Necesitamos un columna para respuestas de la observaciones pero en este caso lo vamos a simular
set.seed(1010) #crear semilla para guardar distribucion
respuestas <- rnorm(20, mean = 5, sd = 2) #simular, rnorm indica un normal  
respuestas

#Grafica de densidad de probabilidad

plot(density(respuestas))
tabla_diseno$respuestas = respuestas #añadir un nueva tabla
tabla_diseno

attach(tabla_diseno)

#Ctr + A + ctrl + Enter - Copilar el codigo
boxplot(respuestas ~ tratamientos, data = tabla_diseno, col = c("#2F8FE7", "#B849E0", "#49E0C6", "#70E049"), #midificar color de cada tratamiento
        xlab = "Tratamientos", #cambiar nombre de los ejes
        ylab = "Respuestas",
        main = "Experimento simulado")
#este es un analisis descriptivo