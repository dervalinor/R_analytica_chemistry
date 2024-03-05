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
require(agricolae)
#library(agricolae)
tratamientos <- c("T1", "T2", "T3", "T4") #vector en R, c indicar un vector
#VER VARIABLES EN R

#cargar variable
tratamientos

#replicas de experimentos
replicas = c(5, 5, 5, 5)
replicas

#diseño completamente aleatorizado
?design.crd #ver manual de esta función
design.crd()

