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

#Nota: funcion design.crd es un diseño completamente aleatorio esto evita sesgos al
#realizar comparaciones de tratamiento sobre un variable respuesta
?design.crd #ver manual de esta función
design.crd(tratamientos, replicas) #nos da tratamientos y replicas aleatorias 

#impotante guardar la tabla usar seed como id
#importante guardar matrix de diseño

design.crd(tratamientos, replicas, seed = 20)
#Nota: parametros de la funcion design.crd
# tratamientos: cuantos tratamientos diferentes queremos en nuestro experimento
# Replicas: cuantas veces vamos a repetir el tratamiento
# seed: siempre cada vez que se ejecute el codigo se obtenga la misma secuencia aleatoria
#y no cambie cada vez que ejecutamos el codigo

#leer numeros: 101 - repolicar 1 del tratamiento 1, 103 replica 1 del tratamiento 3

#Ahora debemos tomar observaciones
#TRAER UN EXPERIMENTO DE UNA CLASE !!!!

#Guardar tabla
mi_tabla <- design.crd(tratamientos, replicas, seed = 20)
mi_tabla

#$ indica salidas atomicas como vectores
#Guardar tabla 
tabla_diseno <- mi_tabla$book
# Nota: en R el simbolo $ sirve para acceder a un componente especifico de un objeto 

#Necesitamos un columna para respuestas de la observaciones pero en este caso lo vamos a simular
set.seed(1010) #crear semilla para guardar distribucion
#como no existe un algoritmo realmente aleotorio de numeros entonces la semilla 
#es un numero que alimenta a este algoritmo y te da un secuencia aleotoria constante

#Nota: algoritmo de numeros aleatorios: X_{n+1} = rX_{n}(1- X_{n}), a ecuacion del caos donde
#un valor inical de X_{0} da un secuenca determina al etirararse n veces
#ver veritasium: https://www.bilibili.com/video/BV1B7411W7LB?t=14.8


respuestas <- rnorm(20, mean = 5, sd = 2) #simular, rnorm indica un normal  
respuestas

#Grafica de densidad de probabilidad

plot(density(respuestas))
tabla_diseno$respuestas = respuestas #añadir un nueva tabla
tabla_diseno

attach(tabla_diseno)

#Ctr + A + ctrl + Enter - Copilar el codigo

boxplot(respuestas ~ tratamientos, data = tabla_diseno, 
        col = c("#2F8FE7", "#B849E0", "#49E0C6", "#70E049"), #midificar color de cada tratamiento
        xlab = "Tratamientos", #cambiar nombre de los ejes
        ylab = "Respuestas",
        main = "Experimento simulado")
#Nota: Boxplot: funcion de caja y bigotes - ¿Como se construye un boxplot?
# respuestas ~ tratamientos expresa la relacion entre dos variables de un fenomeno
#data = tabla_diseno especifica que datos se van utilizar para generar el boxplot
# El parámetro 'col' se utiliza para especificar los colores de los boxplots. 
# Se proporciona un vector de colores utilizando 'c()'.
# En este caso, se proporcionan cuatro colores diferentes en formato hexadecimal para representar cada uno de los tratamientos.
#col = c("#2F8FE7", "#B849E0", "#49E0C6", "#70E049")

# Los parámetros 'xlab' y 'ylab' se utilizan para etiquetar los ejes X e Y del diagrama, respectivamente.
# 'xlab' establece el texto "Tratamientos" como etiqueta del eje X.
# 'ylab' establece el texto "Respuestas" como etiqueta del eje Y.
#xlab = "Tratamientos"
#ylab = "Respuestas"

# El parámetro 'main' se utiliza para agregar un título al diagrama.
# 'main' establece el texto "Experimento simulado" como título del diagrama.
#main = "Experimento simulado"


#este es un analisis descriptivo
#Ver que el experimento sea replicable