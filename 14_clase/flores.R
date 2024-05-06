# Cargar la librería agricolae
#Esto se hace antes del experimento

#Considere la siguiente situación experimental. Un estudiante querı́a
#investigar sobre métodos para extender la vida de las flores cortadas.
#El factor de tratamiento fue el lı́quido para llenar el florero. Los niveles
#fueron:
#1. Agua del grifo
#2. Agua del grifo con una cucharada de azúcar agregada.
#3. Agua del grifo con una taza de agua carbonatada.
#4. Agua del grifo con una taza de 7-up

library(car)
library(agricolae)
library(gmodels)
library(mvtnorm)
library(multcomp)
library(survival)
library(ggplot2)

# Definir los tratamientos
tratamientos <- c("Agua del grifo", 
                  "Agua del grifo con azúcar", 
                  "Agua del grifo con agua carbonatada", 
                  "Agua del grifo con 7-up")

# Definir los bloques
bloques <- c("Rosa", "Clavel", "Margarita", "Tulipán")

# Generar el diseño de bloques completos aleatorizados
set.seed(1010) # Fijar una semilla para reproducibilidad
disenoFlores <- design.rcbd(trt = tratamientos, r = length(bloques), seed = 1010)

# Imprimir el diseño
disenoFlores

#matriz de diseño

matrizflores = disenoFlores$book
matrizflores

attach(matrizdiseno)

#ver matriz
disenoFlores$sketch

#ver tipo de variable, los bloques deben ser factores
typeof(block)

#cambiar tipo la variable block a un factor

matrizflores = as.factor(matrizflores$block)
typeof(block)

attach(matrizflores) #bloque como factor
block #como tiene niveles es un factor
levels(block)

length(block) #debe dar 16 y no 20

#ahora cambiar los nombres de los bloques

levels(block) = c("B1", "B2", "B3", "B4")
block

#HACER DIAGNOSTICO 
#install.packages("daewr")
library(daewr)

#Prueba de de aditividad 
?Tukey1df 
