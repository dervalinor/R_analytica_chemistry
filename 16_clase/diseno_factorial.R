#antes los tratamientos eran los niveles de un factor
#ahora los tratamientos son interacciones entre niveles de factores

#hacer un grafico de interaccion

#Dos razas de Drosophila pseudoobscura fueron producidas
#por endogamia para resistir un insecticida. Se analizaron cuatro nive-
#les de concentración de insecticida en ambas razas. Los datos de la
#siguiente tabla, que expresan la mortalidad en pocentaje durante un
#periodo de tiempo determinado, están basado en tres repeticiones de
#cada tratamiento

#para le primer tratamiento

razas = rep(c("R1", "R2"), each = 12)
concentraciones = rep(rep(c("C1", "C2", "C3", "C4"), each = 3), 2)
respuestas = c(60, 55, 52, 44, 37, 54, 46, 51, 63, 31, 57, 66, 37, 43, 50, 63, 59, 54, 30, 38, 38, 51, 50, 41)
length(respuestas)

data.frame(respuestas, razas, concentraciones)
