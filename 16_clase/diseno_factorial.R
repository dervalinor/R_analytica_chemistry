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

#\begin{tabular}{cccc}
#Respuesta & Raza & Concentración \\
#\hline
#60 & R1 & C1 \\
#55 & R1 & C1 \\
#52 & R1 & C1 \\
#44 & R1 & C2 \\
#37 & R1 & C2 \\
#54 & R1 & C2 \\
#46 & R1 & C3 \\
#51 & R1 & C3 \\
#63 & R1 & C3 \\
#31 & R1 & C4 \\
#57 & R1 & C4 \\
#66 & R1 & C4 \\
#37 & R2 & C1 \\
#43 & R2 & C1 \\
#50 & R2 & C1 \\
#63 & R2 & C2 \\
#59 & R2 & C2 \\
#54 & R2 & C2 \\
#30 & R2 & C3 \\
#38 & R2 & C3 \\
#38 & R2 & C3 \\
#51 & R2 & C4 \\
#50 & R2 & C4 \\
#41 & R2 & C4 \\
#\end{tabular}

library(agricolae)

razas = rep(c("R1", "R2"), each = 12)
concentraciones = rep(rep(c("C1", "C2", "C3", "C4"), each = 3), 2)
respuestas = c(60, 55, 52, 44, 37, 54, 46, 51, 63, 31, 57, 
               66, 37, 43, 50, 63, 59, 54, 30, 38, 38, 51, 50, 41)
length(respuestas)

datos_razas = data.frame(respuestas, razas, concentraciones)
attach(datos_razas)

#segun la inteligence artificial: 

modelo <- aov(respuestas ~ razas * concentraciones, data = datos_razas)

summary(modelo) #Si este es correcto segun la diapositiva del profesor !!!!!

resumen <- LSD.test(modelo, "razas", "concentraciones", p.adj = "bonferroni")
print(resumen)

?LSD.test

interaction.plot(razas, concentraciones, respuestas, legend = TRUE, 
                 xlab = "Raza", ylab = "Mortalidad (%)", 
                 trace.label = "Concentración")
?interaction.plot