#problema de bloques al azar

#Lew (2007) presenta los datos de un experimento para determinar si
#las células cultivadas responden a dos fármacos. El experimento se
#llevó a cabo usando una lı́nea celular estable en placas de Petri, y cada
#prueba experimental incluyó ensayos de respuestas en tres placas de
#Petri: una tratada con el fármaco 1, una tratada con el fármaco 2 y
#una no tratada que sirve como control. Los datos se muestran en la
#tabla a continuación:

#\begin{tabular}{lccc}
#\hline
#& Control & Droga 1 & Droga 2 \\
#\hline
#Experimento 1 & 1147 & 1169 & 1009 \\
#Experimento 2 & 1273 & 1323 & 1260 \\
#Experimento 3 & 1216 & 1276 & 1143 \\
#Experimento 4 & 1046 & 1240 & 1099 \\
#Experimento 5 & 1108 & 1432 & 1385 \\
#Experimento 6 & 1265 & 1562 & 1164 \\
#\hline
#\end{tabular}

#creado por AI

# Crear un data frame con los datos
datos <- data.frame(
  Bloque = rep(1:6, each = 3),
  Tratamiento = rep(c("Control", "Droga 1", "Droga 2"), times = 6),
  Respuesta = c(1147, 1169, 1009, 1273, 1323, 1260, 1216, 1276, 1143,
                1046, 1240, 1099, 1108, 1432, 1385, 1265, 1562, 1164)
)

# Ajustar el modelo ANOVA de dos vías
modelo <- aov(Respuesta ~ Bloque + Tratamiento, data = datos)

# Mostrar el resumen del análisis
summary(modelo)