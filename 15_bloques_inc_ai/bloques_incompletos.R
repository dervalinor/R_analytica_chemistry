#Ejemplo: Supongamos que eres un investigador que estudia el rendimiento de 
#diferentes variedades de maíz bajo diferentes condiciones de riego. 
#Tienes 8 variedades de maíz (tratamientos) que deseas evaluar, 
#pero solo puedes manejar 6 parcelas experimentales 
#(bloques) debido a limitaciones de recursos. 
#Cada parcela experimental se someterá a una condición de 
#riego diferente, lo que representa el factor de bloqueo.

#\begin{table}[H]
#\centering
#\begin{tabular}{lll}
#\hline
#Bloque & Tratamiento & Rendimiento \\
#\hline
#Riego normal & V1 & 5.2 \\
#Riego normal & V2 & 4.8 \\
#Riego normal & V5 & 5.1 \\
#Riego normal & V8 & 4.6 \\
#Riego escaso & V2 & 3.9 \\
#Riego escaso & V3 & 4.2 \\
#Riego escaso & V6 & 3.7 \\
#Riego escaso & V7 & 4.0 \\
#Riego excesivo & V1 & 4.5 \\
#Riego excesivo & V4 & 4.8 \\
#Riego excesivo & V7 & 4.2 \\
#Riego excesivo & V8 & 4.1 \\
#Riego salino & V3 & 3.8 \\
#Riego salino & V4 & 4.1 \\
#Riego salino & V5 & 4.0 \\
#Riego salino & V6 & 3.6 \\
#Agua reciclada & V2 & 4.6 \\
#Agua reciclada & V5 & 4.7 \\
#Agua reciclada & V6 & 4.3 \\
#Agua reciclada & V8 & 4.4 \\
#Agua de lluvia & V1 & 5.0 \\
#Agua de lluvia & V3 & 4.9 \\
#Agua de lluvia & V4 & 4.8 \\
#Agua de lluvia & V7 & 4.7 \\
#\hline
#\end{tabular}
#\caption{Datos observados}
#\label{tab:datos_maiz}
#\end{table}

# Cargar la librería necesaria
library(agricolae)
library(AlgDesign)

# Definir los tratamientos (variedades de maíz)
tratamientos <- paste("V", 1:8, sep = "")

# Generar el diseño BIA
set.seed(123) # Fijar una semilla para reproducibilidad

diseño_BIA <- design.bib(tratamientos, k = 4, serie=2, seed = 123)
#k es el tamaño de los bloques

?design.bib #no se si esta funcion es la que se utiliza

#bloques, tratamientos y respuestas

Bloque = rep(c("Riego normal", "Riego escaso", "Riego excesivo", "Riego salino", "Agua reciclada", "Agua de lluvia"), each = 4)
Tratamiento = rep(diseño_BIA$Tratamiento, 1)
Rendimiento = c(5.2, 4.8, 5.1, 4.6, 3.9, 4.2, 3.7, 4.0, 4.5, 4.8, 4.2, 4.1, 3.8, 4.1, 4.0, 3.6, 4.6, 4.7, 4.3, 4.4, 5.0, 4.9, 4.8, 4.7)

# Crear un data frame con los datos observados
datos <- data.frame(Re)

# Ajustar el modelo ANOVA
modelo <- aov(Rendimiento ~ Bloque + Tratamiento, data = datos)

# Mostrar el resumen del análisis
summary(modelo)
