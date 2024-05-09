#Estos son los resultados de un experimento de dos
#factores dado por Hunter (1983). En estos datos, un experimento
#consistió en quemar una cantidad de combustible y determinar las
#emisiones de CO liberadas. La unidad experimental es la porción de
#un combustible estándar requerido para una ejecución, y la respuesta,
#y, es la concentración de emisiones de monóxido de carbono (CO) en
#g/m 3 determinada a partir de esa ejecución. Hubo dos ejecuciones
#repetidas para cada combinación de niveles de factores. El factor A es
#la cantidad de etanol agregado a una unidad experimental o porción
#del combustible estándar, y el factor B es la relación combustible-aire
#utilizada durante la combustión de ese combustible.

#\begin{tabular}{ccc}
#\hline
#Adiciones de etanol & aire/combustible & emisiones CO \\
#\hline
#0,1 & 14 & 66;62 \\1
#0,1 & 15 & 72;67 \\2
#0,1 & 16 & 68;66 \\3
#0,2 & 14 & 78;81 \\4
#0,2 & 15 & 80;81 \\5
#0,2 & 16 & 66;69 \\6
#0,3 & 14 & 90;94 \\7
#0,3 & 15 & 75;78 \\8
#0,3 & 16 & 60;58 \\9
#\hline
#\end{tabular}

#Hipotesis: existe algun efecto por la adiccion de etanol, relacion de 
#aire/combustible o la interaccion de ambos factores causan algun cambio 
#en la emision de CO.

#Es importante convertir a factor las adiciones de etanol y aire/combustible en
#factores
library(agricolae)

addicion_etanol = rep(c("0.1", "0.2", "0.3"), each = 6)
addicion_etanol
length(addicion_etanol)

aire_combustible = rep(rep(c("14", "15", "16"), 3), each = 2)
aire_combustible
length(aire_combustible)

#solucionar como agregar datos
emisiones_co = c(66,62,72,67,68,66,78,81,80,81,66,69,90,94,75,78, 60,58) 
#IMPORTANTE VER QUE LOS DATOS SE INTRODUCCIERON BIEN
#Si si se puedo todo salio bien
emisiones_co
length(emisiones_co)

datos_emisiones = data.frame(emisiones_co, addicion_etanol, aire_combustible)
datos_emisiones
attach(datos_emisiones)

#Analisis de ANOVA

modelo_emisiones = aov(emisiones_co ~ addicion_etanol * aire_combustible, 
                       data = datos_emisiones)
summary(modelo_emisiones) #el mismo resultado que el profesor en el pdf

dev.off()

#grafico de interaccion
interaction.plot(addicion_etanol, aire_combustible, emisiones_co,
                 xlab = "Adiciones Etanol", ylab = "Aire/Combustible",
                 trace.label = "Emisiones de CO")
