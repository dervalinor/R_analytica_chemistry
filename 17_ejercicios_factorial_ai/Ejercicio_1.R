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
#0,1 & 14 & 66;62 \\
#0,1 & 15 & 72;67 \\
#0,1 & 16 & 68;66 \\
#0,2 & 14 & 78;81 \\
#0,2 & 15 & 80;81 \\
#0,2 & 16 & 66;69 \\
#0,3 & 14 & 90;94 \\
#0,3 & 15 & 75;78 \\
#0,3 & 16 & 60;58 \\
#\hline
#\end{tabular}

#Hipotesis: existe algun efecto por la adiccion de etanol, relacion de 
#aire/combustible o la interaccion de ambos factores causan algun cambio 
#en la emision de CO.

#Es importante convertir a factor las adiciones de etanol y aire/combustible en
#factores

addicion_etanol = rep(rep(c("0.1", "0.2", "0.3"), each = 3), 2)
addicion_etanol

aire_combustible = rep(rep(c("14", "15", "16"), 3), each = 2)
aire_combustible

#solucionar como agregar datos
emisiones_co = c(c(66,62), c(72,67), c(68, 66), c(78, 81), c(66, 69), c(90, 94), c(75, 78), c(60, 58))
emisiones_co

datos_emisiones = data.frame(emisiones_co, addicion_etanol, aire_combustible)
