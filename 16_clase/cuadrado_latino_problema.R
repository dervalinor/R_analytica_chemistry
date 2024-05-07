library(agricolae)

#Los datos que se muestran en la siguiente tabla fueron
#tomados de (Selwyn and Hall, 1984).

#\begin{table}[h]
#\caption{Datos tomados de (Selwyn and Hall, 1984)}
#\centering
#\begin{tabular}{cccc}
#\hline
#Sujeto & Período 1 & Período 2 & Período 3 \\
#\hline
#1 & A = 1186 & B = 642 & C = 1183 \\
#2 & B = 984 & C = 1135 & A = 1305 \\
#3 & 1426 & 1540 & 873 \\
#\hline
#\end{tabular}
#\label{tab:datos}
#\end{table}

#El propósito era probar la bioequivalencia de tres formulaciones. (A
#= solución, B = tableta, C = cápsula) de un medicamento medido
#por el AUC o el área bajo la curva, que relaciona la concentración del
#medicamento en la sangre en función del tiempo transcurrido desde
#la dosificación.
#Tres sujetos voluntarios tomaron cada formulación en sucesión con un
#perı́odo de lavado suficiente entre ellas. Después de la dosificación, se
#obtuvieron muestras de sangre cada media hora durante cuatro horas
#y se analizaron para determinar la concentración del fármaco. El AUC
#se calculó con los datos resultantes.

# Cargar los datos desde la tabla en un data frame

#segun la AI
#datos <- data.frame(
#  Sujeto = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#  Formulacion = rep(c("Solucion", "Tableta", "Capsula"), each = 3),
#  AUC = c(1186, 984, 1426, 642, 1135, 1540, 1183, 1305, 873)
#)


#Modo del profesor

tratamientos = c("A", "B", "C",
                 "B", "C", "A",
                 "C", "A", "B")

filas = rep(c(1,2,3), each = 3) #periodos
columnas = rep(c(1,2,3), 3) #sujetos
respuestas = c(1186, 984, 1426, 642, 1135, 1540, 1183, 1305, 873)

#convertir a factor
filas = factor(filas)
columnas = factor(columnas)

datos_medic = data.frame(respuestas, tratamientos,filas, columnas)
attach(datos_medic)

datos_medic

modelo <- aov(respuestas ~ tratamientos + filas + columnas, data = datos_medic)
#el p valor util es el de tratamientos por los cual no hay diferencias 
#en los tratamientos es decir el medicamentos no efecta como se da
#los demas no se interpretan pero las pruebas F de filas y columnas
# si f < 1 nos indica que no fueron significativos los bloques se 
#Puede hacer un DCA

summary(modelo)

#Hacer un entonces un DCA

madelo_DCA = aov(respuestas ~ tratamientos, data = datos_medic)
summary(madelo_DCA) #pero en DCA pero nos indica que hay diferencias de 
#LSD es mejor hacerle caso al DCA por que no era necesarion bloqueos por LSD
#hacer caso al DCA.

#hacer prueba de aditividad para ver si es valido y supuestos
#require(asbio)
#tukey.add.test(respuestas, tratamientos, filas) ver hipotesis de la aditividad
