#Para comparar cuatro dietas D 1 , D 2 , D 3 y D 4 , respecto a su influencia
#en el tiempo de coagulación de la sangre, se seleccionaron 24 animales y
#cada un recibió aleatoriamente una de las dietas. Los datos se muestran a
#continuación

#Hacer un diseño totalmente aleotarizado y Annova para comprobar hipotesis

# D1 = {61, 60, 63, 59} - 4 animales
# D2 = {63, 67, 71, 64, 65, 66} - 6 animales
# D3 = {68, 66, 71, 67, 68, 68} - 6 animales
# D4 = {56, 62, 60, 63, 63, 64, 63, 59} - 8 animales

#El diseño completamente aleorizado permite asignar de forma aleaotoria un tratamiento y evitar el sesgo de asignacion
#sesgo de asignacion.


tratamientos=rep(c("D1","D2","D3","D4"),c(4,6,6,8)) # c(4,6,6,8) indica el numero de animales que se aplico la dieta
tratamientos
#aplicando la funcion rep que sirve para repetir elementos de un vector por ejemplo D1 se repite 4 veces, D2 se repite 6 veces
#asi sucesivamente es decir obtenido un vector: "D1" "D1" "D1" "D1" "D2" "D2" "D2" "D2" "D2" "D2" "D3" "D3" 
# "D3" "D3" "D3" "D3" "D4" "D4" "D4" "D4" "D4" "D4" "D4" "D4" es cual se empareja con la variable respuesta

length(tratamientos) #Para indicar el numero de datos coompilados en la tablas, ademas para ver si el comando esta bien 

respuestas= c(62,60,63,59,
              63,67,71,64,65,66,
              68,66,71,67,68,68,
              56,62,60,63,63,64,63,59) #Los datos son separados para que se vean ordenados
length(respuestas)
datosdietas=data.frame(tratamientos,respuestas) #crear un marco de datos en una tabla donde se relaciona tratamientos y respuestas
datosdietas #ver en una tabla la relacion de datos de tratamientos y respuestas
attach(datosdietas) #Adjuntar datos al entorno de trabajo en R, esto hacer que la variable del marco de trabajo son accesibles por su
#nombre, estas variables se entiende como columnas para esta caso son las variable llamadas "tratamientos" y "respuestas"

boxplot(respuestas~tratamientos,data=datosdietas, col=c("#30C9DB","#E247E0","#2DCC90","#C2DE48")) #crear diagrama de caja
#que muestra la distribucion de datos en cuartiles
#como interpretar un boxplot

#La linea central representa la mediana, es decir el valor medio cuando los datos se ordenar de
#forma ascendente
#cuartiles - dividen los datos en 4 parte cada una con el 25%, donde debajo de la mediana de encuentra el cuartil uno que es el
#mediana de los datos debajo de la linea central
# ejemplo: datos = 3, 5, 6, 7, 9, 12 y 15 donde el cualtil 1 es 5 ya que es la mediana de los datos debajo de linea central (3, 5, 6)
#cuartil 2 es la linea central, cuartil 3 es la mediana de los datos arriba de linea central (9, 12, 15)

#informacion que nos da un boxplot: 1. identificación de la tendencia central y la dispersión  es decir ver donde se 
#concentran los datos y como se dispersan estos. 2. también permite identificar valores que estan fuera de la tendencia de los 
#datos, 3. comparación de distintos tratamientos y como se distribuye sus datos

#ANOVA - Analisis de Varianza es utilizada para evaluar si hay diferencias significativas de las medias
#entre varios grupos en esta caso son en las dietas como afectan el tiempo de coagulación, para esto se plantea
#dos hipotesis, la hipotesis nula donde no existen diferencias significativas de medias en los grupos y la hipotesis
#alternativa donde existen diferencias entre la medias de los grupos, al final obtenemos el valor F el cual nos indica si
#la variabilidad entre los grupos grupos es significativa respecto a la variabilidad dentro del grupo
# F = SSG/SSE donde SSG representa la variabilidad entre grupos (respecto a la media de todos los datos) y 
#SSE representa la variabilidad dentro del grupo (respecto a la media de cada grupo). Si F es alta
#esto quiere decir que la variabilidad es mayor entre grupos que dentro de estos
# La variabilidad se mide como la suma de cuadrados respecto a la media \[ SST = \sum_{i=1}^{n} (x_i - \bar{x})^2 \]

modelodietas=aov(respuestas~tratamientos,data=datosdietas)
summary(modelodietas)

#SUPUESTOS -diagnosticos
residualesd=residuals(modelodietas) #calcula la diferencia entre el modelo de regresión y los datos reales 
# Residuo = datos real - datos predicho por la regresion el cual viene del ANOVA, esto sirve para saber si existe
#un distribucion normal en tal caso los residuos son pequeños y evaluar aleatoridad
residualesd

#Es prime supuesto que se va evaluar es Normalidad, esto se hara por medio de 
#graficos de densidad y Q-Q plot

#Gráficos
par(mfrow=c(1,2))#División plano: Esto permite mostrar en la ventana grafica los dos graficos
plot(density(residualesd)) #Este es primer grafico que se muestra y es la densidad de residuos esta debe ser
#de una forma de campana simetrica 
plot(modelodietas,which=2) #Grafico cuantil cuantil, which = 2 indica para evaluar la normalidad de los residuos
#para q-q plot, donde el eje y indica los residuos reales y eje x los residuos teoricos, si muchos puntos forman una
#linea recta en la diagonal entonces existe un distribución normal de los datos, pero en caso contrario formando por ejemplo 
#un arco, un tendencia exponencial hacia arriba no normalidad no es buena o no existe 
#(ver https://www.statology.org/qq-plot-interpretation/)


#Los graficos ayudan en la coclusión del anova, sin embargo hace falta una prueba de hipotesis
dev.off()

#Prueba de normalidad
shapiro.test(residualesd)
#No rechazo H, acepto H
#Hay normalidad

#Supuesto de Homocedasticidad
#Gráfico
plot(modelodietas,which=1) #No hay embudos
#install.packages("car")

#Test de Bartlett
bartlett.test(respuestas~tratamientos,data=datosdietas)
#Hay homocedasticidad, no rechazo H

#Independencia (Supuesto)
plot(1:24,residualesd,pch=8)

#Interpretacion anova
#los tratamientos o las dietas si tienen efecto en la variable respuesta que es el tiempo de coagulación 
