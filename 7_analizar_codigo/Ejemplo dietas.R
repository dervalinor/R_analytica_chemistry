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

modelodietas = aov(respuestas~tratamientos,data=datosdietas)
resumen_anova = summary(modelodietas)

#Hipotesis nula: no hay diferencias significativas entre las medias de los grupos es decir
#los tratamientos de dietas no afectan la coagulacion de la sangre

#ver si rechaza o se acepta la hipotesis nula
p_valor_aov <- resumen_anova[[1]][["Pr(>F)"]][1]

#se rechaza H cuando el nivel p < niveles de significancia en caso contrario no rechazo H
if(p_valor_aov >= 0.05){
  print("Se acepta la hipotesis nula no hay diferencias significativas entre las dietas")
} else {
  print("Se rechaza la hipotesis nula si hay diferencias significativas entre las dietas")
}


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


#Los graficos ayudan en la conclusión del anova, sin embargo hace falta una prueba de hipotesis
dev.off() #cerrar graficos creados para dejar limpio la seccion de graficos

#Prueba de normalidad
resultado_test <- shapiro.test(residualesd) #esta prueba nos devuelve un p valor para evaluar normalidad de un modelo anova
#para este caso la hipotesis nula es que la modelo tiene un distribucion normal 
#se acepta la hipotesis nula cuando p-valor es mayor que el nivel se significancia en caso contrario
#se rechaza la hipotesis nula y no existe normalidad

#obtener p-valor
p_valor <- resultado_test$p.value

#vamos a ver si acepta o se rechaza la hipotesis nula

if(p_valor >= 0.05) {
  print("Se acepta la hipotesis nula, entonces existe normalidad en el modelo")
} else {
  print("Se rechaza la hipotesis nula, entonces no existe normalidad")
}

#para esta caso para aceptar la hipotesis de normalidad el p valor debe ser
#mayor al nivel de significancia es decir a 0.05  

#nivel de significancia es el umbral para aceptar o rechazar la hipotesis nula

#No rechazo H, acepto H la cual es que existe normalidad
#Hay normalidad ya que  p-value = 0.9582 es mayor al nivel de significancia 


#Supuesto de Homocedasticidad
#Gráfico
plot(modelodietas,which=1) #the argumento "which = 1" indica que se evaluara homocedasticidad
#El concepto de homocedasticidad es la suposicion donde los errores de un modelo estadistico es constante en la
#variable independiente, por ende no existen variaciones de errores en la variable independiente es decir se 
#tiene la mismo precision en las medidas
#Nos da la precision del modelo de normalidad, es decir que el modelo puede ser apto para tratar estos
#datos y que sus resultados sean viables y la conclusiones obtenidas por el modelo sean validas

#Ejemplos: si esta fabricando cintas metricas cada cinta metrica debe tener la misma precision para sus mediciones
#para asi tener cintas metricas se obtenga resultados confiables.

#Ejemplo: Imagina que quieres estimar con un modelo matematico cuanto tiempo te tomaria llegar en auto a tu destino
#si la viriabilidad de la estimaciones del modelo y la realidad es constante en cualquier punto entonces existe
#homocedasticidad y el modelo es confiable para predicir cuanto tiempo te llevara ir a un lugar pero si la variabilidad
#de estimaciones es variable en distintos puntos entonces el modelo no es confiable en sus prediciones


#No hay embudos, se debe observar un patron aleatorio y no en forma de embudo (cuando lo residuos se estrechan o se amplia a partir
#de aumento de valores), debe existir una distribucion homogenea de errores son patrones, los patrones indican sesgos
#embudos es cuando los residuos son mas pequeños en el centro y mas grande a los lados, esto indicaria
#que la variacion de residuos no es constante los cual viola la homocedasticidad, ya que idealmente
#debe hacer una distribucion uniforme y aleatoria esto indica que los errores son constantes pero si existe
#un patron entonces quiere decir que la variabilidad de errores no es constante y no hay homocedasticidad

#install.packages("car")

#Test de Bartlett
prueba_bartlett <- bartlett.test(respuestas~tratamientos,data=datosdietas) #mira varibilidad de los datos en sus varianzas de varios grupos
#de datos, aqui la hipotesis nula es que la varianza de los grupos son iguales, recordar que la varianza mide la dispersion de los 
#datos

#Si el valor p es mayor que el nivel de significancia (generalmente 0.05), no hay suficiente evidencia para rechazar la hipótesis nula. 
#Esto sugiere que las varianzas de los grupos son iguales y que se cumple la homocedasticidad en el análisis de regresión.

p_valor_b = prueba_bartlett$p.value

if(p_valor_b >= 0.05){
  print("Existe homocedasticidad, varianzas de los dietas similares")
} else {
  print("No existe homocedasticidad, varianzas son diferentes")
}

#p valor indica la probalidad de la hipotesis

#si el p valor es menor al nivel de significancia 0.05 entonces se rechaza la hipotesis nula y se considera que existe una diferencia
#significatica entre la varianza en caso contrario se acepta la hipotesis nula por lo cual se concluye las varianzas entre los 
#grupos son iguales

#Hay homocedasticidad, no rechazo H

#Independencia (Supuesto)
plot(1:24,residualesd,pch=8) #pch = 8 indica un marcador de puntos solidos
#Este codigo genera un grafico de dispersion para evaluar independencia de ANOVA, es decir 
#que la observacion realizada no esta afectada por otra observacion realizada en el analisis 
#se tiene que observar puntos aleatorios y no patrones para decir que existe independencia
#y asi ser valido el analisis de ANOVA, esto se hacer observando lo residuos 
#residuo = respuesta_observada - respuesta_predicha_modelo
#en esta caso es el modelo ANOVA que por medio de las medias piensa predecir los valores que se observara
#ademas de ver si las diferencias de medias son significativas

# ANOVA también evaluaría si las diferencias entre los puntajes promedio de los equipos son lo 
#suficientemente grandes como para ser consideradas significativas desde un punto de vista estadístico. 
#Esto se hace mediante la comparación de la variabilidad entre los grupos con la variabilidad dentro de los grupos.

#Interpretacion anova
#los tratamientos o las dietas si tienen efecto en la variable respuesta que es el tiempo de coagulación 
#siempre que p valor sea mayor a 0.05 se acepta la hipotesis nula !!!!!!

#Es importante recordar que el valor p es una medida de la evidencia en contra de la hipótesis nula, 
#y un valor p mayor que el nivel de significancia no necesariamente significa que la hipótesis nula sea cierta. 
#Simplemente indica que no hay suficiente evidencia en los datos para rechazarla.

#El p-valor es la probabilidad de obtener resultados 
#al menos tan extremos como los observados en la muestra, si la hipótesis nula es verdadera.

#IMPORTANTE!!!!!!!
#Si el p-valor es menor o igual que el nivel de significancia (α), generalmente 0.05, se rechaza la 
#hipótesis nula. Esto significa que hay suficiente evidencia en los datos para concluir que el efecto 
#observado no ocurrió por azar y que hay una diferencia significativa entre los grupos o condiciones.