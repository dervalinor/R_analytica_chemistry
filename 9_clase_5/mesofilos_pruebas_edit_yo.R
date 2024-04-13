#Pruebas no parametricas

#Las pruebas no parametricas se utilizan cuando un conjunto de datos
#no cumplen los supuestos de normalidad, homocedasticidad o independencia

#para esta caso no se cumplen los supuestos ni de normalidad ni homocedasticidad
#en este experimento entonce se realiza la prueba no parametrica de  Kruskal-Wallis

#Prueba de Kruskal-Wallis

#Es la version del ANOVA pero en pruebas no parametricas y se utiliza cuando
#los datos no cumplen los supuestos de normalidad y homocedasticidad como es 
#en este caso.

#Esta prueba consiste en ordenar los datos de menor a mayor sin importar a que grupo pertenecen
#se calculan los rangos y hay diferencias signifitivas entre rangos entoces existen
#diferencias significativas entre los grupos de datos, esto se ve en un p-valor < 0.05
#por lo cual se rechaza la hipotesis nula de que no existen diferencias significativas entre
#los grupos. Es similar al ANOVA pero con rangos.

#En un estudio de diferentes empaques para la carne, Gómez y González(1991)
#realizaron una prueba microbiológica y contabilizaron el número de
#mesófilos en las diferentes muestras analizadas. Los resultados se pre-
#sentan en la siguiente tabla

#se van a hacr  5 replicas, empaques T1, T2, T3, T4, T5 Y T6
#T1 = 280, 30, 0, 10, 50
#T2 = 10, 20, 20, 20, 30
#T3 = 80, 90, 10, 90, 60
#T5 = 10, 10, 10, 20, 0
#T6 = 30, 40, 40, 40, 10

#analizar Si los empaques afecta el Número de mesófilos en cada una de las diferentes envolturas.
#Evalúe los supuestos de normalidad y homogeneidad de varianzas

tratamientos=rep(c("T1","T2","T3","T4","T5","T6"),c(5,5,5,5,5,5))
tratamientos
length(tratamientos)
respuestas=c(280,30,0,10,50,
             10,20,20,20,30,
             80,90,10,90,60,
             10,10,10,20,0,
             10,20,40,10,10,
             30,40,40,40,10)
length(respuestas)
datosmesofilos=data.frame(tratamientos,respuestas)
datosmesofilos
attach(datosmesofilos)

#herramientas de transformacion de datos para mejerar la homoselastidad y normalidad, pero esto
#no puede funcionar, para esta caso se utiliza estadistica no parametrica
#usar prueba de Kruscal

Prueba_kruskal = kruskal.test(respuestas ~ tratamientos, data = datosmesofilos)

Prueba_kruskal #se utliza cuando se tiene violacion de los supuestos para ANNOVA

p_valor = Prueba_kruskal$p.value

if(p_valor >= 0.05){
  cat("Se acepta la hipotesis nula, NO existen diferencias significativas entre los tratamientos con un p valor (p>=0.05): ", p_valor, "\n")
} else {
  cat("Se rechaza la hipotesis nula, SI existen diferencias significativas entre los tratamientos con un p valor (p<0.05): ", p_valor, "\n")
}

#Kruskal-Wallis rank sum test

#data:  respuestas by tratamientos
#Kruskal-Wallis chi-squared = 10.068, df = 5, p-value = 0.07332

#Aquí está lo que significa cada parte:
  
#"Kruskal-Wallis rank sum test": Indica que se está realizando la prueba de Kruskal-Wallis, que es una prueba 
#no paramétrica basada en los rangos de los datos.
#"data: respuestas by tratamientos": Especifica que los datos utilizados para la prueba son las "respuestas" (variable numérica) 
#grupadas por "tratamientos" (variable categórica).
#"Kruskal-Wallis chi-squared = 10.068": Este valor es el estadístico de prueba calculado, 
#que sigue una distribución chi-cuadrada. Un valor más alto indica una mayor evidencia en contra de la hipótesis nula.
#Esto no ayuda a determinar si hay diferencias significativas entre los tratamientos, un valor alto de 
#chi-squared indica que existen diferencias significativas entre los tratamientos (rechazo de la hipotesis nula)

#"df = 5": Indica que hay 5 grados de libertad en esta prueba, lo cual significa que hay 6 grupos o tratamientos diferentes.
#este indica de cuantas maneras se pueden varias los datos para esta caso son el numero de tratamientos, es decir es como elegir
#donde puedes clasificar algo o donde se puede distribuir algo.

#"p-value = 0.07332": Este es el valor p o p-valor, que representa la probabilidad de obtener un resultado 
#tan extremo o más extremo que el observado, si la hipótesis nula es verdadera.