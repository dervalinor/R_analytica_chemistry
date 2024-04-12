#Pruebas no parametricas

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

Prueba_kruskal <- kruskal.test(respuestas ~ tratamientos, data = datosmesofilos)

Prueba_kruskal #se utliza cuando se tiene violacion de los supuestos para ANNOVA