#Pruebas no parametricas

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