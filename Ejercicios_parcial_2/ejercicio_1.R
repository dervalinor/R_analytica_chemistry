#El experimento es para determinar la dureza de cuatro punta en maquina que las
#someteran a presion

#el siguiente es la variables de las puntas a probar
puntas= rep(c("punta1","punta2","punta3","punta4"), 4)
#las repeticiones anteriores son para todo el vecto, segun la orden este se 
#repetira 4 veces
puntas

length(puntas)

#el siguiente son los bloques o ambiente que se someteren las puntas 
bloquesm=rep(c("M1","M2","M3","M4"), each = 4)
#la repeticion anterior hace repetir cada elemento segun la orden, en este caso
#se repetira 4 vecves cada elemanto 
bloquesm
length(bloquesm)

#el siguiente son los resutados obtenidos 
resultado=c(9.3, 9.4, 9.2, 9.7,
            9.4, 9.3, 9.4, 9.6,
            9.6, 9.8, 9.5, 10,
            10, 9.9, 9.7, 10.2)
length(resultado)

#el siguiente es para formar un marco de datos el cual relacionara los elementos enteriores 
marcodedatos=data.frame(resultado, puntas,bloquesm)
marcodedatos
#lo anterios relaciono de la manera propuesta los datos, en este caso las puntas
#se relacionas 4 en cada bloque 

#el attach nos sirve para dividir en marco de datos en sus variables y no tener
#que llamarlas dentro de este.
attach(marcodedatos)

#Anova
modeloanovapuntas = aov(resultado ~ puntas + bloquesm, data = marcodedatos)
summary(modeloanovapuntas)
#lo anterio es que como es mallor del 5%=0,05 dice que se acepta la hipotesis nula

#el siguiente es una prueba de aditividad, es para saber si cada bloque no tienen
#interaciones entre ellos.

puntasad = factor(puntas)
bloquesmad = factor(bloquesm)

marco_ad = data.frame(resultado, puntasad, bloquesmad)

attach(marco_ad)

#Como en el resultado el p valor es mayor al 0.05 
#entonces se acepta la hipotesis nula existe aditividad
#por lo cual no hay interaccion de los bloques y las puntas
Tukey1df(marco_ad)