#Se diseñó un experimento para comparar la efectividad de cinco méto-
#dos de almacenamiento en la preservación del contenido de agua de
#un producto alimenticio. Se hicieron cinco repeticiones por tratamien-
#to en un diseño completamente aleatorizado. Por fallas en el proceso
#varias repeticiones se perdieron. Los métodos de almacenamiento y
#los resultados obtenidos se dan en porcentajes en la siguiente tabla



icoportres<-c(7.8,8.3,7.6,8.4,8.3)
icopordiez<-c(5.4,7.4,7.1)
icoporquince<-c(8.1,6.4)
shopakdiez<-c(7.9,9.5,10)
shopakquince<-c(7.1)
empaques<-rep(c("icoportres","icopordiez","icoporquince","shopakdiez","shopakquince"),c(5,3,2,3,1))
tiempos<-c(icoportres,icopordiez,icoporquince,shopakdiez,shopakquince)

empaques = factor(empaques)

#Marco de datos
datosAgua = data.frame(tiempos, empaques)

#dividir los datos en columnas
attach(datosAgua)

#Anova

modelo_agua = aov(tiempos ~ empaques, data = datosAgua) #Esta prueba sirva cuando el ANOVA detecta diferencias
resumen_anova = summary(modelo_agua)

#ver si hay diferencias

p_valor_aov = resumen_anova[[1]][["Pr(>F)"]][1]

if(p_valor_aov >= 0.05){
  print(paste("Existen pruebas suficientes para aceptar la hipotesis nula entonces 
        no hay diferencias significativas entre tratamientos con p valor (p >= 0.05): ", p_valor_aov))
} else {
  print(paste("Se rechaza la hipotesis nula si hay diferencias significativas entre tratamientos con p valor (p < 0.05): ", p_valor_aov))
}

#En este caso no funciona la Prueba de Battlet por que no hay las mismas replicas

boxplot(tiempos ~ empaques, col = c("#2EE689","#DF4BF3","#4BE2F3", "#DDD53E", "#DD513E"), #apender colores en ingles
        xlab = "Tiempos",
        ylab = "Empaques")

#Hacer contrastes de los tratamientos sin importar los tiempos comparacion de materiales

#Programar esto a mano para estudiar bien esto!!!!












