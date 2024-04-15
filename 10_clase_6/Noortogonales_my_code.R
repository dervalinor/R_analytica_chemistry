#contrastes no ortogonales

#Es cuando los tratamientos estan relacionados con los demas es decir cada 
#tratamiento no es independiente de los otros, es decir se superponen entre si. 

#Ejemplo: es como tener en una 
#mesa platos que comparten ingredientes similares

#Se diseñó un experimento para comparar la efectividad de cinco méto-
#dos de almacenamiento en la preservación del contenido de agua de
#un producto alimenticio. Se hicieron cinco repeticiones por tratamien-
#to en un diseño completamente aleatorizado. Por fallas en el proceso
#varias repeticiones se perdieron. Los métodos de almacenamiento y
#los resultados obtenidos se dan en porcentajes en la siguiente tabla

#1. Se desea comparar materiales- Icopor y Shopak - y tiempos

#esto tratamientos no son indenpendientes ya que compartes cosas similares como
#meteriales y tiempo.

# En este experimento, se están comparando cinco métodos de almacenamiento
# (icoportres, icopordiez, icoporquince, shopakdiez, shopakquince) para
# preservar el contenido de agua de un producto alimenticio.
#
# Estos métodos de almacenamiento no son completamente independientes entre sí,
# ya que comparten factores en común:
#   - Dos de ellos utilizan el material Icopor (icoportres, icopordiez, icoporquince)
#   - Dos de ellos utilizan el material Shopak (shopakdiez, shopakquince)
#   - Tres de ellos tienen el mismo tiempo de almacenamiento de 10 días (icopordiez, shopakdiez)
#   - Dos de ellos tienen el mismo tiempo de almacenamiento de 15 días (icoporquince, shopakquince)
#
# Debido a esta superposición de factores (material y tiempo de almacenamiento),
# los tratamientos no son completamente independientes entre sí. Esto implica que
# los efectos de un tratamiento pueden estar relacionados o influenciados por los
# efectos de los otros tratamientos.
#
# En un caso de contrastes ortogonales, los tratamientos serían completamente
# independientes entre sí, sin ninguna superposición de factores.
#
# Al tener esta falta de independencia entre los tratamientos, los contrastes
# que se definan (en este caso, q1, q2 y q3) no serán ortogonales, es decir,
# los productos internos entre algunos pares de contrastes no serán cero.
#
# El código verifica la no ortogonalidad de los contrastes calculando los
# productos internos entre todos los pares de contrastes, y muestra que
# algunos de ellos no son cero.
#
# Por lo tanto, este problema se considera de contrastes no ortogonales
# debido a la falta de independencia entre los métodos de almacenamiento
# (tratamientos), lo que requiere un análisis y ajustes adicionales para
# interpretar los resultados correctamente.

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

#Prueba de contraste - Matrix de contrastes

q1 = c(2,2,2, -3, -3)
q2 = c(0,2,2,-2,-2)
q3 = c(2,-1,-1,0,0)


#ver si son ortogonales

# Calculamos el producto interno entre todos los pares de contrastes
contrastes <- list(q1, q2, q3)

for (i in 1:(length(contrastes) - 1)) {
  for (j in (i + 1):length(contrastes)) {
    producto_interno <- sum(contrastes[[i]] * contrastes[[j]])
    cat("Producto interno entre q", i, "y q", j, ": ", producto_interno, "\n") #poner un condicional para ver 
    #si son ortogonales
  }
}

#crear matriz de contrastes

Matrix_contraste = matrix(c(q1, q2, q3),  nrow = 3, ncol = 5, byrow = TRUE)
row.names(Matrix_contraste) = c("q1", "q2", "q3")


#Prueba de Contrastes
library(multcomp)

comp = glht(modelo_agua, linfct = mcp(empaques = Matrix_contraste))
comp
  
#Ver metodos de Bonferroni y otro de la diapositivas

#Este metodo ajusta el nivel de significancia segun el numero de comparaciones que se realiza
#es decir se toma el nivel de significancia generalmente 0.05 y se divide por el numero de comparaciones
#esto se hace para evitar rechazar la hipotesis nula cuando es cierta, es decir, cuando mas comparaciones
#se realiza se corre el riesgo que por azar una de ellas sea mayor al nivel de significancia 0.05
#rechazando la hipotesis nula cuando esta es cierta pero esto es debido al azar, al ajustar el 
#nivel de significancia evitamos esta error.

Resumen_contrastes = summary(comp, test = adjusted("bonferroni"))
Resumen_contrastes  

#ajuste del nivel de signiificancia
nivel_significancia = 0.05/length(contrastes)
nivel_significancia

#valor p de los contrastes
p_valores = Resumen_contrastes$test$pvalues

#ver si acepta o rechaza la hipotesis nula, es decir que no hay efectos en la 
#conservacion de agua segun el material y el tiempo

for(i in 1:length(contrastes)){
  if(p_valores[i] >= nivel_significancia){
    cat("Se acepta la hipotesis nula para el contraste ", i, " con p valor de ", p_valores[i], " mayor al nivel de significancia ", nivel_significancia, " \n")
  } else {
    cat("NO se acepta la hipotesis nula para el contraste ", i, " con p valor de ", p_valores[i], " menor al nivel de significancia ", nivel_significancia, " \n")
  }
}

#p-valor es muy alto por lo cual se acepta todas la hipotesis nulas

#Prueba de Sheffe ver en las diapositivas !!!!

