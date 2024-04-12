#vamos a crear un funcion que nos permita crear contrastes entre tratamientos
#donde los coefientes su suma sea cero en un combinacion lineal de las medias al
#Se diseñó un experimento para comparar la longitud de las secciones
#de guisantes sembrados en un cultivo de tejidos, cuyo medio con-
#tenı́a auxina. El objetivo del experimento era contrastar los efectos de
#la adición de diferentes azúcares en el crecimiento, expresado por la
#longitud alcanzada. Se utilizaron cinco tratamientos: Cuatro grupos
#experimentales y un control sin azucar. Entre los grupos experimenta-
#les se ensayaron tres azúcares por aparte (glucosa, fructosa y sacarosa
#y una mezcla de glucosa y fructosa). Se hicieron 10 repeticiones por
#tratamiento en un diseño completamente aleatorizado.

#sin azucar control<-c(75,67,70,75,65,71,67,67,76,68)
#Glucosa al 2% D1<-c(57,58,60,59,62,60,60,57,59,61)
#Fructosa al 2% D2<-c(58,61,56,58,57,56,61,60,57,58)
#Sacarosa al 2% D3<-c(62,66,65,63,64,62,65,65,62,67)
#Glucosa al 1% y fructosa al 1% D4<-c(58,59,58,61,57,56,58,57,57,59)

#Preguntas

#1. Se afecta la longitud de las secciones de guisantes por la adición de
#azúcares?
#2. Habrá diferencias de longitud al adicionar azúcares puros y azúcares
#mezclados.
#3. Habrá diferencias entre agregar disacáridos(sacarosa) y monosacári-
#dos(glucosa, fructosa y glucosa + frucosa)?
#4. Habrá diferencias entre glucosa y fructosa?

#Crear una funcion que me permita calcular coeficientes igual a cero dependiendo que se compara

control<-c(75,67,70,75,65,71,67,67,76,68)
D1<-c(57,58,60,59,62,60,60,57,59,61)
D2<-c(58,61,56,58,57,56,61,60,57,58)
D3<-c(62,66,65,63,64,62,65,65,62,67)
D4<-c(58,59,58,61,57,56,58,57,57,59)

#creamos un variabla para guardar todos los tratamientos para hacer luego algunos
#calculos

longitudes<-c(control,D1,D2,D3,D4)
length(longitudes)

azuc<-rep(c("control","D1","D2","D3","D4"),
          each=10)
azuc=factor(azuc)###azuc: categorica
datos<-data.frame(longitudes,azuc)
attach(datos)
datos

#Prueba de ANOVA 
modelogui=aov(longitudes~azuc,data=datos)
residuosgui=residuals(modelogui)
residuosgui
par(mfrow=c(1,2))
plot(density(residuosgui))
plot(modelogui,which=2)
shapiro.test(residuosgui)
plot(modelodietas,which=1)
library(car)
bartlett.test(longitudes~azuc,data=datos)

dev.off()
plot(1:50,residuosgui,pch=9) #es hasta 50 ya que son 50 datos
#interprestacion anova
summary(modelogui)

#Prueba de contrastes ortogonales
##introduccion contrastes - Como rayos se genera esta estos vectores de contrastes

#ClaudeAI: No, los coeficientes de los contrastes ortogonales no se eligen de forma arbitraria. Hay un proceso sistemático para determinarlos, y este proceso está directamente relacionado con las preguntas de investigación que se quieren responder.

#En general, los pasos para definir los contrastes ortogonales son los siguientes:
  
#Identificar las preguntas de interés que se quieren responder comparando los tratamientos.
#Establecer una combinación lineal de las medias de los tratamientos que represente cada una de esas preguntas.
#Asegurarse de que los coeficientes de esa combinación lineal sumen cero.

#1. Se afecta la longitud de las secciones de guisantes por la adición de
#azúcares? - control vs otros tratamientos
q1=c(-4,1,1,1,1) 
#comparacion de control vs D1, D2, D3 y D4
#combinacion lineal -4*Media_control + (1)Media_D1 + (1)Media_D2 + (1)Media_D3 + (1)Media_D4 = numero
#donde debe cumplirse -4+1+1+1+1 = 0 para los coeficientes, estas dos condiciones deben cumplirse

#2. Habrá diferencias de longitud al adicionar azúcares puros y azúcares
#mezclados.
q2=c(0,1,1,1,-3)

#3. Habrá diferencias entre agregar disacáridos(sacarosa) y monosacári-
#dos(glucosa, fructosa y glucosa + frucosa)?
q3=c(0,1,1,-3,1)

#4. Habrá diferencias entre glucosa y fructosa?
q4=c(0,1,-1,0,0)


#Las hipótesis nulas correspondientes a cada uno de los contrastes definidos en el código serían:
  
#H0 para q1: No hay diferencia en la longitud de las secciones de guisantes entre el tratamiento control y el 
#promedio de los demás tratamientos con azúcares. H0: -4 * μ_control + 1 * μ_D1 + 1 * μ_D2 + 1 * μ_D3 + 1 * μ_D4 = 0
#H0 para q2: No hay diferencia en la longitud de las secciones de guisantes entre los tratamientos 
#con azúcares puros y el tratamiento con azúcares mezclados. H0: 1 * μ_D1 + 1 * μ_D2 + 1 * μ_D3 - 3 * μ_D4 = 0
#H0 para q3: No hay diferencia en la longitud de las secciones de guisantes entre el 
#tratamiento con disacárido (sacarosa) y los tratamientos con monosacáridos. H0: 1 * μ_D1 + 1 * μ_D2 - 3 * μ_D3 + 1 * μ_D4 = 0
#H0 para q4: No hay diferencia en la longitud de las secciones de 
#guisantes entre los tratamientos con glucosa y fructosa. H0: 1 * μ_D1 - 1 * μ_D2 = 0

##matriz de contrastes
L=matrix(c(q1,q2,q3,q4),nrow=4,ncol=5,byrow=TRUE)

L
rownames(L)=c("Q1","Q2","Q3","Q4")
##ortogonalidad funciona cuando los contrastes sean ortogonales
#mejor esto hacer en un for con un condicional

#Codigo del profesor

sum(q1*q2)
sum(q1*q3)
sum(q1*q4)
sum(q2*q3)
sum(q2*q4)
sum(q3*q4)

#Mi codigo

contrastes <- list(q1, q2, q3, q4)

for (i in 1:(length(contrastes) - 1)) {  #Este ciclo for recorre todos los contrastes, excepto el último.
  for (j in (i + 1):length(contrastes)) { #Este ciclo for recorre todos los contrastes a partir del i-ésimo, 
    #es decir, compara cada contraste con todos los demás que vienen después de él.
    producto_interno <- sum(contrastes[[i]] * contrastes[[j]])
    cat("Producto interno entre q", i, "y q", j, ": ", producto_interno, "\n")
    if(producto_interno == 0){
      cat("Los contrastes q", i, "y q", j, " son ortogonales \n")
    } else {
      cat("Los contrastes q", i, "y q", j, " no son ortogonales \n")
    }
  }
}

#Pero en este caso q2 y q3 no son ortogonales por ende debe hacer un analisis secuencia o jerarquico !!!!!


#Mi idea: Vamos a intentar entender este codigo viendo a que es igual al sumatorioa de medias 
#coeficientes y la suma de estos coeficientes y por que debe ser asi

#tomamos la variable de tratamientos y recorremos con un for esta variable
#para calcular la media de cada tratamiento

#Dar un lista de tratamientos

lista_trat = list(control, D1, D2, D3, D4)

medias <- numeric(length(lista_trat))

# Calculamos la media de cada vector usando un ciclo for
for (i in 1:length(lista_trat)) {
  start <- (i - 1) * 10 + 1
  end <- i * 10
  medias[i] <- mean(longitudes[start:end])
}

# Imprimimos los resultados
print(medias)

#suma de media y coeficientes

for (i in 1:nrow(L)) {
  media_sum <- sum(medias*L[i, ])
  print(media_sum)
}

#suma de medias y coeficientes

##analisis de contrastes
#install.packages("gmodels")
library(gmodels) #usar library en vez de require
fit.contrast(modelogui,"azuc",L)

#Esto lo podemos programar en un codicional

# Ejecuta el análisis de contrastes
fit.contrast_results <- fit.contrast(modelogui, "azuc", L)

# Extrae los valores-p de cada contraste
p_values <- unlist(fit.contrast_results[, "Pr(>|t|)"])

# Imprime los resultados con la decisión sobre la hipótesis nula
alpha <- 0.05 # Nivel de significancia
for (i in 1:length(p_values)) {
  if (p_values[i] < alpha) {
    cat("Contraste", i, ": Se rechaza la hipótesis nula, no hay diferencias significativas entre tratamientos.\n")
  } else {
    cat("Contraste", i, ": No se puede rechazar la hipótesis nula, si hay diferencias significativas entre tratamientos.\n")
  }
}

#conclusion: Las pruebas de hpótesis para los contrastes expresadas como H 0 : Qi = 0 contra
#H 1 : Q i != 0 son altamente significativas (p < 0, 01), excepto para Q 4 .

#CREAR UN FUNCION!!!
#una función en R que genere constantes con la propiedad de que su suma sea igual a cero. La función debe 
#recibir como parámetro el número de constantes que se desean generar. Estas constantes deben ser 
#diferentes de cero, pero su suma total debe ser igual a cero. Por favor, 
#piensa en el proceso paso a paso y asegúrate de que las constantes cumplan con estas condiciones
