library(car)
library(agricolae)
library(daewr)
library(ggplot2)
library(dplyr)
library(reshape2)

#Problema: Algunos autores seleccionaron los factores factores que se muestran en la tabla 1, cada
#uno de ellos con dos niveles, para determinar las condiciones óptimas de extracción
#de pectinas (polisacarido de origen natural usado como espesante) utilizando la
#corteza de naranja como materia prima.

#Resolver:

#b. Usando R studio, verifique los resultados obtenidos en el item previo
#y además analice la significancia estadística. Interprete.

#d. Construya el o los gráficos de interacción que usted considere necesarios
#para dar respuesta al objetivo de interés.

r = 2 #numero de repeticiones para de los niveles de los factores

Respuesta<-c(117.6, 118.4, 124.0, 120.0, 110.9, 111.1, 111.0, 119.0,
             111.0, 109.0, 114.0, 110.0, 100.0, 106, 113.0, 115.0)

Respuestamat<-matrix(Respuesta,byrow=T,ncol=2)
Respuestamat
dimnames(Respuestamat)<-list(c("(1)","a","b","ab","c","ac","bc","abc"),
                             c("Rep1","Rep2")) #Agregar a este vector 
#cuantas repeticiones se realizaron en el experimento
factorA<-rep(rep(c("60", "80"),each=8),2) 
#Tiempo

factorB<-rep(rep(c("90","96"),each=2),4) 
#Temperatura

factorC<-rep(c("1.5","2.5"),each=2^{3}) 
#PH

#####################################################################
###########  Total por tratamientos   ###############################
#####################################################################
Total <- apply(Respuestamat,1,sum)
Total
#####################################################################
########### Estimaci�n de los efectos ###############################
#####################################################################
A <- rep(c(-1,1),4) ### signos de A
B <- rep(c(-1,-1,1,1),2) ### signos de B
AB <- A*B ### signos de AB
C<-rep(c(-1,1),each=4)
AC<-A*C
BC<-B*C
ABC<-A*B*C
########################Efecto promedio de A,B y AB
Aeff <- (Total %*% A)/(4*r)
Beff <- (Total %*% B)/(4*r)
ABeff <- (Total %*% AB)/(4*r)
Ceff<-(Total %*% C)/(4*r)
ACeff<- (Total %*% AC)/(4*r)
BCeff<- (Total %*% BC)/(4*r)
ABCeff<-(Total %*% ABC)/(4*r)
######################## Resumen de todos los efectos
efectos <- t(Total) %*% cbind(A,B,AB,C,AC,BC,ABC)/(4*r)####Efectos
efectos
resumen <- rbind( cbind(A,B,AB,C,AC,BC,ABC),efectos)
dimnames(resumen)[[1]] <- c(dimnames(Respuestamat)[[1]],"efecto")
resumen
# Ajuste como un modelo ANOVA
Respuestamat.vec <- c(t(Respuestamat))
Af <- rep(as.factor(A),rep(r,8))### r replicas; 8 tratamientos
Bf <- rep(as.factor(B),rep(r,8))### r replicas; 8 tratamientos
Cf <- rep(as.factor(C),rep(r,8))### r replicas; 8 tratamientos
options(contrasts=c("contr.sum","contr.poly"))
etch.lm<-lm(Respuestamat.vec ~ Af*Bf*Cf)####opcion1
anova(etch.lm)


# Datos de respuesta
Respuesta <- c(117.6, 118.4, 124.0, 120.0, 110.9, 111.1, 111.0, 119.0,
               111.0, 109.0, 114.0, 110.0, 100.0, 106.0, 113.0, 115.0)

# Matriz de respuesta
Respuestamat <- matrix(Respuesta, byrow=T, ncol=2)
dimnames(Respuestamat) <- list(c("(1)", "a", "b", "ab", "c", "ac", "bc", "abc"),
                               c("Rep1", "Rep2"))

# Factores
factorA <- rep(rep(c("60", "80"), each=8), 2)  # Tiempo
factorB <- rep(rep(c("90", "96"), each=8), 2)  # Temperatura
factorC <- rep(c("1.5", "2.5"), each=2^3)      # pH

# Convertir los datos a un data frame para facilitar el trabajo con ellos
Respuestamat.df <- as.data.frame(Respuestamat)
Respuestamat.df$Tratamiento <- rownames(Respuestamat)
Respuestamat.df <- reshape2::melt(Respuestamat.df, id.vars = "Tratamiento", variable.name = "Repeticion", value.name = "Respuesta")

# Añadir los factores al data frame
Respuestamat.df$Tiempo <- factor(rep(c("60", "80"), each=4))
Respuestamat.df$Temperatura <- factor(rep(rep(c("90", "96"), each=2), 2))
Respuestamat.df$pH <- factor(rep(c("1.5", "2.5"), each=8))

# Gráficos de interacción
par(mfrow=c(1, 3))  # Crear un panel de 1 fila y 3 columnas para los gráficos

# Interacción entre Tiempo y pH
interaction.plot(Respuestamat.df$Tiempo, Respuestamat.df$pH, Respuestamat.df$Respuesta,
                 main="Interacción entre Tiempo y pH",
                 xlab="Tiempo (min)", ylab="Respuesta (mg)",
                 trace.label="pH", col=c("red", "blue"), legend=TRUE)

# Interacción entre Tiempo y Temperatura
interaction.plot(Respuestamat.df$Tiempo, Respuestamat.df$Temperatura, Respuestamat.df$Respuesta,
                 main="Interacción entre Tiempo y Temperatura",
                 xlab="Tiempo (min)", ylab="Respuesta (mg)",
                 trace.label="Temperatura (°C)", col=c("green", "purple"), legend=TRUE)

# Interacción entre pH y Temperatura
interaction.plot(Respuestamat.df$pH, Respuestamat.df$Temperatura, Respuestamat.df$Respuesta,
                 main="Interacción entre pH y Temperatura",
                 xlab="pH", ylab="Respuesta (mg)",
                 trace.label="Temperatura (°C)", col=c("orange", "brown"), legend=TRUE)

# Restaurar la configuración original de la pantalla
par(mfrow=c(1, 1))