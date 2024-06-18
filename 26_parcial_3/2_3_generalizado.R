#codigo generalizado

#Diseño 2^k donde k = 3 es decir 2³



#Remplazar para hacer por lo especifico del problema !!!!
#igual para los valores que toma los vectores de Respuesta
r = 2 #numero de repeticiones para de los niveles de los factores
m = 4 #numero de veces que se repite el vector del factorA
n= 2 #numero de veces que se repite el vector del factorB


Respuesta<-c(0.312,0.576,0.391,0.309,0.412,0.280,0.376,0.201,
                  0.479,0.656,0.481,0.631,0.465,0.736,0.451,0.814)
Respuestamat<-matrix(Respuesta,byrow=T,ncol=2)
Respuestamat
dimnames(Respuestamat)<-list(c("(1)","a","b","ab","c","ac","bc","abc"),
                                  c("Rep1","Rep2")) #Agregar a este vector 
#cuantas repeticiones se realizaron en el experimento
factorA<-rep(rep(c("0.06","0.30"),each=r),m) #Debe camabia
factorB<-rep(rep(c("2","10"),each=2*r),n)
factorC<-rep(c("20","100"),each=2^{3})
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
etch.lm2 <- aov(pesomat.vec ~ Af*Bf)####opcion2
summary(etch.lm2)
