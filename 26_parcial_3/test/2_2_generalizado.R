#creando codigo generalizado donde solo se necesite remplazar los vectores de 
#respuesta, factor A y B con su niveles alto y bajo.

Respuesta<-c(28,25,27,36,32,32,18,19,23,31,30,29)
length(Respuesta)


renmat<-matrix(Respuesta,byrow=T,ncol=3)
renmat



dimnames(renmat)<-list(c("(1)","a","b","ab"),
                       c("Rep1","Rep2","Rep3"))


renmat
FactorA<-rep(rep(c("bajo A","alto A"),each=3),2)
FactorB<-rep(c("bajo B","alto B"),each=6)
FactorA
FactorB

Total <- apply(renmat,1,sum)
Total





A <- rep(c(-1,1),2) ### signos de A
A
B <- c(-1,-1,1,1) ### signos de B
B
AB <- A*B ### signos de AB
AB
###r�plicas: r=3
r <- 3 #Cambiar esto para la replicas que se utlizaran
cbind(A,B,renmat,Total)
########################Efecto promedio de A,B y AB
Aeff <- (Total %*% A)/(2*r)
Beff <- (Total %*% B)/(2*r)
ABeff <- (Total %*% AB)/(2*r)
Aeff
Beff
ABeff
######################## Resumen de todos los efectos
efectos <- t(Total) %*% cbind(A,B,AB)/(2*r)####Efectos
efectos
resumen <- rbind( cbind(A,B,AB),efectos)
dimnames(resumen)[[1]] <- c(dimnames(renmat)[[1]],"efecto")
resumen

#######Ajuste
renmat.vec <- c(t(renmat))
renmat.vec
Af <- rep(as.factor(A),rep(r,4))### r r�plicas; 4 tratamientos
Af
Bf <- rep(as.factor(B),rep(r,4))### r r�plicas; 4 tratamientos
Bf
options(contrasts=c("contr.sum","contr.poly"))
etch.lm2 <- aov(renmat.vec ~ Af*Bf)####opci�n2
summary(etch.lm2)
etch.lm<-lm(renmat.vec ~ Af*Bf)####opci�n1
summary(etch.lm)
anova(etch.lm)