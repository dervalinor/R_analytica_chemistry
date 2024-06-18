#Entre los experimentos factoriales simétricos el más sencillo de realizar
#es el factorial 2 n , el cuál está compuesto de n factores con dos niveles.

#In symmetric factorial experiments, a "2^n factorial" design is commonly 
#considered the easiest to perform because it involves n factors, 
#each with only two levels (e.g., high/low, on/off, present/absent). 

#Diseño de experimentos 2^k donde k = 2 es decir 2²

#considere la investigación del efecto de la concentración del
#reactivo y de la cantidad del catalizador sobre la conversión (rendimiento) 
#de un proceso quı́mico. Sea la concentración del reactivo en
#el factor A, y sean 15 y 25 por ciento los dos niveles de interés. El
#catalizador es el factor B, con el nivel alto denotanto el uso de dos
#libras del catalizador y el nivel bajo denotando el uso de una libra. Se
#hacen tres réplicas del experimento, y los datos son los siguientes

#Resultados: el efecto A es significativo y interaccion entre AB no es signfica-
#tivo lo hace de manera independiente no hay interacciones entre ellos y afecta
#el redimiento de forma independiente

rendimiento<-c(28,25,27,36,32,32,18,19,23,31,30,29)
length(rendimiento)

#colocar los datos en un matriz
renmat<-matrix(rendimiento,byrow=T,ncol=3)
renmat

#dar nombres a las filas y columnas de tal forma que se defina
#todas la combinaciones de los niveles de los factores en una 
#matriz de tal forma:

#(1): 15% concentracion, 1 libra de catalizador
#a: 25% concentracion, 1 libra de catalizador
#b: 15% concentracion, 2 libras de catalizador
#ab: 25% concentracion, 2 libras de catalizador

dimnames(renmat)<-list(c("(1)","a","b","ab"),
                       c("Rep1","Rep2","Rep3"))


renmat
concentracion<-rep(rep(c("15%","25%"),each=3),2)
catalizador<-rep(c("Una libra","dos libras"),each=6)
concentracion
catalizador
#####################################################################
###########  Total por tratamientos   ###############################
#####################################################################
Total <- apply(renmat,1,sum)
Total

#####################################################################
########### Estimacion de los efectos ###############################
#####################################################################

#Explicar por que se usa la tabla de simbolos

A <- rep(c(-1,1),2) ### signos de A
A
B <- c(-1,-1,1,1) ### signos de B
B
AB <- A*B ### signos de AB
AB
###r�plicas: r=3
r <- 3
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