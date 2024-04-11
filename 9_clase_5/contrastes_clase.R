control<-c(75,67,70,75,65,71,67,67,76,68)
D1<-c(57,58,60,59,62,60,60,57,59,61)
D2<-c(58,61,56,58,57,56,61,60,57,58)
D3<-c(62,66,65,63,64,62,65,65,62,67)
D4<-c(58,59,58,61,57,56,58,57,57,59)

longitudes<-c(control,D1,D2,D3,D4)
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
require(car)
bartlett.test(longitudes~azuc,data=datos)
plot(1:24,residualesd,pch=19)
#interprestacion anova
summary(modelogui)

#Prueba de contrastes ortogonales
##introduccion contrastes - Como rayos se genera esta estos vectores de contrastes
q1=c(-4,1,1,1,1) #comparacion de control vs D1, D2, D3 y D4
#combinacion lineal -4*Media_control + (1)Media_D1 + (1)Media_D2 + (1)Media_D3 + (1)Media_D4 = 0
#donde debe cumplirse -4+1+1+1+1 = 0, estas dos condiciones deben cimplirse

q2=c(0,1,1,1,-3)
q3=c(0,1,1,-3,1)
q4=c(0,1,-1,0,0)
##matriz de contrastes
L=matrix(c(q1,q2,q3,q4),nrow=4,ncol=5,byrow=TRUE)

L
rownames(L)=c("Q1","Q2","Q3","Q4")
##ortogonalidad funciona cuando los contrastes sean ortogonales
sum(q1*q2)
sum(q1*q3)
sum(q1*q4)
sum(q2*q3)
sum(q2*q4)
sum(q3*q4)
##analisis de contrastes
install.packages("gmodels")
require(gmodels)
fit.contrast(modelogui,"azuc",L)
