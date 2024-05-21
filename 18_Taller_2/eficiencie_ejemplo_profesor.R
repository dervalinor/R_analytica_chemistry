#####ejercicico diapo 52 a.
tratamientos=rep(c("control","droga1","droga2"),c(6,6,6))
tratamientos
respuestas=c(1147,1273,1216,1046,1108,1265,
             1169,1323,1276,1240,1432,1562,
             1009,1260,1143,1099,1385,1164)
length(respuestas)
datospetri=data.frame(tratamientos,respuestas)
datospetri
attach(datospetri)
boxplot(respuestas~tratamientos,data=datospetri,col=c("red","yellow","green"),Xlab="trataminetos",Ylab="respuestas",main="experimento bacterias en petri")
modelopetri=aov(respuestas~tratamientos,data=datospetri)
summary(modelopetri)
##evaluar supuestos.
##normalidad
residualesp=residuals(modelopetri)
residualesp
par(mfrow=c(1,2))
plot(density(residualesp))
plot(modelopetri,which=2)
dev.off()
shapiro.test(residualesp)###hay normalidad
#HOMOCEDASTICIDAD
plot(modelopetri,which=1)
##normalidad, test de bartlett
#install.packages("car")
require(car)
bartlett.test(respuestas~tratamientos,data=datospetri)##hay mocedasticidad
#INDEPENDENCIA
plot(1:18,residualesp,pch=19)
summary(modelopetri)###si hay efecto de los tratamientos sobre la variable respuesta
#######
#######DBA
#####ejercicio b
tratamientos=rep(c("control","D1","D2"),6)
tratamientos
length(tratamientos) #Para saber si tengo el numero de datos completos
bloques=rep(c("B1","B2","B3","B4","B5","B6"),each=3) #Para ingresar 
bloques
length(bloques)#Para saber si tengo el numero de datos completos
exp1 <- c(1147,1169,1009)
exp2 <- c(1273,1323,1260)
exp3 <- c(1216,1276,1143)
exp4 <- c(1046,1240,1099)
exp5 <- c(1108,1432,1385)
exp6 <- c(1265,1562,1164)
respuestas=c(exp1,exp2,exp3,exp4,exp5,exp6)
length(respuestas)
datosbacp=data.frame(respuestas,tratamientos,bloques)
datosbacp
attach(datosbacp)
modelopetrib=aov(respuestas~tratamientos+bloques,data=datosbacp)
summary(modelopetrib)
##evaluar supuestos.
##normalidad
residualesDBA=residuals(modelopetrib)
residualesDBA
par(mfrow=c(1,2))
plot(density(residualesDBA))
plot(modelopetrib,which=2)
dev.off()
shapiro.test(residualesDBA)###hay normalidad
#HOMOCEDASTICIDAD
plot(modelopetrib,which=1)
##normalidad, test de bartlett
#install.packages("car")
require(car)
bartlett.test(respuestas~tratamientos,data=datosbacp)##hay mocedasticidad
#INDEPENDENCIA
plot(1:18,residualesp,pch=19)
####ADITIVIDAD  
#install.packages("asbio")
#require(asbio)
#tukey.add.test(respuestas,tratamientos,bloques)
summary(modelopetrib)
((10+1)*(15+3)*15214)/((10+3)*(15+1)*9402)
((1-(9402/15214))*100)
