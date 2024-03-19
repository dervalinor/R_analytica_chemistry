tratamientos=rep(c("T1","T2","T3","T4","T5","T6"),c(5,5,5,5,5,5))
tratamientos
length(tratamientos)
respuestas=c(280,30,0,10,50,
             10,20,20,20,30,
             80,90,10,90,60,
             10,10,10,20,0,
             10,20,40,10,10,
             30,40,40,40,10)
length(respuestas)
datosmesofilos=data.frame(tratamientos,respuestas)
datosmesofilos
attach(datosmesofilos)
boxplot=(respuestas~tratamientos, data=datosmesofilos,col=c("pink","purple","lightgreen","skyblue","yellow","red","beige"))

#anova
modelomeso=aov(respuestas~tratamientos, data=datosmesofilos)

#diagnosticos supuestos
residuosmeso=residuals(modelomeso)
residuosmeso

#Normalidad
#graficop
par(mfrow=c(1,2))
plot(density(residuosmeso))
plot(modelomeso,which=2)
shapiro.test(residuosmeso)
#P valor menor que el nivel de significancia por lo que rechazo H y acepto A
#En A no hay normalidad

#Homocedasticidad
dev.off()
plot(modelomeso,which=1)
install.packages("car")
require(car)
leveneTest(respuestas~factor(tratamientos),data=datosmesofilos) #El factor es para que no salga el error
#Para el test de levene hay homocedasticidad, pero por los embudos hay que preocuparse, por eso este ejemplo es todo lo malo que puede pasarte en la vida
#Hay una simetria demasiado marcada
#El objetivo de trasnformar los datos es corregir los errores de homocedasticidad (Por culpa del embudo), lo que trae efectos colaterales
#Puede que se arregle un poco la normalidad

datosmesofilos$rt=log(datosmesofilos$respuestas+1)
attach(datosmesofilos)
datosmesofilos
modelomesot=aov(rt~tratamientos,data=datosmesofilos)
datosmesofilos
residualesmesot=residuals(modelomesot)
residualesmesot
par(mfrow=c(1,2))
plot(density(residualesmesot))
plot(modelomesot,which=2)
shapiro.test(residualesmesot)
#No hay normalidad pero ya hay un poco de simetria
#Esto el anova ya lo puede resistir aun cuando no hay normalidad
dev.off()
plot(modelomesot,which=1)
leveneTest(rt~factor(tratamientos),data=datosmesofilos)

summary(modelomesot)
#El tipo de empaque no afecta el número de mesofilos