require(car)

#En un estudio de diferentes empaques para la carne, Gómez y González(1991)
#realizaron una prueba microbiológica y contabilizaron el número de
#mesófilos en las diferentes muestras analizadas. Los resultados se pre-
#sentan en la siguiente tabla

#se van a hacr  5 replicas, empaques T1, T2, T3, T4, T5 Y T6
#T1 = 280, 30, 0, 10, 50
#T2 = 10, 20, 20, 20, 30
#T3 = 80, 90, 10, 90, 60
#T5 = 10, 10, 10, 20, 0
#T6 = 30, 40, 40, 40, 10

#analizar Si los empaques afecta el Número de mesófilos en cada una de las diferentes envolturas.
#Evalúe los supuestos de normalidad y homogeneidad de varianzas


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
boxplot(respuestas ~ tratamientos, data = datosmesofilos, col = c("pink","purple","lightgreen","skyblue","yellow","red","beige"))

#1. anova
modelomeso=aov(respuestas~tratamientos, data=datosmesofilos)

#2. diagnosticos supuestos
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
#install.packages("car")
Prueba_levene = leveneTest(respuestas~factor(tratamientos),data=datosmesofilos) 
summary(Prueba_levene)
#El test de Levene es utilizado cuando se viola el principio de normalidad en el anova solo en tal caso se 
#se usa, es para evaluar homocedasticidad 
#Formulación de hipótesis en el test de Levene:
  
#Hipótesis nula (H0): La varianza es la misma para todos los grupos - existe homocedasticidad
#Hipótesis alternativa (H1): Al menos una de las varianzas es diferente de las demás.

#Interpretación del resultado:
  
#Si el valor p es menor que un nivel de significancia predefinido (comúnmente 0.05), 
#se rechaza la hipótesis nula y se concluye que al menos una de las varianzas es significativamente 
#diferente de las demás.
#Si el valor p es mayor que el nivel de significancia, no hay suficiente evidencia para rechazar 
#la hipótesis nula, lo que sugiere que las varianzas son similares entre los grupos.

p_levene_valor =  Prueba_levene$`Pr(>F)`[1] #acceder al numero del elemento Pr(>F)
p_levene_valor

if(p_levene_valor >= 0.05){
  print("Se acepta la hipotesis de que si puede existir homocedasticidad")
} else {
  print("Se rechaza la hipotesis de que existe homocedasticidad")
}


#El factor es para que no salga el error
#Para el test de levene hay homocedasticidad, pero por los embudos hay que preocuparse, por eso este ejemplo es todo lo malo que puede pasarte en la vida
#Hay una simetria demasiado marcada
#El objetivo de trasnformar los datos es corregir los errores de homocedasticidad (Por culpa del embudo), lo que trae efectos colaterales
#Puede que se arregle un poco la normalidad

datosmesofilos$rt=log(datosmesofilos$respuestas+1)
datosmesofilos

#similar a decir

datos_mesofilo_trans = log(respuestas + 1)
datos_mesofilo_trans

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

#Test de Bartlett:
  
#Se utiliza cuando los datos siguen una distribución normal.
#Es sensible a las desviaciones de la normalidad.
#Si los datos no son normales, el test de Bartlett puede ser engañoso y perder potencia.

#Test de Levene:
  
#Es un test no paramétrico, lo que significa que no asume normalidad en los datos.
#Es más robusto a las desviaciones de la normalidad que el test de Bartlett.
#Se recomienda utilizar el test de Levene cuando los datos no siguen una distribución normal o cuando se desconoce si cumplen con la normalidad.

summary(modelomesot)
#El tipo de empaque no afecta el número de mesofilos