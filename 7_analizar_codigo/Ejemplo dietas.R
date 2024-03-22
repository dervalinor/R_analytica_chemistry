#Para comparar cuatro dietas D 1 , D 2 , D 3 y D 4 , respecto a su influencia
#en el tiempo de coagulación de la sangre, se seleccionaron 24 animales y
#cada un recibió aleatoriamente una de las dietas. Los datos se muestran a
#continuación

#Hacer un diseño totalmente aleotarizado y Annova para comprobar hipotesis

# D1 = {61, 60, 63, 59}
# D2 = {63, 67, 71, 64, 65, 66}
# D3 = {68, 66, 71, 67, 68, 68}
# D4 = {56, 62, 60, 63, 63, 64, 63, 59}

#El diseño completamente aleorizado permite asignar de forma aleaotoria un tratamiento y evitar el sesgo de asignacion
#sesgo de asignacion.


tratamientos=rep(c("D1","D2","D3","D4"),c(4,6,6,8)) 
tratamientos
length(tratamientos) #Para indicar el numero de datos coompilados en la tablas, ademas para ver si el comando esta bien 
respuestas= c(62,60,63,59,
              63,67,71,64,65,66,
              68,66,71,67,68,68,
              56,62,60,63,63,64,63,59) #Los datos son separados para que se vean ordenados
length(respuestas)
datosdietas=data.frame(tratamientos,respuestas) #Une la tabla #caja es frame
datosdietas #Sale la tabla de datos
attach(datosdietas) #Para trabajar con cada uno por separdo la columna 
boxplot(respuestas~tratamientos,data=datosdietas, col=c("skyblue","pink","purple","yellow"))

#SUPUESTOS -diagnosticos
residualesd=residuals(modelodietas)
residualesd

#Normalidad
#Gráficos
par(mfrow=c(1,2))#División plano
plot(density(residualesd))
plot(modelodietas,which=2) #Grafico cuantil cuantil
#Los graficos ayudan en la coclusión del anova, sin embargo hace falta una prueba de hipotesis
dev.off()

#Prueba de normalidad
shapiro.test(residualesd)
#No rechazo H, acepto H
#Hay normalidad

#Supuesto de Homocedasticidad
#Gráfico
plot(modelodietas,which=1) #No hay embudos
install.packages("car")

#Test de Bartlett
bartlett.test(respuestas~tratamientos,data=datosdietas)
#Hay homocedasticidad, no rechazo H

#Independencia (Supuesto)
plot(1:24,residualesd,pch=8)

#ANOVA
modelodietas=aov(respuestas~tratamientos,data=datosdietas)
summary(modelodietas)

#Interpretacion anova
#los tratamientos o las dietas si tienen efecto en la variable respuesta que es el tiempo de coagulación 
