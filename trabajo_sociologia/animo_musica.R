library(agricolae)
library(car)

#Se hace un estudio para ver como impacta la musica en el estado de animo de 
#las personas

#marco de datos
datos <- data.frame(
  estado_animo = c(5, 6, 7, 8, 9, 10),
  frecuencias = c(2, 2, 5, 6, 7, 16)
)


attach(datos)

modelo_musica = aov(frecuencias ~ estado_animo, data = datos)
summary(modelo_musica)

#ver supuestos
residuos_anova = residuals(modelo_musica)

par(mfrow=c(1,2))

#normalidad
plot(density(residuos_anova))
shapiro.test(residuos_anova) #existe normalidad

#Homocedasticidad
plot(modelo_musica,which=1)

dev.off()

#independencia
plot(1:length(estado_animo),residuos_anova,pch=9)


