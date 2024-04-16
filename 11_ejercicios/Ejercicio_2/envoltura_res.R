#librerias
library(car)
library(agricolae)
library(gmodels)
library(mvtnorm)
library(multcomp)
library(survival)


#Gomez y Gonzalez(1991) investigaron la pérdida de peso (en porcentaje del peso
#inicial) de la carne de res tipo milanesa después de cinco días de empacada en
#envolturas diferentes.

Icopor = c(5.33, 4.95, 5.10, 7.14, 7.84)
Biopak = c(6.59, 7.90, 4.48, 7.32, 6.41)
Cry_o_vac = c(4.95, 4.44, 3.48, 3.92, 8.62)
Shopak = c(2.41, 2.83, 2.97, 2.38, 2.11)

#a) Especificar el diseño y el modelo ANOVA para analizar estos datos.
#b) Efectuar el ANOVA correspondiente al modelo e interpretar la prueba F.
#c) Calcular un intervalo de confianza del 95 % para la pérdida de peso promedio
#con la envoltura Shopak.
#d) Comparar la pérdida promedio de peso entre Icopor y Biopak.
#e) Describir los factores, los niveles, las unidades experimentales, la variable
#de respuesta y una aleatoriazación correcta para este diseño.

#El diseño que se utiliza es un diseño completamenta aleotarizado

#Cargando datos de tipo de envoltura y porcentaje de perdida de peso en cada envoltura
tratamientos_envolturas = rep(c("Icopor", "Biopak", "Cry_o_vac", "Shopak"), each = 5)
