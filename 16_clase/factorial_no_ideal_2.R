library(dplyr)
library(daewr)
library(car)
library(agricolae)

#Que hacer si tengo una observacion por celda, niveles cualitativos

attach(COdata)
COdata

COdatauna = COdata%>%group_by(Eth, Ratio)%>%
  summarise(COu = mean(CO))

attach(COdatauna)
COdatauna


#Esto no se debe hacer
anova_mal = aov(COu ~ Eth*Ratio, data = COdatauna)
summary(anova_mal)

Etho = as.ordered(COdatauna$Eth)
Ratioo = as.ordered(COdatauna$Ratio)

Ethlin = contr.poly(Etho)[Etho, ".L"]
Ratiolin = contr.poly(Ratioo)[Ratioo, ".L"]

modelo_bueno = lm(COu ~ Etho + Ratioo + Ethlin:Ratiolin, data = COdatauna)
anova(modelo_bueno)

#Hacer el ejemplo de virus en la diapositiva