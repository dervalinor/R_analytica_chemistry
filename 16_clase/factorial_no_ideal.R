#en el caso que algunas observaciones no realizadas
#Anova tipo III mejor para esta caso, corregir la observaciones factantes
#polinomios ortogonales

library(daewr)
library(car)

#Estos son los resultados de un experimento de dos
#factores dado por Hunter (1983). En estos datos, un experimento
#consistió en quemar una cantidad de combustible y determinar las
#emisiones de CO liberadas. La unidad experimental es la porción de
#un combustible estándar requerido para una ejecución, y la respuesta,
#y, es la concentración de emisiones de monóxido de carbono (CO) en
#g/m 3 determinada a partir de esa ejecución. Hubo dos ejecuciones
#repetidas para cada combinación de niveles de factores. El factor A es
#la cantidad de etanol agregado a una unidad experimental o porción
#del combustible estándar, y el factor B es la relación combustible-aire
#utilizada durante la combustión de ese combustible.

#\begin{tabular}{ccc}
#\hline
#Adiciones de etanol & aire/combustible & emisiones CO \\
#\hline
#0,1 & 14 & 66;62 \\1
#0,1 & 15 & 72;67 \\2
#0,1 & 16 & 68;66 \\3
#0,2 & 14 & 78;81 \\4
#0,2 & 15 & 80;81 \\5
#0,2 & 16 & 66;69 \\6
#0,3 & 14 & 90;94 \\7
#0,3 & 15 & 75;78 \\8
#0,3 & 16 & 60;58 \\9
#\hline
#\end{tabular}

#Hipotesis: existe algun efecto por la adiccion de etanol, relacion de 
#aire/combustible o la interaccion de ambos factores causan algun cambio 
#en la emision de CO.

#Es importante convertir a factor las adiciones de etanol y aire/combustible en
#factores
library(agricolae)

COdata_no_ideal = COdata

#calcular dimension de la base de datos
dim(COdata_no_ideal)

#Imaginemos que no hicimos un observacion
COdata_no_ideal[18,3] = NA

COdata_no_ideal

#Lo siguiente no se debe hacer
#analizar como si no faltara datos

attach(COdata_no_ideal)

anova_factorial_malo = aov(CO ~ Eth*Ratio, data = COdata_no_ideal)
summary(anova_factorial_malo)

#Te da un p valor pero no es valido hacer ya que falta un observacion esto 
#seria un error

#Se recomienda hacer lo siguiente

#Modelo de regresion como un curva de calibracion, es un funcion de prediccion

#Revisar este codigo del profesor
#modelo_CO = lm(CO ~ Eth*Ratio), contrasts = list(Eth = contr.sum, Ratio = contr.Sum)

#modeloCO=lm(CO ~ Eth*Ratio,
#            contrasts=list(Eth=contr.sum,Ratio=contr.Sum),
#            data=COdatam)
#Anova(modeloCO,type="III")

modelo_CO = lm(CO ~ Eth * Ratio, data = COdata_no_ideal)

Anova(modelo_CO, type = "III")

#Hacer grafico de interaccion

library(ggplot2)

# Convertir los factores a factores ordenados para que los niveles se muestren en el orden correcto
COdata_no_ideal$Eth <- factor(COdata_no_ideal$Eth, levels = c("0.1", "0.2", "0.3"))
COdata_no_ideal$Ratio <- factor(COdata_no_ideal$Ratio, levels = c("14", "15", "16"))

# Crear el gráfico de interacción
ggplot(COdata_no_ideal, aes(x = Ratio, y = CO, color = Eth, group = Eth)) +
  geom_point() +
  geom_line() +
  labs(x = "Ratio aire/combustible", y = "Emisiones de CO (g/m3)", color = "Adiciones de etanol") +
  theme_bw()

