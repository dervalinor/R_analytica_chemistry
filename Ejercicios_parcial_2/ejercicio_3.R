library(ggplot2)

aetanol=rep(c(0.1,0.2,0.3), each = 6)
aetanol
airecombus=rep(rep(c(14,15,16), each = 2), 3)
airecombus
emisionesCO=c(66,62,72,67,68,66,78,81,80,81,66,69,90,94,75,78,60,58)

#marco de datos
marco_co = data.frame(aetanol, airecombus, emisionesCO)

attach(marco_co)

modelo_co = aov(emisionesCO ~ aetanol*airecombus, data = marco_co)
summary(modelo_co)

# Gráfico de interacción
ggplot(marco_co, aes(x = aetanol, y = emisionesCO, color = factor(airecombus))) +
  geom_point() +
  geom_line(aes(group = airecombus)) +
  labs(x = "Etanol", y = "Emisiones CO", color = "Aire de Combustión") +
  ggtitle("Gráfico de Interacción") +
  theme_minimal()

