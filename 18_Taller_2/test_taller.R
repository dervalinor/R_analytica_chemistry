library(car)
library(agricolae)
library(daewr)

# Importar datos

Tratamiento = rep(c("Droga A", "Droga B", "Placebo"), times = 7) #luego convertir
#a factor para analisis
Bloque = rep(1:7, each = 3)
Respuesta = c(6.0, 6.1, 5.4, 
              4.8, 6.9, 4.0, 
              6.9, 6.5, 7.0, 
              6.4, 5.6, 5.8, 
              5.5, 3.9, 3.5, 
              9.0, 7.0, 7.6, 
              6.8, 5.4, 5.5)

datos <- data.frame(Respuesta, Tratamiento, Bloque)

attach(datos)

modelo_fac = aov(Respuesta ~ Tratamiento*Bloque, data = datos)
resumen_modelo_fac = summary(modelo_fac)
resumen_modelo_fac