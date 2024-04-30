#Suponga que quiere determinarse si cuatro puntas diferentes producen
#o no lecturas diferentes en una máquina para probar la dureza. Un
#experimento como este podrı́a ser parte de un estudio de la aptitud
#en la calibración de los instrumentos.

#ver si los puntas perforando un material y ver si hay diferencias

#DCA
# Cargar los datos desde la imagen
puntas_DCA <- c("P1", "P2", "P3", "P4")
datos_DCA <- c(9.3, 9.4, 9.2, 9.7,
           9.4, 9.3, 9.4, 9.6,
           9.6, 9.8, 9.5, 10.0,
           10.0, 9.9, 9.7, 10.2)

# Crear un data frame con los datos
df_DCA <- data.frame(punta_DCA = rep(puntas_DCA, each = 4),
                 dureza_DCA = datos_DCA)

# Ajustar el modelo de ANOVA
modelo_DCA <- aov(dureza_DCA ~ punta_DCA, data = df_DCA)

# Mostrar el resumen del análisis
summary(modelo_DCA)


#DBA
# Cargar los datos desde la imagen
puntas_DBA <- c("P1", "P2", "P3", "P4")
bloques_DBA <- c(1, 2, 3, 4)
datos_DBA <- c(9.3, 9.4, 9.2, 9.7,
           9.4, 9.3, 9.4, 9.6,
           9.6, 9.8, 9.5, 10.0,
           10.0, 9.9, 9.7, 10.2)

# Crear un data frame con los datos
df_DBA <- data.frame(punta_DBA = rep(puntas_DBA, each = 4),
                 bloque_DBA = rep(bloques_DBA, times = 4),
                 dureza_DBA = datos_DBA)

# Ajustar el modelo de ANOVA de dos vías
modelo_DBA <- aov(dureza_DBA ~ punta_DBA + bloque_DBA, data = df_DBA)

# Mostrar el resumen del análisis
summary(modelo_DBA)

#Debe dar en este problema F 30.90 y eficiencia relativa 8.15