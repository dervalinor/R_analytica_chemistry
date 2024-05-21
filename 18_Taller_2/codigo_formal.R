library(car)
library(agricolae)
library(daewr)
library(ggplot2)


# Importar datos

Tratamiento = rep(c("Droga A", "Droga B", "Placebo"), times = 7)
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

#Ver si existen interacciones intre los bloque y tratamientos

Tratamiento = factor(Tratamiento)
Bloque = factor(Bloque)

datos_adt <- data.frame(Respuesta, Tratamiento, Bloque)

attach(datos_adt)

Tukey1df(datos_adt) #no existen interacciones entre los bloques y 
#tratamientos, no se rechaza la hipotesis nula

#Entonces hacemos un analisis por medio de un diseño de 
#bloques al azar

#Comenzemos por un analisis descriptivo de este diseño por medio
#de Boxplots

#bloxplot de respuesta-tratamientos 

ggplot(datos, aes(x = Tratamiento, y = Respuesta, fill = Tratamiento)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#2FD9B7", "#B72FD9", "#D92F42")) +
  theme_minimal() +
  labs(title = "Boxplot de Respuesta por Tratamiento",
       x = "Tratamiento",
       y = "Respuesta") +
  theme(plot.title = element_text(hjust = 0.5))


#bloxplot de respuesta-bloques

ggplot(datos, aes(x = Bloque, y = Respuesta, fill = as.factor(Bloque))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#2FD9B7", "#B72FD9", "#D92F42", 
                               "#6FD92F", "#D9B02F", 
                               "#64C8C5", "#6485C8")) +
  theme_minimal() +
  labs(title = "Boxplot de Respuesta por Bloque",
       x = "Bloque",
       y = "Respuesta") +
  theme(plot.title = element_text(hjust = 0.5))


modelo_dba = aov(Respuesta ~ Tratamiento + Bloque, data = datos_adt)
resumen_modelo_dba = summary(modelo_dba)
resumen_modelo_dba

#Ahora vamos a calcular la eficiencia de esta modelo vs un DCA

modelo = aov(Respuesta ~ Tratamiento, data = datos_adt)
resumen_modelo = summary(modelo)
resumen_modelo

p_valor_aov = resumen_modelo[[1]][["Pr(>F)"]][1]

if(p_valor_aov >= 0.05){
  cat("Se acepta la hipotesis nula, NO existen diferencias significativas
      entre los tratamientos: ", p_valor_aov)
} else {
  cat("Se rechaza la hipotesis nula, exista al menos un tratamiento con 
      diferencias significativas: ", p_valor_aov)
}

# Evaluación de supuestos - cambiar para la evalucion de DBA
# 1. Normalidad

residuos = residuals(modelo_dba)

par(mfrow = c(1, 2))# Dividir la ventana gráfica en dos paneles

# Gráfico de densidad de residuos
ggplot(data.frame(residuos), aes(x = residuos)) +
  geom_density(fill = "#6BA7CA", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Densidad de Residuos del Modelo",
       x = "Residuos",
       y = "Densidad") +
  theme(plot.title = element_text(hjust = 0.5))

# Q-Q plot de residuos
ggplot(data.frame(residuos), aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line(color = "#E52AF3") +
  theme_minimal() +
  labs(title = "Q-Q Plot de Residuos del Modelo",
       x = "Teóricos",
       y = "Muestrales") +
  theme(plot.title = element_text(hjust = 0.5))

resultado_shapiro = shapiro.test(residuals(modelo_dba))  
# Prueba de Shapiro-Wilk
resultado_shapiro

p_valor_shapiro = resultado_shapiro$p.value

if(p_valor_shapiro >= 0.05){
  print(paste("Se acepta la hipotesis nula, entonces existe normalidad en el 
              modelo con un p valor (p > 0.05): ", p_valor_shapiro,
              "\n"))
} else {
  print(paste("Se rechaza la hipotesis nula, entonces no existe 
              normalidad con un p valor (p < 0.05): ", p_valor_shapiro, "\n"))
}

dev.off()

# 2. Homocedasticidad (homogeneidad de varianzas)

# Gráfico de residuos vs valores ajustados
ggplot(data.frame(fitted = fitted(modelo_dba), residuos = residuos), 
       aes(x = fitted, y = residuos)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#2AF3B9") +
  theme_minimal() +
  labs(title = "Residuos vs Valores Ajustados",
       x = "Valores Ajustados",
       y = "Residuos") +
  theme(plot.title = element_text(hjust = 0.5))


resultado_bartlett = bartlett.test(Respuesta ~ Tratamiento, data = datos) 
#ya que existe normalidad usamos el test de Bartlett

p_valor_bartlett = resultado_bartlett$p.value

if(p_valor_bartlett >= 0.05){
  cat("Se cumple el supuesto de homocedasticidad con un 
      p valor (p >= 0.05): ", p_valor_bartlett)
} else {
  cat("No se cumple el supuesto de 
      homocedasticidad con un p valor de: ", p_valor_bartlett)
}

# 3. Independencia
# Crear data frame con índices y residuos
residuos_data <- data.frame(
  indice = 1:length(Respuesta),
  residuos = residuals(modelo_dba)
)

# Crear el gráfico con ggplot2
ggplot(residuos_data, aes(x = indice, y = residuos)) +
  geom_point(shape = 9, color = "#27AFBF") +  # Punto con forma 9 y color azul
  theme_minimal() +  # Tema minimalista para un gráfico limpio
  labs(title = "Residuos del Modelo vs Índice de Observación",
       x = "Índice de Observación",
       y = "Residuos") +
  theme(plot.title = element_text(hjust = 0.7))
#Existe un patron aleoatorio por lo cual hay independencia

#EFICIENCIA

resumen_modelo_dba
resumen_modelo

#Para BDA 
DF_DBA = resumen_modelo_dba[[1]][["Df"]][3]
DF_DBA
sigma_2_DBA = resumen_modelo_dba[[1]][["Mean Sq"]][3]
sigma_2_DBA

#Para DCA
DF_DCA = resumen_modelo[[1]][["Df"]][1]
DF_DCA
sigma_2_DCA = resumen_modelo[[1]][["Mean Sq"]][2]
sigma_2_DCA

Eficiencia_formal = 
  ((DF_DBA+1)*(DF_DCA + 3)*sigma_2_DCA)/((DF_DBA+3)*(DF_DCA + 1)*sigma_2_DBA)
cat("La eficiencia del diseño de bloques al azar en comparacion con el diseño
    completamente aleatorizado es: ", Eficiencia_formal)
