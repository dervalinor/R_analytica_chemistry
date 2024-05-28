library(car)
library(agricolae)
library(daewr)

#Con el fin de comparar los rendimientos proteínicos de cuatro variedades
#de avena, Gaviria y otros (1989) diseñaron un experimento 3×4 con dos repeticiones.
#El factor A representa el estado fisiológico en el momento del corte y el factor B la
#variedad de la semilla. Los resultados, en porcentajes promedios y en base seca,
#fueron:

# 1. Ingresar Datos
# Crear los vectores de datos
estados <- rep(c("Prefloración", "Floración", "Grano lechoso"), each = 8)
estados
length(estados)

variedades <- rep(rep(c("Bacatá", "Cajicá", "Coyuse", "Nehuen"), each = 2), 3)
variedades
length(variedades)

rendimientos <- c(9.55, 10.32, 11.76, 11.60, 7.16, 7.23, 10.68, 11.26,
                  8.61, 8.06, 11.05, 9.80, 6.92, 6.69, 6.96, 10.80,
                  6.77, 6.02, 7.26, 8.72, 6.68, 4.53, 9.92, 13.58)
length(rendimientos)

# Crear el data frame
datos_avena <- data.frame(Estados = estados, Variedades = variedades, Rendimientos = rendimientos)
attach(datos_avena)

# A.  Análisis descriptivo
library(ggplot2)
ggplot(datos_avena, aes(x = Variedades, y = Rendimientos, fill = Estados)) +
  geom_boxplot() +
  labs(title = "Rendimientos proteínicos por variedad y estado fisiológico",
       x = "Variedad de la avena",
       y = "Rendimientos proteínicos (%)") +
  theme_minimal()

#B. Modelo matematico


### Modelo Matemático

#El modelo matemático para un ANOVA de dos vías con interacción se puede expresar de la siguiente manera:
  
#  \[ Y_{ijk} = \mu + \alpha_i + \beta_j + (\alpha\beta)_{ij} + \epsilon_{ijk} \]

#Donde:
  
#- \( Y_{ijk} \): Rendimiento proteínico observado.
#- \( \mu \): Media general.
#- \( \alpha_i \): Efecto del \(i\)-ésimo estado fisiológico (Prefloración, Floración, Grano lechoso).
#- \( \beta_j \): Efecto de la \(j\)-ésima variedad de avena (Bacatá, Cajicá, Coyuse, Nehuen).
#- \( (\alpha\beta)_{ij} \): Efecto de la interacción entre el estado fisiológico \(i\) y la variedad \(j\).
#- \( \epsilon_{ijk} \): Error aleatorio, que se asume que sigue una distribución normal con media cero y varianza constante \(N(0, \sigma^2)\).

### Hipótesis

#Para cada efecto (estado fisiológico, variedad, e interacción), formulamos hipótesis nulas y alternativas:
  

# 3. ANOVA de dos vías

#modelo <- aov(Rendimiento ~ Estado + Variedad + Estado:Variedad, data = datos)
#summary(modelo)

modelo_anova <- aov(Rendimientos ~ Estados*Variedades, data = datos_avena)
summary(modelo_anova)

#a) Realizar un análisis descriptivo de los datos.
#b) Establecer el modelo matemático y las hipótesis del problema.
#c) Construya el ANOVA adecuaado y evalúe supuestos. Interprete.
#d) Usando opciones del paquete dplyr calcule las medias por tratamiento y cons-
#truya una nueva base de datos que debe contener las medias ya calculadas y
#los niveles de los factorés de interés.
#e) Con ayuda del item previo y usando el paquete ggplot2 construya un gráfico
#de interacción adecuado a la situación planteada e interprete.


#Evaluacion de supuestos

#Normalidad

residuos <- residuals(modelo_anova)
par(mfrow = c(1, 2))# Dividir la ventana gráfica en dos paneles

#Densidad de probalidad de residuos
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

#Prueba de normalidad de Shapiro-Wilk
shapiro_test <- shapiro.test(residuos)
print(shapiro_test) #Se muestra que existe normalidad


#Homocedasticidad

#Gráfico de residuos vs valores ajustados
ggplot(data.frame(fitted = fitted(modelo_anova), residuos = residuos), 
       aes(x = fitted, y = residuos)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#2AF3B9") +
  theme_minimal() +
  labs(title = "Residuos vs Valores Ajustados",
       x = "Valores Ajustados",
       y = "Residuos") +
  theme(plot.title = element_text(hjust = 0.5))

bartlett.test(rendimientos ~ estados, data = datos_avena)
#Existe homocedasticidad
#No existen embudos por lo cual existe homocedasticidad

#independencia

# Crear data frame con índices y residuos
residuos_data <- data.frame(
  indice = 1:length(rendimientos),
  residuos = residuals(modelo_anova)
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


# D) Calcular las medias por tratamiento y crear nueva base de datos
medias_tratamiento <- datos_avena %>%
  group_by(Estados, Variedades) %>%
  summarise(Media = mean(Rendimientos))

print(medias_tratamiento)

# E) Crear gráfico de interacción
ggplot(medias_tratamiento, aes(x = Variedades, y = Media, group = Estados, color = Estados)) +
  geom_line() +
  geom_point() +
  labs(title = "Gráfico de Interacción de Rendimientos Proteínicos",
       x = "Variedad de la avena",
       y = "Rendimiento Proteínico (%)") +
  theme_minimal()

#El gráfico de interacción muestra una clara interacción entre los factores 
#estado fisiológico y variedad de avena en relación con el rendimiento 
#proteínico. Las líneas no paralelas indican que el efecto de la variedad 
#sobre el rendimiento depende del estado fisiológico, y viceversa. 
#En prefloración, Coyuse tuvo el mayor rendimiento; en floración, 
#Bacatá fue la mejor; y en grano lechoso, Nehuen mostró el rendimiento más alto. 
#Este patrón sugiere que la elección de la variedad más adecuada para maximizar 
#el rendimiento proteínico dependerá del estado 
#fisiológico en el que se realice el corte.