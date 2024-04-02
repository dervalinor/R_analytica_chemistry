#En un estudio de diferentes empaques para la carne, Gómez y González(1991)
#realizaron una prueba microbiológica y contabilizaron el número de
#mesófilos en las diferentes muestras analizadas. Los resultados se pre-
#sentan en la siguiente tabla

#se van a hacr  5 replicas, empaques T1, T2, T3, T4, T5 Y T6
#T1 = 280, 30, 0, 10, 50
#T2 = 10, 20, 20, 20, 30
#T3 = 80, 90, 10, 90, 60
#T5 = 10, 10, 10, 20, 0
#T6 = 30, 40, 40, 40, 10

#analizar Si los empaques afecta el Número de mesófilos en cada una de las diferentes envolturas.
#Evalúe los supuestos de normalidad y homogeneidad de varianzas

# Datos de las muestras
replicas <- c("T1", "T2", "T3", "T4", "T5", "T6")
T1 <- c(280, 30, 0, 10, 50)
T2 <- c(10, 20, 20, 20, 30)
T3 <- c(80, 90, 10, 90, 60)
T4 <- c(10, 10, 10, 20, 0)
T5 <- c(10, 20, 40, 10, 10)
T6 <- c(30, 40, 40, 40, 10)

# Combinar en un solo vector
respuestas <- c(T1, T2, T3, T4, T5, T6)

# Crear vector de tratamientos
tratamientos <- rep(replicas, each = 5)

# Crear dataframe
datos_empaques <- data.frame(tratamientos, respuestas)

# Análisis exploratorio
summary(datos_empaques)
boxplot(respuestas ~ tratamientos, data = datos_empaques,
        xlab = "Tipo de Empaque",
        ylab = "Número de Mesófilos",
        main = "Boxplot de Número de Mesófilos por Tipo de Empaque")

# Prueba ANOVA
anova_result <- aov(respuestas ~ tratamientos, data = datos_empaques)
anova_result
p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]

# Supuestos de ANOVA
# Normalidad

# Prueba de normalidad
plot(density(residuals(anova_result)))
plot(anova_result, which = 2)

# Gráfico para evaluar homogeneidad de varianzas
plot(anova_result, which = 1)


shapiro.test(residuals(anova_result))

# Homogeneidad de varianzas, homocedasticidad, hacer prueba de Levene

# Cargar el paquete necesario
library(car)

# Realizar la prueba de Levene para evaluar la homogeneidad de varianzas
levene_test <- leveneTest(respuestas ~ tratamientos, data = datos_empaques)
levene_test

#se rechaza H, no hay normalidad

#Transformar datos por medio de un funcion para corregir homocedasticidad o aplicar un prueba no parametrica
#preocuparse de los supuestos !!!!

#Box matematico dijo para transformar datos - 1. funcion logaritmo natural, 2- raiz cuadrada, 3- numero reciproco
#con logaritmo arreglo homocedastidad y normalidad

#R: log  es Ln y Log en base 10 es log10

#para evitar el error de Ln(0) = infinito se agrega + 1
datos_empaques$rt = log(datos_empaques$respuestas + 1)
attach(datos_empaques)
datos_empaques

#datos transformadas
modelo_meso = aov(rt ~ tratamientos, data = datos_empaques)
residualest = residuals(modelo_meso)
par(mfrow = c(1, 2))
plot(density(residualest))
plot(modelo_meso, which = 2)

#ver prueba de normalidad
shapiro.test(residualest)

#el annova no resiste la asimetria
