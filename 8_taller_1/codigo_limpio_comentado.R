# Problema: Se compararon tres métodos para reducir la flora bacteriana de la piel. Los conteos
# de las bacterias se efectuaron en el pie derecho de las personas antes y después
# del tratamiento. La variable respuesta fue el porcentaje de disminución de las
# bacterias

# Establecer el idioma de la sesión de R a español
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

# Modificar los parámetros gráficos predeterminados
par(
  family = "sans",
  cex.axis = 0.8,
  cex.lab = 0.8,
  cex.main = 0.9,
  mgp = c(2, 0.5, 0)
)

# Cargar el paquete agricolae
require(agricolae)

# 1. Ingresar los datos
metodos <- rep(c("Remolino", "Duchado", "Baño de Pies"), c(9, 9, 9))
respuestas <- c(91, 87, 88, 84, 86, 80, 92, 81, 93,
                18, 22, 20, 29, 25, 16, 15, 26, 19,
                6, 6, 8, 9, 13, 10, 12, 5, 9)
datosbacterias <- data.frame(metodos, respuestas)
attach(datosbacterias)

# 2. Análisis descriptivo
# Crear un gráfico de cajas y bigotes para visualizar las diferencias entre los métodos
boxplot(respuestas ~ metodos, data = datosbacterias, col = c("skyblue", "yellow", "green"),
        xlab = "Métodos", ylab = "Respuestas", main = "Experimento Bacterias")

# El gráfico sugiere que el método "remolino" es el más efectivo para reducir el porcentaje
# de flora bacteriana debido a su amplio rango de dispersión de datos, mayor porcentaje de
# disminución y mayor variabilidad. Los métodos "duchado" y "baño de pies" son menos efectivos.

# 3. Prueba de hipótesis
# Hipótesis a evaluar: existe una diferencia significativa entre los 3 tratamientos
# para la reducción de la flora bacteriana en los pies
modelobacterias <- aov(respuestas ~ metodos, data = datosbacterias)
resumen_bac_pies <- summary(modelobacterias)
p_valor_aov <- resumer_bac_pies[[1]][["Pr(>F)"]][1]

# El p-valor obtenido (p_valor_aov) es menor que el nivel de significancia (0.05), lo que
# indica que se rechaza la hipótesis nula y existe un efecto significativo de los
# tratamientos sobre la variable respuesta (porcentaje de disminución de flora bacteriana).

# 4. Análisis de supuestos

# Normalidad
residualesd <- residuals(modelobacterias)
par(mfrow = c(1, 2))
plot(density(residualesd))  # Gráfico de densidad de residuales
plot(modelobacterias, which = 2)  # Gráfico de cuartil-cuartil de residuales

# El p-valor de la prueba de Shapiro (p_valor_shapiro) es mayor que 0.05, lo que indica
# que se acepta la hipótesis nula y los residuales tienen distribución normal.
resultado_shapiro <- shapiro.test(residualesd)
p_valor_shapiro <- resultado_shapiro$p.value

# Homocedasticidad
dev.off()
plot(modelobacterias, which = 1)  
#Para cambiar los nombres de los ejes quitar which y programar uno mismo los ejes x y x de los 
#valores extrayendolos o creandolos por medio de tratamiento de variables
# Gráfico de residuos contra valores ajustados

# El p-valor de la prueba de Bartlett (p_valor_bartlett) es mayor que 0.05, lo que indica
# que se acepta la hipótesis nula y se cumple el supuesto de homocedasticidad.
require(car)
resultado_bartlett <- bartlett.test(respuestas ~ metodos, data = datosbacterias)
p_valor_bartlett <- resultado_bartlett$p.value

# Independencia
plot(1:27, residualesd, pch = 9)  # Gráfico de residuales

# El gráfico de residuales no muestra una tendencia clara, lo que sugiere que se cumple
# el supuesto de independencia.

# 5. Interpretación del ANOVA
summary(modelobacterias)

# Los resultados del ANOVA indican que hay diferencias estadísticamente significativas
# entre los tres métodos en cuanto al porcentaje de disminución de bacterias (p-valor < 2e-16).
# Esto significa que al menos uno de los métodos es significativamente diferente de los otros
# dos en su efecto sobre la variable de respuesta.