# Establecer el idioma de la sesión de R a español
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

# Modificar los parámetros gráficos predeterminados para mejorar la presentación
par(
  family = "sans",
  cex.axis = 0.8,
  cex.lab = 0.8,
  cex.main = 0.9,
  mgp = c(2, 0.5, 0)
)

# Cargar el paquete 'agricolae' para trabajar con diseños experimentales
require(agricolae)

# Definir los datos del experimento
metodos = rep(c("Remolino", "Duchado", "Baño de Pies"), c(9, 9, 9))
respuestas = c(91, 87, 88, 84, 86, 80, 92, 81, 93,
               18, 22, 20, 29, 25, 16, 15, 26, 19,
               6, 6, 8, 9, 13, 10, 12, 5, 9)

# Organizar los datos en un data frame
datosbacterias = data.frame(metodos, respuestas)
attach(datosbacterias)

# Análisis descriptivo
# Generar un gráfico de cajas y bigotes (boxplot) para visualizar la distribución de los datos
boxplot(respuestas ~ metodos, data = datosbacterias, col = c("skyblue", "yellow", "green"),
        xlab = "Métodos", ylab = "Respuestas", main = "Experimento Bacterias")

# Interpretar el análisis descriptivo
# El boxplot muestra que el método "Remolino" tiene un rango más amplio de disminución del porcentaje de flora bacteriana
# y una mayor variabilidad en los resultados, lo que sugiere que es el más efectivo de los tres métodos.
# Por otro lado, los métodos "Duchado" y "Baño de Pies" presentan una distribución más agrupada y con medianas más bajas,
# indicando una menor efectividad en la reducción del porcentaje de flora bacteriana.

# Prueba de hipótesis
# Definir el modelo ANOVA para evaluar las diferencias entre los métodos
modelobacterias = aov(respuestas ~ metodos, data = datosbacterias)
resumen_bac_pies = summary(modelobacterias)

# Obtener el p-valor del ANOVA
p_valor_aov <- resumen_bac_pies[[1]][["Pr(>F)"]][1]

# Interpretar el resultado del ANOVA
if (p_valor_aov >= 0.05) {
  print(paste("No hay diferencias significativas entre los tratamientos para disminuir el porcentaje de flora bacteriana (p >=", p_valor_aov, ")"))
} else {
  print(paste("Hay diferencias significativas entre los tratamientos para disminuir el porcentaje de flora bacteriana (p <", p_valor_aov, ")"))
}

# Análisis de supuestos del ANOVA
# Verificar el supuesto de normalidad
residualesd = residuals(modelobacterias)
resultado_shapiro = shapiro.test(residualesd)
p_valor_shapiro = resultado_shapiro$p.value

if (p_valor_shapiro >= 0.05) {
  print(paste("Se cumple el supuesto de normalidad (p >=", p_valor_shapiro, ")"))
} else {
  print(paste("No se cumple el supuesto de normalidad (p <", p_valor_shapiro, ")"))
}

# Verificar el supuesto de homocedasticidad
resultado_bartlett = bartlett.test(respuestas ~ metodos, data = datosbacterias)
p_valor_bartlett = resultado_bartlett$p.value

if (p_valor_bartlett >= 0.05) {
  print(paste("Se cumple el supuesto de homocedasticidad (p >=", p_valor_bartlett, ")"))
} else {
  print(paste("No se cumple el supuesto de homocedasticidad (p <", p_valor_bartlett, ")"))
}

# Verificar el supuesto de independencia
plot(1:27, residualesd, pch = 9)

# Interpretación del ANOVA
summary(modelobacterias)

# Conclusión
# Los resultados del ANOVA indican que existen diferencias estadísticamente significativas entre los tres métodos
# en cuanto al porcentaje de disminución de la flora bacteriana (p-valor < 2e-16).
# Esto significa que al menos uno de los métodos es significativamente diferente de los otros dos en su efecto sobre la variable de respuesta.
# El análisis descriptivo y la verificación de los supuestos del ANOVA permiten concluir que el método "Remolino" es el más efectivo
# para reducir el porcentaje de flora bacteriana en comparación con los métodos "Duchado" y "Baño de Pies".