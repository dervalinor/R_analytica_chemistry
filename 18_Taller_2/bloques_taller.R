library(car)
library(agricolae)
library(daewr)

#Ver en caso F es significativo o no para este experimento.

#Mediante un diseño experimental de bloques se comparó el número promedio de
#linfocitos (en miles/mm 3 ) en ratones a los cuales se les administraron dos drogas
#experimentales y un placebo. Se seleccionaron ratones de la misma camada y del
#mismo sexo para formar los bloques

#\begin{tabular}{lccccccc}
#\multicolumn{1}{c}{} & \multicolumn{7}{c}{Bloques} \\
#Tratamientos & 1 & 2 & 3 & 4 & 5 & 6 & 7\\
#\hline
#Droga A & 6 & 4.8 & 6.9 & 6.4 & 5.5 & 9 & 6.8\\
#Droga B & 6.1 & 6.9 & 6.5 & 5.6 & 3.9 & 7.0 & 5.4\\
#Placebo & 5.4 & 4.0 & 7.0 & 5.8 & 3.5 & 7.6 & 5.5\\
#\end{tabular}

#a) (Valor 1,0) Establezca el modelo matemático apropiado para el análisis de es-
#tos datos y formule las hipótesis de forma adecuada si el interés es estableces
#si hay efecto de las dorgas y el placebo sobre el número de lifocitos.
#b) (Valor 2,0) Realice un ANOVA e interprete solamente si los supuestos se
#satisfacen.
#c) (Valor 2,0) Discuta la efectividad de la formación de bloques en este experi-
#mento. Asuma que el diagnóstico del modelo, desarrollado en el item previo,
#es satisfactorio.

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

# Ajustar el modelo ANOVA

#Prueba de aditividad
#Es necesario verificar si existen interacciones entre 
#los bloques y los tratamientos, para asi saber si elegir una diseño de 
#bloques al azar o un diseño factorial

#El modelo matemático adecuado para evaluar la interacción entre 
#los bloques y los tratamientos sería el siguiente:
#Y_ijk = μ + τ_i + β_j + (τβ)_ij + ε_ijk
#Donde:
  
#Y_ijk es la respuesta (número de linfocitos) para el 
#tratamiento i, bloque j y réplica k.
#μ es la media general.
#τ_i es el efecto del tratamiento i (Droga A, Droga B, Placebo).
#β_j es el efecto del bloque j.
#(τβ)_ij es el efecto de la interacción entre el tratamiento i y el bloque j.
#ε_ijk es el error aleatorio.

#Las hipótesis para evaluar la interacción 
#entre los bloques y los tratamientos serían:

#  Hipótesis nula (H0):
#  No existe interacción entre los bloques y los tratamientos.
#(τβ)_ij = 0 para todos i, j

#Hipótesis alternativa (H1):
#  Existe interacción entre los bloques y los tratamientos.
#(τβ)_ij ≠ 0 para al menos un par i, j

#Podemos suponer un diseño factorial para verificar si existen interacciones
#entre bloques y tratamientos

modelo_fac = aov(Respuesta ~ Tratamiento*Bloque, data = datos)
resumen_fac = summary(modelo_fac)

p_valor_fac = resumen_fac[[1]][["Pr(>F)"]][3]

if(p_valor_fac >= 0.05){
  cat("Se acepta la hipotesis nula, NO existen interacciones entre los
      bloques y los tratamientos: ", p_valor_fac)
} else {
  cat("Se rechaza la hipotesis nula, SI existen interacciones
      entre bloques y tratamientos: ", p_valor_fac)
}

#O tambien solo haciendo la prueba de aditividad
#convertir a factor las tratamientos y bloques

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

boxplot(Respuesta ~ Tratamiento, col = c("#2FD9B7", "#B72FD9", "#D92F42"))

#bloxplot de respuesta-bloques

#No puedeo hacer analisis
boxplot(Respuesta ~ Bloque, col = c("#2FD9B7", "#B72FD9", "#D92F42", 
                                            "#6FD92F", "#D9B02F", 
                                            "#64C8C5", "#6485C8"))


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
par(mfrow = c(1, 2))# Dividir la ventana gráfica en dos paneles
plot(density(residuals(modelo_dba)))  # Gráfico de densidad de residuos
qqnorm(residuals(modelo_dba))  # Gráfico de cuantiles normales
qqline(residuals(modelo_dba), col = "red")  # Línea de referencia

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
plot(modelo_dba, which = 1)  # Gráfico de residuos vs. valores ajustados
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
# Debe existe un patron aleatorio
plot(1:length(Respuesta),residuals(modelo_dba),pch=9)
#Existe un patron aleoatorio por lo cual hay independencia

# Diagnóstico general
#par(mfrow = c(2, 2))  # Dividir la ventana gráfica en 4 paneles
#plot(modelo)  # Gráficos de diagnóstico

# Interpretación de los resultados
# Analizar los gráficos y las pruebas estadísticas para 
#determinar si los supuestos se cumplen o no

#EFICIENCIA
#calculo de eficiencia del modelo de bloques

var_error_DBA = deviance(modelo_dba)/df.residual(modelo_dba)
var_error_DCA = deviance(modelo)/df.residual(modelo)

eficiencia_nueva = var_error_DCA/var_error_DBA
eficiencia_nueva #si hay una mejor eficiencia del BDA vs DCA

#Ahora vemos la formula que nos enseño el profesor en clase.

#calcula el cuadrado medio del error (Mean Square Error, MSE) para el Diseño 
#Compuesto Aumentado (DCA), denotado por σ^2_DCA.
#Esta fórmula se compone de dos sumandos:
  
#SCB (Suma de Cuadrados de Bloques): Esta suma se obtiene del análisis de 
#varianza (ANOVA) del Diseño de Bloques al 
#Azar (DBA), que es un componente del DCA. SCE (Suma de Cuadrados del Error): 
#También se obtiene del ANOVA del DBA.

#Estos dos componentes se suman y se dividen por k(b-1), donde:
  
#k es el número de bloques del DBA
#b es el número de unidades experimentales por bloque

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
