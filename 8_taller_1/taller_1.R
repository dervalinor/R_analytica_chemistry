#Se compararon tres métodos para reducir la flora bacteriana de la piel. Los conteos
#de las bacterias se efectuaron en el pie derecho de las personas antes y después
#del tratamiento. La variable respuesta fué el porcentaje de disminución de las
#bacterias

#\begin{table}[ht]
#\centering
#\begin{tabular}{|l|*{9}{c|}} % Se agregó un asterisco (*) para especificar que hay nueve columnas de tipo 'c'
#\toprule
#Método & \multicolumn{9}{c|}{Repeticiones}\\ % Se especifica que la multicolumna debe tener nueve columnas
#\midrule
#Agitación por remolino & 91 & 87 & 88 & 84 & 86 & 80 & 92 & 81 & 93\\
#Duchado                & 18 & 22 & 20 & 29 & 25 & 16 & 15 & 26 & 19\\
#Baño de pies            &  6 &  6 &  8 &  9 & 13 & 10 & 12 &  5 &  9\\
#\bottomrule
#\end{tabular}
#\caption{Ejemplo de tabla con \texttt{booktabs}}
#\label{tab:ejemplo}
#\end{table}

#El objetivo del experimento es esstablecer si hay diferencias entre los efectos de los
#tratamientos sobre la variable respuesta. Para este fin, de respuesta a las siguientes
#preguntas:
  #a) Realice un análisis descriptivo en el pueda darse una idea del efecto de
#los tratamientos sobre la variable respuesta. Interprete en el contexto del
#problema.
#b) Especificar el diseño y el modelo para analizar estos datos.
#c) Describir los factores, los niveles, las unidades experimentales y la variable
#de respuesta.
#d) Efectuar el ANOVA correspondiente al modelo y haga un diagnóstico com-
  #pleto del modelo.
#e) Interpretar la prueba F si usted considera que esto es adecuado. Caso contra-
  #rio, realice un procedimiento estadístico adecuado que le permita concluir
#sobre el objetivo del experimento.

#Solucion de GPT-4

# Primero, definimos los datos de los tratamientos y las respuestas correspondientes.
tratamientos = rep(c("Agitación por remolino", "Duchado", "Baño de pies"), c(9, 9, 9))
respuestas = c(91, 87, 88, 84, 86, 80, 92, 81, 93,
               18, 22, 20, 29, 25, 16, 15, 26, 19,
               6,  6,  8,  9, 13, 10, 12,  5,  9)

# Creamos un dataframe que combina los vectores de tratamientos y respuestas.
datos = data.frame(tratamientos, respuestas)

# Usamos la función attach() para poder referirnos a las columnas del dataframe por su nombre.
attach(datos)

# Generamos un boxplot para visualizar la distribución de las respuestas para cada tratamiento.
boxplot(respuestas ~ tratamientos, data = datos, col = c("pink","purple","lightgreen"))

# Realizamos el ANOVA utilizando la función aov().
modelo = aov(respuestas ~ tratamientos, data = datos)

# Mostramos el resumen del modelo.
summary(modelo)

# Calculamos los residuos del modelo.
residuos = residuals(modelo)

# Realizamos la prueba de Shapiro-Wilk para verificar la normalidad de los residuos.
shapiro.test(residuos)

# Si el valor p obtenido de la prueba de Shapiro-Wilk es menor que 0.05, entonces los residuos no siguen una distribución normal.
# En este caso, podríamos considerar transformar los datos o usar un modelo de regresión no paramétrico.

# Realizamos la prueba de Levene para verificar la homocedasticidad de los residuos.
#install.packages("car")
require(car)
leveneTest(respuestas ~ tratamientos, data = datos)

# Si el valor p obtenido de la prueba de Levene es menor que 0.05, entonces los residuos no tienen varianzas iguales.
# En este caso, podríamos considerar transformar los datos o usar un modelo de regresión no paramétrico.

# Finalmente, interpretamos el resultado del ANOVA.
# Si el valor p obtenido del ANOVA es menor que 0.05, entonces al menos uno de los tratamientos tiene un efecto significativo en la disminución de las bacterias.


