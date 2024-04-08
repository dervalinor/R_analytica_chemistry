#Problema: Se compararon tres métodos para reducir la flora bacteriana de la piel. Los conteos
#de las bacterias se efectuaron en el pie derecho de las personas antes y después
#del tratamiento. La variable respuesta fué el porcentaje de disminución de las
#bacterias

# Establecer el idioma de la sesión de R a español - Toco usar GIMP para editar las imagenes jajaja
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

# Modificar los parámetros gráficos predeterminados
par(
  family = "sans",
  cex.axis = 0.8,
  cex.lab = 0.8,
  cex.main = 0.9,
  mgp = c(2, 0.5, 0)
)

#\begin{table}[ht]
#\centering
#\begin{tabular}{|l|*{9}{c|}} 
#\toprule
#Método & \multicolumn{9}{c|}{Repeticiones}\\ 
#\midrule
#Agitación por remolino & 91 & 87 & 88 & 84 & 86 & 80 & 92 & 81 & 93\\
#Duchado                & 18 & 22 & 20 & 29 & 25 & 16 & 15 & 26 & 19\\
#Baño de pies            &  6 &  6 &  8 &  9 & 13 & 10 & 12 &  5 &  9\\
#\bottomrule
#\end{tabular}
#\caption{Ejemplo de tabla con \texttt{booktabs}}
#\label{tab:ejemplo}
#\end{table}

#El objetivo del experimento es establecer si hay diferencias entre los efectos de los
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

#install.packages("agricolae")#se necesita instalar el paquete donde se puede realizar los estudios del Diseño Completamente Aleatorizados.

#1. Ingresar Datos
require(agricolae)#al finalizar la instalación del paquete se llama el paquete para identificar con cual se va a trabajar
metodos=rep(c("Remolino","Duchado","Baño de Pies"),c(9,9,9))#se introduce una columna de datos en este caso el nombre de cada método y se señala cuantos datos tendrá la columna(9)
metodos#se verifica que se haya insertado correctamente la columna 
respuestas=c(91,87,88,84,86,80,92,81,93,
             18,22,20,29,25,16,15,26,19,
             6,6,8,9,13,10,12,5,9)#se introduce la columna de datos en este caso, porcentaje de bacterias.
length(respuestas)#se verifica que sea correcta la cantidad de datos insertados
datosbacterias=data.frame(metodos,respuestas)#se unen las columnas para organizarlos en tabla de datos
datosbacterias#verificar que los datos estén en una columna
attach(datosbacterias)#para rabajar el diseño completamente aleatorizados con cada columna por separado


#2. Analisis descriptivos
#vamos a hacer un analisis descriptivo
boxplot(respuestas~metodos,data=datosbacterias,col=c("skyblue","yellow","green")
        , xlab="Metodos", ylab="Respuestas", main="Experimento Bacterias")#realiza el grafico de cajas y 
        #bigotes con las cajas de colores y nombres de cada eje y el nombre del gráfico
#en el boxplot se muestra que el metodo remolino tiene rango mas amplio de dispersion de datos y
#un mayor porcentaje de disminucion de flora bacteriana ademas de una mayor variabilidad de la 
#dismunucion de porcentaje de disminucion de la flora bacteriana.
#En contraste, los métodos "duchado" y "baño de pies" muestran una distribución más agrupada y con medianas más bajas, 
#lo que sugiere que son menos efectivos que el método "remolino" en reducir el porcentaje de flora bacteriana.

#El análisis del boxplot sugiere que el método "remolino" parece ser el más efectivo de los 
#tres en términos de reducción del porcentaje de flora bacteriana

#modelo de ANOVA para evaluar hipotesis

#Nota: el valor p nos proporciona una medida de cuán compatible son los datos observados con la hipótesis nula, 
#y nos ayuda a tomar decisiones sobre la 
#validez de nuestras afirmaciones basadas en la evidencia proporcionada por los datos experimentales.


#3. Prueba de hipotesis
#para estos casos se toma un nivel de significancia de 0.05

#Hipotesis a evaluar: existe una diferencias significativa entre los 3 tratamientos para la reduccion de la flora bacteriana en los 
#pies

modelobacterias = aov(respuestas~metodos,data=datosbacterias)#como se trata de un diseño de un solo factor se usa un ANOVA de una vía, con el cual se verifica la hipotesis principal del estudio(si los métodos estan afectando las respuestas)
resumen_bac_pies = summary(modelobacterias)#este resumen es para obtener el pvalor, sin embargo, este solo será valido verificando los supuestos del ANOVA
#Al generar en R un p valor, se genera en sí, una prueba de hipótesis
#Hipótesis Nula= no hay efecto del tratamiento sobre la variable respuesta
#hipótesis alternativa= Hay efecto de algún tratamiento sobre la variable respuesta
#por ello si el pvalor es mayor que el valor de significancia acepto la hipótesis nula, que es lo mismo que decir rechazo la hipótesis alternativa
#si el pvalor es menor que el valor de significancia rechazo la hipótesis nula o acepto la hipótesis alternativa
#vamos a tomar a F (valor de significancia) =0,05 como Pr < F (el p valor es menor al valor de significancia),
#eso nos da a entender que: acepto el la hipotesis alternativo, hay efecto de los tratamientos; los tratamientos están afectando a la 
#variable de respuesta que en este caso es el porcentaje de 
#disminuación de las bacterias.sin embargo, no podemos estar completamente seguros.

#vamos a ver esto por medio de un condicional en R

p_valor_aov <- resumen_bac_pies[[1]][["Pr(>F)"]][1]

if(p_valor_aov >= 0.05){
  print(paste("Existen pruebas suficientes para aceptar la hipotesis nula entonces 
        no hay diferencias significativas entre los tratamientos para dismunuir el porcentaje
        de flora bacteriana en los pies con un p valor (p >= 0.05): ", p_valor_aov))
} else {
  print(paste("Se rechaza la hipotesis nula si hay diferencias significativas entre los tratamientos para disminuir el
        porcentaje de flora bacteriana con un p valor (p < 0.05): ", p_valor_aov))
}

#4. Analisis de supuestos

#Ahora vamos a ver los supuestos para el modelo de ANOVA para los tratamientos:

#1. Normalidad
#2. Homocedasticidad
#3. Independencia

#Esto se hace ya que para que el modelo de ANOVA sus resultados sean confiables es decir
#los supuestos para el ANOVA de normalidad nos permite tener resultos confiables 
#y evitar sesgos en estos para la determinacion de medias, mientras el supuesto de homocedasticidad nos 
#permite saber si las prediccion del modelo son precisas y evitar rechazar un hipotesis de forma erronea

#NORMALIDAD
residualesd=residuals(modelobacterias)#para cada estudio de los supuestos se requiere usar los residuales 
residualesd#se llama a los residuales para observarlos en la consola, con ello se puede observar si la diferencia 
#es aceptable
par(mfrow=c(1,2))# se divide la ventana de gráfico en una fila y dos columnas

#gráfico de distribución normal de los residuales de los datos
# Graficar la densidad de los residuales y cambiar los títulos al español
plot(density(residualesd))


#gráfico cuartil-cuartil de residuales para verificar que los datos si tengan una relación lineal 
#en los graficos se observa que hay normalidad en el estudio, sin embargo, se va a 
#hacer un test de shapiro para verificar la hipotesis
# Graficar el gráfico de cuartil-cuartil de los residuales y cambiar los títulos al español

plot(modelobacterias, which = 2)

resultado_shapiro = shapiro.test(residualesd)#realiza el test de shapiro con el cual se verifica que haya normalidad o no haya
#Al realizar la prueba de shapiro las hipótesis serían
#Hipótesis Nula= Hay normalidad
#Hipótesis alternativa= No hay normalidad
#por ello si el pvalor es mayor que el valor de significancia acepto la hipótesis nula, que es lo mismo que decir rechazo 
#la hipótesis alternativa
#si el pvalor es menor que el valor de significancia rechazo la hipótesis nula o acepto la hipótesis alternativa
#como p-value >0,005 eso nos da entender que aceptamos la hipótesis nula es decir los residuales son normales. 

#obtencion del p valor
p_valor_shapiro = resultado_shapiro$p.value

if(p_valor_shapiro >= 0.05){
  print(paste("Se acepta la hipotesis nula, entonces existe normalidad en el modelo con un p valor (p > 0.05): ", p_valor_shapiro))
} else {
  print(paste("Se rechaza la hipotesis nula, entonces no existe normalidad con un p valor (p < 0.05): ", p_valor_shapiro))
}


#HOMOCEDASTICIDAD
dev.off()#termina la división en la ventana de gráfico

# Graficar el gráfico de residuos vs. valores ajustados y cambiar los títulos al español

plot(modelobacterias, which = 1)


#grafico de residuos contra los valores ajustados para verificar homocedasticidad 
#siempre que no existan embudos de los residuales
#como se observa en el grafico no hay formas de cono en los valores, aparentemente hay homocedasticidad. 
#como hay normalidad se va a hacer el test de bartlett
#install.packages("car")#instalación de paquete para poder realizar el test de bartlett o levene, según cual sea necesario
require(car)#llamar el paquete descargado para realizar test de bartlett
resultado_bartlett = bartlett.test(respuestas~metodos,data=datosbacterias)#se realiza test de bartlett para obtener pvalor y comprobar la homocedasticidad
#Al realizar el test de bartlett las hipótesis serían
#Hipótesis Nula= Hay homocedasticidad
#Hipótesis alternativa= Hay heterocedasticidad
#por ello si el pvalor es mayor que el valor de significancia acepto la hipótesis nula, que es lo mismo que decir rechazo la hipótesis alternativa
#si el pvalor es menor que el valor de significancia rechazo la hipótesis nula o acepto la hipótesis alternativa
#como vemos p-value > a 0,05 esto nos da a entender que se acepta la hipótesis nula por ende hay homocedasticidad. 

p_valor_bartlett = resultado_bartlett$p.value

if(p_valor_bartlett >= 0.05){
  print(paste("Se cumple el supuesto de homocedasticidad con un p valor (p >= 0.05): ", p_valor_bartlett))
} else {
  print(paste("No se cumple el supuesto de homocedasticidad con un p valor de: ", p_valor_bartlett))
}


#INDEPENDENCIA - Evitar tener sesgos que afecten la observaciones ya que cada observacion deber se independiente de la otra, sin preferencias
#de una por la otra, evitar que concepciones preconcebidas efecte nuestra intrepetacion del experimento

#Los sesgos cognitivos son atajos mentales o patrones de pensamiento que pueden distorsionar nuestra percepción y toma de decisiones!!!!

plot(1:27,residualesd,pch=9)#grafico de residuales de cada resultado para verificar independencia de los datos, visualizando que los datos no tengan tendencia.
#como se observa los valores estan distribuidos aleatoriamente, es decir no hay una tendencia en los resultados, 
#esto nos da entender que si hay independencia.
#interpretación anova
summary(modelobacterias)#de nuevo el p valor con el cual al verificar los supuestos ya se tiene seguridad de las conclusiones.
#ahora sí, como observamos el pvalor< a 0,05.
#entonces los métodos están afectando el porcentaje de disminución de las bacterias en las personas. 
#esto lo puedo decir con toda confianza, puesto que ya se hizo todo el estudio de supuestos (Normalidad, Homocedasticidad e Independencia). 


#Interpretacion del ANOVA: 

#Nota: El valor F es la relacion de la variabilidad entre los grupos y la variabilidad interna de cada grupo si F es alto
#entonces existen diferencias significativas entre grupos.

#Claro, vamos a interpretar paso a paso los resultados del ANOVA que se muestran en la terminal:

#```

#Df Sum Sq Mean Sq F value   Pr(>F)    

#metodos           2  31801   15900  922.3 < 2e-16 ***
  
#  Residuals        24    414      17                    

#---
  
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#```

#Cada fila representa una fuente de variación en el modelo ANOVA:
  
#  1. metodos: Esta fila corresponde al efecto del factor "metodos" en el modelo. Tiene:
  
#  - Df: Grados de libertad para el factor "metodos", que son 2 (ya que hay 3 métodos).

#- Sum Sq: Suma de cuadrados para el factor "metodos", que es 31801.

#- Mean Sq: Cuadrado medio para el factor "metodos", que es 15900.

#- F value: El estadístico F, que es 922.3. Este valor se usa para evaluar si hay diferencias significativas entre los métodos.

#- ¡¡¡¡¡Pr(>F): El p-valor asociado al estadístico F, que es < 2e-16. Este p-valor es extremadamente pequeño, lo que significa 
#que hay evidencia muy fuerte para rechazar la hipótesis nula de que no hay diferencias entre los métodos !!!!!!!

#2. Residuals: Esta fila corresponde a la variación residual del modelo, es decir, la variación que no es explicada por el factor "metodos".

#- Df: Grados de libertad para los residuos, que son 24.

#- Sum Sq: Suma de cuadrados de los residuos, que es 414.

#- Mean Sq: Cuadrado medio de los residuos, que es 17.

#Finalmente, los códigos de significancia (**Signif. codes**) indican la fuerza de la evidencia para rechazar la hipótesis nula:
  
#  - "***" indica que el p-valor es menor a 0.001, lo que significa que hay una evidencia muy fuerte en contra de la hipótesis nula!!!!!!

#En resumen, los resultados del ANOVA indican que hay diferencias estadísticamente significativas entre los 
#tres métodos en cuanto al porcentaje de disminución de bacterias (p-valor < 2e-16). 
#Esto significa que al menos uno de los métodos es significativamente diferente de los otros dos en 
#su efecto sobre la variable de respuesta.
