#Problema: Se compararon tres métodos para reducir la flora bacteriana de la piel. Los conteos
#de las bacterias se efectuaron en el pie derecho de las personas antes y después
#del tratamiento. La variable respuesta fué el porcentaje de disminución de las
#bacterias

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
require(agricolae)#al finalizar la instalación del paquete se llama el paquete para identificar con cual se va a trabajar
metodos=rep(c("remolino","duchado","pies"),c(9,9,9))#se introduce una columna de datos en este caso el nombre de cada método y se señala cuantos datos tendrá la columna(9)
metodos#se verifica que se haya insertado correctamente la columna 
respuestas=c(91,87,88,84,86,80,92,81,93,
             18,22,20,29,25,16,15,26,19,
             6,6,8,9,13,10,12,5,9)#se introduce la columna de datos en este caso, porcentaje de bacterias.
length(respuestas)#se verifica que sea correcta la cantidad de datos insertados
datosbacterias=data.frame(metodos,respuestas)#se unen las columnas para organizarlos en tabla de datos
datosbacterias#verificar que los datos estén en una columna
attach(datosbacterias)#para rabajar el diseño completamente aleatorizados con cada columna por separado
#vamos a hacer un analisis descriptivo
boxplot(respuestas~metodos,data=datosbacterias,col=c("skyblue","yellow","green")
        ,Xlab="metodos",Ylab="respuestas",main="experimento bacterias")#realiza el grafico de cajas y 
        #bigotes con las cajas de colores y nombres de cada eje y el nombre del gráfico


#modelo de ANOVA para evaluar hipotesis

#Nota: el valor p nos proporciona una medida de cuán compatible son los datos observados con la hipótesis nula, 
#y nos ayuda a tomar decisiones sobre la 
#validez de nuestras afirmaciones basadas en la evidencia proporcionada por los datos experimentales.

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
  print("Existen pruebas suficientes para aceptar la hipotesis nula entonces 
        no hay diferencias significativas entre los tratamientos para dismunuir el porcentaje
        de flora bacteriana en los pies")
} else {
  print("Se rechaza la hipotesis nula si hay diferencias significativas entre los tratamientos para disminuir el
        porcentaje de flora bacteriana")
}



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
plot(density(residualesd))#gráfico de distribución normal de los residuales de los datos
plot(modelobacterias,which=2)#gráfico cuartil-cuartil de residuales para verificar que los datos si tengan una relación lineal 
#en los graficos se observa que hay normalidad en el estudio, sin embargo, se va a hacer un test de shapiro para verificar la hipotesis
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
  print("Se acepta la hipotesis nula, entonces existe normalidad en el modelo")
} else {
  print("Se rechaza la hipotesis nula, entonces no existe normalidad")
}


#HOMOCEDASTICIDAD
dev.off()#termina la división en la ventana de gráfico
plot(modelobacterias,which=1)#grafico de residuos contra los valores ajustados para verificar homocedasticidad siempre que no existan embudos de los residuales
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
  print("Se cumple el supuesto de homocedasticidad")
} else {
  print("No se cumple el supuesto de homocedasticidad")
}


#INDEPENDENCIA - Evitar tener sesgos que afecten la observaciones ya que cada observacion deber se independiente de la otra, sin preferencias
#de una por la otra, evitar que concepciones preconcebidas efecte nuestra intrepetacion del experimento

#Los sesgos cognitivos son atajos mentales o patrones de pensamiento que pueden distorsionar nuestra percepción y toma de decisiones!!!!

plot(1:27,residualesd,pch=9)#grafico de residuales de cada resultado para verificar independencia de los datos, visualizando que los datos no tengan tendencia.
#como se observa los valores estan distribuidos aleatoriamente, es decir no hay una tendencia en los resultados, 
#esto nos da entender que si hay independencia.
#interpretación anova
summary(modelobacterias)#de nuevo el p valor con el cual al verificar los supuestos ya se tiene seguridad de las conclusiones.
#ahora sí, como observamos el pvalor< a 0,005.
#entonces los métodos están afectando el porcentaje de disminución de las bacterias en las personas. 
#esto lo puedo decir con toda confianza, puesto que ya se hizo todo el estudio de supuestos (Normalidad, Homocedasticidad e Independencia). 
