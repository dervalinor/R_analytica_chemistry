#DISEÑO ALEATORIZADO
#Caso: Vamos a considerar un experimento en el que 30 ratones tuvieron una condición de leucemia inducida y 
#fueron aleatorizados en tres grupos de 10 animales. Cada grupo fue tratado con uma droga quimioterápica y 
#cada animal fue observado en cuatro  ocasiones. Las siguientes variables fueron observadas:

#Celulas: número de colonias de células cancerosas

#wbc: número de células blancas

#rbc: número de células rojas

#OBJETIVO: Afectan los tratamientos a la enfermedad

#Diseño de experimentos: debe hacer una variable que uno controla y como afecta a otra variable

#ESTUDIAR: entender que es un variables en estadisticas y como se analizan y que modelos se utilizan

#como cargar datos txt en R- usar import databaset o por codigos


#como trabaja el codigo: Aquí, se está utilizando la función read.table() para cargar los datos 
#desde el archivo "datoscel.txt". El argumento header=TRUE indica que la primera fila del archivo contiene 
#los nombres de las columnas, y quote="\"" especifica que las comillas dobles se deben tratar como parte de los datos.
datoscel <- read.table("datoscel.txt", header=TRUE, quote="\"")



#ver datos en otra ventana: View(datoscel)

#separar datos en vectores y poder trabajar en ellas


#Al adjuntar los datos al espacio de trabajo, puedes referenciar las columnas directamente por 
#su nombre sin tener que usar el nombre del objeto (datoscel$...). Esto facilita la escritura del código, pero puede 
#llevar a confusiones si hay nombres de columnas que coinciden con nombres de variables existentes en el espacio de trabajo.
attach(datoscel) #importante colocar esto para trabajar con los datos !!!!!!



#tratamientos es un variable categorica o cualitativa
#variable de celulas es la variable de respuesta debe ser cuantitativa
#siempre un analisis de forma descriptiva

#EStudiar histogramas, graficos de barra

#histograma de la variable de celulas para analizar un sola variable
#hist(celulas) #no sirve para el analisis
#graficos de tortas
#pie(celulas)

#pero tambien existen formas de analizar dos variables para saber si los tratamientos sirven
#graficos de boxplot util para esta caso

#guardar como imagen ir a Export y guardar como imagen y puedes modificar su tamaño


#Aquí, se utiliza la función boxplot() para crear un diagrama de caja que compara la 
#distribución de los datos de la columna 'celulas' en función de los diferentes niveles de la columna 'trat'. 
#La expresión celulas ~ trat indica que queremos comparar los valores de 'celulas' para cada nivel único de 'trat'. 
#Esto nos permite visualizar 
#fácilmente cualquier diferencia en la distribución de las células para cada tratamiento.

boxplot(celulas ~ trat) #celulas vs trat, tratamiento 2 no recomendable por que tiene mas celulas cancerigenas
#pero el tratamiento 1 es mas recomendable menos celulas cancerigenas
#la mediana es la raya negra indica el 50% de los datos y los puntos significa los datos atipicos

#ver estadistica inferencial- intervalos de confianza y pruebas de hipotesis
#Estudiar LaTeX




