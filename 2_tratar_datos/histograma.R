#Aquí, se utiliza la función hist() para crear un histograma de los valores en la columna 'celulas'. 
#La función hist() toma como argumento el vector de datos que se desea visualizar. Al utilizar celulas directamente, 
#se está refiriendo a la columna 'celulas' del conjunto de datos datoscel, que fue adjuntado al espacio de trabajo anteriormente.



datoscel <- read.table("datoscel.txt", header=TRUE, quote="\"")
attach(datoscel)

#crear histograma
hist(celulas)
