#instalar R-studio desde https://posit.co/download/rstudio-desktop/
#apt install r-base
#operaciones matematicas
#1+1



#> es el simbolo de la terminal y es un prompt

#como cargar datos txt en R- usar import databaset o por codigos
datoscel <- read.table("~/R_analytica_chemistry/1_introduccion/datoscel.txt", header=TRUE, quote="\"")

#ver datos, ejecutar en consola, el prompt es exclusivo de la consola
#View(datoscel)

#cargar datos en consola
datoscel
