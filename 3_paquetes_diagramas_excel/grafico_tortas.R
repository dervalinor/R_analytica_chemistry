#Realizar un diagrama de torta desde un archivo de excel
#IMPORTANTE: import dataset e instalar dependencias de excel

# Como instralar un paquete y como cargarlo: Instala y carga el paquete readxl si no lo has hecho antes
#install.packages("readxl")

#cargar paquete
library(readxl)

# Lee los datos del archivo Excel

#saber rutas de archivos
#file.choose()

datos <- read_excel("R_analytica_chemistry/3_paquetes_diagramas_excel/datos.xlsx")
View(datos)

# Crea el gráfico de torta
# Función pie() para crear un gráfico de torta
pie(datos$Valor,  # Vector numérico con los tamaños de las porciones de la torta
    labels = datos$Categoría,  # Etiquetas de las categorías para cada porción de la torta
    main = "Gráfico de Torta")  # Título principal del gráfico

