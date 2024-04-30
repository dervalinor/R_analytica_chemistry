# Cargar la librería agricolae
library(agricolae)

# Definir los tratamientos
tratamientos <- c("Agua del grifo", 
                  "Agua del grifo con azúcar", 
                  "Agua del grifo con agua carbonatada", 
                  "Agua del grifo con 7-up")

# Definir los bloques
bloques <- c("Rosa", "Clavel", "Margarita", "Tulipán")

# Generar el diseño de bloques completos aleatorizados
set.seed(1010) # Fijar una semilla para reproducibilidad
disenoFlores <- design.rcbd(trt = tratamientos, r = length(bloques), seed = 1010)

# Imprimir el diseño
disenoFlores

#matriz de diseño

matrizflores = disenoFlores$book
matrizflores

attach(matrizdiseno)

#ver matriz
disenoFlores$sketch