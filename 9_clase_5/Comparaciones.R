#Este es mi codigo editado por mi con AI pero no puede esta bien

#install.packages("gmodels")
library(gmodels)

#compara distintos tratamientos, pruebas multiples, Análisis de contrastes ortogonales

#Esta es un analisis estadistico para comparar distintos tratamientos de forma eficiente
#y ver si existen diferencias entre los distintos tratamientos entre ellos
#para este fin se requiere que existe ortogonalidad, es decir que 
#cada tratamiento se independiente de otros y que no se vean afectados por 
#los demas y tampoco que no se vean afectados por sus comparaciones

#Deben ser ortogonoles ya que nos aportarian informacion unica y relevente, es decir
#al ser linealmente independientes no redundan en la informacion dada.

#Los contrastes son la combinacion lineal de la media de los tratamientos que se comparan
#donde las constantes de esta combinacion nos da como difieren los metodos entre si

# Paso 1: Cargar los datos proporcionados
# Definimos los vectores para cada tratamiento
control <- c(75, 67, 70, 75, 65, 71, 67, 67, 76, 68)
D1 <- c(57, 58, 60, 59, 62, 60, 60, 57, 59, 61)
D2 <- c(58, 61, 56, 58, 57, 56, 61, 60, 57, 58)
D3 <- c(62, 66, 65, 63, 64, 62, 65, 65, 62, 67)
D4 <- c(58, 59, 58, 61, 57, 56, 58, 57, 57, 59)

# Combinamos los datos en un solo vector
longitudes <- c(control, D1, D2, D3, D4)

# Creamos un vector para identificar los tratamientos
azuc <- rep(c("control", "D1", "D2", "D3", "D4"), each = 10)

# Convertimos 'azuc' en una variable categórica
azuc <- factor(azuc)

# Creamos el dataframe 'datos' con los datos y la variable categórica
datos <- data.frame(longitudes, azuc)

# Adjuntamos los datos al entorno de trabajo
attach(datos)

# Paso 2: Realizar el análisis de varianza (ANOVA)
# Realizamos el ANOVA
anova <- aov(longitudes ~ azuc)

# Mostramos los resultados del ANOVA
summary(anova)

# Paso 3: Realizar los contrastes ortogonales
# Definimos las matrices de contrastes ortogonales
contrasts(azuc) <- contr.helmert(5)


  q1 = c(-4, 1, 1, 1, 1)
  q2 = c(0, 1, 1, 1, -3)
  q3 = c(0, 1, 1, -3, 1)
  q4 = c(0, 1, -1, 0, 0)

#crear un matriz de contrastes

Matrix_contraste = matrix(c(q1, q2, q3, q4),  nrow = 4, ncol = 5, byrow = TRUE)

Matrix_contraste

row.names(Matrix_contraste) = c("q1", "q2", "q3", "q4")

#ver si son ortogonales

#sum(q1*q2)

# Calculamos el producto interno entre todos los pares de contrastes
contrastes <- list(q1, q2, q3, q4)

for (i in 1:(length(contrastes) - 1)) {
  for (j in (i + 1):length(contrastes)) {
    producto_interno <- sum(contrastes[[i]] * contrastes[[j]])
    cat("Producto interno entre q", i, "y q", j, ": ", producto_interno, "\n")
  }
}

fit.contrast(anova, "azuc", Matrix_contraste)
