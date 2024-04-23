#crear codigo generales para ANOVA, bloques y otras pruebas para solo pegar y 
#remplazar datos.

#librerias
library(car)
library(agricolae)
library(gmodels)
library(mvtnorm)
library(multcomp)
library(survival)
library(ggplot2)

#El diseño de bloque al azar (DBA)

#Este diseño busca quitar el ruido que puede sesgar 
#nuestros resultados o interpretaciones del 
#experimento, ya que existen factores externos 
#que afectan a nuestra variable respuesta
#por lo cual se crean bloques donde los 
#factores externos sean homoceneos 
#los bloques agrupan a unidades experimentales
#muy similares entre si.

#Por ejemplo: si se esta estudiando el efecto de una
#dieta en la en la perdida peso en poblacion pero 
#se conoce que factores como actividad fisica, edad
# y metabolismo, por lo cual se reune a los participantes
#del estudio en bloques como:

# - Mujeres de 30 a 40 años con un nivel de actividad
#fisica moderada

#-hombres de 40 a 50 años con un metabolismo lento


#Problema

#Supongamos que estamos realizando un experimento para evaluar 
#el rendimiento de diferentes variedades de maíz. 
#Tenemos 4 variedades (A, B, C y D) y hemos dividido el campo 
#experimental en 3 bloques (bloques 1, 2 y 3) 
#para controlar la variabilidad del suelo. Cada bloque 
#contiene las 4 variedades, y queremos 
#comparar el rendimiento de las variedades después de la cosecha.

#Primero, crearemos un data frame con los datos del experimento:

rendimiento <- c(80, 75, 85, 90, 78, 82, 88, 84, 76, 79, 87, 83)
variedad <- rep(c("A", "B", "C", "D"), times = 3)

bloque <- rep(1:3, each = 4) #se divide la tierra de siembra en 3 secciones
#debido a las caracteristicas del terreno y en cada seccion se siembra las 
#4 variedades de maiz.

#En este experimento, hay 3 bloques (1, 2 y 3) y 
#4 variedades de maíz (A, B, C y D). 
#Cada bloque contiene las 4 variedades.

#La tierra para sembrar las 4 especies de maiz en  se divide 
#3 secciones y cada seccion o
#bloque se siembra las cuatro 4 variadades de maiz, esto se 
#hace ya que cada bloque tiene
#tierra que tiene unas caracteristicas especificas y 
#esto hace tener datos mas precisos
#ya que se minimiza el los efectos externos que 
#afectan a la variable respuesta.

#Para enterder este codigo: 

datos <- data.frame(rendimiento, variedad, bloque)

#Ahora, ajustaremos el modelo de DBA utilizando la función aov():

modelo <- aov(rendimiento ~ variedad + bloque, data = datos)
summary(modelo)

#Este resumen nos muestra los grados de libertad, la suma de cuadrados, 
#el cuadrado medio, el valor F y el valor p para cada fuente 
#de variación (variedad y bloque).

#Si el efecto de la variedad es significativo (valor p pequeño), 
#podemos realizar pruebas de comparaciones múltiples para identificar 
#qué variedades difieren entre sí. Una opción es usar la prueba de Tukey:

#como la prueba de ANOVA nos dice si existen variaciones significativas 
#o no entre los grupos, pero no nos dice cual o cuales variadades
#difieren mas que las otras para esto hacemos Tukey para comparaciones
#multiples esto lo hace en comparacion de parejas de variedades.
#(A-B, A-C, A-D, B-C, B-D, C-D) y se determina si hay diferencias 
#significativas.

prueba_tukey = TukeyHSD(modelo, 'variedad')
prueba_tukey

# Acceder a los valores-p
p_valores_tukey = prueba_tukey[["variedad"]][, "p adj"]
p_valores_tukey

# Acceder a los valores-p
p_valores_tukey = prueba_tukey[["variedad"]][, "p adj"]

# Acceder a los nombres de los tratamientos
nombres_tratamientos = rownames(prueba_tukey[["variedad"]])

# Definir el nivel de significancia
alpha = 0.05

# Comparar los valores-p con el nivel de significancia
for (i in 1:length(p_valores_tukey)) {
  tratamiento1 = strsplit(nombres_tratamientos[i], "-")[[1]][1]
  tratamiento2 = strsplit(nombres_tratamientos[i], "-")[[1]][2]
  
  if (p_valores_tukey[i] < alpha) {
    cat("La diferencia entre el tratamiento", tratamiento1, "y el tratamiento", tratamiento2, "es estadísticamente significativa (p-valor =", 
        round(p_valores_tukey[i], 4), ")\n")
  } else {
    cat("La diferencia entre el tratamiento", tratamiento1, "y el tratamiento", tratamiento2, "no es estadísticamente significativa (p-valor =", 
        round(p_valores_tukey[i], 4), ")\n")
  }
}

#grafico de Tukey
plot(prueba_tukey, las = 2, cex.axis = 0.4) 
#En este grafico se nota que la comparacion C-A son estadisticamente 
#significativo entre ellos ya que no toca la linea de cero vertical


#Esta función realiza la prueba de Tukey 
#y muestra las diferencias entre las medias de 
#las variedades y los intervalos de confianza ajustados.

#podemos crear un gráfico de medias e intervalos de 
#confianza para visualizar mejor los resultados:

ggplot(datos, aes(x = variedad, y = rendimiento)) +
  geom_point(position = position_jitter(width = 0.2), size = 3) +
  geom_errorbar(aes(group = variedad, ymin = rendimiento - 1, 
                    ymax = rendimiento + 1), 
                width = 0.2, position = position_dodge(0.05)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, 
               position = position_dodge(0.05)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               position = position_dodge(0.05)) +
  labs(x = "Variedad", y = "Rendimiento") +
  theme_bw()