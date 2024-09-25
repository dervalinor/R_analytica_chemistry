library(car)
library(agricolae)
library(gmodels)
library(mvtnorm)
library(multcomp)
library(survival)
library(ggplot2)

#Para prueba de aditividad
library(daewr)

#El bloque es mas efectivo en un diseño factorial


Tratamiento = rep(c("Tratado 1", "Control"), 8)
Bloques = rep(rep(c("b1", "b2"), each = 2), 4)
InterB = rep(c("c1", "c2", "c3", "c4"), each = 4)


# Combinar los datos en un vector
respuesta <- c(18.7, 7.7, 16.7, 6.4,
               17.9, 8.4, 14.4, 6.7,
               19.2, 9.8, 12.6, 8.1,
               26.3, 9.7, 19.8, 6.0)

datos <- data.frame(
  Tratamiento = factor(Tratamiento),
  InterB = factor(InterB),
  Bloques = factor(Bloques),
  respuesta
)

#debe dar una interaccion entre las InterBs y la respuesta por lo cual 
#interaccion.

modelo = aov(respuesta ~ Tratamiento*InterB + Bloques, data = datos)
summary(modelo)

#Evaluacion de supuestos:

#Convertir graficas a ggplot2

#Normalidad

residualesd=residuals(modelo)

par(mfrow=c(1,2))

#Densidad de probabilidad

ggplot(data.frame(residualesd), aes(x = residualesd)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  labs(title = "Density Plot of Residuals", x = "Residuals", y = "Density") +
  theme_minimal()

#q-q cuartil grafico

ggplot(data.frame(residualesd), aes(sample = residualesd)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of Residuals") +
  theme_minimal()

dev.off() 

resultado_test <- shapiro.test(residualesd)

summary(resultado_test) #ver resultado del test

#obtener p-valor
p_valor <- resultado_test$p.value

#vamos a ver si acepta o se rechaza la hipotesis nula

if(p_valor >= 0.05) {
  print("Se acepta la hipotesis nula, entonces existe normalidad en el modelo")
} else {
  print("Se rechaza la hipotesis nula, entonces no existe normalidad")
}


#Homocedasticidad
#Supuesto de Homocedasticidad
#Gráfico
plot(modelo,which=1)

prueba_bartlett <- bartlett.test(respuesta~Tratamiento,data=datos)

p_valor_b = prueba_bartlett$p.value

if(p_valor_b >= 0.05){
  print("Existe homocedasticidad, varianzas de los dietas similares")
} else {
  print("No existe homocedasticidad, varianzas son diferentes")
}

#Independencia (Supuesto)
ggplot(data.frame(Index = 1:length(respuesta), Residuals = residualesd), aes(x = Index, y = Residuals)) +
  geom_point() +
  labs(title = "Independence of Residuals", x = "Index", y = "Residuals") +
  theme_minimal()


#Evaluacion de aditividad

datos_ad = data.frame(respuesta, Tratamiento = factor(Tratamiento), Bloques = factor(Bloques), InterB = factor(InterB))

# Fit a linear model with interaction terms
modelo_interaccion = aov(respuesta ~ Tratamiento * Bloques, data = datos_ad)

# Perform ANOVA to evaluate interaction terms
anova_interaccion = anova(modelo_interaccion)
anova_interaccion

# Extract p-value for interaction term
p_valor_interaccion = anova_interaccion["Tratamiento:Bloques", "Pr(>F)"]

cat("Resultados de la prueba de aditividad: \n")

if(p_valor_interaccion >= 0.05) {
  cat("No hay interacción significativa entre los tratamientos y las bloques. La aditividad se mantiene.\n")
} else {
  cat("Hay interacción significativa entre los tratamientos y las bloques. La aditividad no se mantiene.\n")
}

#The significant F value for the blocks 
#indicate that including blocks in the design was a good decision. 
#This significance means that the variability due to blocks is non-negligible 
#and accounting for it improves the model's ability to explain 
#the variability in the response variable.

#By including blocks, you effectively reduce the residual error and 
#increase the power of the statistical tests for the factors of interest 
#(Tratamiento and InterB). In summary, the analysis supports the conclusion that 
#blocking has helped in controlling variability and should be considered 
#a beneficial aspect of the experimental design.