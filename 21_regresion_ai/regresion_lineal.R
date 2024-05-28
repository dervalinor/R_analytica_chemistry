library(ggplot2)
library(scatterplot3d)
library(plotly)

#Un profesor intenta predecir las notas de los examenes finales de sus 
#estudiantes en funcion de las notas de las tareas y las horas de 
#estudio por lo cual quiere crear un modelo que relaciones estas 
#variables para hacer un prediccion

#Y = \beta_0 + \beta_1 x1 + \beta_2 x2

#Y es el puntaje de los examenes
#x1 son las notas de las tareas
#x2 son las horas de estudio

#crear el modelo

# Marco de datos
data <- data.frame(
  exam_score = c(88, 92, 79, 85, 90),
  homework_score = c(85, 95, 80, 90, 88),
  study_hours = c(10, 12, 9, 11, 13)
)

# crear regresion lineal
model <- lm(exam_score ~ homework_score + study_hours, data = data)
#al final ontenemos los coeficientes del modelo 

#resumen de modelo
summary(model)

# Obtener los coeficientes del modelo
coefficients <- coef(model)

# Formatear la ecuación de la regresión
equation <- paste("Exam Score = ",
                  round(coefficients[1], 2), " + ",
                  round(coefficients[2], 2), " * Homework Score + ",
                  round(coefficients[3], 2), " * Study Hours", sep="")

# Imprimir la ecuación en la pantalla
print(equation)


#Observacion grafica

#En una grafica 2D

#grafica de notas de los examenes y notas de las tareas
ggplot(data, aes(x = homework_score, y = exam_score, color = study_hours)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Exam Scores vs Homework Scores",
       x = "Homework Score",
       y = "Exam Score") +
  theme_minimal()

##grafica de notas de los examenes y horas de estudio

ggplot(data, aes(x = study_hours, y = exam_score, color = homework_score)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Exam Scores vs Study Hours",
       x = "Study Hours",
       y = "Exam Score") +
  theme_minimal()


#En 3D

library(scatterplot3d)

s3d = scatterplot3d(data$homework_score, data$study_hours, data$exam_score, 
              pch = 16, highlight.3d = TRUE, 
              type = "h", angle = 55,
              xlab = "Homework Score", 
              ylab = "Study Hours", 
              zlab = "Exam Score")

# Calcular los valores ajustados - linea de tendencia
grid_homework <- seq(min(data$homework_score), max(data$homework_score), length.out = 10)
grid_study <- seq(min(data$study_hours), max(data$study_hours), length.out = 10)
grid_data <- expand.grid(homework_score = grid_homework, study_hours = grid_study)
grid_data$exam_score <- predict(model, newdata = grid_data)

# Agregar la línea de tendencia
s3d$points3d(grid_data$homework_score, grid_data$study_hours, grid_data$exam_score, type = "l", col = "blue", lwd = 2)


#Mostrar con un plano la funcion en forma interactiva
library(plotly)

# Crear una cuadrícula de valores para las predicciones
grid_homework <- seq(min(data$homework_score), max(data$homework_score), length.out = 30)
grid_study <- seq(min(data$study_hours), max(data$study_hours), length.out = 30)
grid_data <- expand.grid(homework_score = grid_homework, study_hours = grid_study)
grid_data$exam_score <- predict(model, newdata = grid_data)

# Crear el gráfico 3D - es sorprendente
plot_ly() %>%
  add_markers(x = data$homework_score, y = data$study_hours, z = data$exam_score, 
              marker = list(size = 5), name = "Datos Originales") %>%
  add_surface(x = ~grid_homework, y = ~grid_study, z = matrix(grid_data$exam_score, nrow = 30), 
              showscale = FALSE, name = "Plano de Regresión") %>%
  layout(scene = list(xaxis = list(title = "Homework Score"),
                      yaxis = list(title = "Study Hours"),
                      zaxis = list(title = "Exam Score")),
         title = "Gráfico 3D con Plano de Regresión")

dev.off()

#ahora podemos hacer predicciones de un par de datos y saber que nota se 
#obtendra
new_data <- data.frame(homework_score = c(87, 90), study_hours = c(10, 14))
predictions <- predict(model, newdata = new_data)
predictions