#el diseño de superficies de respuestas en un modelo utilizado para optmizar
#procesos en el cual se buscar el resultado mas optimo segun la variables
#que se esta tratando, donde se buscar un region donde los factores maximizan
#o minimizan la variable respuesta, esto se hace con un modelo matematico
#polinomial con terminos lineales para conocer el aporte de cada factor 
#a la variable respuesta, terminos cuadraticos para detectar los maximos y
#minimos para encontrar el punto optimo y la interaccion entre factores lo cual
#indica como actuan en conjuntos los factores


### Mathematical Model of Response Surface Designs

#The mathematical model used in Response Surface Methodology (RSM) is typically 
#a polynomial equation. The goal is to fit this model to the experimental 
#data to approximate the true response surface. For simplicity, let's consider 
#a second-order (quadratic) model for two factors. The general 
#form of this model is:

#\[
#Y = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_{11} X_1^2 + 
#\beta_{22} X_2^2 + \beta_{12} X_1 X_2 + \epsilon
#\]

#where:
#- \( Y \) is the response variable (dependent variable).
#- \( X_1 \) and \( X_2 \) are the independent variables (factors).
#- \( \beta_0 \) is the intercept term.
#- \( \beta_1 \) and \( \beta_2 \) are the linear coefficients.
#- \( \beta_{11} \) and \( \beta_{22} \) are the quadratic coefficients.
#- \( \beta_{12} \) is the interaction coefficient.
#- \( \epsilon \) is the random error term.

#Let's break this down step by step:
  
#  1. **Intercept Term (\( \beta_0 \))**:
#  - This is the baseline value of the response when all the 
#factors (\( X_1 \) and \( X_2 \)) are zero.

#2. **Linear Terms (\( \beta_1 X_1 \) and \( \beta_2 X_2 \))**:
#  - These terms represent the individual contribution of each 
#factor to the response. They describe how the response 
#changes as each factor changes linearly.

#3. **Quadratic Terms (\( \beta_{11} X_1^2 \) and \( \beta_{22} X_2^2 \))**:
#  - These terms capture the curvature in the response surface. They 
#show how the response changes when each factor is squared. 
#These terms are important for identifying the optimal levels of the factors.

#4. **Interaction Term (\( \beta_{12} X_1 X_2 \))**:
#  - This term describes how the factors interact with each other. 
#It shows how the effect of one factor on the response 
#depends on the level of the other factor.

#5. **Random Error (\( \epsilon \))**:
#  - This term accounts for the variability in the response that cannot be 
#explained by the model. It represents the random noise in the data.

#In our cooking analogy, the effect of adding salt might depend on the amount 
#of pepper present. The optimal taste might be achieved with 
#a specific combination of both ingredients, rather than 
#optimizing them independently.

#Design the Experiment

#Using a Central Composite Design (CCD), we'll set up experiments with 
#different combinations of temperature and reaction time. Let's assume 
#we have the following experimental design with temperature ranging 
#from 150°C to 250°C and reaction time ranging from 30 minutes to 90 minutes.

### Step 2: Collect Data

#Suppose we have conducted the experiments and collected the following data:
  
#  #|Temperature (°C) #| Reaction Time (minutes) | Yield (%) |
#  #|------------------|-------------------------|-----------|
#  #|150              #| 30                      #| 40        |
#  #|150              | 60                      | 50        |
#  #|150              | 90                      | 45        |
#  #|200              | 30                      | 55        |
#  #|200              | 60                      | 65        |
#  #|200              | 90                      | 60        |
#  #|250              | 30                      | 50        |
#  #|250              | 60                      | 70        |
#  #|250              | 90                      | 65        |
#  #|175              | 45                      | 48        |
#  #|175              | 75                      | 52        |
#  #|225              | 45                      | 68        |
#  #|225              | 75                      | 72        |
#  #|200              | 60                      | 66        |  (Center point)

# Install and load necessary packages
#install.packages("rsm")
library(rsm)

# Input the data
data <- data.frame(
  Temperature = c(150, 150, 150, 200, 200, 200, 250, 250, 250, 
                  175, 175, 225, 225, 200),
  Time = c(30, 60, 90, 30, 60, 90, 30, 60, 90, 45, 75, 45, 75, 60),
  Yield = c(40, 50, 45, 55, 65, 60, 50, 70, 65, 48, 52, 68, 72, 66)
)

# Fit a second-order model
model <- rsm(Yield ~ SO(Temperature, Time), data = data)

# Display the summary of the model
summary(model)

# Generate response surface plots
par(mfrow = c(1, 2)) # set up the plotting area
contour(model, ~ Temperature + Time, image = TRUE, main = "Contour Plot")
persp(model, ~ Temperature + Time, col = "lightblue", theta = 30, 
      phi = 30, main = "3D Surface Plot")
