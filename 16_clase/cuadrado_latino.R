#sirve para reducir el error experimental para generar bloqueo de estas fuentes
#de error, aqui se puede controlar dos factores una extension de diseño de 
#bloques.

library(agricolae)

?design.lsd

#crear un diseño de LSD crear un diseño con 4 tratamientos

tratamientos <- paste("T", 1:4, sep = "")

disenolsd = design.lsd(tratamientos, seed = 1010)

disenolsd

disenolsd$book

disenolsd$sketch #ver diferencias con un DBA