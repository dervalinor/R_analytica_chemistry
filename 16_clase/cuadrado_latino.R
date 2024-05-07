#sirve para reducir el error experimental para generar bloqueo de estas fuentes
#de error, aqui se puede controlar dos factores una extension de diseño de 
#bloques.

#Este modelo se caracteriza por organizar los elementos en una cuadricula
#donde en cada fila y columna solo se repita un tipo de elemento una sola
#vez

library(agricolae)

?design.lsd

#crear un diseño de LSD crear un diseño con 4 tratamientos

tratamientos <- paste("T", 1:4, sep = "")

disenolsd = design.lsd(tratamientos, seed = 1010) # em este diseño se controla 
#dos factores que no son de nuestro interes pero que si afectan nuestra 
#respuesta de nuestra variable respuesta.

disenolsd

disenolsd$book

disenolsd$sketch #ver diferencias con un DBA