#este codigo tiene como fin la asignacion al azar de los tratamientos en 
#cada bloque esto se hace antes de experimentacion

library(agricolae)

#como crear un diseño de bloques en R

?design.rcbd

#crear un diseño de 5 tratamiento y con cuatro bloques

tratamiento = c("T1", "T2", "T3", "T4", "T5")

#Este codigo es para genera un diseño de bloques completamente al azar
#Donde por medio de esta codigo se genera un diseño para los tratamientos
#con 4 bloques y para obtener un misma secuencia de aleatorizacion se usa
#la semillas seed con el numero 1010.

disenobloques = design.rcbd(tratamiento, 
                            4, seed = 1010)

disenobloques

matrizdiseno = disenobloques$book
#ver como distribuyen los tratamiento de forma
#aleotoria

attach(matrizdiseno)

matrizdiseno

#ver tabla de tratamiento y bloques en forma de una matriz eje x son los 
#tratamientos y eje y son los bloques
disenobloques$sketch #existe hay aleotarizacion de los
#bloques