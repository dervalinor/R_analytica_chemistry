library(agricolae)

#como crear un diseño de bloques en R

?design.rcbd

#crear un diseño de 5 tratamiento y con cuatro bloques

tratamiento = c("T1", "T2", "T3", "T4", "T5")
disenobloques = design.rcbd(tratamiento, 
                            4, seed = 1010)
disenobloques

matrizdiseno = disenobloques$book

attach(matrizdiseno)

matrizdiseno

#ver tabla de tratamiento y bloques en un matriz
disenobloques$sketch #existe hay aleotarizacion de los
#bloques
