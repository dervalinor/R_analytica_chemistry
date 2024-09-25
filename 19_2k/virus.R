library(daewr)
data(virus)
virus(virus)
Tukey1df(virus)# genera un solo p valor
#desde el paquete asbio hacer la prueba de tukey 
#install.packages("asbio")
#require(asbio)
names(virus)
attach(virus)
tukey.add.test(y,sample,Dilution)
#se realiza el anova por individual y se concluye que las diferencias solo corresponden a la saluciones 
#se debeia conciderar un DCA 
###### se comienza los diseños 2K
#que son? son diseños en donde se consideran k factores y en esos factores e consideran 2 niveles 
# si se habla de un diseño 2^3
#sirve para seleccionar los factores que si sirven, por ejemplo de 10 puede seleccionar solo 1 o 2 o 5
#no se puede considerar mas de 2 niveles por valor
#N1:nivel bajo
#N2:nivel alto