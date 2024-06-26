#Prueba de Tukey Dunnet 

#Hacer comparaciones de tratamientos
#Ver prueba de Dunnet 

#El efecto de los reguladores de crecimiento de las plantas sobre el alargamiento de esparr´agos fu´e investigado por 
#Yang-Gyu and Woolley (2006).
#La tasa de alargamiento de los tallos es un factor importante que determina el rendimiento 
#final de los esp´arragos en muchas condiciones clim´aticas
#templadas. Se cosecharon tallos de plantas de esp´arragos Jersey Giant
#de 6 a˜nos de edad cultivadas en una plantaci´on comercial en Bulls (latitud 40.2S, 
#longitud 175.4E), Nueva Zelanda. Los tallos fueron cosechados
#al azar y transportadas del campo al laboratorio para su investigaci´on.
#Despu´es de recortar a 80 mm de longitud, las tallos se sumergieron por
#completo durante 1 h en Soluciones acuosas de 10 mg l-1 de concentraci´on
#de ´acido indol-3-ac´etico (IAA), ´acido absc´ısico (ABA), GA3 o CPPU (Sitofex EC 2.0 %; 
#SKW, Trostberg, Alemania) en tubos de ensayo. Los tallos
#de control se sumergieron en agua destilada durante 1 h. El experimento
#fue un dise˜no completamente aleatorizado con cinco repeticiones (tallos)
#por tratamiento. Los datos resultantes (longitud final del tallo en mm) se
#muestran a continuaci´on:

################################################
### Entering data: studs ###
################################################
################################################
AAAAcontrol<-c(94.7,96.1,86.5,98.5,94.9)
IAA<-c(89.9,94.0,99.1,92.8,99.4)
ABA<-c(96.8,87.8,89.1,91.1,89.4)
GA3<-c(99.1,95.3,94.6,93.1,95.7)
CPPU<-c(104.4,98.9,98.9,106.5,104.8)
longitudes<-c(AAAAcontrol,IAA,ABA,GA3,CPPU)
soluciones<-rep(c("AAAAcontrol","IAA","ABA","GA3","CPPU"),
                each=5) #each para repetir datos

soluciones=factor(soluciones) #Factor - Que es esto?  

#marco de trabajo
datojesp = data.frame(longitudes, soluciones)

attach(datojesp)

#analisis descriptivo
boxplot(longitudes ~ soluciones,data=datojesp,col=c("#2EE689","#DF4BF3","#4BE2F3", "#DDD53E", "#DD513E")
        , xlab="Longitudes", ylab="Soluciones", main="Experimento Esparragos")

#crear modelo en este caso es ANOVA
modeloesp = aov(longitudes ~ soluciones, data = datojesp)
resumen_modeloesp = summary(modeloesp)
p_valor_aov = resumen_modeloesp[[1]][["Pr(>F)"]][1] #cargar el resumen del anova como variable y acceder al p-valor

if(p_valor_aov >= 0.05){
  print(paste("Existen pruebas suficientes para aceptar la hipotesis nula entonces 
        no hay diferencias significativas entre tratamientos con p valor (p >= 0.05): ", p_valor_aov))
} else {
  print(paste("Se rechaza la hipotesis nula si hay diferencias significativas entre tratamientos con p valor (p < 0.05): ", p_valor_aov))
}

#prueba de Tukey
prueba_tukey = TukeyHSD(modeloesp)
prueba_tukey

plot(prueba_tukey, las = 2, cex.axis = 0.4) #raya punteada justo en cero
#es un grafico de un intervalo de confianza y los no que tocan el valor cero indica que hay
#diferencias estadisticas de los tratamientos para este caso CPPU-ABA

#Hipotesis de la prueba de Tukey

#H  que la media de i es igual a media de j
#A que la media de i es diferente de media de j

#Prueba de Dunnet
#install.packages("mvtnorm")
#install.packages("multcomp")
#install.packages("survival")

#Como cargar varias librerias en un sola linea de codigo
library("mvtnorm")
library("multcomp")
library("survival")

# Crear el objeto de contraste de Dunnett
dunnett_prueba = glht(modeloesp, linfct = mcp(soluciones = "Dunnett"), alternative = "two.sided")
resumen_dunnet = summary(dunnett_prueba)

#Grafica de la prueba de Dunnet
plot(resumen_dunnet) #solo una tiene diferencias entre ellas he incluyendo el control

#Hipotesis de la prueba de Dunnet
#Hipotesis nula: cada tratamiento es igual al control
#Hipotesis alternativa: al menos alguno de los tratamientos es diferente al control















