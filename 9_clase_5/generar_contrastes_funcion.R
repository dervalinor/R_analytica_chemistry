#vamos a crear un funcion que nos permita crear contrastes entre tratamientos
#donde los coefientes su suma sea sero en un combinacion lineal de las medias al
#comparar tratamientos

control<-c(75,67,70,75,65,71,67,67,76,68)
D1<-c(57,58,60,59,62,60,60,57,59,61)
D2<-c(58,61,56,58,57,56,61,60,57,58)
D3<-c(62,66,65,63,64,62,65,65,62,67)
D4<-c(58,59,58,61,57,56,58,57,57,59)

longitudes<-c(control,D1,D2,D3,D4)
azuc<-rep(c("control","D1","D2","D3","D4"),
          each=10)
azuc=factor(azuc)
datos<-data.frame(longitudes,azuc)
attach(datos)
datos


modelogui=aov(longitudes~azuc,data=datos)
residuosgui=residuals(modelogui)
residuosgui
par(mfrow=c(1,2))
plot(density(residuosgui))
plot(modelogui,which=2)
shapiro.test(residuosgui)
plot(modelodietas,which=1)
require(car)
bartlett.test(longitudes~azuc,data=datos)
plot(1:24,residualesd,pch=19)

summary(modelogui)

#creacion de la funcion- codigo generado por Claude ai


#Este codigo no funciona!!!!!

create_contrasts <- function(treatments) { #donde de cumplirse que la suma de coefientes debe ser cero pero 
  #a los tratamientos que se esta comparando no pueden ser cero esto para definir un combinacion lineal 
  #de coefientes con las media, es decir sumatorio de coeficiente y medias igual a cero
  # Número total de tratamientos
  n_treatments <- length(treatments)
  
  # Crear matriz de contrastes
  contrasts <- matrix(0, nrow = n_treatments - 1, ncol = n_treatments)
  
  # Contraste 1: Control vs Tratamientos
  contrasts[1, ] <- c(1, rep(-1/n_treatments, n_treatments - 1))
  
  # Otros contrastes
  for (i in 2:n_treatments) {
    # Contraste i: Tratamiento i vs Resto de tratamientos
    contrasts[i, ] <- c(rep(0, i - 1), 1, rep(-1/(n_treatments - i + 1), n_treatments - i))
  }
  
  return(contrasts)
}

#llamar a funcion

treatments <- c(control, D1, D2, D3, D4)
contrasts <- create_contrasts(treatments)


