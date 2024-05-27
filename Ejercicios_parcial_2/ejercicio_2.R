#el cuadrado latino DCL es para cuado hay dos gurpos de bloques y solo
#nos intera dos tipos de datos

semillas=c("C","D","A","B",
           "B","C","D","A",
           "A","B","C","D",
           "D","A","B","C"
           )
semillas

resultado=c(36.0,33.8,32.6,26.9,
            31.4,29.1,25.0,30.3,
            28.4,34.3,25.3,28.7,
            31.1,33.3,34.2,26.3
            )
resultado

suelos=rep(c("S1","S2","S3","S4"),4)
suelos


riego=rep(c("R1","R2","R3","R4"),each=4)
riego


datossemillas=data.frame(semillas,resultado,suelos,riego)
datossemillas

attach(datossemillas)

modelosemillas=aov(resultado~semillas+suelos+riego,data = datossemillas)
summary(modelosemillas)
#el mayor del 0,05 la hipotesis nula se acepta, las semillas de mani con su tipo de variable

