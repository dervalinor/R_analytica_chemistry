library(multcomp)

icoportres<-c(7.8,8.3,7.6,8.4,8.3)
icopordiez<-c(5.4,7.4,7.1)
icoporquince<-c(8.1,6.4)
shopakdiez<-c(7.9,9.5,10)
shopakquince<-c(7.1)
empaques<-rep(c("icoportres","icopordiez","icoporquince","shopakdiez","shopakquince"),c(5,3,2,3,1))
tiempos<-c(icoportres,icopordiez,icoporquince,shopakdiez,shopakquince)
empaques=factor(empaques)
datosagua=data.frame(tiempos,empaques)
attach(datosagua)
modeloagua=aov(tiempos~empaques,data=datosagua)
summary(modeloagua)
#diagnostico si hay normalidad, se hace la prueba de barnnet y da error por que la prueba de barnnet necesita mas de una replica 
#por ende se aplica la prueba de leven 
#ver como hacer el boxplot____
# se introducen contrastes 
q1=c(2,2,2,-3,-3)
q2=c(0,2,2,-2,-2)
q3=c(2,-1,-1,0,0)
#se debe comprobar si son ortogonales con el siguente comando 
sum(q1*q2)
sum(q1*q3)
sum(q2*q3)
#no se puede aplicar el fit.contras, se debe aplicar otra cosa
#se deben cargar los paquetes
L=matrix(c(q1,q2,q3),nrow=3,ncol=5,byrow=TRUE)
#se le colocan los nombres a la matrix
rownames(L)=c("Q1","Q2","Q3")
L
comp=glht(modeloagua,linfct=mcp(empaques=L))
# se aplica metodo de bonferroni
summary(comp,test=adjusted("bonferroni"))
#no hay diferencias significativas sobre el agua que se perserva y los dias 
#prueba de scheffe consultar y hasta aqui el primer parcial