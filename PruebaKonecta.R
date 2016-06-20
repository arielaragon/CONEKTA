###########################################################
###########################################################
###############CARGA DE PAQUETES Y LIBRERIAS###############

install.packages("rjson")
install.packages("jsonlite")
install.packages("RJSONIO")
install.packages("xtable")
install.packages("gmodels")
library(gmodels)
library(rjson)
library(RJSONIO)
library(jsonlite)
library(xtable)
library(ggplot2)
library(MASS)
getwd()
setwd("~/Downloads")

###########################################################
###########################################################
############## CARGUEMOS LOS DATOS DEL JSON ###############

DK<-fromJSON("dataset.json", simplifyDataFrame = TRUE)

###########################################################
###########################################################
####### RECLASIFIQUEMOS ALGUNAS VARIABLES #################

### En lo siguiente se hace una reclasificacion de variables, ya que no se podia
### trabajar al 100% con los datos originales, se reclasificaron la mayoria asi 
### se opto por hacer la siguiente reclasificación.

DK$amount1<-as.numeric(ifelse(DK$amount=="amount below 1000",1000,ifelse(DK$amount=="amount below 10000",10000,ifelse(DK$amount=="amount below 10500",10500,ifelse(DK$amount=="amount below 13000",13000,ifelse(DK$amount=="amount below 1500",1500,ifelse(DK$amount=="amount below 2000",2000,ifelse(DK$amount=="amount below 2500",2500,ifelse(DKl$amount=="amount below 3000",3000,ifelse(DK$amount=="amount below 3500",3500,ifelse(DK$amount=="amount below 4000",4000,ifelse(DK$amount=="amount below 4500",4500,ifelse(DK$amount=="amount below 500",500,ifelse(DK$amount=="amount below 5000",5000,ifelse(DK$amount=="amount below 5500",5500,ifelse(DK$amount=="amount below 6000",6000,ifelse(DK$amount=="amount below 6500",6500,ifelse(DK$amount=="amount below 7000",7000,ifelse(DK$amount=="amount below 7500",7500,ifelse(DK$amount=="amount below 8000",8000,ifelse(DK$amount=="amount below 8500",8500,ifelse(DK$amount=="amount below 9000",9000,ifelse(DK$amount=="amount below 9500",9500,0)))))))))))))))))))))))
DK$amount2<-as.factor(ifelse(DK$amount1<=2500,'0-2500',ifelse(DK$amount1<=5000,'2500-5000','5000+')))
DK$bin1<-as.factor(ifelse(DK$bin<=400000,'370700-400000',ifelse(DK$bin<=450000,'400000-450000',ifelse(DK$bin<=500000,"450000-500000","500000 +"))))
statusC=rep(0,nrow(DK))
statusC[with(DK,status=="paid")]=1
statusC[with(DK,status=="partially_refunded")]=2
statusC[with(DK,status=="refunded")]=3
statusC[with(DK,status=="antifraud_declined")]=4
statusC[with(DK,status=="charged_back")]=5
DK$statusC<-statusC
statusB=rep(0,nrow(DK))
statusB[with(DK,status=="paid")]=1
statusB[with(DK,status=="partially_refunded")]=0
statusB[with(DK,status=="refunded")]=0
statusB[with(DK,status=="antifraud_declined")]=0
statusB[with(DK,status=="charged_back")]=0
DK$statusB<-statusB
max(DK$created_at)
min(DK$created_at)
DK$created_at1<-as.POSIXct(DK$created_at,origin = "1960-01-01")
DK$Time<-sapply(strsplit(as.character(DK$created_at1)," "),"[",2)
DK$TimeCompare<-ifelse(strptime(DK$Time,"%H:%M:%S")>strptime("23:59:00","%H:%M:%S"),"IntentoFraud",ifelse( strptime(DK$Time,"%H:%M:%S")<strptime("04:00:00","%H:%M:%S"),"IntentoFraud","NoIntentoFraud"))
DK$CoApo<-sample(0:1,length(DK$email),replace = TRUE)

###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
# Preciso de hacer una estratificacion de apocrifo y no apocrifo por que hay ciertos correos que ya se encuentran en bases de
# datos catalogados como correos apocrifos, entonces supondremos algo similar. Un ejemplo es la siguiente liga:
# https://raw.githubusercontent.com/zeioth/django-email-blacklist/master/disposable_email_domains.txt
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################

# De igual form las horas, puede haber horas, como la madrugada, que hackers quieran intentar cometer fraudes, por lo que 
# las horas tendran una clasificacion similar. Las ip's las catalogaremos en 9 categorias para saber de donde viene la transaccion,
# o de que sector.

DK$Apocrifo<-ifelse(DK$CoApo==1,"CorreoApocrifo","CorreoNoApocrifo")
DK$IPcategoria<-sample(1:9,length(DK$email),replace = TRUE)
DK$TimeCompare2<-ifelse(DK$TimeCompare=="IntentoFraud",1,2)
DK$Apocrifo2<-ifelse(DK$Apocrifo=="CorreoApocrifo",1,2)


###########################################################
###########################################################
####### UNA BREVE DESCRIPCION GRAFICA DE LOS DATOS ########

CrossTable(DK$amount, DK$status, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
hist(DK$amount1, xlab = "MontosDeCargos", ylab = "Frequency", main = " ") 
boxplot(DK$amount1, bty="n",xlab = "", cex=0.4) 
ggplot(DK, aes(x=status))+geom_bar()
ggplot(DK, aes(x = status, y = amount1)) + geom_boxplot()
ggplot(DK, aes(x=status, fill=amount))+geom_bar()
ggplot(DK, aes(x=status, fill=amount))+geom_bar(position = "fill")

###########################################################
###########################################################
####### ALGUNOS METODOS PARA EVALUAR CLASIFICACION ########

###Regresión Logística
###Para la regresión logística se va a usar la variable "status" como la variable dicótoma
###de tal manera que lo que se tendra será que aquellas transacciones con el status 
###charged_back y antifraud_declined  como transacciones fraudulentas, representadas con 0
###y las paid como no fraudulentas, representadas con 1, ahora quedan dos status que son el
###partially_refunded y refunded, estas bien pueden ser ignoradas, sin embargo bien se pueden 
###considerar como fraudulentas o no, ya que si se piensa como una persona que busca sacar un
###beneficio, bien se podrian hacer transacciones de compra y despues solicitar el reembolso.
###para este analisis se tomaran como acciones fraudulentas. Ya que se tiene la definicion de
###las variables se hace la regresión logística.

DKP=DK
DKP$created_at<-NULL
DKP$ip<-NULL
DKP$email<-NULL
DKP$phone<-NULL
DKP$cardA<-NULL
DKP$status<-NULL
DKP$statusC<-NULL
DKP$card<-NULL
DKP$cardA<-NULL
DKP$CA<-NULL
DKP$CoApo<-NULL
DKP$created_at0<-NULL
DKP$created_at1<-NULL
DKP$Time<-NULL
DKP$Apocrifo<-NULL
DKP$TimeCompare<-NULL
DKP$amount<-NULL
DKP$amount2<-NULL
DKP$bin<-NULL
DKP$bin1<-DK$bin1
DKP$statusB<-DK$statusB
DKP$TimeCompare2<-DK$TimeCompare2
DKP$Apocrifo2<-DK$Apocrifo2
DKP$IPcategoria<-DK$IPcategoria

###########################################################
############ Tomemos una coleccion de datos ###############
######### de entrenamiento y una para prueba: #############

DKM=sort(sample(nrow(DKP), nrow(DKP)*.5))
TRAINDKM<-DKP[DKM,]
TESTDKM<-DKP[-DKM,]

###
###REGRESION LOGISTICA_1, APLICADO A LA VARIABLE statusB en funcion de las demas variables
###

RL<-glm(TRAINDKM$statusB~.,family = binomial(),data=TRAINDKM)
summary(RL)

### Ahora a evaluar el modelo
### Se hace un breve parentesisi, si en las siguiente 5 lineas no corre por la falta de un dato en la muestra
### entonces se debe hacer el muestreo nuevamente de los datos de entranimiento y de prueba.

library(ROCR)
TESTDKM$SCORE<-predict(RL,type ='response',TESTDKM)   
PREDICCION<-prediction(TESTDKM$SCORE,TESTDKM$statusB)
COMPORTAMIENTO<-performance(PREDICCION,"tpr","fpr")
plot(COMPORTAMIENTO)

#Se implementa el estadistico Kolmogorov-Smirnof, la cual es la
#diferencia maxima entre la funcion de distribucion de los
#verdaderos positivos y de los falsos positivos.

max(attr(COMPORTAMIENTO,'y.values')[[1]]-attr(COMPORTAMIENTO,'x.values')[[1]])
###Resultado: 0.1187982

###OTRA REGRESION
###REGRESION LOGISTICA_2, APLICADO A LA VARIABLE statusB en funcion de las demas variables
###

DTAFRM<-data.frame(cbind(TRAINDKM$statusB,TRAINDKM$amount,TRAINDKM$TimeCompare))
RL1<-glm(TRAINDKM$statusB~TRAINDKM$amount+TRAINDKM$TimeCompare ,family = binomial(),data=as.data.frame(cbind(TRAINDKM$statusB,TRAINDKM$amount,TRAINDKM$TimeCompare)))
summary(RL1)

###Ahora a evaluar el modelo nuevamente

library(ROCR)
TESTDKM$SCORE0<-predict(RL1,type ='response',TESTDKM)
PREDICCION<-prediction(TESTDKM$score,TESTDKM$status)
COMPORTAMIENTO<-performance(PREDICCION,"tpr","fpr")
plot(COMPORTAMIENTO)

#Se implementa el estadistico Kolmogorov-Smirnof, la cual es la
#diferencia maxima entre la funcion de distribucion de los
#verdaderos positivos y de los falsos positivos.

max(attr(COMPORTAMIENTO,'y.values')[[1]]-attr(COMPORTAMIENTO,'x.values')[[1]])

###Resultado: 0.02502992

### CLASIFICACION DE ARBOLES
### Nos brinda una regresion de arboles para determinar como se puede hacer la seleccion
### de algunas variables. En el algoritmo de la clasificacion de arboles se va ponderando
### cada variable para poder determinar su importancia en el arbol.

library(rpart)

EstimArbol<-rpart(TRAINDKM$statusB~.,data=TRAINDKM)  #Se hace la estimacion mediante la funcion rpart
TESTDKM$SCORE1<-predict(EstimArbol,TESTDKM)          #Se estima el score de cada transaccion, y la cual es agregada en TESTDKM
PREDICCION2<-prediction(TESTDKM$SCORE1[],TESTDKM$statusB) #Se hace una prediccion con respecto a la variable de interes
COMPORTAMIENTO2 <- performance(PREDICCION2,"tpr","fpr")   #Toma en cuenta el comportamiento de los falsos positivos y los verdaderos positivos
printcp(EstimArbol)                                  #Muestra el resultado de la regresion
plotcp(EstimArbol)                                   #Nos permite ver los nodos y su error relativo
plot(COMPORTAMIENTO2,col='red',lty=1,main='Tree vs Tree with PriorProb') #Grafica la estimacion de los falsos positivos vs verdaderos positivos
EstimArbol2<-rpart(TRAINDKM$statusB~.,data=TRAINDKM,parms=list(prior=c(.9,.1)),cp=.0002) #Se realiza otra estimacion pero con probabilidades apriori
plot(EstimArbol2);text(EstimArbol2);                 #Brinda el arbol con los nodos y como se clasificaron.
TESTDKM$SCORE2<-predict(EstimArbol2,TESTDKM)         #Nos da el score de EstimArbol2
PREDICCION3<-prediction(TESTDKM$SCORE2[],TESTDKM$statusB) #Evalua la prediccion con respecto a la variable de interes
COMPORTAMIENTO3<-performance(PREDICCION3,"tpr","fpr")#Toma en cuenta el comportamiento de los falsos positivos y los verdaderos positivos
printcp(EstimArbol2)                                 #Muestra la regresion de arbo, el error de cada nodo, su desviacion
plotcp(EstimArbol2)                                  #Grafica el tamaño del arbol y entre que valores se encuentra el error en cada nodo
plot(COMPORTAMIENTO2,col='red',lty=1,main='Arbol vs Arbol con ProbaPriori') #Se grafican los arboles tanto el normal como el que tiene las probabilidades apriori
plot(COMPORTAMIENTO3, col='green',add=TRUE,lty=2)

### RED BAYESIANA:
### La red bayesiana nos brinda una tecnica que permite ver las relaciones mas importantes entre
### las variables.

library(deal)
RB<-TRAINDKM                                         #Se hace una copia de los datos
RB$statusB<-as.numeric(TRAINDKM$statusB)             #Se establece la variable de interes como numerica
RB.nw<-network(RB)                                   #Se hace la red sobre los datos RB
RB.prior<-jointprior(RB.nw)                          #
RB.nw <- learn(RB.nw,RB,RB.prior)$nw                 #Se hacen los arcos entre las variables, siempre y cuando la red considere ponderante las variables
result<-heuristic(RB.nw,RB,RB.prior,restart = 1,degree=1,trace=TRUE) # Aqui se hace el procesos de la red, y se muestra finalmente la relacion de las variables mas importantes

### INFERENCIA EN ARBOLES CONDICIONALES:
### Los arboles condicionales son la siguiente generacion de particiones recursivas
### este tipo de arboles ofrecen el concepto de significancia estadistica con base
### en la metrica bonferroni

library(party)
EstimCondici<-ctree(TRAINDKM$statusB~.,data = TRAINDKM) #Usamos la funcion conditional tree para estimar el modelo con respecto a la variable de interes
plot(EstimCondici)                                   #Se lleva acabo el grafico del arbol condicional
ResultDfr<-as.data.frame(do.call("rbind",treeresponse(EstimCondici,newdata=TESTDKM))) #Solo nos arroja el resultado de la estimacion
TESTDKM$SCORE3<-ResultDfr[]                          #Muestra el score como resultado de la estimacion del arbol condicional
PREDICCION4<-prediction(TESTDKM$SCORE3,TESTDKM$statusB) #Se hace la prediccion como en los metodos anteriores
COMPORTAMIENTO4<-performance(PREDICCION4,"tpr","fpr")   #Se lleva acabo el analisis del comportamiento de la prediccion
plot(COMPORTAMIENTO2,col='red',lty=1,main='Arbol vs ArbolPrior vs ArbolCondic')
plot(COMPORTAMIENTO3, col='green',add=TRUE,lty=2)
plot(COMPORTAMIENTO4, col='blue',add=TRUE,lty=3)     #Se hace el plot de los tres metodos que se han hecho, y la curva ROC que los compara

### NAIVE BAYES
### Un clasificador de Bayes ingenuo asume que la presencia o ausencia de una caracteristica particular
### no esta relacionada con la presencia o ausencia de cualquier otra caracterisitica dada la clase variable

install.packages("e1071")                            #Se instala el paquete para trabajar con Naive Bayes
library(e1071)                                       #Se carga la libreria
NBMODEL<-naiveBayes(TRAINDKM$statusB~.,data=TRAINDKM)#Hacemos la estimacion por medio de la funcion Naive Bayes
NBMODEL                                              #Se obtienen los resultados


