# Para cada tarjeta de credito se deberian de tener datos unicos que comprueben la identidad de alguien
# si hay una tarjeta con dos datos distintos ya sea de telefono, correo, o ip entonces la transaccion es
# una transaccion fraudulenta. Para estos algoritmos cargese la base DK, enviada como csv

#1)   ##################################################################
########################################################################
########################################################################
# Este primer algoritmo brnda una breve seleccion de las tarjetas que son consideradas como fraudulentas
# la funcionalidad del algoritmo es muy simple, ya que este primero ubica a todas las tarjetas que estan
# duplicadas, entonces empieza por ubicar a ip's, si estos ip's estan duplicados para una misma tarjeta 
# entonces la considerara como fraude. Este algoritmo usa tambien las variables phone y email para detectar
# aquellas tarjetas fraudulentas, todo esto si se duplican algunos de los elementos de phone, y email seran
# catalogadas como fraudulentas.

# Para usar el algortimo 1, solo se le debe de pasar los datos, en este caso DK, y el algortimo con base a
# las variables ip, phone y email clasificara las tarjetas que son fraudulentas.


MD=DK
mdkc<-MD$card
mdkcar<-DK$card

ListaDeFraudes<-function(Data){
  m<-Data$card[duplicated(Data$card)]
  n1=length(m)
  for (i in 1:n1){
    if (length(unique(Data[Data$card==m[i],]$ip))>1) Falso[i]<-paste(m[i],"Fraude",sep = " ")
    else 
      if(length(unique(Data[Data$card==m[i],]$phone))>1) Falso[i]<-paste(m[i],"Fraude",sep = " ")
      else
        if(length(unique(Data[Data$card==m[i],]$email))>1) Falso[i]<-paste(m[i],"Fraude",sep = " ")
        else Falso[i]<-"NoFraude"
        }
  NF<-table(Falso)
  n2<-NF["NoFraude"]
  n3<-n1-n2
  for (i in 1:n3){
    if(Falso[i]!="NoFraude") M0[i]<-Falso[i]
    ListaFraudes<-M0[!is.na(M0)]
    }
  return(ListaFraudes)
}
ListaDeFraudes(DK)

#2)   ##################################################################
########################################################################
########################################################################
# Al igual que en el algoritmo anterior, este algoritmo lo que va a hacer es cruzar dos a dos las variables ip, phone e email, esto para 
# determinar las variables que pueden conjuntamente duplicarse.


# El funcionamietno es igual al anterior, uno solo debe de pasarle los datos, el pequeño algoritmo hara el resto, pues ya tiene predeterminado
# fijarse en las variables y empezar a clasificar las tarjetas fraudulentas.

ListaDeFraudes2<-function(Data){
  m<-Data$card[duplicated(Data$card)]
  n1=length(m)
  for (i in 1:n1){
    if (length(unique(Data[Data$card==m[i],]$ip))>1 && length(unique(Data[Data$card==m[i],]$phone))>1) 
      Falso[i]<-paste(m[i],"Fraude",sep = " ")
    else 
      if(length(unique(Data[Data$card==m[i],]$ip))>1 && length(unique(Data[Data$card==m[i],]$email))>1)
        Falso[i]<-paste(m[i],"Fraude",sep = " ")
      else
        if(length(unique(Data[Data$card==m[i],]$email))>1 && length(unique(Data[Data$card==m[i],]$phone))>1)
          Falso[i]<-paste(m[i],"Fraude",sep = " ")
        else Falso[i]<-"NoFraude"
        }
  NF<-table(Falso)
  n2<-NF["NoFraude"]
  n3<-n1-n2
  for (i in 1:n3){
    if(Falso[i]!="NoFraude") M0[i]<-Falso[i]
    ListaFraudes<-M0[!is.na(M0)]
  }
  return(ListaFraudes)
}

ListaDeFraudes2(DK)

#3)   ##################################################################
########################################################################
########################################################################
# En este algoritmo ahora uno puede determinar si permite o no a las transacciones tener varios ip's, phone's o email
# este algoritmo permite que uno eliga si quiere ser mas restrictivo en alguna variable o menos.


# Su funcionamiento consiste en pasar la base de datos, pasar cuantos ip's vamos a permitir, h, cuantos phone's vamos a permitir, k,
# y cuantos email's vamos a permitir, l.

Listas<-function(Data,h,k,l){
  m<-Data$card[duplicated(Data$card)]
  n1=length(m)
  Falsata=vector()
  for (i in 1:n1){
    if (length(unique(Data[Data$card==m[i],]$ip))>h) 
      Falsata[i]<-paste(m[i],"Fraude",sep = " ")
    else
      if(length(unique(Data[Data$card==m[i],]$phone))>k)
        Falsata[i]<-paste(m[i],"Fraude",sep = " ")
      else
        if(length(unique(Data[Data$card==m[i],]$email))>l)
          Falsata[i]<-paste(m[i],"Fraude",sep = " ")
        else Falsata[i]<-paste(m[i],"NoFraude",sep = " ")
  }
  return(unique(Falsata))
}

Listas(DK,2,2,2)

#4)   ##################################################################
########################################################################
########################################################################
# Al igual que en el algoritmo anterior, este algoritmo lo que va a hacer es cruzar dos a dos las variables ip, phone e email, esto para 
# determinar las variables que pueden conjuntamente duplicarse.


# El funcionamietno es igual al anterior, uno solo debe de pasarle los datos y ahora los parametros de interes h,f,k,g,l y t, el pequeño 
# algoritmo hara el resto, pues ya tiene predeterminado fijarse en las variables y empezar a clasificar las tarjetas fraudulentas.


ListaDeFraudesC<-function(Data,h,f,k,g,l,t){
  m<-Data$card[duplicated(Data$card)]
  n1=length(m)
  for (i in 1:n1){
    if (length(unique(Data[Data$card==m[i],]$ip))>h && length(unique(Data[Data$card==m[i],]$phone))>f) 
      Falso[i]<-paste(m[i],"Fraude",sep = " ")
    else 
      if(length(unique(Data[Data$card==m[i],]$ip))>k && length(unique(Data[Data$card==m[i],]$email))>g)
        Falso[i]<-paste(m[i],"Fraude",sep = " ")
      else
        if(length(unique(Data[Data$card==m[i],]$email))>l && length(unique(Data[Data$card==m[i],]$phone))>t)
          Falso[i]<-paste(m[i],"Fraude",sep = " ")
        else Falso[i]<-"NoFraude"
  }
  NF<-table(Falso)
  n2<-NF["NoFraude"]
  n3<-n1-n2
  for (i in 1:n3){
    if(Falso[i]!="NoFraude") M0[i]<-Falso[i]
    ListaFraudes<-M0[!is.na(M0)]
  }
  return(ListaFraudes)
}

ListaDeFraudesC(DK,1,1,1,2,2,2)


#5)   ##################################################################
########################################################################
########################################################################
# Este algoritmo es muy diferente a los anteriores, ya que de entrada no se fija en las tarjetas duplicadas en las transacciones
# mira todas las tarjetas y con base a los variables status, amount y bin empezara a clasificar todas las tarjetas como fraudulentas
# o no fraudulentas, ahora en cada variable hay ciertas categorias, mas abajo aparece el comando para saber que nombre hay
# para escojer, al escojer se debe de tomar en cuenta que se selecciona un numero, por lo que las categorias estan ordenadas asi 
# podemos escoger cualquier categoria y no se esta restringida a las categorias escojidas dentro del algoritmo.

# Su funcionamiento es pasar los datos, decir si sí queremos escojer la variable, y de ser asi que categoria queremos escojer.

Lisfra<-function(Data,variablestatus,f,variableamount,g,variablebin,h){
  m<-Data$card
  n2=length(m)
  Falsatas=vector()
  Falsatas1=vector()
  Falsatas2=vector()
  for (i in 1:n2){   
    
    if (variablestatus=="si")
      if (Data[Data$card==m[i],]$status!=names(table(Data$status))[f])
        Falsatas[i]<-paste(m[i],"Fraude",sep = " ")
    if (variableamount=="si")
      if (Data[Data$card==m[i],]$amount!=names(table(Data$amount))[g])
        Falsatas1[i]<-paste(m[i],"Fraude",sep = " ")
    if (variablebin=="si")
      if (Data[Data$card==m[i],]$bin1!=names(table(Data$bin1))[h])
        Falsatas2[i]<-paste(m[i],"Fraude",sep = " ")

  }
  
  Falsatas3=intersect(Falsatas,Falsatas1)
  Falsatas4=intersect(Falsatas3,Falsatas2)
  n3=length(unique(Falsatas4)) 
  return(unique(Falsatas4)[2:n3])
}

# 
# 
# 

names(table(DK$status))
names(table(DK$amount))
names(table(DK$bin1))

Lisfra(DK,"si",3,"si",1,"si",1)

#6)   ##################################################################
########################################################################
########################################################################
# Este algoritmo, uno de los mas interesantess, de entrada considerara los efectos economicos de la actualidad, ya que si 
# la economia se encuentra mal el sistema llevara a cabo una seleccion mas estricta, que si la economia esta bien. Las variables
# que usa este algoritmo son status, ip, phone, email, y a partir de lo que uno le diga de la economia el sistema hara el resto.


# Funcionamiento, pasar la base de datos y decir si la estabilidad economica es mla o buena, el algoritmo hara el resto.

ListaEfecEco<-function(Data,estabilidadeconomica){
 CardDuplicated=Data$card[duplicated(Data[Data$status=="paid",]$card)]
 c=length(CardDuplicated)
 Flsta=vector()
 table(Data$status)
 m<-Data$card
 n2=length(m)
 Fls=vector()
 if(estabilidadeconomica=="mala")
   for (i in 1:n2){
     if (Data[Data$card==m[i],]$status!="paid")
       Fls[i]<-paste(m[i],"Fraude",sep = " ")
      else
        if (Data[Data$card==m[i],]$status=="paid")
         for (j in 1:c){
           if (length(unique(Data[Data$card==CardDuplicated[j],]$ip))>1) 
             Flsta[j]<-paste(CardDuplicated[j],"Fraude",sep = " ")
           else
             if(length(unique(Data[Data$card==CardDuplicated[j],]$phone))>1)
               Flsta[j]<-paste(CardDuplicated[j],"Fraude",sep = " ")
             else
               if(length(unique(Data[Data$card==CardDuplicated[j],]$email))>1)
                 Flsta[j]<-paste(CardDuplicated[j],"Fraude",sep = " ")
         }
   }
 else
   if(estabilidadeconomica=="buena")
     for(i in 1:n2){
       if ((Data[Data$card==m[i],]$status!="paid")||(Data[Data$card==m[i],]$status!="refunded"||(Data[Data$card==m[i],]$status!="partially_refunded")))
         Fls[i]<-paste(m[i],"Fraude",sep = " ")
     }
 if(estabilidadeconomica=="buena")
   FalCre=Fls
 else
   FalCre=union(Flsta,Fls)
 nm=length(FalCre)
 return(unique(FalCre)[2:nm])
}      

ListaEfecEco(DK,"buena")
