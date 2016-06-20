Pkg.add("DataFrames")
Pkg.add("Regression")
Pkg.add("GLM")
using Regression
using GLM
using DataFrames

#######################
#######################
#######################

data=readtable("/Users/arielaragonoliva/Downloads/DKl.csv",header=true)
data1=readtable("/Users/arielaragonoliva/Downloads/DK.csv",header=true)
data1=convert(Array,data1)
data=convert(Array,data)
y=data[:,4]
x=data[:,[2:3,5:end]]
x1=data[:,[2:7]]

#######################
#######################
#######################
### Con esto brevemente se hara la estimacion mediante ProbitLink, y LogitLink, de este ulitmo se muiestran dos metodos
### hay que notar que en julia, estos metods no son totalmente estable, hay incluso otras formas de estimar la regresion
### logistica, en mi respositorio de github en la carpeta de julia se encuentra un algoritmo diferente.

fit(GeneralizedLinearModel,y~x,x1,Binomial(),ProbitLink())
fit(GeneralizedLinearModel,y~x,x1,Binomial(),LogitLink())
glm(y~x,x1,Binomial(),LogitLink())

#######################
#######################
#######################
#######################
#######################
#######################
###Los siguiente nos va a mostrar el indice de las tarjetas que deben seer consideradas como fraude
###Lo cual resulta mas comodo para evitar buscar todo el numero de tarjeta en en campo de card, y asi 
###solo nos muestra la transaccion que debe ser considerada como fraudulenta.

# La funcion StatusCorreo nos brinda la posibilidad de distinguir aquellos creditos que por el status y el correo
# deberian de ser descartados, se tiene que brindar los datos, el status, y el correo, para fines practicos de 
# esta prueba, el estatus puede ser cualquiera que queramos que sea distinto en la seleccion, en este caso, consideramos
# a aquellos que no son paid como malos, y en correo los que no son como CorreoNoApocrifo como malos, o sea los Apocrifos.

function StatusCorreo(data1,status,correo)
  n=size(data1)[1]
  m=zeros(n)
  for i in 1:n
    if data1[:,9][i]!=status    
      m[i]=i 
    else    
      m[i]=0
    end
  end
  m1=zeros(n)
  for i in 1:n
    if data1[:,14][i]!=correo
      m1[i]=i
    else
      m1[i]=0  
    end
  end
  Pseudolista=intersect(m1,m)
  ListaFinal=unique(Pseudolista)[2:end]
  return(ListaFinal)
end

StatusCorreo(data1,"paid","CorreoNoApocrifo") #El resultado de este pequeño algoritmo nos arroja las transacciones fraudulentas
# Recordemos que en este algoritmo se estan considerando asi por el tipo de status y correo.

#######################
#######################
#######################
#######################
#######################
#######################

Se hara por el BIN y por el IP, ya que podemos 

function BinIp(data1,bin,ip)
  n=size(data1)[1]
  m=zeros(n)
  for i in 1:n
    if data1[:,7][i]!=bin    
      m[i]=i 
    else    
      m[i]=0
    end
  end
  m1=zeros(n)
  for i in 1:n
    if data1[:,15][i]!=ip
      m1[i]=i
    else
      m1[i]=0  
    end
  end
  Pseudolista=intersect(m1,m)
  ListaFinal=unique(Pseudolista)[2:end]
  return(ListaFinal)
end

unique(data1[:,7]) #Para saber que bin introducir checamos este vector
unique(data1[:,15])#Para ver que IP de la categoria queremeos checamos este vector
BinIp(data1,"500000 +",2)

#######################
#######################
#######################
#######################
#######################
#######################

function ListaMalos(data1,bin,ip,status,correo)
  n=size(data1)[1]
  m=zeros(n)
  for i in 1:n
    if data1[:,7][i]!=bin    
      m[i]=i 
    else    
      m[i]=0
    end
  end
  m1=zeros(n)
  for i in 1:n
    if data1[:,15][i]!=ip
      m1[i]=i
    else
      m1[i]=0  
    end
  end
  m2=zeros(n)
  for i in 1:n
    if data1[:,9][i]!=status    
      m2[i]=i 
    else    
      m[i]=0
    end
  end
  m3=zeros(n)
  for i in 1:n
    if data1[:,14][i]!=correo
      m3[i]=i
    else
      m1[i]=0  
    end
  end
  Pseudolista=intersect(m1,m,m2,m3)
  ListaFinal=unique(Pseudolista)[2:end]
  return(ListaFinal)
end

unique(data1[:,7])
unique(data1[:,9])
unique(data1[:,15])
unique(data1[:,14])

# Con las consultas anteriores se pueden ver que tipo de seleccion se puede hacer. Ahora cabe recordar que de lo que se ponga
# queremos aquellos que son diferentes de los seleccionados. Por ejemplo en la siguiente funcion, se escojeran los que son 
# diferentes de 5000 +, 8, paid, y correo no apocrifo.

ListaMalos(data1,"500000 +",8,"paid","CorreoNoApocrifo")
