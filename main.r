## ---- eval=FALSE, include=TRUE----------------------------------------------------
## "Protocolo:
## 
## 1. Daniel Felipe Villa Regifo
## 
## 2. Lenguaje: R
## 
## 3. Tema: Cree funciones que manejen Data Frames en R  (realice al menos dos ejercicios que requieran cargar archivos externos *.csv cada uno con al menos 50 filas y tres datos por fila, lea y procese la información del archvo leído, y guarde las respuestas a los ejercicios  en archivos independientes tipo *.txt)
## 
## 4. Fuentes:
##    https://www.generatedata.com"


## ---------------------------------------------------------------------------------
# Importamos la base
data <- read.csv("Base.csv")

# COnvertimos a dataframe:
data <- data.frame(data)


## ---------------------------------------------------------------------------------
rand.impute <- function(x) {
  # missing contiene un vector de valores T/F dependiendo del NA de x
  missing <- is.na(x)
  #n.missing contiene cuantos valores son NA dentro de x
  n.missing <- sum(missing)
  #x.obs son los valores conocidos que tienen dato diferente de NA en x
  x.obs <- x[!missing]
  #por defecto, devolveré lo mismo que había entrado por parámetro
  imputed <- x
  #en los valores que faltaban, los reemplazamos por una muestra
  #de los que si conocemos (MAS)
  imputed[missing] <- sample(x.obs, n.missing, replace = TRUE)
  return (imputed)
}



## ---------------------------------------------------------------------------------
random.impute.data.frame <- function(dataframe, cols){
  # Nombres del dataframe
  names <- names(dataframe)
  #Recorro las columnas de dataframe
  for(col in cols){
    # nuevo nombre para la variable que ya no contendra NA
    name <- paste(names[col], "imputeNA", sep = "_")
    #Aplico la funcion que remplaza aleatoriamente los NA
    dataframe[name] = rand.impute(dataframe[,col])
  }
  #Retorno el data frame
  return(dataframe)
}


## ---------------------------------------------------------------------------------
# las valore vacios los remplazo por Na para porder aplicar la funcion 
data$Tipo_Cel[data$Tipo_Cel == ""] <- NA
#Aplico la funcion
data <- random.impute.data.frame(data, c(3))


## ---------------------------------------------------------------------------------
#exportamos los datos:
write.table(data, file="SinNA.txt", row.names = F)


## ---------------------------------------------------------------------------------
#importamos la base
base2 <- read.csv(file = "base2.csv", header = T, sep = ",")
base2 <- data.frame(base2)

base2$Estado_Civil <- as.factor(base2$Estado_Civil)


## ---------------------------------------------------------------------------------
# intalamos un paquete para reescalar variables
#install.packages("scales")
# llamo la libreria
library(scales)

rescale.many <- function(dataframe, cols){
  #Tomo todos los nombres de las columnas 
  names <- names(dataframe)
  #Recorro el vector que tiene los nombres para aplicar la funcion rescale que esta en el paquete cargado
  #A cada columna numerica
  for(col in cols){
    # Nombre nuevo de la columna que se crea con los valores reescalados
    name <- paste(names[col], "rescalado", sep = "_")
    #Aplico la funcion del paquete
    dataframe[name] <- rescale(dataframe[,col]) 
  }
  # Mensaje de informacion, dice cuantas columnas reescalaron
  cat(paste("Hemos reescalado ", length(cols), " variable(s)"))
  dataframe
}


## ---------------------------------------------------------------------------------
#Aplico la funcion
base2 <- rescale.many(base2, c(2))



## ---------------------------------------------------------------------------------
# Exportamos el archivo
write.table(base2, file="Rescalar.txt", row.names = F)
