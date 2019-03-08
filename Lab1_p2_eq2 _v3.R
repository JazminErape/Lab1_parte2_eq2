#Laboratorio 1 parte 2 equipo 2

# Remover todos los objetos del "Environment"
rm(list = ls())

# los 0s aceptados antes de expresas una cifra en notaci?n cient?fica
options("scipen"=100, "digits"=4)
#seccion 1 ----
###############################################################################################
################### Cargar librerias a utilizar ###############################################
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(Quandl)) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teor?a Moderna de Portafolios
suppressMessages(library(ROI)) # Optimizacion para portafolio
suppressMessages(library(knitr))  # Opciones de documentaci?n + c?digo
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html") 



###############################################################################################
################# Función para leer los 12 archivos ###########################################

funcion_pesos <- function(i){
  
  #i <- 1
  f <- paste("~/Trading/lab1/IYW_holdings_",i,".csv", sep="")
  datos_iak <- read.delim2(f)
  
  primero <- which(x = datos_iak[,1] == "Ticker")
  ultimo <- length(datos_iak[,1])
  
  tk <- datos_iak[(primero+1):ultimo,1]
  pesos <- datos_iak[(primero+1):ultimo,4]
  #pesos1 <- as.numeric(as.character(pesos))
  fecha <- datos_iak[2,2]
  precios <- datos_iak[(primero+1):ultimo,5]
  #precios1 <- as.numeric(as.character(precios))

  
  resultado <- data.frame(tk,pesos,precios) 
  
  return(resultado)
  
}
base_datos <- list()

for (i in 1:12){
  base_datos[[i]] <- funcion_pesos(i)
} 


#as.numeric(as.character(maquina))
#View(base_datos[[1]][,2])


#f <- paste("~/Trading/lab1/IYW_holdings_",8,".csv", sep="")
#datos_iak <- read.delim2(f)

###############################################################################################
######################### Obtención de rendimientos ###########################################

# Cargar el token de QUANDL
Quandl.api_key("1YRXno4c2f3x5e96L3Dx")
Capital_Inicial <- 10000

# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Funcion para descargar N cantidad de activos desde QUANDL
  # -- Dependencias: QUANDL
  # -- Columns : columnas a incluir : character : c("date", "adj_close", ... )
  # -- Tickers : Tickers o claves de pizarra de los activos : character : "TSLA"
  # -- Fecha_In : Fecha Inicial : character : "2017-01-02"
  # -- Fecha_Fn : Fecha Final : character : "2017-08-02"
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  
  
  return(Datos)
}

# Tickers de accciones y datos a solicitar a QUANDL
cs <- c("date", "adj_close")
# Fecha inicial y fecha final
fechas <- c("2017-03-31", "2017-04-28", "2017-05-31", "2017-06-30", 
            "2017-07-31", "2017-08-31","2017-09-29","2017-10-31","2017-11-30","2017-12-29",
            "2018-01-31","2018-02-28","2018-03-29")

#--------------------------------------------------------------#
#--------------------------------------------------------------#
#--------------------------------------------------------------#
# Descargar Precios y Calcular rendimientos ARCHIVO 1
Datoss <- list()

#for(i in 1:length(base_datos)){ # contador para archivos o fechas
for(j in 1:length(base_datos[[1]][,1])){ # contador para tickers
  Datoss[[j]] <- Bajar_Precios(Columns=cs, Ticker=as.character(base_datos[[1]][j,1]), Fecha_In=fechas[1], Fecha_Fn=fechas[1])
}
tk <- base_datos[[1]][[1]]
longitudes <- c()
for(j in 1:length(Datoss)) { #segunda lista de archivos
  longitudes[j] <- length(Datoss[[j]][,1]) #tamaño de fechas o cierres de precio
}
maximo <-- (max(longitudes))*-1
completos <- which(longitudes == maximo)
DatosN <- Datoss[completos]
df_Datos <- do.call(cbind, DatosN) #Data]
#df_Datos_REV <- df_Datos[order(df_Datos$date), ]
####rendimientos primer archivo (finmarzo fin abril)
#rend1<-list()
#for (g in seq(2, 238, by = 2)){
 # print(g)
   # rend1[g]<- ((df_Datos_REV[[g]][1])/(df_Datos_REV[[g]][20]))-1
    #df_rend1 <- do.call(cbind, rend1)
#}
#--------------------------------------------------------------#
#--------------------------------------------------------------#
#--------------------------------------------------------------#

# Descargar Precios y Calcular rendimientos ARCHIVO 12
Datoss12 <- list()

#for(i in 1:length(base_datos)){ # contador para archivos o fechas
for(j in 1:length(base_datos[[1]][,1])){ # contador para tickers
  Datoss12[[j]] <- Bajar_Precios(Columns=cs, Ticker=as.character(base_datos[[1]][j,1]), Fecha_In=fechas[12], Fecha_Fn=fechas[12])
}
#tk12 <- base_datos[[12]][[1]] los tk son los mismos de mar17
longitudes12 <- c()
for(j in 1:length(Datoss12)) { #segunda lista de archivos
  longitudes12[j] <- length(Datoss12[[j]][,1]) #tamaño de fechas o cierres de precio
}
maximo12 <-- (max(longitudes12))*-1
completos12 <- which(longitudes12 == maximo12)
DatosN12 <- Datoss12[completos12]
df_Datos12 <- do.call(cbind, DatosN12) #Data]
#df_Datos_REV12 <- df_Datos12[order(df_Datos12$date), ]
################ RENDIMIENTO SERIE 1 #############################
rend_serie1<-list()
for (g in seq(2, 230, by = 2)){
  print(g)
  rend_serie1[g]<- ((df_Datos12[[g]][1])/(df_Datos[[g]][1]))-1
  df_rend_serie1 <- do.call(cbind, rend_serie1)
}
rend_serie1f <- sum(df_rend_serie1)
#--------------------------------------------------------------#
#--------------------------------------------------------------#
#--------------------------------------------------------------#
# Descargar Precios y Calcular rendimientos ARCHIVO 2
Datoss2 <- list()

#for(i in 1:length(base_datos)){ # contador para archivos o fechas
for(m in 1:length(base_datos[[1]][,1])){ # contador para tickers
  Datoss2[[m]] <- Bajar_Precios(Columns=cs, Ticker=as.character(base_datos[[2]][m,1]), Fecha_In=fechas[2], Fecha_Fn=fechas[2])
}
tk <- base_datos[[2]][[1]]
longitudes2 <- c()
for(j in 1:length(Datoss2)) { #segunda lista de archivos
  longitudes2[j] <- length(Datoss2[[j]][,1]) #tamaño de fechas o cierres de precio
}
maximo2 <-- (max(longitudes2))*-1
completos2 <- which(longitudes2 == maximo)
DatosN2 <- Datoss2[completos2]
df_Datos2 <- do.call(cbind, DatosN2) #Data]
#df_Datos_REV <- df_Datos[order(df_Datos$date), ]
####rendimientos primer archivo (finmarzo fin abril)
#rend1<-list()
#for (g in seq(2, 238, by = 2)){
# print(g)
# rend1[g]<- ((df_Datos_REV[[g]][1])/(df_Datos_REV[[g]][20]))-1
#df_rend1 <- do.call(cbind, rend1)
#}















########################### IGNORAR ######################################################
########################### IGNORAR ######################################################
# Descargar Precios y Calcular rendimientos SERIE 1
#Datos viejos
Datoss <- list()

  for(j in 1:length(base_datos[[1]][,1])){ # contador para tickers
    Datoss[[j]] <- Bajar_Precios(Columns=cs, Ticker=as.character(base_datos[[1]][j,1]), Fecha_In=fechas[1], Fecha_Fn=fechas[13])
  }
# datos nuevos
Datoss1 <- list()
for(i in 1:length(base_datos[[1]][,1])){ # contador para tickers
  
  Datoss1[[i]] <- Bajar_Precios(Columns=cs, Ticker=as.character(base_datos[[1]][i,1]), Fecha_In=fechas[12], Fecha_Fn=fechas[13])
}

  #################################################################
  ######Cambiar el orden de los datos que previamente descargamos
  
  longitudes <- c()

  #for(i in 1: length(Datoss[[1]])){ #primer lista de archivos
    #i<-1
    #h<-1
    #j<-1
    #(Datoss[[i]][[j]])) {
    for(j in 1:length(Datoss)) { #segunda lista de archivos
    longitudes[j] <- length(Datoss[[j]][,1]) #tamaño de fechas o cierres de precio
    }
  #}
  maximo <-- (max(longitudes))*-1
  completos <- which(longitudes == maximo)

  DatosN <- Datoss[completos]
  #for (n in 1:length(DatosN)) {#ordenar de viejo a mayor
  #DatosN_rev <- rev(DatosN[[n]][[1]])
  df_Datos <- do.call(cbind, DatosN) #Data
  df_Datos_REV <- df_Datos[order(df_Datos$date), ]
  #borrar <- c(df_Datos[3],df_Datos[5])
  #df_Datos[ , !(names(df_Datos) %in% borrar)]
  #df_Datos <- do.call(cbind, DatosN[2]) acomoda por fecha pero solo de una lista no n listas
  #}
#}

#names(Datoss) <- base_datos

########################################Redimientos#####################################
rendimientos <- list()
for (g in seq(2, 218, by = 2)){
  print(g)
for(h in 1:length(df_Datos_REV[1][,1])){
  print(h)
  rendimientos[g][h] <- ((df_Datos_REV[[g]][h+1])/(df_Datos_REV[[g]][h]))-1
}
}
  ####################################################
  # Descargar Precios y Calcular rendimientos SERIE 2
  Datoss2 <- list()
  for(j in 1:length(base_datos[[2]][,1])){ # contador para tickers
    Datoss2[[j]] <- Bajar_Precios(Columns=cs, Ticker=as.character(base_datos[[2]][j,1]), Fecha_In=fechas[2], Fecha_Fn=fechas[13])
  }
  #################################################################
  ######Cambiar el orden de los datos que previamente descargamos
  longitudes2 <- c()
  for(j in 1:length(Datoss2)) { #segunda lista de archivos
  longitudes2[j] <- length(Datoss2[[j]][,1]) #tamaño de fechas o cierres de precio
  }
  maximo2 <-- (max(longitudes2))*-1
  completos2 <- which(longitudes2 == maximo2)

  DatosN2 <- Datoss2[completos2]
  df_Datos2 <- do.call(cbind, DatosN2) #Data
  df_Datos_REV2 <- df_Datos2[order(df_Datos2$date), ]
  
  ########################################Redimientos#####################################
  rendimientos2 <- list()
  for (g in seq(2, 218, by = 2)){
    print(g)
    for(h in 1:length(df_Datos_REV2[1][,1])){
      print(h)
      rendimientos2[g][h] <- ((df_Datos_REV2[[g]][h+1])/(df_Datos_REV2[[g]][h]))-1
    }
  }
  
  ####################################################
  # Descargar Precios y Calcular rendimientos SERIE 3
  Datoss3 <- list()
  for(j in 1:length(base_datos[[3]][,1])){ # contador para tickers
    Datoss3[[j]] <- Bajar_Precios(Columns=cs, Ticker=as.character(base_datos[[3]][j,1]), Fecha_In=fechas[3], Fecha_Fn=fechas[13])
  }
  #################################################################
  ######Cambiar el orden de los datos que previamente descargamos
  longitudes3 <- c()
  for(j in 1:length(Datoss3)) { #segunda lista de archivos
    longitudes3[j] <- length(Datoss3[[j]][,1]) #tamaño de fechas o cierres de precio
  }
  maximo3 <-- (max(longitudes3))*-1
  completos3 <- which(longitudes3 == maximo3)
  
  DatosN3 <- Datoss3[completos3]
  df_Datos3 <- do.call(cbind, DatosN3) #Data
  df_Datos_REV3 <- df_Datos3[order(df_Datos3$date), ]
  
  ########################################Redimientos#####################################
  rendimientos3 <- list()
  for (g in seq(2, 218, by = 2)){
    print(g)
    for(h in 1:length(df_Datos_REV3[1][,1])){
      print(h)
      rendimientos3[g][h] <- ((df_Datos_REV3[[g]][h+1])/(df_Datos_REV3[[g]][h]))-1
    }
  }
  
  