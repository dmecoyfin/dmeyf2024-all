# ideas para un clustering derivado del Machnie Learning
# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("ggplot2")
require("RColorBrewer")
require("ggallin")

require("randomForest")
require("ranger")

PARAM <- list()
PARAM$experimento <- "clu-randomforest"
PARAM$semilla_primigenia <- 500041   # aqui va SU semilla
PARAM$dataset <- "~/Maestria/DMEyF/datasets/competencia_01.csv"


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/Maestria/DMEyF")

# leo el dataset
dataset <- fread(PARAM$dataset) ###leer el dataframe


# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings= FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


# campos arbitrarios, solo como ejemplo
# usted DEBE MANDARIAMENTE agregar más campos aqui
# no permita que la pereza se apodere de su alma
campos_cluster <- c("mrentabilidad",
                    "mrentabilidad_annual",
                    "mcomisiones",
                    "mactivos_margen",
                    "mpasivos_margen",
                    "mcuenta_corriente_adicional",
                    "mcaja_ahorro",
                    "mcaja_ahorro_adicional",
                    "mcaja_ahorro_dolares",
                    "mcuentas_saldo",
                    "ctarjeta_debito_transacciones",
                    "mautoservicio",
                    "ctarjeta_visa",
                    "ctarjeta_visa_transacciones",
                    "mtarjeta_visa_consumo",
                    "mtarjeta_master_consumo",
                    "mprestamos_prendarios",
                    "mprestamos_hipotecarios",
                    "mplazo_fijo_dolares",
                    "mplazo_fijo_pesos",
                    "minversion1_pesos",
                    "minversion2",
                    "mpayroll",
                    "mcuenta_debitos_automaticos",
                    "mttarjeta_visa_debitos_automaticos",
                    "mttarjeta_master_debitos_automaticos",
                    "mpagodeservicios",
                    "mpagomiscuentas",
                    "mtarjeta_visa_descuentos",
                    "mcomisiones_mantenimiento",
                    "mforex_sell",
                    "mtransferencias_recibidas",
                    "mtransferencias_emitidas",
                    "mcheques_depositados",
                    "mcheques_depositados_rechazados",
                    "mcheques_emitidos_rechazados",
                    "chomebanking_transacciones",
                    "matm_other",
                    "ctrx_quarter",
                    "Master_msaldototal",
                    "Master_msaldopesos",
                    "Master_msaldodolares",
                    "Master_mconsumospesos",
                    "Master_madelantopesos",
                    "Master_mpagominimo",
                    "Visa_mpagominimo"
)


# genero el dataset chico
dchico <- dataset[
  clase_ternaria=="BAJA+2", 
  c("numero_de_cliente",campos_cluster),
  with=FALSE]

# arreglo los valores NA
dchico  <- na.roughfix( dchico ) ###los valores faltantes se reemplazan por la media
# no hace falta escalar

# invoco a la distancia de Random Forest
 # ahora, a esperar .. con esta libreria de la prehistoria
#  que NO corre en paralelo

set.seed(PARAM$semilla_primigenia)

modelo <- randomForest( 
  x= dchico[, campos_cluster, with=FALSE ],
  y= NULL, ###para hacer un modelo no supervisado
  ntree= 1000, #se puede aumentar a 10000
  proximity= TRUE, ###matriz que tan cerca estan las observaciones de las otras
  oob.prox=  TRUE ) ###las observaciones que no fueron usadas para entrenar los arboles

# genero los clusters jerarquicos
# distancia = 1.0 - proximidad
hclust.rf <- hclust( 
  as.dist ( 1.0 - modelo$proximity), ###para colocar que lo más cercano es 0 y lo mas distante es 1
  method= "ward.D2" )


# imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()


kclusters <- 5  # cantidad de clusters
h <- 20 ##altura inicial
distintos <- 0

while(  h>0  &  !( distintos >=kclusters & distintos <=kclusters ) )
{
  h <- h - 1
  rf.cluster <- cutree( hclust.rf, h)

  dchico[, cluster := paste0("cluster_", rf.cluster) ]

  distintos <- nrow( dchico[, .N, cluster ] )
  cat( distintos, " " )
}


#--------------------------------------

setorder( dchico, cluster, numero_de_cliente )

fwrite(dchico,
       file= "dchico.txt",
       sep= "\t")

#--------------------------------------
# Analisis de resultados del clustering jerarquico
# cantidad de registros por cluster

dcentroides <- dchico[, lapply(.SD, mean, na.rm=TRUE), 
    by= cluster, 
    .SDcols= campos_cluster ]

dcentroides

fwrite(dcentroides,
       file= "centroides.txt",
       sep= "\t" )

#--------------------------------------
# gafico los clusters en forma bivariada

# Solo voy a mostrar un porcentaje de dchico
dchico[, azar := runif(nrow(dchico)) ]
muestra <- 0.1  # me voy a quedar con los menores a este valor

# calculo la cantidad de campos
n <- length(campos_cluster)

# voy a graficar en escala logaritmica
# cuidado con 

pdf("bivariado.pdf")

for( i in 1:(n-1) ){
  for( j in (i+1):n ){

  grafico <- ggplot( dchico[azar< muestra],
      aes_string(x= campos_cluster[i],
                 y= campos_cluster[j],
                 color= "cluster"))  +
      scale_colour_brewer(palette = "Dark2") +
      geom_point(alpha = 0.50) +
      xlab(campos_cluster[i]) +
      # scale_x_continuous(trans = pseudolog10_trans) +
      ylab(campos_cluster[j]) 
      # scale_y_continuous(trans = pseudolog10_trans)

   print( grafico )
  }
}

dev.off()

# -----------------------------------------------------------------------------
# Ahora incorporo la evolucion historica antes de la BAJA

# leo la historia ( desde donde hay,  202101 )
dhistoria <- fread(PARAM$dataset)
thewalkingdead <- dhistoria[ clase_ternaria =="BAJA+2", unique(numero_de_cliente) ]

dwalkingdead <- dhistoria[ numero_de_cliente %in% thewalkingdead ]


# asigno el cluster a los 
dwalkingdead[ dchico,
           on= "numero_de_cliente",
           cluster := i.cluster ]

# asigno cuentra regresiva antes de la BAJA
setorder( dwalkingdead, numero_de_cliente, -foto_mes )

dwalkingdead[, periodo := - rowid(numero_de_cliente)]

# ejemplo
dwalkingdead[numero_de_cliente==249246268, list( numero_de_cliente, foto_mes, periodo ) ]


# grafico la evolucion de cada < cluster, variable >  univariado ------

# todos los campos menos los que no tiene sentido
campos_totales <- setdiff( colnames(dwalkingdead),
  c("numero_de_cliente","foto_mes","clase_ternaria","cluster","periodo") )



# Genero el grafico intervalo confianza 95%
pdf("evol_RandomForest.pdf")

for( campo in campos_totales ) {

  cat( campo, " " )

  grafico <- ggplot( dwalkingdead[periodo >= -6],
    aes_string(x= "periodo",
               y= campo,
               color= "cluster"))  +
    scale_colour_brewer(palette= "Dark2") +
    xlab("periodo") +
    ylab(campo) +
    geom_smooth( method= "loess", level= 0.95,  na.rm= TRUE )

  print( grafico )
}

dev.off()



#--------------------------------------------------------------------
# quito los CEROS  de los graficos

# reemplazo los CEROS  por NA
#  los registros NA no se grafican
dwalkingdead[ dwalkingdead==0, ] <- NA

# Genero el grafico intervalo confianza 95%
pdf("evol_noceros_RandomForest.pdf")

for( campo in campos_totales ) {

  cat( campo, " " )

  grafico <- ggplot( dwalkingdead[periodo >= -6],
    aes_string(x= "periodo",
               y= campo,
               color= "cluster"))  +
    scale_colour_brewer(palette= "Dark2") +
    xlab("periodo") +
    ylab(campo) +
    geom_smooth( method= "loess", level= 0.95,  na.rm= TRUE )

  print( grafico )
}

dev.off()


