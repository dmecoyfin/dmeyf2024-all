# Este script esta pensado para correr en Google Cloud
#   8 vCPU
# 32 GB memoria RAM, en la medida que no agregue mas campos

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")
require("ulimit")  # para controlar la memoria


# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()

PARAM$experimento <- "PP7230_2024_10_11"

PARAM$input$dataset <- "./datasets/competencia_01.csv"

PARAM$semilla_azar <- 111667 # Aqui poner su  primer  semilla


PARAM$driftingcorreccion <- "ninguno"
PARAM$clase_minoritaria <- c("BAJA+1","BAJA+2")

# los meses en los que vamos a entrenar
#  la magia estara en experimentar exhaustivamente
PARAM$trainingstrategy$testing <- c(202104)
PARAM$trainingstrategy$validation <- c(202103)
PARAM$trainingstrategy$training <- c(202102)


PARAM$trainingstrategy$final_train <- c(202102, 202103, 202104)
PARAM$trainingstrategy$future <- c(202106)

# un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$training_undersampling <- 0.5

# esta aberracion fue creada a pedido de Joaquin Tschopp
#  Publicamente Gustavo Denicolay NO se hace cargo de lo que suceda
#   si se asigna un valor menor a 1.0
PARAM$trainingstrategy$finaltrain_undersampling <- 1.0

#------------------------------------------------------------------------------
# limita el uso de memoria RAM a  Total_hardware - GB_min

action_limitar_memoria <- function( GB_min = 4 ) {

  MemTotal <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE))
  MemTotal <- as.integer( MemTotal/ 1024 - GB_min*1024 )
  if( MemTotal < 0 )  action_abortar( " No hay suficiente RAM para trabajar (min 4GB ) " )
  ulimit::memory_limit( MemTotal )
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Aqui empieza el programa

# Limito la memoria, para que ningun alumno debe sufrir que el R 
#  aborte sin avisar si no hay suficiente memoria
#  la salud mental de los alumnos es el bien mas preciado 
action_limitar_memoria( 4 )


setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)



# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


# Training Strategy  ----------------------------------------------

dataset[, part_future := 0L ]
dataset[ foto_mes %in% PARAM$trainingstrategy$future,
  part_future := 1L]

dataset[, part_validation := 0L ]
dataset[ foto_mes %in% PARAM$trainingstrategy$validation,
  part_validation := 1L]

dataset[, part_testing := 0L ]
dataset[ foto_mes %in% PARAM$trainingstrategy$testing,
  part_testing := 1L]

set.seed(PARAM$semilla_azar, kind = "L'Ecuyer-CMRG")
dataset[, azar := runif(nrow(dataset))]

dataset[, part_training := 0L ]
dataset[ foto_mes %in% PARAM$trainingstrategy$training &
  (azar <= PARAM$trainingstrategy$training_undersampling |
   clase_ternaria %in% PARAM$clase_minoritaria ),
  part_training := 1L
]


dataset[, part_final_train := 0L ]
dataset[ foto_mes %in% PARAM$trainingstrategy$final_train &
  (azar <= PARAM$trainingstrategy$finaltrain_undersampling |
   clase_ternaria %in% PARAM$clase_minoritaria ),
  part_final_train := 1L
]

# elimino el campo azar, ya no lo uso mas
dataset[, azar := NULL ]

# Grabo el dataset
fwrite( dataset,
  file = "dataset.csv.gz",
  sep = "\t"
)

cat("\n\nEl preprocesamiento ha terminado\n")
