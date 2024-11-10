# para correr el Google Cloud

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
PARAM <- list()
PARAM$experimento <- "KA4210"

PARAM$semilla_primigenia <- 113311


PARAM$input$dataset <- "./datasets/competencia_01_R.csv"
PARAM$input$training <- c(202104) # meses donde se entrena el modelo
PARAM$input$future <- c(202106) # meses donde se aplica el modelo


PARAM$finalmodel$num_iterations <- 1526
PARAM$finalmodel$learning_rate <- 0.010045
PARAM$finalmodel$feature_fraction <- 0.7685    
PARAM$finalmodel$min_data_in_leaf <- 3103      
PARAM$finalmodel$num_leaves <- 555             


PARAM$finalmodel$max_bin <- 31

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")


# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
######## campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

# Leer los archivos de importancia desde la carpeta del experimento
######## impo_1 <- fread("./exp/HT4220/impo_1.txt")
######## impo_35 <- fread("./exp/HT4220/impo_35.txt")
######## impo_36 <- fread("./exp/HT4220/impo_36.txt")

# Seleccionar las variables con un 'Gain' significativo (ajusta el umbral si es necesario)
######## importantes <- unique(c(
########   impo_1[Gain > 0.01, Feature],
########   impo_35[Gain > 0.01, Feature],
########   impo_36[Gain > 0.01, Feature]
######## ))

#--------------------------------------
# Los campos que se van a utilizar
# Utilizar solo las variables importantes que están en el dataset,
# excluyendo las columnas 'clase_ternaria' y 'clase01'
######## campos_buenos <- intersect(
########   setdiff(colnames(dataset), c("clase_ternaria", "clase01")),
########   importantes
######## )



# Listar todos los archivos impo_XX.txt en la carpeta del experimento
archivos_impo <- list.files(path = "./exp/HT4220/", pattern = "impo_\\d+\\.txt$", full.names = TRUE)

# Leer y combinar todos los archivos de importancia
lista_impo <- lapply(archivos_impo, fread)

# Unir todas las tablas en una sola
importancia_combinada <- rbindlist(lista_impo)

# Seleccionar las variables con un 'Gain' significativo (ajusta el umbral si es necesario)
importantes <- unique(importancia_combinada[Gain > 0.01, Feature])

#--------------------------------------
# Los campos que se van a utilizar
# Utilizar solo las variables importantes que están en el dataset,
# excluyendo las columnas 'clase_ternaria' y 'clase01'
campos_buenos <- intersect(
  setdiff(colnames(dataset), c("clase_ternaria", "clase01")),
  importantes
)
#--------------------------------------


#--------------------------------------



# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

# genero el modelo
# estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo <- lgb.train(
  data = dtrain,
  param = list(
    objective = "binary",
    max_bin = PARAM$finalmodel$max_bin,
    learning_rate = PARAM$finalmodel$learning_rate,
    num_iterations = PARAM$finalmodel$num_iterations,
    num_leaves = PARAM$finalmodel$num_leaves,
    min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
    feature_fraction = PARAM$finalmodel$feature_fraction,
    seed = PARAM$semilla_primigenia
  )
)

#--------------------------------------
# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "impo.txt"

fwrite(tb_importancia,
       file = archivo_importancia,
       sep = "\t"
)

#--------------------------------------
# grabo a disco el modelo en un formato para seres humanos ... ponele ...

lgb.save(modelo, "modelo.txt" )

#--------------------------------------

# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
)

# genero la tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
tb_entrega[, prob := prediccion]

# grabo las probabilidad del modelo
fwrite(tb_entrega,
       file = "prediccion.txt",
       sep = "\t"
)

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)


# genero archivos con los  "envios" mejores
# suba TODOS los archivos a Kaggle

cortes <- c(12099)
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]
  
  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
         file = paste0(PARAM$experimento, "_", envios, ".csv"),
         sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")