# para correr el Google Cloud

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
PARAM <- list()
PARAM$experimento <- "KA4210_Bayesian_lightgbm_214363"

PARAM$semilla_primigenia <- 214363#607417#


#PARAM$input$dataset <- "./datasets/competencia_01.csv"
PARAM$input$dataset <- "G:/Mi unidad/01-Maestria Ciencia de Datos/DMEyF/TPs/dmeyf-2024/datasets/competencia_01_julia.csv"

PARAM$input$training <- c(202104) # meses donde se entrena el modelo
PARAM$input$future <- c(202106) # meses donde se aplica el modelo


#PARAM$finalmodel$num_iterations <- 613
#PARAM$finalmodel$learning_rate <- 0.01
#PARAM$finalmodel$feature_fraction <- 0.5489793063
#PARAM$finalmodel$min_data_in_leaf <- 1014
#PARAM$finalmodel$num_leaves <- 928

#PARAM$finalmodel$num_iterations <- 1153
#PARAM$finalmodel$learning_rate <- 0.01030
#PARAM$finalmodel$feature_fraction <- 0.85257
#PARAM$finalmodel$min_data_in_leaf <- 1794
#PARAM$finalmodel$num_leaves <- 549
#PARAM$finalmodel$max_bin <- 31

PARAM$finalmodel$num_iterations <- 1390
PARAM$finalmodel$learning_rate <- 0.01461
PARAM$finalmodel$feature_fraction <- 0.870024
PARAM$finalmodel$min_data_in_leaf <- 1576
PARAM$finalmodel$num_leaves <- 105
PARAM$finalmodel$max_bin <- 31
#nuevos
PARAM$finalmodel$envios <- 11761
PARAM$finalmodel$max_depth <- 6
PARAM$finalmodel$min_gain_to_split <- 0.42588
PARAM$finalmodel$lambda_l1 <- 0.024686
PARAM$finalmodel$lambda_l2 <- 0.047802
PARAM$finalmodel$bagging_fraction <- 0.748268
PARAM$finalmodel$bagging_freq <- 1


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
#setwd("~/buckets/b1")
setwd("G:/Mi unidad/01-Maestria Ciencia de Datos/DMEyF/TPs/dmeyf-2024/")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

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
    envios = PARAM$finalmodel$envios,
    max_depth = PARAM$finalmodel$max_depth,
    min_gain_to_split = PARAM$finalmodel$min_gain_to_split,
    lambda_l1 = PARAM$finalmodel$lambda_l1,
    lambda_l2 = PARAM$finalmodel$lambda_l2,
    bagging_fraction = PARAM$finalmodel$bagging_fraction,
    bagging_freq = PARAM$finalmodel$bagging_freq,
    seed = 214363
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

cortes <- seq(9000, 15000, by = 500)
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]

  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_", envios, ".csv"),
    sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
