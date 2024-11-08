#!/usr/bin/env Rscript
cat( "ETAPA  z1301_FE_intrames_manual.r  INIT\n")

# Workflow  Feature Engineering intrames manual artesanal

# inputs
#  * dataset
# output  
#   un dataset algo mas grande:
#     misma cantidad de registros
#     nuevos atributos construidos en forma artesanal y con mucho sufrimiento
#     generados en codigo R,  especificos para este dataset y clase

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE)  # garbage collection

require("data.table", quietly=TRUE)
require("yaml", quietly=TRUE)


#cargo la libreria
# args <- c( "~/labo2024ba" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )
#------------------------------------------------------------------------------

atributos_presentes <- function( patributos )
{
  atributos <- unique( patributos )
  comun <- intersect( atributos, colnames(dataset) )

  return(  length( atributos ) == length( comun ) )
}
#------------------------------------------------------------------------------
# Esta es la parte que los alumnos deben desplegar todo su ingenio
# Agregar aqui sus PROPIAS VARIABLES manuales

AgregarVariables_IntraMes <- function(dataset) {
  cat( "inicio AgregarVariables_IntraMes()\n")
  gc(verbose= FALSE)
  # INICIO de la seccion donde se deben hacer cambios con variables nuevas

  nuevas_variables <- data.table()

# Bucle para crear nuevas variables y agregar al dataset
k=1  
l=1
for (i in 1:length(variables_importantes)) {
  for (j in i:length(variables_importantes)) {
    # Multiplica las columnas
    nueva_variable <- dataset[[variables_importantes[i]]] * dataset[[variables_importantes[j]]]
    
    # Crea el nombre de la nueva columna
    colname <- paste0("iter_",k,"_var_",l)
    l=l+1
#    colname <- paste0(variables_importantes[i], "_x_", variables_importantes[j])
    
    # Agrega la nueva variable al dataset original
    dataset[, (colname) := nueva_variable]
    
    # Agrega al diccionario nuevas_variables
    nuevas_variables <- rbind(nuevas_variables, data.table(nombre = colname,
                                                           explicacion = paste0(variables_importantes[i], "_x_", variables_importantes[j])))

#ESTAS LINEAS QUIZÁS LAS BORRE    
#    nuevas_variables <- rbind(nuevas_variables, data.table(original_var1 = variables_importantes[i], 
#                                                           original_var2 = variables_importantes[j], 
#                                                           new_variable_name = colname,))
  }
}

for (i in 1:length(variables_importantes)) {
  for (j in 1:length(variables_importantes)) {
    # Multiplica las columnas
    nueva_variable <- dataset[[variables_importantes[i]]] / dataset[[variables_importantes[j]]]
    
    # Crea el nombre de la nueva columna
    colname <- paste0("iter_",k,"_var_",l)
    l=l+1
#    colname <- paste0(variables_importantes[i], "_x_", variables_importantes[j])
    
    # Agrega la nueva variable al dataset original
    dataset[, (colname) := nueva_variable]
    
    # Agrega al diccionario nuevas_variables
    nuevas_variables <- rbind(nuevas_variables, data.table(nombre = colname,
                                                           explicacion = paste0(variables_importantes[i], "_/_", variables_importantes[j])))

#ESTAS LINEAS QUIZÁS LAS BORRE    
#    nuevas_variables <- rbind(nuevas_variables, data.table(original_var1 = variables_importantes[i], 
#                                                           original_var2 = variables_importantes[j], 
#                                                           new_variable_name = colname,))
  }
}

for (i in 1:length(variables_importantes)) {
  for (j in i:length(variables_importantes)) {
    # Multiplica las columnas
    nueva_variable <- dataset[[variables_importantes[i]]] + dataset[[variables_importantes[j]]]
    
    # Crea el nombre de la nueva columna
    colname <- paste0("iter_",k,"_var_",l)
    l=l+1
#    colname <- paste0(variables_importantes[i], "_+_", variables_importantes[j])
    
    # Agrega la nueva variable al dataset original
    dataset[, (colname) := nueva_variable]
    
    # Agrega al diccionario nuevas_variables
    nuevas_variables <- rbind(nuevas_variables, data.table(nombre = colname,
                                                           explicacion = paste0(variables_importantes[i], "_+_", variables_importantes[j])))

#ESTAS LINEAS QUIZÁS LAS BORRE    
#    nuevas_variables <- rbind(nuevas_variables, data.table(original_var1 = variables_importantes[i], 
#                                                           original_var2 = variables_importantes[j], 
#                                                           new_variable_name = colname,))
  }
}

  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )

  infinitos_qty <- sum(unlist(infinitos))
  if (infinitos_qty > 0) {
    cat(
      "ATENCION, hay", infinitos_qty,
      "valores infinitos en tu dataset. Seran pasados a NA\n"
    )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )

  nans_qty <- sum(unlist(nans))
  if (nans_qty > 0) {
    cat(
      "ATENCION, hay", nans_qty,
      "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
    )

    cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

  cat( "fin AgregarVariables_IntraMes()\n")
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
cat( "ETAPA  z1301_FE_intrames_manual.r  START\n")
action_inicializar() 


# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset

cat( "lectura del dataset\n")
action_verificar_archivo( "~/buckets/b1/expw/CN-0006/dataset.csv.gz" )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread("~/buckets/b1/expw/CN-0006/dataset.csv.gz")
cat( "Finalizada lectura del dataset\n" )

#variables importantes del lightgbm de canaritos
impo_1 <- fread("~/buckets/b1/expw/CN-0006/impo_1.txt)
variables_importantes <- impo_1[1:20, Feature] #Selecciono las 20 variables mas importantes
k = n

GrabarOutput()

# Agrego las variables manuales
cat( "variables intra mest\n")
AgregarVariables_IntraMes(dataset)

#------------------------------------------------------------------------------
# grabo el dataset
cat( "grabado del dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)
cat( "Finalizado grabado del dataset\n" )

# grabo las nuevas variables
cat( "grabado de nuevas variables\n")
cat( "Iniciando grabado de nuevas variables\n" )
nombre_archivo <- paste0("nuevas_variables_iter_", k, ".csv")
fwrite(nuevas_variables,
  file = nombre_archivo,
  logical01 = TRUE,
  sep = ","
)
cat( "Finalizado grabado de las nuevas variables\n" )

# copia la metadata sin modificar
cat( "grabado de metadata\n")
write_yaml( envg$PARAM$dataset_metadata, 
  file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
  file = "dataset.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
cat( "Fin del programa\n")

envg$OUTPUT$dataset$ncol <- ncol(dataset)
envg$OUTPUT$dataset$nrow <- nrow(dataset)
envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("dataset.csv.gz","dataset_metadata.yml", nombre_archivo)) 
cat( "ETAPA  z1301_FE_intrames_manual.r  END\n")
