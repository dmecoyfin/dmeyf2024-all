require( "data.table" )

#Especificar carpeta donde guarda el dataset los canarios asesinos.
#Si va a ser la primer iteracion, especificar carpeta del dataset original
setwd("~/buckets/b1/expw/CN-0007")  # Establezco el Working Directory

#Nombre del dataset
dataset <- fread("dataset.csv.gz")

#Nombre de la carpeta donde se guardan la importancia de las variables. suele ser la del ultimo canario asesino
#Si es primer iteración, correr un canario asesino que da el archivo con la importancia
setwd("~/buckets/b1/expw/CN-0007")

#Nombre del archivo con las variables ordendas por importancia.
impo_1 <- fread("impo_1.txt")

variables_importantes <- impo_1[1:20, Feature] #Selecciono las 20 variables mas importantes

#Cambiar de acuerdo a medida que vamos avanzando con la nueva generación de variables
k=1
#AQUI COMIENZO A CREAR NUEVAS VARIABLES-----------------------------------------

# Inicializa nuevas_variables como un data.table vacío
nuevas_variables <- data.table()

# Bucle para crear nuevas variables y agregar al dataset
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

setwd("~/buckets/b1/datasets")

# grabo las variables
cat( "escritura de variables nuevas\n")
cat( "Iniciando grabado de variables nuevas\n" )
# Crea el nombre del archivo usando la iteración k
nombre_archivo <- paste0("nuevas_variables_iter_", k, ".csv")

# Guarda el archivo
fwrite(nuevas_variables, file = nombre_archivo, logical01 = TRUE, sep = ",")
cat( "Finalizado grabado de nuevas variables\n" )

# grabo el dataset
cat( "escritura del dataset nuevo\n")
cat( "Iniciando grabado del dataset nuevo\n" )
nombre_dataset <- paste0("dataset_iter_", k, ".csv.gz")

# Guarda el archivo
fwrite(dataset, file = nombre_dataset, logical01 = TRUE, sep = ",")
cat( "Finalizado grabado del dataset nuevo\n" )

#ACA EMPIEZA CANARIOS----------------------------------------------------------------------------------
require("rlang", quietly=TRUE) 

# workflow que voy a correr
PARAM <- "src/workflows/918_workflow_base_f202108_canaritos.r"

envg <- env()

envg$EXPENV <- list()
envg$EXPENV$repo_dir <- "~/dmeyf2024/"

#------------------------------------------------------------------------------

correr_workflow <- function( wf_scriptname )
{
  dir.create( "~/tmp", showWarnings = FALSE)
  setwd("~/tmp" )

  # creo el script que corre el experimento
  comando <- paste0( 
      "#!/bin/bash\n", 
      "source /home/$USER/.venv/bin/activate\n",
      "nice -n 15 Rscript --vanilla ",
      envg$EXPENV$repo_dir,
      wf_scriptname,
      "   ",
      wf_scriptname,
     "\n",
     "deactivate\n"
    )
  cat( comando, file="run.sh" )

  Sys.chmod( "run.sh", mode = "744", use_umask = TRUE)

  system( "./run.sh" )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# aqui efectivamente llamo al workflow
correr_workflow( PARAM )
