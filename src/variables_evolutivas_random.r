require( "data.table" )

#Especificar carpeta donde guarda el dataset los canarios asesinos.
#Si va a ser la primer iteracion, especificar carpeta del dataset original
setwd("~/buckets/b1/expw/CN-0015") # Establezco el Working Directory

#Nombre del dataset
dataset <- fread("dataset.csv.gz")

#Nombre de la carpeta donde se guardan la importancia de las variables. suele ser la del ultimo canario asesino
#Si es primer iteración, correr un canario asesino que da el archivo con la importancia
#setwd("~/buckets/b1/expw/CN-0014")

#Nombre del archivo con las variables ordendas por importancia.
impo_1 <- fread("impo_1.txt")

# obtengo la fila hasta antes del primer canarito
fila_objetivo <- impo_1[grepl("^canarito", Feature), .I[1]]  # Encuentra el índice de la primera ocurrencia
vector_filas <- 1:(fila_objetivo - 1)

variables_1 <-  sample(vector_filas, min(10, length(vector_filas)))
variables_2 <-  sample(vector_filas, min(10, length(vector_filas)))
variables_3 <-  sample(vector_filas, min(10, length(vector_filas)))
variables_4 <-  sample(vector_filas, min(10, length(vector_filas)))
variables_5 <-  sample(vector_filas, min(10, length(vector_filas)))
variables_6 <-  sample(vector_filas, min(10, length(vector_filas)))


variables_importantes <- impo_1[vector_filas, Feature] #Selecciono las 20 variables mas importantes

#ACA HAY QUE EMPEZAR UN MEGA BUCLE
k=1
#AQUI COMIENZO A CREAR NUEVAS VARIABLES-----------------------------------------

# Inicializa nuevas_variables como un data.table vacío
nuevas_variables_random <- data.table()

# Bucle para crear nuevas variables y agregar al dataset
l=1
for (i in variables_1) {
  for (j in variables_2) {
    # Multiplica las columnas
    nueva_variable <- dataset[[variables_importantes[i]]] * dataset[[variables_importantes[j]]]
    
    # Crea el nombre de la nueva columna
    colname <- paste0("iter_",k,"_var_",l)
    l=l+1
#    colname <- paste0(variables_importantes[i], "_x_", variables_importantes[j])
    
    # Agrega la nueva variable al dataset original
    dataset[, (colname) := nueva_variable]
    
    # Agrega al diccionario nuevas_variables
    nuevas_variables_random <- rbind(nuevas_variables_random, data.table(nombre = colname,
                                                           explicacion = paste0(variables_importantes[i], "_x_", variables_importantes[j])))

#ESTAS LINEAS QUIZÁS LAS BORRE    
#    nuevas_variables <- rbind(nuevas_variables, data.table(original_var1 = variables_importantes[i], 
#                                                           original_var2 = variables_importantes[j], 
#                                                           new_variable_name = colname,))
  }
}

for (i in variables_3)) {
  for (j in variables_4) {
    # Multiplica las columnas
    nueva_variable <- dataset[[variables_importantes[i]]] / dataset[[variables_importantes[j]]]
    
    # Crea el nombre de la nueva columna
    colname <- paste0("iter_",k,"_var_",l)
    l=l+1
#    colname <- paste0(variables_importantes[i], "_x_", variables_importantes[j])
    
    # Agrega la nueva variable al dataset original
    dataset[, (colname) := nueva_variable]
    
    # Agrega al diccionario nuevas_variables
    nuevas_variables_random <- rbind(nuevas_variables_random, data.table(nombre = colname,
                                                           explicacion = paste0(variables_importantes[i], "_/_", variables_importantes[j])))

#ESTAS LINEAS QUIZÁS LAS BORRE    
#    nuevas_variables <- rbind(nuevas_variables, data.table(original_var1 = variables_importantes[i], 
#                                                           original_var2 = variables_importantes[j], 
#                                                           new_variable_name = colname,))
  }
}

for (i in variables_5) {
  for (j in variables_6) {
    # Multiplica las columnas
    nueva_variable <- dataset[[variables_importantes[i]]] + dataset[[variables_importantes[j]]]
    
    # Crea el nombre de la nueva columna
    colname <- paste0("iter_",k,"_var_",l)
    l=l+1
#    colname <- paste0(variables_importantes[i], "_+_", variables_importantes[j])
    
    # Agrega la nueva variable al dataset original
    dataset[, (colname) := nueva_variable]
    
    # Agrega al diccionario nuevas_variables
    nuevas_variables_random <- rbind(nuevas_variables_random, data.table(nombre = colname,
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
nombre_archivo <- paste0("nuevas_variables_random_iter_", k, ".txt")

# Guarda el archivo
fwrite(nuevas_variables_random, file = nombre_archivo, logical01 = TRUE, sep = ",")
cat( "Finalizado grabado de nuevas variables\n" )

# grabo el dataset
cat( "escritura del dataset nuevo\n")
cat( "Iniciando grabado del dataset nuevo\n" )
nombre_dataset <- paste0("dataset_random_iter_", k, ".csv.gz")

# Guarda el archivo
fwrite(dataset, file = nombre_dataset, logical01 = TRUE, sep = ",")
cat( "Finalizado grabado del dataset nuevo\n" )
