# cargo las librerias que necesito
require("data.table")

# Aqui se debe poner la carpeta de la materia de SU computadora local

setwd("C:/Users/German/Documents/MaestriaDataMining/DMEyF") # Establezco el Working Directory


# cargo el dataset que tiene la clase calculada !
dataset <- fread("./datasets/competencia_01.csv")

dapply <- dataset[foto_mes == 202106] # defino donde voy a aplicar el modelo

set.seed(111667)  # Fijar una semilla para reproducibilidad
dapply$Predicted <- FALSE  # Crear la columna booleana inicial

# Seleccionar aleatoriamente 10,000 filas
seleccion <- sample(nrow(dapply), 4000)

# Actualizar la columna booleana para las filas seleccionadas
dapply$Predicted[seleccion] <- TRUE

# Crear el nuevo dataset con solo las columnas de interés
azar001 <- dapply[, c("numero_de_cliente", "Predicted")]

dir.create("./exp/azar")

fwrite(dapply[, list(numero_de_cliente, Predicted)],
       file = "./exp/azar/azar002.csv",
       sep = ","
)
