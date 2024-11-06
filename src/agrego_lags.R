PARAM$input$dataset <- "./datasets/competencia_01.csv"
# Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/ferna/OneDrive/_Maestria/2do_Cuatri/DMEF")
# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)

# Eliminar las columnas cprestamos_personales y mprestamos_personales
dataset[, `:=`(cprestamos_personales = NULL, mprestamos_personales = NULL)]

# Lista de columnas a las que se les aplicará el lag y delta
exclude_cols <- c("clase_ternaria", "clase01", "numero_de_cliente", "foto_mes", "cliente_edad")
atributos <- setdiff(names(dataset), exclude_cols)

# Ordenar por cliente y foto_mes
setorder(dataset, numero_de_cliente, foto_mes)

# Aplicar shift y delta para todas las columnas de atributos en un solo paso
dataset[, paste0("lag1_", atributos) := lapply(.SD, shift, n = 1, type = "lag"), 
        by = numero_de_cliente, .SDcols = atributos]

dataset[, paste0("deltalag1_", atributos) := lapply(.SD, function(x) x - shift(x, n = 1, type = "lag")), 
        by = numero_de_cliente, .SDcols = atributos]

# Mostrar las primeras filas para verificar los cambios
head(dataset)


# Guardar el dataset modificado en el directorio y nombre especificado
fwrite(dataset, "C:/Users/ferna/OneDrive/_Maestria/2do_Cuatri/DMEF/datasets/competencia_01_lag1.csv")