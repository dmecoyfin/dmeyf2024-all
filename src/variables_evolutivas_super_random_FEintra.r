require( "data.table" )

#Especificar carpeta donde guarda el dataset los canarios asesinos.
#Si va a ser la primer iteracion, especificar carpeta del dataset original
setwd("~/buckets/b1/expw/FEintra-0001") # Establezco el Working Directory

dataset <- fread("dataset.csv.gz")

# Crear el vector con los nombres de las columnas excluidas
variables_a_excluir <- c("numero_de_cliente", "foto_mes", "clase_ternaria")

# Crear el vector con los nombres de las columnas restantes
variables <- setdiff(names(dataset), variables_a_excluir)
# Inicializa nuevas_variables como un data.table vacío
nuevas_variables_random <- data.table(nombre = character(), explicacion = character())

for (k in 1:10){
  l=1
  for (m in 1:10){
    set.seed(m)
    variables_1 <-  sample(c(1:length(variables)), min(2, length(variables)))
    variables_2 <-  sample(c(1:length(variables)), min(2, length(variables)))
    variables_3 <-  sample(c(1:length(variables)), min(2, length(variables)))
    variables_4 <-  sample(c(1:length(variables)), min(2, length(variables)))
    variables_5 <-  sample(c(1:length(variables)), min(2, length(variables)))
    variables_6 <-  sample(c(1:length(variables)), min(2, length(variables)))

#AQUI COMIENZO A CREAR NUEVAS VARIABLES-----------------------------------------

# Bucle para crear nuevas variables y agregar al dataset
    for (i in variables_1) {
      for (j in variables_2) {
    # Multiplica las columnas
        var1 <- variables[i]
        var2 <- variables[j]
        explicaciones <- c( paste0(var1, "_x_", var2),
                            paste0(var2, "_x_", var1))
        if (length(intersect(explicaciones, nuevas_variables_random$explicacion)) != 0) 
          {l=l} 
        else {nueva_variable <- dataset[[var1]] * dataset[[var2]]
        # Crea el nombre de la nueva columna
        colname <- paste0("iter_",k,"_var_",l)
        l=l+1
        # Agrega la nueva variable al dataset original
        dataset[, (colname) := nueva_variable]
        # Agrega al diccionario nuevas_variables
        nuevas_variables_random <- rbind(nuevas_variables_random, 
                                         data.table(nombre = colname,
                                                    explicacion = paste0(var1, "_x_", var2)))}
      }
      }

    for (i in variables_3) {
      for (j in variables_4) {
    # Multiplica las columnas
        var1 <- variables[i]
        var2 <- variables[j]
        explicaciones <- c( paste0(var1, "_/_", var2))
        if ( (length(intersect(explicaciones, nuevas_variables_random$explicacion)) != 0) |
             (var1 == "mpayroll" &  var2== "cliente_edad") |
             (var1 == "Master_mlimitecompra" &  var2== "vm_mlimitecompra") |
             (var1 == "Visa_mlimitecompra" &  var2== "vm_mlimitecompra") |
             (var1 == "vm_msaldototal" &  var2== "vm_mlimitecompra") |
             (var1 == "vm_msaldopesos" &  var2== "vm_mlimitecompra") |
             (var1 == "vm_msaldopesos" &  var2== "vm_msaldototal") |
             (var1 == "vm_msaldodolares" &  var2== "vm_mlimitecompra") |
             (var1 == "vm_msaldodolares" &  var2== "vm_msaldototal") |
             (var1 == "vm_mconsumospesos" &  var2== "vm_mlimitecompra") |
             (var1 == "vm_mconsumosdolares" &  var2== "vm_mlimitecompra") |
             (var1 == "vm_madelantopesos" &  var2== "vm_mlimitecompra") |
             (var1 == "vm_madelantodolares" &  var2== "vm_mlimitecompra") |
             (var1 == "vm_mpagado" &  var2== "vm_mlimitecompra") |
             (var1 == "vm_mpagospesos" &  var2== "vm_mlimitecompra") |
             (var1 == "vm_mpagosdolares" &  var2== "vm_mlimitecompra") |
             (var1 == "vm_mconsumototal" &  var2== "vm_mlimitecompra") |
             (var1 == "vm_mpagominimo" &  var2== "vm_mlimitecompra")
        )
          {l=l} 
        else {nueva_variable <- dataset[[var1]] / dataset[[var2]]
        # Crea el nombre de la nueva columna
        colname <- paste0("iter_",k,"_var_",l)
        l=l+1
        # Agrega la nueva variable al dataset original
        dataset[, (colname) := nueva_variable]
        # Agrega al diccionario nuevas_variables
        nuevas_variables_random <- rbind(nuevas_variables_random, 
                                         data.table(nombre = colname,
                                                    explicacion = paste0(var1, "_/_", var2)))}
      }
      }

    for (i in variables_5) {
      for (j in variables_6) {
    # Multiplica las columnas
        var1 <- variables[i]
        var2 <- variables[j]
        explicaciones <- c( paste0(var1, "_+_", var2),
                            paste0(var2, "_+_", var1))
        if ( (length(intersect(explicaciones, nuevas_variables_random$explicacion)) != 0) |
             ( (var1 == "Master_status" & var2 == "Visa_status") | 
               (var1 == "Visa_status" & var2 == "Master_status" ) ) |
             ( (var1 == "Master_mfinanciacion_limite" & var2 == "Visa_mfinanciacion_limite") | 
               (var1 == "Visa_mfinanciacion_limite" & var2 == "Master_mfinanciacion_limite" ) ) |
             ( (var1 == "Master_msaldototal" & var2 == "Visa_msaldototal") | 
               (var1 == "Visa_msaldototal" & var2 == "Master_msaldototal" ) ) |
             ( (var1 == "Master_msaldopesos" & var2 == "Visa_msaldopesos") | 
               (var1 == "Visa_msaldopesos" & var2 == "Master_msaldopesos" ) ) |
             ( (var1 == "Master_msaldodolares" & var2 == "Visa_msaldodolares") | 
               (var1 == "Visa_msaldodolares" & var2 == "Master_msaldodolares" ) ) |
             ( (var1 == "Master_mconsumospesos" & var2 == "Visa_mconsumospesos") | 
               (var1 == "Visa_mconsumospesos" & var2 == "Master_mconsumospesos" ) ) |
             ( (var1 == "Master_mconsumosdolares" & var2 == "Visa_mconsumosdolares") | 
               (var1 == "Visa_mconsumosdolares" & var2 == "Master_mconsumosdolares" ) ) |
             ( (var1 == "Master_mlimitecompra" & var2 == "Visa_mlimitecompra") | 
               (var1 == "Visa_mlimitecompra" & var2 == "Master_mlimitecompra" ) ) |
             ( (var1 == "Master_madelantopesos" & var2 == "Visa_madelantopesos") | 
               (var1 == "Visa_madelantopesos" & var2 == "Master_madelantopesos" ) ) |
             ( (var1 == "Master_madelantodolares" & var2 == "Visa_madelantodolares") | 
               (var1 == "Visa_madelantodolares" & var2 == "Master_madelantodolares" ) ) |
             ( (var1 == "Master_mpagado" & var2 == "Visa_mpagado") | 
               (var1 == "Visa_mpagado" & var2 == "Master_mpagado" ) ) |
             ( (var1 == "Master_mpagospesos" & var2 == "Visa_mpagospesos") | 
               (var1 == "Visa_mpagospesos" & var2 == "Master_mpagospesos" ) ) |
             ( (var1 == "Master_mpagosdolares" & var2 == "Visa_mpagosdolares") | 
               (var1 == "Visa_mpagosdolares" & var2 == "Master_mpagosdolares" ) ) |
             ( (var1 == "Master_mconsumototal" & var2 == "Visa_mconsumototal") | 
               (var1 == "Visa_mconsumototal" & var2 == "Master_mconsumototal" ) ) |
             ( (var1 == "Master_cconsumos" & var2 == "Visa_cconsumos") | 
               (var1 == "Visa_cconsumos" & var2 == "Master_cconsumos" ) ) |
             ( (var1 == "Master_cadelantosefectivo" & var2 == "Visa_cadelantosefectivo") | 
               (var1 == "Visa_cadelantosefectivo" & var2 == "Master_cadelantosefectivo" ) ) |
             ( (var1 == "Master_mpagominimo" & var2 == "Visa_mpagominimo") | 
               (var1 == "Visa_mpagominimo" & var2 == "Master_mpagominimo" ) ) )
        {l=l}
          else {nueva_variable <- dataset[[var1]] + dataset[[var2]]
        # Crea el nombre de la nueva columna
        colname <- paste0("iter_",k,"_var_",l)
        l=l+1
        # Agrega la nueva variable al dataset original
        dataset[, (colname) := nueva_variable]
        # Agrega al diccionario nuevas_variables
        nuevas_variables_random <- rbind(nuevas_variables_random, 
                                         data.table(nombre = colname,
                                                    explicacion = paste0(var1, "_+_", var2)))
        }
      }
    }
  }
    variables <- setdiff(names(dataset), variables_a_excluir)
}

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



unicos <- unique(nuevas_variables_random$explicacion)



setwd("~/buckets/b1/datasets")

# grabo las variables
cat( "escritura de variables nuevas\n")
cat( "Iniciando grabado de variables nuevas\n" )
# Crea el nombre del archivo usando la iteración k
nombre_archivo <- paste0("nuevas_variables_super_random_FEintra.txt")

# Guarda el archivo
fwrite(nuevas_variables_random, file = nombre_archivo, logical01 = TRUE, sep = ",")
cat( "Finalizado grabado de nuevas variables\n" )

# grabo el dataset
cat( "escritura del dataset nuevo\n")
cat( "Iniciando grabado del dataset nuevo\n" )
nombre_dataset <- paste0("dataset_super_random_FEintra.csv.gz")

# Guarda el archivo
fwrite(dataset, file = nombre_dataset, logical01 = TRUE, sep = ",")
cat( "Finalizado grabado del dataset nuevo\n" )
