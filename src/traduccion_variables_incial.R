#IMPORTANTE: ESTABLECER EL DIRECTORIO DE TRABAJO DE LA MANERA QUE LES PAREZCA

#Cargar los dataset

# Poner los nombres de los archivos
archivo0<- "dataset.campos_antesdeiter1.txt"
archivo1 <- "nuevas_variables_iter_1.txt"
#archivo2 <- "nuevas_variables_iter_1.txt"

#data1 tiene que ser el que ya tiene las variables en funcion de las variables originales y es la iteración anterior a data2
#data2 el que quiero traducir
original <- read.table(archivo0, header = TRUE)
original <- original[c(2)]
original$explicacion <- original$campo
original$explicacion_final <- original$campo
colnames(original)[1] <- "nombre"
data1 <- read.csv(archivo1)
#data2 <- read.csv(archivo2)
#data2 <- data2[c(1,2)]

# Inicializar las columnas var1 y var2 con valores NA
#data1$var1 <- NA
#data1$var2 <- NA

# Extraigo de la funcion explicativa en data2 las variables que vienen de data1 
# van cada una a una columna diferente, var1 y var2
# Iterar sobre cada elemento de data2$explicacion
#for (i in seq_along(data2$explicacion)) {
#  if (grepl("_x_", data2$explicacion[i])) {
#    data2$var1[i] <- sub("_x_.*", "", data2$explicacion[i])
#    data2$var2[i] <- sub(".*_x_", "", data2$explicacion[i])
#  } else if (grepl("_/_", data2$explicacion[i])) {
#    data2$var1[i] <- sub("_/_.*", "", data2$explicacion[i])
#    data2$var2[i] <- sub(".*_/_", "", data2$explicacion[i])
#  } else {
#    data2$var1[i] <- sub("_\\+_.*", "", data2$explicacion[i])
#    data2$var2[i] <- sub(".*_\\+_", "", data2$explicacion[i])
#  }
#}



# Crear una nueva variable en data2 para guardar en funcion de cuales variables originales esta expesada var1
#data2$explicacion_var1_en_data1 <- NA

# Iterar sobre cada valor de var1 en data2
#for (i in 1:nrow(data2)) {
  # Buscar la coincidencia del valor de var1 en el nombre de data1
#  match_index <- match(data2$var1[i], data1$nombre)
  
  # Si hay una coincidencia, asignar el valor correspondiente de explicacion a la nueva variable
#  if (!is.na(match_index)) {
#    data2$explicacion_var1_en_data1[i] <- data1$explicacion[match_index]
#  }
#}


# Crear una nueva variable en data2 para guardar en funcion de cuales variables originales esta expresada var2
# Crear una nueva variable en data2 para guardar los resultados
#data2$explicacion_var2_en_data1 <- NA

# Iterar sobre cada valor de var2 en data2
#for (i in 1:nrow(data2)) {
  # Buscar la coincidencia del valor de var1 en el nombre de data1
#  match_index <- match(data2$var2[i], data1$nombre)
  
  # Si hay una coincidencia, asignar el valor correspondiente de explicacion a la nueva variable
#  if (!is.na(match_index)) {
#    data2$explicacion_var2_en_data1[i] <- data1$explicacion[match_index]
#  }
#}
# Ahora data2 tendrá la columna explicacion_var1_en_data1 y explicacion_var2_en_data1 con los valores correspondientes de explicacion con las variables originales

# Crea la variable explicacion_final
# Inicializar la columna explicacion_final con valores NA
#data2$explicacion_final <- NA


# Pone en explicacion_final como se expresa esa variable en función de las variables originales
# Iterar sobre cada elemento de data2$explicacion
#for (i in seq_along(data2$explicacion)) {
#  if (grepl("_x_", data2$explicacion[i])) {
#    data2$explicacion_final[i] <- paste("(",
#                                        data2$explicacion_var1_en_data1[i],
#                                        ")",
#                                        "_x_",
#                                        "(",
#                                        data2$explicacion_var2_en_data1[i],
#                                        ")",
#                                        sep = "")
#  } else if (grepl("_/_", data2$explicacion[i])) {
#    data2$explicacion_final[i] <- paste("(",
#                                        data2$explicacion_var1_en_data1[i],
#                                        ")",
#                                        "_/_",
#                                        "(",
#                                        data2$explicacion_var2_en_data1[i],
#                                        ")",
#                                        sep = "")
#  } else {
#    data2$explicacion_final[i] <- paste("(",
#                                        data2$explicacion_var1_en_data1[i],
#                                        ")",
#                                        "_+_",
#                                        "(",
#                                        data2$explicacion_var2_en_data1[i],
#                                        ")",
#                                        sep = "")
#  }
#}

# Guardar el data frame data2 en un archivo de texto
data1$explicacion_final <- data1$explicacion
data_final <- rbind(original,data1)
write.table(data_final, file = "variables_traducidas.txt", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(data1, file = archivo1, sep = ",", row.names = FALSE, col.names = TRUE)
