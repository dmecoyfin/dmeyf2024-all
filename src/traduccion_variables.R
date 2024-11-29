#IMPORTANTE: ESTABLECER EL DIRECTORIO DE TRABAJO DE LA MANERA QUE LES PAREZCA

#Cargar los dataset

# Poner los nombres de los archivos
#archivo1 <- "datasets_nuevas_variables_iter_2.txt"
data_final <- read.csv("variables_traducidas.txt")

# Bucle para traducir y guardar en un solo archivo todas las variables generadas. El k debe corresponder con los archivos donde se fueron guardando las nuevas variables sin traducir.
for (k in 2:20) {
archivo <- paste0("nuevas_variables_iter_", k, ".txt")

#data1 tiene que ser el que ya tiene las variables en funcion de las variables originales y es la iteración anterior a data2
#data2 el que quiero traducir
data <- read.csv(archivo)
data <- data[c(1,2)]

# Inicializar las columnas var1 y var2 con valores NA
data$var1 <- NA
data$var2 <- NA

# Extraigo de la funcion explicativa en data2 las variables que vienen de data1 
# van cada una a una columna diferente, var1 y var2
# Iterar sobre cada elemento de data2$explicacion
for (i in seq_along(data$explicacion)) {
  if (grepl("_x_", data$explicacion[i])) {
    data$var1[i] <- sub("_x_.*", "", data$explicacion[i])
    data$var2[i] <- sub(".*_x_", "", data$explicacion[i])
  } else if (grepl("_/_", data$explicacion[i])) {
    data$var1[i] <- sub("_/_.*", "", data$explicacion[i])
    data$var2[i] <- sub(".*_/_", "", data$explicacion[i])
  } else {
    data$var1[i] <- sub("_\\+_.*", "", data$explicacion[i])
    data$var2[i] <- sub(".*_\\+_", "", data$explicacion[i])
  }
}



# Crear una nueva variable en data2 para guardar en funcion de cuales variables originales esta expesada var1
data$explicacion_var1_en_data_final <- NA

# Iterar sobre cada valor de var1 en data2
for (i in 1:nrow(data)) {
  # Buscar la coincidencia del valor de var1 en el nombre de data1
  match_index <- match(data$var1[i], data_final$nombre)
  
  # Si hay una coincidencia, asignar el valor correspondiente de explicacion a la nueva variable
  if (!is.na(match_index)) {
    data$explicacion_var1_en_data_final[i] <- data_final$explicacion_final[match_index]
  }
}


# Crear una nueva variable en data2 para guardar en funcion de cuales variables originales esta expresada var2
# Crear una nueva variable en data2 para guardar los resultados
data$explicacion_var2_en_data_final <- NA

# Iterar sobre cada valor de var2 en data2
for (i in 1:nrow(data)) {
  # Buscar la coincidencia del valor de var1 en el nombre de data1
  match_index <- match(data$var2[i], data_final$nombre)
  
  # Si hay una coincidencia, asignar el valor correspondiente de explicacion a la nueva variable
  if (!is.na(match_index)) {
    data$explicacion_var2_en_data_final[i] <- data_final$explicacion_final[match_index]
  }
}
# Ahora data2 tendrá la columna explicacion_var1_en_data1 y explicacion_var2_en_data1 con los valores correspondientes de explicacion con las variables originales

# Crea la variable explicacion_final
# Inicializar la columna explicacion_final con valores NA
data$explicacion_final <- NA


# Pone en explicacion_final como se expresa esa variable en función de las variables originales
# Iterar sobre cada elemento de data2$explicacion
for (i in seq_along(data$explicacion)) {
  if (grepl("_x_", data$explicacion[i])) {
    data$explicacion_final[i] <- paste("(",
                                       data$explicacion_var1_en_data_final[i],
                                        ")",
                                        "_x_",
                                        "(",
                                       data$explicacion_var2_en_data_final[i],
                                        ")",
                                        sep = "")
  } else if (grepl("_/_", data$explicacion[i])) {
    data$explicacion_final[i] <- paste("(",
                                       data$explicacion_var1_en_data_final[i],
                                        ")",
                                        "_/_",
                                        "(",
                                       data$explicacion_var2_en_data_final[i],
                                        ")",
                                        sep = "")
  } else {
    data$explicacion_final[i] <- paste("(",
                                       data$explicacion_var1_en_data_final[i],
                                        ")",
                                        "_+_",
                                        "(",
                                       data$explicacion_var2_en_data_final[i],
                                        ")",
                                        sep = "")
  }
}


# Guardar el data frame data2 en un archivo de texto
data_final <- rbind(data_final,data[c(1,2,7)])
write.table(data, file = archivo, sep = ",", row.names = FALSE, col.names = TRUE)
}

write.table(data_final, file = "variables_traducidas.txt", sep = ",", row.names = FALSE, col.names = TRUE)
