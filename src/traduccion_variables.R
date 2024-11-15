#IMPORTANTE: ESTABLECER EL DIRECTORIO DE TRABAJO DE LA MANERA QUE LES PAREZCA

#Cargar los dataset

# Poner los nombres de los archivos
#archivo1 <- "datasets_nuevas_variables_iter_2.txt"
archivo2 <- "datasets_nuevas_variables_iter_3.txt"

#data1 tiene que ser el que ya tiene las variables en funcion de las variables originales y es la iteración anterior a data2
#data2 el que quiero traducir
data1 <- read.csv(archivo1)
data2 <- read.csv(archivo2)
data2 <- data2[c(1,2)]

data_final <- read.csv("variables_traducidas.txt")

# Inicializar las columnas var1 y var2 con valores NA
data2$var1 <- NA
data2$var2 <- NA

# Extraigo de la funcion explicativa en data2 las variables que vienen de data1 
# van cada una a una columna diferente, var1 y var2
# Iterar sobre cada elemento de data2$explicacion
for (i in seq_along(data2$explicacion)) {
  if (grepl("_x_", data2$explicacion[i])) {
    data2$var1[i] <- sub("_x_.*", "", data2$explicacion[i])
    data2$var2[i] <- sub(".*_x_", "", data2$explicacion[i])
  } else if (grepl("_/_", data2$explicacion[i])) {
    data2$var1[i] <- sub("_/_.*", "", data2$explicacion[i])
    data2$var2[i] <- sub(".*_/_", "", data2$explicacion[i])
  } else {
    data2$var1[i] <- sub("_\\+_.*", "", data2$explicacion[i])
    data2$var2[i] <- sub(".*_\\+_", "", data2$explicacion[i])
  }
}



# Crear una nueva variable en data2 para guardar en funcion de cuales variables originales esta expesada var1
data2$explicacion_var1_en_data1 <- NA

# Iterar sobre cada valor de var1 en data2
for (i in 1:nrow(data2)) {
  # Buscar la coincidencia del valor de var1 en el nombre de data1
  match_index <- match(data2$var1[i], data_final$nombre)
  
  # Si hay una coincidencia, asignar el valor correspondiente de explicacion a la nueva variable
  if (!is.na(match_index)) {
    data2$explicacion_var1_en_data1[i] <- data_final$explicacion_final[match_index]
  }
}


# Crear una nueva variable en data2 para guardar en funcion de cuales variables originales esta expresada var2
# Crear una nueva variable en data2 para guardar los resultados
data2$explicacion_var2_en_data1 <- NA

# Iterar sobre cada valor de var2 en data2
for (i in 1:nrow(data2)) {
  # Buscar la coincidencia del valor de var1 en el nombre de data1
  match_index <- match(data2$var2[i], data_final$nombre)
  
  # Si hay una coincidencia, asignar el valor correspondiente de explicacion a la nueva variable
  if (!is.na(match_index)) {
    data2$explicacion_var2_en_data1[i] <- data_final$explicacion_final[match_index]
  }
}
# Ahora data2 tendrá la columna explicacion_var1_en_data1 y explicacion_var2_en_data1 con los valores correspondientes de explicacion con las variables originales

# Crea la variable explicacion_final
# Inicializar la columna explicacion_final con valores NA
data2$explicacion_final <- NA


# Pone en explicacion_final como se expresa esa variable en función de las variables originales
# Iterar sobre cada elemento de data2$explicacion
for (i in seq_along(data2$explicacion)) {
  if (grepl("_x_", data2$explicacion[i])) {
    data2$explicacion_final[i] <- paste("(",
                                        data2$explicacion_var1_en_data1[i],
                                        ")",
                                        "_x_",
                                        "(",
                                        data2$explicacion_var2_en_data1[i],
                                        ")",
                                        sep = "")
  } else if (grepl("_/_", data2$explicacion[i])) {
    data2$explicacion_final[i] <- paste("(",
                                        data2$explicacion_var1_en_data1[i],
                                        ")",
                                        "_/_",
                                        "(",
                                        data2$explicacion_var2_en_data1[i],
                                        ")",
                                        sep = "")
  } else {
    data2$explicacion_final[i] <- paste("(",
                                        data2$explicacion_var1_en_data1[i],
                                        ")",
                                        "_+_",
                                        "(",
                                        data2$explicacion_var2_en_data1[i],
                                        ")",
                                        sep = "")
  }
}


# Guardar el data frame data2 en un archivo de texto
data_final <- rbind(data_final,data2[c(1,2,7)])
write.table(data_final, file = "variables_traducidas.txt", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(data2, file = archivo2, sep = ",", row.names = FALSE, col.names = TRUE)
