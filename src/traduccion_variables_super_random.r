#IMPORTANTE: ESTABLECER EL DIRECTORIO DE TRABAJO DE LA MANERA QUE LES PAREZCA

#Cargar los dataset

#IMPORTANTE: ESTABLECER EL DIRECTORIO DE TRABAJO DE LA MANERA QUE LES PAREZCA

#Cargar los dataset

# Poner los nombres de los archivos
# archivo0: las variables del dataset con el que se generaron las variables
# archivo1: las nuevas variables
archivo0<- "dataset.campos.txt"
archivo1 <- "nuevas_variables_super_random.txt"

#data1 tiene que ser el que ya tiene las variables en funcion de las variables originales y es la iteración anterior a data2
#data2 el que quiero traducir
original <- read.table(archivo0, header = TRUE)
original <- original[c(2)]
original$explicacion <- original$campo
original$explicacion_final <- original$campo
colnames(original)[1] <- "nombre"
data <- read.csv(archivo1)

# Guardar el data frame data2 en un archivo de texto
data$explicacion_final <- data$explicacion
data_final <- rbind(original,data)

# Extraigo de la funcion explicativa en data2 las variables que vienen de data1 
# van cada una a una columna diferente, var1 y var2
# Iterar sobre cada elemento de data2$explicacion
for (i in seq_along(data_final$explicacion ) ) {
  
  if (grepl("iter", data_final$explicacion[i] ) ) {
    
    if ( grepl("_x_", data_final$explicacion[i] ) ) {
      var1 <- sub("_x_.*", "", data_final$explicacion[i])
      var2 <- sub(".*_x_", "", data_final$explicacion[i])
      
      for (j in seq_along(data_final$nombre)) {
        match_index <- match(var1, data_final$nombre)
        
        if (!is.na(match_index)){
          var1_explicacion <- data_final$explicacion_final[match_index]
        }
        }
      for(j in seq_along(data_final$nombre)) {
        match_index <- match(var2, data_final$nombre)
        if(!is.na(match_index)){
          var2_explicacion <- data_final$explicacion_final[match_index]
        }
        }
      data_final$explicacion_final[i] <- paste("(",var1_explicacion,")","_x_",
                                         "(",var2_explicacion,")",sep = "")
      } 
    
    else if (grepl("_/_", data_final$explicacion[i])) {
      var1 <- sub("_/_.*", "", data_final$explicacion[i])
      var2 <- sub(".*_/_", "", data_final$explicacion[i])
      
      for (j in seq_along(data_final$nombre)) {
        match_index <- match(var1, data_final$nombre)
        
        if (!is.na(match_index)){
          var1_explicacion <- data_final$explicacion_final[match_index]
        }
      }
      for(j in seq_along(data_final$nombre)) {
        match_index <- match(var2, data_final$nombre)
        if(!is.na(match_index)){
          var2_explicacion <- data_final$explicacion_final[match_index]
        }
      }
      data_final$explicacion_final[i] <- paste("(",var1_explicacion,")","_/_",
                                         "(",var2_explicacion,")",sep = "")
      } 
    else {
      var1 <- sub("_\\+_.*", "", data_final$explicacion[i])
      var2 <- sub(".*_\\+_", "", data_final$explicacion[i])
      for (j in seq_along(data_final$nombre)) {
        match_index <- match(var1, data_final$nombre)
        
        if (!is.na(match_index)){
          var1_explicacion <- data_final$explicacion_final[match_index]
        }
      }
      for(j in seq_along(data_final$nombre)) {
        match_index <- match(var2, data_final$nombre)
        if(!is.na(match_index)){
          var2_explicacion <- data_final$explicacion_final[match_index]
        }
      }
      data_final$explicacion_final[i] <- paste("(",var1_explicacion,")","_+_",
                                         "(",var2_explicacion,")",sep = "")
    } 
  }
  }
 
# Guardar el data frame en un archivo de texto
write.table(data_final, file = "variables_traducidas.txt", sep = ",", row.names = FALSE, col.names = TRUE)
