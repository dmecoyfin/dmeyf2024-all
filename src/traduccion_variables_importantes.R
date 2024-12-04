
# Queda fijo: es el archivo de todas las variables nuevas traducidas en función de las
# variables originales
data_final <- read.csv("variables_traducidas.txt")


# Bucle para guardar la traducción de las variables ordenadas por importancia. La k deben corresponder con los experimentos que se quieren traducir.
for (k in 17:20) {
# Es el archivo de las variables ordenadas por importancia que sale de canaritos

  archivo <- paste0("expw_CN-00",k,"_impo_1.txt")
  impo_1 <- read.table(archivo, header = TRUE)

impo_1$explicacion_final <- NA

# La posta: es el que traduce las variables. Las 20 mas importantes de acá son las que van
# a servir para hacer las próxima generación de variables
for (i in 1:nrow(impo_1)) {
  # Buscar la coincidencia del valor de var1 en el nombre de data1
  match_index <- match(impo_1$Feature[i], data_final$nombre)
  
  # Si hay una coincidencia, asignar el valor correspondiente de explicacion a la nueva variable
  if (!is.na(match_index)) {
    impo_1$explicacion_final[i] <- data_final$explicacion_final[match_index]
  }
}

# Guarda en un txt los resultados.
# HAY QUE CAMBIAR iter_n con n=iteración.
archivo_impo <- paste0("traduccion",archivo)
write.table(impo_1, archivo_impo, sep = ",", row.names = FALSE, col.names = TRUE)
}
