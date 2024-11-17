

data_final <- read.csv("variables_traducidas.txt")
impo_1 <- read.table("expw_CN-0015_impo_1.txt", header = TRUE)

impo_1$explicacion_final <- NA

# Iterar sobre cada valor de var1 en data2
for (i in 1:nrow(impo_1)) {
  # Buscar la coincidencia del valor de var1 en el nombre de data1
  match_index <- match(impo_1$Feature[i], data_final$nombre)
  
  # Si hay una coincidencia, asignar el valor correspondiente de explicacion a la nueva variable
  if (!is.na(match_index)) {
    impo_1$explicacion_final[i] <- data_final$explicacion_final[match_index]
  }
}


write.table(impo_1, "impo_1_variables_explicadas.txt", sep = ",", row.names = FALSE, col.names = TRUE)
