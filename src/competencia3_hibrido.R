library("data.table")
library("ggplot2")
library("dplyr")

setwd("/Users/jorgefernandez/Documents/Cienciadedatos/DMenEyF/competencia3")

#modelos <- merge(modelo_1, modelo_2, by="numero_de_cliente")
#modelos <- merge(modelos, modelo_3, by="numero_de_cliente")
#modelos <- merge(modelos, modelo_4, by="numero_de_cliente")
#modelos <- merge(modelos, modelo_5, by="numero_de_cliente")
#modelos <- merge(modelos, modelo_6, by="numero_de_cliente")

# EDA de ganancias de kaggle----

kaggle_modelo_1 <- read.table("expw227_KA-0007_ganancias_log.txt", header = TRUE)[c(1:6),]
kaggle_modelo_2 <- read.table("expw227_KA-0006_ganancias_log.txt", header = TRUE)[c(1:6),]
kaggle_modelo_3 <- read.table("expw227_KA-0008_ganancias_log.txt", header = TRUE)[c(1:6),]
kaggle_modelo_4 <- read.table("expw227_KA-0009_ganancias_log.txt", header = TRUE)[c(1:6),]
kaggle_modelo_5 <- read.table("expw227_KA-0010_ganancias_log.txt", header = TRUE)[c(1:6),]
kaggle_modelo_6 <- read.table("expw227_KA-0011_ganancias_log.txt", header = TRUE)[c(1:6),]
kaggle_modelo_7 <- read.table("expw227_KA-0012_ganancias_log.txt", header = TRUE)[c(1:6),]
kaggle_modelo_8 <- read.table("expw227_KA-0013_ganancias_log.txt", header = TRUE)[c(1:6),]
kaggle_modelo_9 <- read.table("expw227_KA-0014_ganancias_log.txt", header = TRUE)[c(1:6),]
kaggle_modelo_10 <- read.table("expw227_KA-0015_ganancias_log.txt", header = TRUE)[c(1:6),]
kaggle_modelo_11 <- read.table("expw227_KA-0016_ganancias_log.txt", header = TRUE)[c(1:6),]

# Une todos los modelos
kaggle_modelos <- rbind(kaggle_modelo_1, 
                        #kaggle_modelo_2, 
                        kaggle_modelo_3,
                        #kaggle_modelo_4, 
                        #kaggle_modelo_5, 
                        #kaggle_modelo_6,
                        kaggle_modelo_7, 
                        kaggle_modelo_8, 
                        #kaggle_modelo_9,
                        kaggle_modelo_10, 
                        kaggle_modelo_11)
kaggle_modelos$modelos <- c(rep("modelo_1",6), 
                            #rep("modelo_2",6), 
                            rep("modelo_3",6),
                            #rep("modelo_4",6), 
                            #rep("modelo_5",6), 
                            #rep("modelo_6",6),
                            rep("modelo_7",6), 
                            rep("modelo_8",6), 
                            #rep("modelo_9",6),
                            rep("modelo_10",6),
                            rep("modelo_11",6))

# Me quedo con las variables que me importan: corte, ganancia y modelos
kaggle_modelos <- kaggle_modelos[,c(6,7,9)]

# Promedio de las ganancias de los modelos y los agrego a la tabla
promedio_ganancias <- kaggle_modelos %>%
  group_by(corte) %>%
  summarise(ganancia = mean(ganancia, na.rm = TRUE))
promedio_ganancias$modelos <- rep("promedio", 6)

kaggle_modelos <- rbind(kaggle_modelos, promedio_ganancias)

# Gráfico para comprar ganancias y elegir modelo y corte.
ggplot(kaggle_modelos, aes(x=corte, y=ganancia, colour = modelos))+
  geom_line()



# Generacion de archivo para kaggle según hibridación----

# Acá puse todos los modelos, pero tranquilamente se podrían soo agregar los de interés
modelo_1 <- fread("expw227_SC-0007_tb_future_prediccion.txt", header = TRUE)
modelo_2 <- fread("expw227_SC-0006_tb_future_prediccion.txt", header = TRUE)
modelo_3 <- fread("expw227_SC-0008_tb_future_prediccion.txt", header = TRUE)
modelo_4 <- fread("expw227_SC-0009_tb_future_prediccion.txt", header = TRUE)
modelo_5 <- fread("expw227_SC-0010_tb_future_prediccion.txt", header = TRUE)
modelo_6 <- fread("expw227_SC-0011_tb_future_prediccion.txt", header = TRUE)
modelo_7 <- fread("expw227_SC-0012_tb_future_prediccion.txt", header = TRUE)
modelo_8 <- fread("expw227_SC-0013_tb_future_prediccion.txt", header = TRUE)
modelo_9 <- fread("expw227_SC-0014_tb_future_prediccion.txt", header = TRUE)
modelo_10 <- fread("expw227_SC-0015_tb_future_prediccion.txt", header = TRUE)
modelo_11 <- fread("expw227_SC-0016_tb_future_prediccion.txt", header = TRUE)


# Combino los modelos 1,3, 7 8 10 11 para hacer un híbrido
# Cada individuo independiente debería elegir sus mejores modelos
# Todo es muy manual, con mas computer  literacy y tiempo seguro se puede automatizar un poco
modelos_promedio <- merge(modelo_1[,c(1,54)], modelo_3[,c(1,54)], by="numero_de_cliente")
colnames(modelos_promedio) <- c("numero_de_cliente",
                                "modelo_1",
                                "modelo_3")
modelos_promedio <- merge(modelos_promedio, modelo_7[,c(1,54)], by="numero_de_cliente")
colnames(modelos_promedio) <- c("numero_de_cliente",
                                "modelo_1",
                                "modelo_3",
                                "modelo_7")
modelos_promedio <- merge(modelos_promedio, modelo_8[,c(1,79)], by="numero_de_cliente")
colnames(modelos_promedio) <- c("numero_de_cliente",
                                "modelo_1",
                                "modelo_3",
                                "modelo_7",
                                "modelo_8")
modelos_promedio <- merge(modelos_promedio, modelo_10[,c(1,54)], by="numero_de_cliente")
colnames(modelos_promedio) <- c("numero_de_cliente",
                                "modelo_1",
                                "modelo_3",
                                "modelo_7",
                                "modelo_8",
                                "modelo_10")
modelos_promedio <- merge(modelos_promedio, modelo_11[,c(1,54)], by="numero_de_cliente")
colnames(modelos_promedio) <- c("numero_de_cliente",
                                "modelo_1",
                                "modelo_3",
                                "modelo_7",
                                "modelo_8",
                                "modelo_10",
                                "modelo_11")

# Calculas el promedio para cada uno de los cortes
modelos_promedio$promedio <- rowMeans(modelos_promedio[, c("modelo_1",
                                                           "modelo_3",
                                                           "modelo_7", 
                                                           "modelo_8",
                                                           "modelo_10",
                                                           "modelo_11")], 
                                      na.rm = TRUE)

# Ordena las probabilidades según modelos_promedio
setorder(modelos_promedio, -promedio)

# Genera los archivos para subir a Kaggle con cortes (9500:12500)
modelos_promedio$Predicted <- c(rep(1, 9500), rep(0, 165644-9500))
kaggle_hibrido_9500_modelos_1_3_7_8_10_11 <- data.table(numero_de_cliente = modelos_promedio$numero_de_cliente,
                                  Predicted = modelos_promedio$Predicted)
write.csv(kaggle_hibrido_9500_modelos_1_3_7_8_10_11,
          "kaggle_hibrido_9500_modelos_1_3_7_8_10_11.csv", row.names = FALSE)

modelos_promedio$Predicted <- c(rep(1, 10000), rep(0, 165644-10000))
kaggle_hibrido_10000_modelos_1_3_7_8_10_11 <- data.table(numero_de_cliente = modelos_promedio$numero_de_cliente,
                                   Predicted = modelos_promedio$Predicted)
write.csv(kaggle_hibrido_10000_modelos_1_3_7_8_10_11,
          "kaggle_hibrido_10000_modelos_1_3_7_8_10_11.csv", row.names = FALSE)


modelos_promedio$Predicted <- c(rep(1, 10500), rep(0, 165644-10500))
kaggle_hibrido_10500_modelos_1_3_7_8_10_11 <- data.table(numero_de_cliente = modelos_promedio$numero_de_cliente,
                                   Predicted = modelos_promedio$Predicted)
write.csv(kaggle_hibrido_10500_modelos_1_3_7_8_10_11,
          "kaggle_hibrido_10500_modelos_1_3_7_8_10_11.csv", row.names = FALSE)

modelos_promedio$Predicted <- c(rep(1, 11000), rep(0, 165644-11000))
kaggle_hibrido_11000_modelos_1_3_7_8_10_11 <- data.table(numero_de_cliente = modelos_promedio$numero_de_cliente,
                                   Predicted = modelos_promedio$Predicted)
write.csv(kaggle_hibrido_11000_modelos_1_3_7_8_10_11,
          "kaggle_hibrido_11000_modelos_1_3_7_8_10_11.csv", row.names = FALSE)


modelos_promedio$Predicted <- c(rep(1, 11500), rep(0, 165644-11500))
kaggle_hibrido_11500_modelos_1_3_7_8_10_11 <- data.table(numero_de_cliente = modelos_promedio$numero_de_cliente,
                                   Predicted = modelos_promedio$Predicted)
write.csv(kaggle_hibrido_11500_modelos_1_3_7_8_10_11,
          "kaggle_hibrido_11500_modelos_1_3_7_8_10_11.csv", row.names = FALSE)


modelos_promedio$Predicted <- c(rep(1, 12000), rep(0, 165644-12000))
kaggle_hibrido_12000_modelos_1_3_7_8_10_11 <- data.table(numero_de_cliente = modelos_promedio$numero_de_cliente,
                                   Predicted = modelos_promedio$Predicted)
write.csv(kaggle_hibrido_12000_modelos_1_3_7_8_10_11,
          "kaggle_hibrido_12000_modelos_1_3_7_8_10_11.csv", row.names = FALSE)

modelos_promedio$Predicted <- c(rep(1, 12500), rep(0, 165644-12500))
kaggle_hibrido_12500_modelos_1_3_7_8_10_11 <- data.table(numero_de_cliente = modelos_promedio$numero_de_cliente,
                                   Predicted = modelos_promedio$Predicted)
write.csv(kaggle_hibrido_12500_modelos_1_3_7_8_10_11,
          "kaggle_hibrido_12500_modelos_1_3_7_8_10_11.csv", row.names = FALSE)

#Subir los archivos a Kaggle manualmente

# Poner las ganancias de Kaggle del modelo hibrido
ganancia_hibrido <- c(134.946, 133.897, 132.847, 134.596, 135.856, 132.427)
corte <- c(10000, 10500, 11000, 11500, 12000, 12500)
modelito <- rep("hibrido", 6)
kaggle_hibrido <- data.frame("corte" = corte, "ganancia" = ganancia_hibrido, "modelos" = modelito)

kaggle_modelos <- rbind(kaggle_modelos, kaggle_hibrido)

# Grafica todos los modelos para comparar ganancias y elegir el mejor modelo y corte.
ggplot(kaggle_modelos, aes(x=corte, y=ganancia, colour = modelos))+
  geom_line()



# Hibrido al azar----

#modelos_absoluto <- merge(merge(merge(modelo_1[,c(1, 4:53)], modelo_2[,c(1, 4:53)], by="numero_de_cliente"),
#                          modelo_3[,c(1,4:53)]), modelo_4[,c(1,4:53)])

#promedito <- c()
#for (i in 1:dim(modelos_absoluto)[1]) {
#  muestreo <- sample(modelos_absoluto[i, 2:length(modelos_absoluto)], 50)
#  promedito <- c(promedito, rowMeans(muestreo))
#}
#modelos_absoluto$promedio_azar <- promedito
#setorder(modelos_absoluto, -promedio_azar)
#modelos_absoluto$prediccion <- c(rep(1, 11000), rep(0, length(promedito)-11000))

#kaggle_hibrido_azar_11000 <- data.table(numero_de_cliente = modelos_absoluto$numero_de_cliente, 
#                                        Predicted = modelos_absoluto$prediccion)
#write.csv(kaggle_hibrido_azar_11000, "kaggle_hibrido_azar_11000.csv", row.names = FALSE)




