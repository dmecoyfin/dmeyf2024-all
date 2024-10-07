
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")
require("primes")
require("ggplot2")


# Inicializamos variables
set.seed(123)  # Semilla general para reproducibilidad
ganancias_1_total <- c()  # Para guardar las ganancias del modelo 1
ganancias_2_total <- c()  # Para guardar las ganancias del modelo 2
p_valores <- c()  # Para guardar los p-valores obtenidos
num_seeds <- 0  # Contador de semillas
max_seeds <- 200  # Máximo de semillas permitidas
p_valor_limite <- 0.05  # Umbral de p-valor
semilla_primigenia <- 111667
#Genero las semillas de números primos
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(semilla_primigenia) 
semillas <- sample(primos, max_seeds )

# Aqui empieza el programa
setwd("~/buckets/b1/exp/")

# cargo el resultado de la Bayesian Optimization
#Se debe especificar bien la ruta de este archivo. Uno es mi modelo base y el otro es el nuevo
tb_BO_log1 <- fread(paste0(PARAM$experimento_bayesiana,"/BO_log.txt"))
tb_BO_log2 <- fread(paste0(PARAM$experimento_bayesiana,"/BO_log.txt"))


# cargo el dataset donde voy a entrenar el modelo
#Debo especificar bien los dataset si es que hice algún datadrifting o cambio de variables
dataset1 <- fread(paste0(PARAM$experimento_data,"/dataset.csv.gz"))
dataset2 <- fread(paste0(PARAM$experimento_data,"/dataset.csv.gz"))

# paso la clase a binaria que tome valores {0,1}  enteros
dataset1[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]
dataset2[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]


# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01",
    "part_training", "part_validation", "part_testing",
    "part_final_train", "part_future")
)


# Función para entrenar y predecir con una semilla
entrenar_y_predecir <- function(seed) {
  # Reemplazamos la semilla en la lista de parámetros
  param_completo1$seed <- seed
  param_completo2$seed <- seed
  
  # Convertimos los datos a formato lgb.Dataset
  dtrain1 <- lgb.Dataset(
    data = data.matrix(dataset1[part_training == 1L, campos_buenos, with = FALSE]),
    label = dataset1[part_training == 1L, clase01],
    free_raw_data = FALSE
  )
  dtrain2 <- lgb.Dataset(
    data = data.matrix(dataset2[part_training == 1L, campos_buenos, with = FALSE]),
    label = dataset2[part_training == 1L, clase01],
    free_raw_data = FALSE
  )
  dvalid1 <- lgb.Dataset(
    data = data.matrix(dataset1[part_validation == 1L, campos_buenos, with = FALSE]),
    label = dataset1[part_validation == 1L, clase01],
    free_raw_data = FALSE
  )
  dvalid2 <- lgb.Dataset(
    data = data.matrix(dataset2[part_validation == 1L, campos_buenos, with = FALSE]),
    label = dataset2[part_validation == 1L, clase01],
    free_raw_data = FALSE
  )
  
  # Entrenamos ambos modelos con los mismos parámetros, pero con la semilla diferente
  model_1 <- lightgbm(
    data = dtrain1,
    params = param_completo1,
    verbose = -100
  )
  model_2 <- lightgbm(
    data = dtrain2,
    params = param_completo2,
    verbose = -100
  )
  # Realizamos las predicciones sobre el set de validación
  predicciones_1 <- predict(
      modelo1,
      data.matrix(dvalid1[, campos_buenos, with = FALSE])
    )
  predicciones_2 <- predict(
    modelo2,
    data.matrix(dvalid2[, campos_buenos, with = FALSE])
  )
  return(list(predicciones_1 = predicciones_1, predicciones_2 = predicciones_2))
}


# Función para ordenar, seleccionar y calcular la ganancia
ordenar_y_seleccionar <- function(predicciones, tam_inicial = 8000, paso = 500, tam_max = 13000) {
  tbl <- data.table("pred" = predicciones, 
                    "label" = dataset1[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)])
  
  # Ordenamos por la probabilidad predicha en orden decreciente
  setorder(tbl, -pred)
  
  # Inicializamos listas para almacenar las ganancias y sus tamaños
  ganancias <- c()
  tamanos <- c()
  
  # Seleccionamos los subsets según los pasos especificados
  for (tam in seq(tam_inicial, tam_max, paso)) {
    subset <- tbl[1:tam, ]
    
    # Calculamos la ganancia para este subset
    ganancia <- sum(ifelse(subset$label == 1, 273000, -7000))
    ganancias <- c(ganancias, ganancia)
    tamanos <- c(tamanos, tam)  # Guardamos el tamaño correspondiente
  }
  
  return(list(ganancias = ganancias, tamanos = tamanos))
}

# Iteramos sobre las semillas
for (seed in semillas) {
  # Aumentamos el contador de semillas
  num_seeds <- num_seeds + 1
  
  # Entrenamos los modelos y obtenemos las predicciones
  predicciones <- entrenar_y_predecir(seed)
  
  # Calculamos las ganancias para ambos modelos en cada tamaño
  for (tam in seq(8000, 13000, 500)) {
    ganancias_1 <- ordenar_y_seleccionar(predicciones$predicciones_1, tam)
    ganancias_2 <- ordenar_y_seleccionar(predicciones$predicciones_2, tam)
    
    # Guardamos las ganancias de la semilla actual
    ganancias_total[[as.character(tam)]]$ganancias_1 <- c(ganancias_total[[as.character(tam)]]$ganancias_1, sum(ifelse(ganancias_1 == 1, 273000, -7000)))
    ganancias_total[[as.character(tam)]]$ganancias_2 <- c(ganancias_total[[as.character(tam)]]$ganancias_2, sum(ifelse(ganancias_2 == 1, 273000, -7000)))
    
    # Aplicamos el test de Wilcoxon y guardamos el p-valor
    if (length(ganancias_total[[as.character(tam)]]$ganancias_1) > 1 && length(ganancias_total[[as.character(tam)]]$ganancias_2) > 1) {
      resultado_wilcox <- wilcox.test(ganancias_total[[as.character(tam)]]$ganancias_1, ganancias_total[[as.character(tam)]]$ganancias_2, paired = TRUE)
      p_valores_totales[[as.character(tam)]] <- c(p_valores_totales[[as.character(tam)]], resultado_wilcox$p.value)
    }
    
    # Verificamos si el p-valor es menor a 0.05 para cada tamaño
    if (!is.na(p_valores_totales[[as.character(tam)]]) && p_valores_totales[[as.character(tam)]] < p_valor_limite) {
      cat("El proceso se detiene en la semilla:", num_seeds, "para el tamaño:", tam, "\n")
      cat("P-valor:", p_valores_totales[[as.character(tam)]], "\n")
      break
    }
  }
  
  # Verificamos si nos detenemos antes de llegar al final
  if (p_valores_totales[[as.character(tam)]] < p_valor_limite) {
    break
  }
}

# Al final mostramos el resultado
cat("Número total de semillas utilizadas:", num_seeds, "\n")
for (tam in seq(8000, 13000, 500)) {
  cat("P-valor para tamaño", tam, ":", p_valores_totales[[as.character(tam)]], "\n")
}


# Al final mostramos el resultado
cat("Número total de semillas utilizadas:", num_seeds, "\n")
cat("Ganancias del modelo 1:", ganancias_1_total, "\n")
cat("Ganancias del modelo 2:", ganancias_2_total, "\n")
cat("P-valor para tamaño", tam, ":", p_valores_totales[[as.character(tam)]], "\n")




