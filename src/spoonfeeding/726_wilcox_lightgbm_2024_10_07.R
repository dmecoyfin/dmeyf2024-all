# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
require("parallel")
require("primes")
require("ggplot2")

# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

# Inicializamos variables
PARAM <- list()
PARAM$experimento_data1 <- "PP7230"
PARAM$experimento_data2 <- "PP7230_2024_10_07"
PARAM$experimento_bayesiana1 <- "HT7240"
PARAM$experimento_bayesiana2 <- "HT7240_2024_10_07"

PARAM$experimento <- "TW7260_2024_10_07"

#------------------------------------------------------------------------------
# limita el uso de memoria RAM a  Total_hardware - GB_min

action_limitar_memoria <- function( GB_min = 4 ) {
  
  MemTotal <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE))
  MemTotal <- as.integer( MemTotal/ 1024 - GB_min*1024 )
  if( MemTotal < 0 )  action_abortar( " No hay suficiente RAM para trabajar (min 4GB ) " )
  ulimit::memory_limit( MemTotal )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui empieza el programa

# Limito la memoria, para que ningun alumno debe sufrir que el R 
#  aborte sin avisar si no hay suficiente memoria
#  la salud mental de los alumnos es el bien mas preciado 
action_limitar_memoria( 4 )

# Aqui empieza el programa
setwd("~/buckets/b1/exp/") # Establezco el Working Directory

# cargo el resultado de la Bayesian Optimization
#Se debe especificar bien la ruta de este archivo. Uno es mi modelo base y el otro es el nuevo
tb_BO_log1 <- fread(paste0(PARAM$experimento_bayesiana1,"/BO_log.txt"))
tb_BO_log2 <- fread(paste0(PARAM$experimento_bayesiana2,"/BO_log.txt"))


# cargo el dataset donde voy a entrenar el modelo
#Debo especificar bien los dataset si es que hice algún datadrifting o cambio de variables
dataset1 <- fread(paste0(PARAM$experimento_data1,"/dataset.csv.gz"))
dataset2 <- fread(paste0(PARAM$experimento_data2,"/dataset.csv.gz"))


# creo la carpeta donde va el experimento
dir.create(paste0(PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./", PARAM$experimento, "/"))


# paso la clase a binaria que tome valores {0,1}  enteros
dataset1[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]
dataset2[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]

PARAM$p_valor_limite <- 0.05  # Umbral de p-valor
PARAM$num_seeds <- 0

#Parametros a modificar
PARAM$max_seeds <- 30  # Máximo de semillas permitidas
PARAM$semilla_primigenia <- 111667 # Semilla general para reproducibilidad
PARAM$envios <- seq(8000, 13000, 500) # Rango de envios que queremos evaluar

#Genero las semillas de números primos
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia) 
PARAM$semillas <- sample(primos, PARAM$max_seeds )

# los campos que se van a utilizar
campos_buenos1 <- setdiff(
  colnames(dataset1),
  c("clase_ternaria", "clase01",
    "part_training", "part_validation", "part_testing",
    "part_final_train", "part_future")
)

campos_buenos2 <- setdiff(
  colnames(dataset1),
  c("clase_ternaria", "clase01",
    "part_training", "part_validation", "part_testing",
    "part_final_train", "part_future",
    "cprestamos_presonales", "mprestamos_personales")
)

setorder(tb_BO_log1, -ganancia )
param_completo1 <- copy(as.list(tb_BO_log1[1]))

setorder(tb_BO_log2, -ganancia )
param_completo2 <- copy(as.list(tb_BO_log2[1]))

# Inicializar las listas
ganancias_total <- list()
p_valores_totales <- list()

# Bucle para llenar las listas
for (envio in PARAM$envios) {
  envio_str <- as.character(envio)
  ganancias_total[[envio_str]] <- list(ganancias_1 = c(), ganancias_2 = c())
  p_valores_totales[[envio_str]] <- c()
}


# Función para entrenar y predecir con una semilla
entrenar_y_predecir <- function(seed) {
  # Reemplazamos la semilla en la lista de parámetros
  require("lightgbm")
  param_completo1$seed <- seed
  param_completo2$seed <- seed
  
  # Convertimos los datos a formato lgb.Dataset
  dtrain1 <- lgb.Dataset(
    data = data.matrix(dataset1[part_training == 1L, campos_buenos1, with = FALSE]),
    label = dataset1[part_training == 1L, clase01],
    free_raw_data = FALSE
  )
  dtrain2 <- lgb.Dataset(
    data = data.matrix(dataset2[part_training == 1L, campos_buenos2, with = FALSE]),
    label = dataset2[part_training == 1L, clase01],
    free_raw_data = FALSE
  )
  dvalid1 <- data.matrix(dataset1[part_validation == 1L, campos_buenos1, with = FALSE])
  
  dvalid2 <- data.matrix(dataset2[part_validation == 1L, campos_buenos2, with = FALSE])
  
  # Entrenamos ambos modelos con los mismos parámetros, pero con la semilla diferente
  modelo_1 <- lightgbm(
    data = dtrain1,
    params = param_completo1,
    verbose = -100
  )
  modelo_2 <- lightgbm(
    data = dtrain2,
    params = param_completo2,
    verbose = -100
  )
  # Realizamos las predicciones sobre el set de validación
  predicciones_1 <- predict(
    modelo_1,
    dvalid1
  )
  predicciones_2 <- predict(
    modelo_2,
    dvalid2
  )
  return(list(predicciones_1 = predicciones_1, predicciones_2 = predicciones_2))
}


# Función para ordenar, seleccionar y calcular la ganancia
ordenar_y_seleccionar <- function(predicciones, dataset, envio_inicial = 8000, paso = 500, envio_max = 13000) {
  dvalid <- dataset[part_validation == 1L]
  dvalid <- dvalid[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]
  tbl <- data.table("prob" = predicciones, 
                    "clase01" = dvalid$clase01)
  
  # Ordenamos por la probabilidad predicha en orden decreciente
  setorder(tbl, -prob)
  
  # Inicializamos listas para almacenar las ganancias y sus tamaños
  ganancias <- c()
  envios <- c()
  
  # Seleccionamos los subsets según los pasos especificados
  for (envio in seq(envio_inicial, envio_max, paso)) {
    subset <- tbl[1:envio, ]
    
    # Calculamos la ganancia para este subset
    ganancia <- sum(ifelse(subset$clase01 == 1, 273000, -7000))
    ganancias <- c(ganancias, ganancia)
    envios <- c(envios, envio)  # Guardamos el tamaño correspondiente
  }
  
  return(list(ganancias = ganancias, envios = envios))
}

# Inicializamos tabla de resultados finales
resultados <- data.frame()


# Iteramos sobre las semillas
for (seed in PARAM$semillas) {
  
  # Entrenamos los modelos y obtenemos las predicciones
  predicciones <- entrenar_y_predecir(seed)
  
  # Calculamos las ganancias para los envios
  ganancias_1 <- ordenar_y_seleccionar(predicciones$predicciones_1, 
                                       dataset1, 
                                       envio_inicial = PARAM$envios[1],
                                       envio_max = PARAM$envios[length(PARAM$envios)])
  ganancias_2 <- ordenar_y_seleccionar(predicciones$predicciones_2, 
                                       dataset2,
                                       envio_inicial = PARAM$envios[1],
                                       envio_max = PARAM$envios[length(PARAM$envios)])
  
  # Calculamos las ganancias para ambos modelos en cada tamaño
  for (envio in PARAM$envios) {
    envio_str <- as.character(envio)
    
    # Obtenemos las ganancias directamente de las listas ganancias_1 y ganancias_2
    ganancia_modelo_1 <- ganancias_1$ganancias[ganancias_1$envios == envio_str]
    ganancia_modelo_2 <- ganancias_2$ganancias[ganancias_2$envios == envio_str]
    
    # Guardamos las ganancias de la semilla actual
    ganancias_total[[envio_str]]$ganancias_1 <- c(ganancias_total[[envio_str]]$ganancias_1, ganancia_modelo_1)
    ganancias_total[[envio_str]]$ganancias_2 <- c(ganancias_total[[envio_str]]$ganancias_2, ganancia_modelo_2)
    
    # Aplicamos el test de Wilcoxon y guardamos el p-valor
    if (length(ganancias_total[[envio_str]]$ganancias_1) > 0 && length(ganancias_total[[envio_str]]$ganancias_2) > 0) {
      resultado_wilcox <- wilcox.test(ganancias_total[[envio_str]]$ganancias_1, ganancias_total[[envio_str]]$ganancias_2, paired = TRUE)
      p_valores_totales[[envio_str]] <- c(p_valores_totales[[envio_str]], resultado_wilcox$p.value)
    }
    
    # Escribimos los resultados en un data frame
    df_soporte <- data.frame(envio_str, ganancia_modelo_1, ganancia_modelo_2, seed, resultado_wilcox$p.value)
    resultados <- rbind(resultados, df_soporte)
    
    
  }
}

write.csv(resultados, file = "resultados.csv", row.names = FALSE)




