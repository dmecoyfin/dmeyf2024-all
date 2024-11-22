rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")
require("primes")
require("ggplot2")

# Función para ordenar, seleccionar y calcular la ganancia
ordenar_y_seleccionar <- function(predicciones, etiquetas, tam_inicial = 8000, paso = 500, tam_max = 13000) {
  tbl <- data.table("pred" = predicciones, "label" = etiquetas)
  
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


# Predicciones con ambos modelos sobre el conjunto de validación
predicciones_1 <- predict(model_1, data = datos_validacion[, -"label", with = FALSE])
predicciones_2 <- predict(model_2, data = datos_validacion[, -"label", with = FALSE])

# Calculamos las ganancias para ambos modelos
ganancias_1 <- ordenar_y_seleccionar(predicciones_1, datos_validacion$label)
ganancias_2 <- ordenar_y_seleccionar(predicciones_2, datos_validacion$label)

# Aplicamos la prueba de Wilcoxon a las ganancias obtenidas
resultado_wilcox <- wilcox.test(ganancias_1, ganancias_2, paired = TRUE)

# Mostramos los resultados del test
print(resultado_wilcox)




# Inicializamos variables
set.seed(123)  # Semilla general para reproducibilidad
ganancias_1_total <- c()  # Para guardar las ganancias del modelo 1
ganancias_2_total <- c()  # Para guardar las ganancias del modelo 2
p_valores <- c()  # Para guardar los p-valores obtenidos
num_seeds <- 0  # Contador de semillas
max_seeds <- 200  # Máximo de semillas permitidas
p_valor_limite <- 0.05  # Umbral de p-valor


# Función para entrenar y predecir con una semilla
entrenar_y_predecir <- function(seed, datos_entrenamiento, datos_validacion, param_completo1, param_completo2) {
  # Reemplazamos la semilla en la lista de parámetros
  param_completo$seed <- seed
  
  # Convertimos los datos a formato lgb.Dataset
  dtrain <- lgb.Dataset(data = datos_entrenamiento[, -"label", with = FALSE], label = datos_entrenamiento$label)
  dvalid <- lgb.Dataset(data = datos_validacion[, -"label", with = FALSE], label = datos_validacion$label)
  
  # Entrenamos ambos modelos con los mismos parámetros, pero con la semilla diferente
  model_1 <- lgb.train(params = param_completo1, data = dtrain, valids = list(validation = dvalid), nrounds = 100, early_stopping_rounds = 10)
  model_2 <- lgb.train(params = param_completo2, data = dtrain, valids = list(validation = dvalid), nrounds = 100, early_stopping_rounds = 10)
  
  # Realizamos las predicciones sobre el set de validación
  predicciones_1 <- predict(model_1, data = datos_validacion[, -"label", with = FALSE])
  predicciones_2 <- predict(model_2, data = datos_validacion[, -"label", with = FALSE])
  
  return(list(predicciones_1 = predicciones_1, predicciones_2 = predicciones_2))
}

# Iteramos sobre las semillas
for (seed in 1:max_seeds) {
  # Aumentamos el contador de semillas
  num_seeds <- num_seeds + 1
  
  # Entrenamos los modelos y obtenemos las predicciones
  predicciones <- entrenar_y_predecir(seed, datos_entrenamiento, datos_validacion, param_completo1, param_completo2)
  
  # Calculamos las ganancias para ambos modelos
  ganancias_1 <- ordenar_y_seleccionar(predicciones$predicciones_1, datos_validacion$label)
  ganancias_2 <- ordenar_y_seleccionar(predicciones$predicciones_2, datos_validacion$label)
  
  # Guardamos la ganancia de la semilla actual
  ganancias_1_total <- c(ganancias_1_total, mean(ganancias_1))
  ganancias_2_total <- c(ganancias_2_total, mean(ganancias_2))
  
  # Aplicamos el test de Wilcoxon con las ganancias acumuladas
  resultado_wilcox <- wilcox.test(ganancias_1_total, ganancias_2_total, paired = TRUE)
  
  # Guardamos el p-valor
  p_valores <- c(p_valores, resultado_wilcox$p.value)
  
  # Verificamos si el p-valor es menor a 0.05 y detenemos si es el caso
  if (resultado_wilcox$p.value < p_valor_limite) {
    cat("El proceso se detiene en la semilla:", num_seeds, "\n")
    cat("P-valor:", resultado_wilcox$p.value, "\n")
    break
  }
}


# Iteramos sobre las semillas
for (seed in 1:max_seeds) {
  # Aumentamos el contador de semillas
  num_seeds <- num_seeds + 1
  
  # Entrenamos los modelos y obtenemos las predicciones
  predicciones <- entrenar_y_predecir(seed, datos_entrenamiento, datos_validacion, param_completo1, param_completo2)
  
  # Calculamos las ganancias para ambos modelos en cada tamaño
  for (tam in seq(8000, 13000, 500)) {
    ganancias_1 <- ordenar_y_seleccionar(predicciones$predicciones_1, datos_validacion$label, tam)
    ganancias_2 <- ordenar_y_seleccionar(predicciones$predicciones_2, datos_validacion$label, tam)
    
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
cat("P-valores obtenidos:", p_valores, "\n")
cat("P-valor para tamaño", tam, ":", p_valores_totales[[as.character(tam)]], "\n")