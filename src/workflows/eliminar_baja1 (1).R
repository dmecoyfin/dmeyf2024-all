# Cargar la librería
library(data.table)

# Paso 1: Cargar el archivo desde la dirección especificada usando fread de data.table
ruta_archivo <- "~/buckets/b1/datasets/datasets_competencia_02.csv.gz"
datos <- fread(ruta_archivo)

# Paso 2: Filtrar los datos para encontrar las filas que cumplan ambas condiciones:
#   - Los valores en foto_mes que correspondan a los meses especificados.
#   - El valor "BAJA+1" en la columna Clase_terciaria.

# Definir los valores específicos para foto_mes que deseamos filtrar
valores_foto_mes <- c(202009, 202010, 202011, 202012, 202101, 202102, 202103)

# Filtrar las filas que no cumplen ambas condiciones y eliminarlas
datos_filtrados <- datos[!(foto_mes %in% valores_foto_mes & clase_ternaria == "BAJA+1")]

# Paso 3: Guardar el resultado en un nuevo archivo comprimido en formato CSV
ruta_salida <- "~/buckets/b1/datasets/datasets_competencia_02_sin_baja.csv.gz"
fwrite(datos_filtrados, ruta_salida, compress = "gzip")

# Mensaje opcional para indicar que el proceso se ha completado
cat("Archivo guardado en:", ruta_salida, "\n")

#Verificar resultado

datos_filtrados <- fread("~/buckets/b1/datasets/datasets_competencia_02_sin_baja.csv.gz")

# Paso 2: Verificar que no existan filas con las condiciones que deberían haberse eliminado
# Definir nuevamente los valores de foto_mes a verificar
valores_foto_mes <- c(202009, 202010, 202011, 202012, 202101, 202102, 202103)

# Buscar filas que cumplan con las condiciones eliminadas
casos_restantes <- datos_filtrados[foto_mes %in% valores_foto_mes & clase_ternaria == "BAJA+1"]


# Paso 3: Comprobar si existen resultados en "casos_restantes"
if (nrow(casos_restantes) == 0) {
  cat("La verificación fue exitosa: No hay casos restantes con los valores de foto_mes y Clase_terciaria especificados.\n")
} else {
  cat("Advertencia: Existen", nrow(casos_restantes), "casos que cumplen las condiciones y no fueron eliminados.\n")
}