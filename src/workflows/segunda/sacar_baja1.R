# Cargar la librería
library(data.table)

# Paso 1: Cargar el archivo desde la dirección especificada usando fread de data.table
ruta_archivo <- "D:/Documentos/Estudio/Maestria/[01] Maestria Cs Datos/DMEF/DMEyF/datasets_segunda_competencia_sin_baja1.csv.gz"

datos <- fread(ruta_archivo)

# Paso 2: Filtrar los datos para encontrar las filas que cumplan ambas condiciones:
#   - Los valores en foto_mes que correspondan a los meses especificados.
#   - El valor "BAJA+1" en la columna Clase_terciaria.

# Definir los valores específicos para foto_mes que deseamos filtrar
valores_foto_mes <- c(   202106, 202105, 202104, 202103, 202102, 202101, 
                        202012, 202011, 202010, 202009, 202008, 202007, 
                        202006,
                        202005, 
                        202004, 202003,  
                        202002, 202001,
                        201912, 201911, 201910,
                        201909, 201908, 201907, 201906, 201905,
                        201904, 201903,201902,201901)

# Filtrar las filas que no cumplen ambas condiciones y eliminarlas
datos_filtrados <- datos[!(foto_mes %in% valores_foto_mes & clase_ternaria == "BAJA+1")]

# Paso 3: Guardar el resultado en un nuevo archivo comprimido en formato CSV
ruta_salida <- "D:/Documentos/Estudio/Maestria/[01] Maestria Cs Datos/DMEF/DMEyF/datasets_competencia_02_sin_baja_final.csv.gz"
fwrite(datos_filtrados, ruta_salida, compress = "gzip")

# Mensaje opcional para indicar que el proceso se ha completado
cat("Archivo guardado en:", ruta_salida, "\n")

#Verificar resultado

datos_filtrados <- fread("D:/Documentos/Estudio/Maestria/[01] Maestria Cs Datos/DMEF/DMEyF/datasets_competencia_02_sin_baja_final.csv.gz")

# Paso 2: Verificar que no existan filas con las condiciones que deberían haberse eliminado
# Definir nuevamente los valores de foto_mes a verificar
valores_foto_mes <- c(   202106, 202105, 202104, 202103, 202102, 202101, 
                         202012, 202011, 202010, 202009, 202008, 202007, 
                         202006,
                         202005, 
                         202004, 202003,  
                         202002, 202001,
                         201912, 201911, 201910,
                         201909, 201908, 201907, 201906, 201905,
                         201904, 201903,201902,201901)

# Buscar filas que cumplan con las condiciones eliminadas
casos_restantes <- datos_filtrados[foto_mes %in% valores_foto_mes & clase_ternaria == "BAJA+1"]


# Paso 3: Comprobar si existen resultados en "casos_restantes"
if (nrow(casos_restantes) == 0) {
  cat("La verificación fue exitosa: No hay casos restantes con los valores de foto_mes y Clase_terciaria especificados.\n")
} else {
  cat("Advertencia: Existen", nrow(casos_restantes), "casos que cumplen las condiciones y no fueron eliminados.\n")
}