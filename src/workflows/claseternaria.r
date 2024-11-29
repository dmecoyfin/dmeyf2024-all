# Ruta del archivo
ruta_archivo <- "C:/Users/glova/OneDrive/Documentos/ejemploternaria.csv"

# Leer el archivo especificando el separador
df <- read.csv(ruta_archivo, sep = ";", stringsAsFactors = FALSE)

# Asegurarse de que foto_mes sea un entero
df$foto_mes <- as.integer(df$foto_mes)

# Ordenar los datos por cliente y por periodo
df <- df[order(df$numero_de_cliente, df$foto_mes), ]

# Obtener el periodo más reciente
ultimo_periodo <- max(df$foto_mes, na.rm = TRUE)

# Crear columnas temporales para verificar la continuidad de registros
library(dplyr)

df <- df %>%
  group_by(numero_de_cliente) %>%
  mutate(
    siguiente_mes = lead(foto_mes, 1),
    dos_meses_despues = lead(foto_mes, 2)
  ) %>%
  ungroup()

# Condiciones para asignar clase_ternaria
df$clase_ternaria <- "CONTINUA"  # Por defecto

# Identificar BAJA+1: cuando no existe el siguiente mes
df <- df %>%
  mutate(
    clase_ternaria = ifelse(
      is.na(siguiente_mes) & foto_mes < ultimo_periodo,
      "BAJA+1",
      clase_ternaria
    )
  )

# Identificar BAJA+2: cuando no existe dos meses después y el cliente no es BAJA+1
df <- df %>%
  mutate(
    clase_ternaria = ifelse(
      is.na(dos_meses_despues) & 
      foto_mes < (ultimo_periodo - 1) & 
      clase_ternaria != "BAJA+1",
      "BAJA+2",
      clase_ternaria
    )
  )

# Eliminar columnas temporales
df <- df %>% select(-siguiente_mes, -dos_meses_despues)

# Mostrar resultados para verificar
print(head(df, 20))

# Si es necesario guardar los resultados
write.csv(df, "C:/Users/glova/OneDrive/Documentos/resultado33_ternaria.csv", row.names = FALSE)
