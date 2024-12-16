rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

# setwd("/Users/ignacio/MAESTRIA/EEA/TP final")


# Cargar librerías necesarias
library(readr)
library(dplyr)

# Función para limpiar y convertir datos
limpiar_csv <- function(archivo) {
  # Leer el CSV especificando tipos de columnas
  datos <- read_csv(archivo, 
                    col_types = cols(
                      cultivo_nombre = col_character(),
                      campania = col_character(),
                      anio = col_integer(),
                      provincia_nombre = col_character(),
                      provincia_id = col_integer(),
                      departamento_nombre = col_character(),
                      departamento_id = col_integer(),
                      superficie_sembrada_ha = col_double(),
                      superficie_cosechada_ha = col_double(),
                      produccion_tm = col_double(),
                      rendimiento_kgxha = col_double()
                    ))
  
  return(datos)
}

# Función para combinar CSV
combinar_csv <- function(archivo1, archivo2, archivo_salida) {
  # Leer ambos archivos con tipos de columnas especificados
  datos1 <- limpiar_csv(archivo1)
  datos2 <- limpiar_csv(archivo2)
  
  # Verificar columnas de ambos dataframes
  print("Columnas en el primer archivo:")
  print(names(datos1))
  print("Columnas en el segundo archivo:")
  print(names(datos2))
  
  # Asegurar que ambos dataframes tengan las mismas columnas
  # Añadir columnas faltantes con NA si es necesario
  columnas_comunes <- union(names(datos1), names(datos2))
  
  datos1 <- datos1 %>% 
    bind_cols(setNames(lapply(setdiff(columnas_comunes, names(datos1)), 
                              function(x) NA), 
                       setdiff(columnas_comunes, names(datos1))))
  
  datos2 <- datos2 %>% 
    bind_cols(setNames(lapply(setdiff(columnas_comunes, names(datos2)), 
                              function(x) NA), 
                       setdiff(columnas_comunes, names(datos2))))
  
  # Reordenar columnas para que coincidan
  datos1 <- datos1[, columnas_comunes]
  datos2 <- datos2[, columnas_comunes]
  
  # Combinar los dataframes
  datos_combinados <- bind_rows(datos1, datos2)
  
  # Escribir el archivo combinado
  write_csv(datos_combinados, archivo_salida)
  
  # Mostrar información
  cat("Archivos combinados exitosamente:\n")
  cat("Número de filas en el archivo 1:", nrow(datos1), "\n")
  cat("Número de filas en el archivo 2:", nrow(datos2), "\n")
  cat("Número total de filas en el archivo combinado:", nrow(datos_combinados), "\n")
  cat("Columnas en el archivo combinado:", paste(names(datos_combinados), collapse = ", "), "\n")
  
  return(datos_combinados)
}

# Uso de la funcion
archivo1 <- "/Users/ignacio/Downloads/soja-1ra-serie-2000-2019.csv"
archivo2 <- "/Users/ignacio/Downloads/soja-2da-serie-2000-2019.csv"
archivo_salida <- "/Users/ignacio/Downloads/soja_total_serie.csv"

# Ejecutar la función
datos_finales <- combinar_csv(archivo1, archivo2, archivo_salida)






