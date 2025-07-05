# Parámetros
num_pacientes <- 5     # Número de columnas
num_fechas <- 7        # Número de filas

# Crear matriz con valores aleatorios entre 35 y 45
set.seed(123)  # Semilla para reproducibilidad
datos <- matrix(
  runif(num_pacientes * num_fechas, min = 35, max = 45),
  nrow = num_fechas,
  ncol = num_pacientes
)

# Asignar nombres a filas (fechas) y columnas (pacientes)
rownames(datos) <- paste("Fecha", 1:num_fechas)
colnames(datos) <- paste("Paciente", 1:num_pacientes)

# Mostrar la matriz
print(datos)

# Crear matriz de temperaturas aleatorias
set.seed(123)
num_pacientes <- 5
num_fechas <- 7

datos <- matrix(
  runif(num_pacientes * num_fechas, min = 35, max = 45),
  nrow = num_fechas,
  ncol = num_pacientes
)

# Asignar nombres
rownames(datos) <- paste("Fecha", 1:num_fechas)
colnames(datos) <- paste("Paciente", 1:num_pacientes)

# Añadir estadísticas por columna (paciente)
fila_min <- apply(datos, 2, min)
fila_max <- apply(datos, 2, max)
fila_media <- round(apply(datos, 2, mean), 2)

# Añadir las filas a la matriz
datos_extendido <- rbind(
  datos,
  Min = fila_min,
  Max = fila_max,
  Media = fila_media
)

# Mostrar matriz completa
print(datos_extendido)





# Crear matriz de temperaturas aleatorias
set.seed(123)
num_pacientes <- 5
num_fechas <- 7

datos <- matrix(
  runif(num_pacientes * num_fechas, min = 35, max = 45),
  nrow = num_fechas,
  ncol = num_pacientes
)

# Asignar nombres
rownames(datos) <- paste("Fecha", 1:num_fechas)
colnames(datos) <- paste("Paciente", 1:num_pacientes)

# Calcular estadísticas por fila (por fecha)
col_min <- apply(datos, 1, min)
col_max <- apply(datos, 1, max)
col_media <- round(apply(datos, 1, mean), 2)

# Añadir las estadísticas como columnas
datos_extendido <- cbind(datos, Min = col_min, Max = col_max, Media = col_media)

# Mostrar la matriz extendida
print(datos_extendido)


# Intercambiar filas 2 y 3
df_con_fecha[c(2, 3), ] <- df_con_fecha[c(3, 2), ]

# Mostrar tabla
print(df_con_fecha)



# Ordenar alfabéticamente por la columna "Fecha"
df_ordenado = df_con_fecha.sort_values(by="Fecha")

# Mostrar tabla ordenada
print(df_ordenado)

# Supongamos que df_con_fecha tiene la primera columna sin nombre
# (por ejemplo, al cargar con encabezado = FALSE)

# Ordenar por la primera columna (columna 1)
df_ordenado <- df_con_fecha[order(df_con_fecha[[1]]), ]

# Mostrar resultado
print(df_ordenado)
