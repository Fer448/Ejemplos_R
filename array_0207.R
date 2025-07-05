# 1. Lista de cuadrados del 1 al 10
cuadrados <- (1:10)^2

# 2. Lista de 5 números aleatorios entre 1 y 100
set.seed(123)  # Fijar semilla para reproducibilidad
aleatorios <- sample(1:100, 5)

# 3. Unir ambas listas
lista_unida <- c(cuadrados, aleatorios)

# 4. Sustituir posiciones 2 y 3 por NA (índices 2 y 3 en R)
lista_unida[2] <- NA
lista_unida[3] <- NA

# 5. Eliminar valores mayores a 42 (manteniendo los NA fuera del filtro)
lista_filtrada <- lista_unida[!is.na(lista_unida) & lista_unida <= 42]

# Mostrar resultados
cat("Cuadrados:\n"); print(cuadrados)
cat("Aleatorios:\n"); print(aleatorios)
cat("Lista unida con NA:\n"); print(lista_unida)
cat("Lista final filtrada (≤ 42):\n"); print(lista_filtrada)


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



# Asumiendo que ya tienes 'datos' creado y nombrado

# Matplot: líneas para cada paciente
matplot(t(datos), type = "o", pch = 1:5, col = 1:5,
        xlab = "Fecha", ylab = "Temperatura",
        main = "Evolución de temperatura por paciente",
        xaxt = "n")

# Etiquetas del eje x
axis(1, at = 1:nrow(datos), labels = rownames(datos))

# Leyenda
legend("topright", legend = colnames(datos), col = 1:5, pch = 1:5, lty = 1)


library(ggplot2)
library(reshape2)  # Para transformar matriz a formato largo

# Convertir matriz a data.frame y luego a formato largo
df <- as.data.frame(datos)
df$Fecha <- factor(rownames(datos), levels = rownames(datos))

df_long <- melt(df, id.vars = "Fecha", variable.name = "Paciente", value.name = "Temperatura")

# Gráfico con ggplot2
ggplot(df_long, aes(x = Fecha, y = Temperatura, group = Paciente, color = Paciente)) +
  geom_line() +
  geom_point() +
  labs(title = "Evolución de temperatura por paciente", x = "Fecha", y = "Temperatura") +
  theme_minimal()





# Crear la matriz de datos
set.seed(123)
num_pacientes <- 5
num_fechas <- 7

datos <- matrix(
  runif(num_pacientes * num_fechas, min = 35, max = 45),
  nrow = num_fechas,
  ncol = num_pacientes
)

# Nombres de filas y columnas
rownames(datos) <- paste("Fecha", 1:num_fechas)
colnames(datos) <- paste("Paciente", 1:num_pacientes)

# Crear fila de encabezado (con nombres de columnas)
fila_encabezado <- colnames(datos)

# Convertir datos a character para unir con la fila de texto
datos_char <- apply(datos, 2, as.character)

# Añadir la fila al principio
datos_con_encabezado <- rbind(fila_encabezado, datos_char)

# Asignar nombres de fila
rownames(datos_con_encabezado)[1] <- "Nombres"

# Ver resultado
print(datos_con_encabezado)

# Crear matriz de ejemplo
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

# Convertir a data.frame
df <- as.data.frame(datos)

# Añadir columna de fechas al principio
df_con_fecha <- cbind(Fecha = rownames(df), df)

# Mostrar
print(df_con_fecha)


