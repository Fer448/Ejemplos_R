# Cargar librerías
library(ggplot2)
library(tidyr)
library(dplyr)
set.seed(42)  # Reproducibilidad

# --- 1. Parámetros ---
num_dias <- 7
num_pacientes <- 5

# --- 2. Crear matriz con temperaturas aleatorias ---
temperaturas <- matrix(runif(num_dias * num_pacientes, min=35, max=45),
                       nrow=num_dias, ncol=num_pacientes)
rownames(temperaturas) <- paste("Día", 1:num_dias)
colnames(temperaturas) <- paste("Paciente", 1:num_pacientes)

cat("Temperaturas iniciales:\n")
print(round(temperaturas, 2))

# --- 3. Añadir estadísticas por paciente (columnas) ---
min_pac <- apply(temperaturas, 2, min)
max_pac <- apply(temperaturas, 2, max)
mean_pac <- apply(temperaturas, 2, mean)

# Añadir filas a la matriz
temperaturas <- rbind(temperaturas, min_pac, max_pac, mean_pac)
rownames(temperaturas)[(num_dias + 1):(num_dias + 3)] <- c("Min Pac", "Max Pac", "Media Pac")

cat("\nTemperaturas con estadísticas por paciente:\n")
print(round(temperaturas, 2))

# --- 4. Añadir estadísticas por día (filas) ---
min_dia <- apply(temperaturas[, 1:num_pacientes], 1, min)
max_dia <- apply(temperaturas[, 1:num_pacientes], 1, max)
mean_dia <- apply(temperaturas[, 1:num_pacientes], 1, mean)

# Añadir columnas
temperaturas <- cbind(temperaturas, "Min Día"=min_dia, "Max Día"=max_dia, "Media Día"=mean_dia)

cat("\nTemperaturas finales con estadísticas por día:\n")
print(round(temperaturas, 2))

# --- 5. Guardar en CSV ---
write.csv(round(temperaturas, 2), "temperaturas_pacientes.csv", row.names = TRUE)
cat("\nArchivo 'temperaturas_pacientes.csv' guardado correctamente.\n")

# --- 6. Visualización gráfica ---
# Usamos solo los días originales para graficar (sin las filas de estadísticas)
temp_df <- as.data.frame(temperaturas[1:num_dias, 1:num_pacientes])
temp_df$Día <- 1:num_dias

# Convertir a formato largo para ggplot
temp_long <- pivot_longer(temp_df, cols = starts_with("Paciente"), names_to = "Paciente", values_to = "Temperatura")

# Graficar
ggplot(temp_long, aes(x = Día, y = Temperatura, color = Paciente)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Temperaturas por paciente a lo largo de los días",
       x = "Día", y = "Temperatura (°C)") +
  theme_minimal() +
  ylim(34, 46)

