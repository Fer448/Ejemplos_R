# Librerías necesarias
set.seed(42)  # Reproducibilidad
library(ggplot2)

# --- 1. Parámetros iniciales ---
num_dias <- 7
num_pacientes <- 5

# Crear matriz con temperaturas aleatorias
temperaturas <- matrix(runif(num_dias * num_pacientes, min = 35, max = 45),
                       nrow = num_dias, ncol = num_pacientes)

# Mostrar matriz inicial
cat("Temperaturas iniciales:\n")
print(round(temperaturas, 2))

# --- 2. Añadir filas con estadísticas por paciente ---
min_pac <- apply(temperaturas, 2, min)
max_pac <- apply(temperaturas, 2, max)
mean_pac <- apply(temperaturas, 2, mean)

temperaturas <- rbind(temperaturas, min_pac, max_pac, mean_pac)
rownames(temperaturas) <- c(paste0("Día ", 1:num_dias), "Min Pac", "Max Pac", "Media Pac")

# Mostrar matriz con estadísticas por paciente
cat("\nTemperaturas con estadísticas por paciente:\n")
print(round(temperaturas, 2))

# --- 3. Añadir columnas con estadísticas por día ---
min_dia <- apply(temperaturas[, 1:num_pacientes], 1, min)
max_dia <- apply(temperaturas[, 1:num_pacientes], 1, max)
mean_dia <- apply(temperaturas[, 1:num_pacientes], 1, mean)

temperaturas <- cbind(temperaturas, min_dia, max_dia, mean_dia)
colnames(temperaturas) <- c(paste0("Paciente ", 1:num_pacientes), "Min Día", "Max Día", "Media Día")

cat("\nTemperaturas finales con estadísticas por día:\n")
print(round(temperaturas, 2))

# --- 4. Guardar CSV ---
write.csv(round(temperaturas, 2), "temperaturas_pacientes.csv")
cat("\nArchivo 'temperaturas_pacientes.csv' guardado correctamente.\n")

# --- 5. Visualización gráfica ---
# Convertir a data frame para ggplot
df_plot <- data.frame(Día = rep(1:num_dias, each = num_pacientes),
                      Temperatura = as.vector(temperaturas[1:num_dias, 1:num_pacientes]),
                      Paciente = factor(rep(1:num_pacientes, times = num_dias)))

ggplot(df_plot, aes(x = Día, y = Temperatura, color = Paciente)) +
  geom_line(size = 1) +
  geom_point() +
  ylim(34, 46) +
  labs(title = "Temperaturas por paciente a lo largo de los días",
       x = "Día", y = "Temperatura (°C)") +
  theme_minimal()
