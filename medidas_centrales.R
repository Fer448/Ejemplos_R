install.packages(c("plotly", "ggplot2"))
# Cargar librerías
library(plotly)
library(ggplot2)

# Datos del ejemplo
datos <- c(7, 9, 16, 36, 39, 45, 45, 46, 48, 51)

# Calcular cuartiles
Q1 <- quantile(datos, 0.25, type = 7)  # type=7 es el método predeterminado en R (igual que en Python con 'linear')
Q2 <- median(datos)
Q3 <- quantile(datos, 0.75, type = 7)

cat("Q1 =", Q1, "\n")
cat("Q2 (mediana) =", Q2, "\n")
cat("Q3 =", Q3, "\n")
cat("Rango intercuartílico:", Q1, "a", Q3, "\n")

# Parámetros de la distribución normal
mu <- mean(datos)
sigma <- sd(datos)  # Por defecto, sd() usa n-1 (muestra), pero con n=10 es aceptable

# Crear un marco de datos para ggplot
df <- data.frame(x = datos)

# Crear gráfico base con ggplot
p <- ggplot(df, aes(x = x)) +
  # Histograma
  geom_histogram(aes(y = ..count..), bins = 8, fill = "lightblue", alpha = 0.7, color = "black") +
  
  # Curva de densidad normal (escalada al conteo)
  stat_function(
    fun = function(x) dnorm(x, mean = mu, sd = sigma) * length(datos) * (max(datos) - min(datos)) / 8,
    color = "red",
    size = 1.2
  ) +
  
  # Líneas de cuartiles
  geom_vline(xintercept = Q1, linetype = "dashed", color = "green", size = 1) +
  geom_vline(xintercept = Q2, linetype = "dashed", color = "orange", size = 1) +
  geom_vline(xintercept = Q3, linetype = "dashed", color = "purple", size = 1) +
  
  # Sombreado del IQR
  annotate("rect", xmin = Q1, xmax = Q3, ymin = -Inf, ymax = Inf,
           fill = "yellow", alpha = 0.2) +
  
  # Etiquetas y título
  labs(
    title = "Distribución de los datos con curva de Gauss y cuartiles",
    x = "Valor",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Convertir a gráfico interactivo con plotly
p_interactivo <- ggplotly(p, tooltip = "x")

# Guardar como archivo HTML
htmlwidgets::saveWidget(p_interactivo, "grafica_gauss_cuartiles_R.html")

cat("\n✅ Gráfica interactiva guardada como 'grafica_gauss_cuartiles_R.html'\n")
cat("Ábrelo en tu navegador para ver la versión interactiva.\n")