# Datos de ejemplo: edad (años) y altura (cm)
edad <- c(8, 10, 12, 14, 16, 18, 25, 35, 45, 55, 65, 75)
altura <- c(128, 140, 152, 162, 168, 170, 172, 171, 170, 168, 165, 162)

# Crear data frame
datos <- data.frame(edad, altura)

# Ajustar modelo polinómico de grado 3
modelo <- lm(altura ~ poly(edad, 3, raw=TRUE), data=datos)

# Mostrar resumen del modelo
summary(modelo)

# Predecir sobre un rango continuo de edades
edad_rango <- seq(8, 75, length.out = 300)
predicciones <- predict(modelo, newdata = data.frame(edad = edad_rango))

# Graficar
plot(edad, altura, col = "blue", pch = 16, xlab = "Edad (años)", ylab = "Altura (cm)",
     main = "Regresión polinómica: Edad vs Altura (8-75 años)")
lines(edad_rango, predicciones, col = "red", lwd = 2)
legend("topright", legend = c("Datos reales", "Modelo polinómico"), 
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))
grid()

