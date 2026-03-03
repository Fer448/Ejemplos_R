install.packages(c("knitr", "ggplot2", "reshape2"))

# ============================================================
# EJEMPLO COMPLETO DE CÁLCULO DE NÚMEROS ÍNDICES EN R
# ============================================================

# Cargar librerías necesarias
library(knitr)
library(ggplot2)
library(reshape2)

# Configuración para mostrar resultados
options(digits = 2)

cat("=", paste(rep("=", 78), collapse = ""), "=\n", sep = "")
cat("CÁLCULO DE NÚMEROS ÍNDICES - EJEMPLO PRÁCTICO EN R\n")
cat("=", paste(rep("=", 78), collapse = ""), "=\n", sep = "")

# ------------------------------------------------------------
# 1. DATOS DE EJEMPLO: 3 productos, 2 períodos (año base y año actual)
# ------------------------------------------------------------
cat("\n📊 DATOS DE PARTIDA:\n")
cat("-", paste(rep("-", 50), collapse = ""), "\n", sep = "")

# Creamos un data frame con los datos
datos <- data.frame(
  Producto = c('Pan', 'Leche', 'Huevos'),
  Precio_base = c(1.0, 1.2, 2.0),      # p₀ (año 2020)
  Cantidad_base = c(100, 50, 30),       # q₀ (año 2020)
  Precio_actual = c(1.3, 1.5, 2.2),     # pₜ (año 2024)
  Cantidad_actual = c(110, 45, 35)       # qₜ (año 2024)
)

print(kable(datos, format = "simple", digits = 2, caption = "Datos base"))

cat("\nAño base: 2020 | Año actual: 2024\n")

# ------------------------------------------------------------
# 2. CÁLCULOS PREVIOS: Gastos y valores totales
# ------------------------------------------------------------
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("🧮 CÁLCULOS PREVIOS\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

# Gastos en cada período
datos$Gasto_base <- datos$Precio_base * datos$Cantidad_base
datos$Gasto_actual <- datos$Precio_actual * datos$Cantidad_actual
datos$Gasto_hibrido1 <- datos$Precio_actual * datos$Cantidad_base  # pₜ·q₀
datos$Gasto_hibrido2 <- datos$Precio_base * datos$Cantidad_actual  # p₀·qₜ

cat("\n📋 Tabla completa de gastos:\n")
print(kable(datos, format = "simple", digits = 2))

# Totales
total_gasto_base <- sum(datos$Gasto_base)
total_gasto_actual <- sum(datos$Gasto_actual)
total_paq0 <- sum(datos$Gasto_hibrido1)  # ∑ pₜ·q₀
total_p0qa <- sum(datos$Gasto_hibrido2)  # ∑ p₀·qₜ

cat("\n📈 Totales:\n")
cat(sprintf("   Gasto total año base (∑ p₀·q₀): %.2f €\n", total_gasto_base))
cat(sprintf("   Gasto total año actual (∑ pₜ·qₜ): %.2f €\n", total_gasto_actual))
cat(sprintf("   ∑ pₜ·q₀ (gasto si compro cesta base a precios actuales): %.2f €\n", total_paq0))
cat(sprintf("   ∑ p₀·qₜ (gasto si compro cesta actual a precios base): %.2f €\n", total_p0qa))

# ------------------------------------------------------------
# 3. ÍNDICES SIMPLES (para cada producto)
# ------------------------------------------------------------
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("📏 1. ÍNDICES SIMPLES\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

datos$Indice_precio_simple <- (datos$Precio_actual / datos$Precio_base * 100)
datos$Indice_cantidad_simple <- (datos$Cantidad_actual / datos$Cantidad_base * 100)

cat("\n📊 Índices simples por producto:\n")
print(kable(datos[, c('Producto', 'Precio_base', 'Precio_actual', 'Indice_precio_simple',
                      'Cantidad_base', 'Cantidad_actual', 'Indice_cantidad_simple')], 
            format = "simple", digits = 2))

# ------------------------------------------------------------
# 4. ÍNDICES COMPLEJOS DE PRECIOS
# ------------------------------------------------------------
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("⚖️ 2. ÍNDICES COMPLEJOS DE PRECIOS\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

# Índice de Laspeyres de Precios
laspeyres_precios <- (total_paq0 / total_gasto_base) * 100

# Índice de Paasche de Precios
paasche_precios <- (total_gasto_actual / total_p0qa) * 100

# Índice de Fisher de Precios
fisher_precios <- sqrt(laspeyres_precios * paasche_precios)

cat("\n📌 Laspeyres (precios): Lₚ = (∑ pₜ·q₀ / ∑ p₀·q₀) × 100\n")
cat(sprintf("   Lₚ = (%.2f / %.2f) × 100 = %.2f\n", total_paq0, total_gasto_base, laspeyres_precios))
cat(sprintf("   ▶️ La cesta de 2020 cuesta hoy un %.1f%% más\n", laspeyres_precios - 100))

cat("\n📌 Paasche (precios): Pₚ = (∑ pₜ·qₜ / ∑ p₀·qₜ) × 100\n")
cat(sprintf("   Pₚ = (%.2f / %.2f) × 100 = %.2f\n", total_gasto_actual, total_p0qa, paasche_precios))
cat(sprintf("   ▶️ La cesta actual cuesta un %.1f%% más que en 2020\n", paasche_precios - 100))

cat("\n📌 Fisher (precios): Fₚ = √(Lₚ × Pₚ)\n")
cat(sprintf("   Fₚ = √(%.2f × %.2f) = %.2f\n", laspeyres_precios, paasche_precios, fisher_precios))

# ------------------------------------------------------------
# 5. ÍNDICES COMPLEJOS DE CANTIDAD
# ------------------------------------------------------------
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("📦 3. ÍNDICES COMPLEJOS DE CANTIDAD\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

# Laspeyres de Cantidad (pondera con precios base)
laspeyres_cantidad <- (total_p0qa / total_gasto_base) * 100

# Paasche de Cantidad (pondera con precios actuales)
paasche_cantidad <- (total_gasto_actual / total_paq0) * 100

# Fisher de Cantidad
fisher_cantidad <- sqrt(laspeyres_cantidad * paasche_cantidad)

cat("\n📌 Laspeyres (cantidad): L_q = (∑ qₜ·p₀ / ∑ q₀·p₀) × 100\n")
cat(sprintf("   L_q = (%.2f / %.2f) × 100 = %.2f\n", total_p0qa, total_gasto_base, laspeyres_cantidad))
cat(sprintf("   ▶️ Las cantidades han variado un %.1f%% (a precios base)\n", laspeyres_cantidad - 100))

cat("\n📌 Paasche (cantidad): P_q = (∑ qₜ·pₜ / ∑ q₀·pₜ) × 100\n")
cat(sprintf("   P_q = (%.2f / %.2f) × 100 = %.2f\n", total_gasto_actual, total_paq0, paasche_cantidad))
cat(sprintf("   ▶️ Las cantidades han variado un %.1f%% (a precios actuales)\n", paasche_cantidad - 100))

cat("\n📌 Fisher (cantidad): F_q = √(L_q × P_q)\n")
cat(sprintf("   F_q = √(%.2f × %.2f) = %.2f\n", laspeyres_cantidad, paasche_cantidad, fisher_cantidad))

# ------------------------------------------------------------
# 6. ÍNDICE DE VALOR
# ------------------------------------------------------------
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("💰 4. ÍNDICE DE VALOR\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

indice_valor <- (total_gasto_actual / total_gasto_base) * 100

cat("\n📌 Índice de Valor: V = (∑ pₜ·qₜ / ∑ p₀·q₀) × 100\n")
cat(sprintf("   V = (%.2f / %.2f) × 100 = %.2f\n", total_gasto_actual, total_gasto_base, indice_valor))
cat(sprintf("   ▶️ El gasto total ha variado un %.1f%%\n", indice_valor - 100))

# ------------------------------------------------------------
# 7. VERIFICACIÓN DE LA RELACIÓN ENTRE ÍNDICES
# ------------------------------------------------------------
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("🔄 5. RELACIÓN ENTRE ÍNDICES\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

# Relación: Valor = Laspeyres Precios × Paasche Cantidad
relacion1 <- (laspeyres_precios * paasche_cantidad) / 100  # Dividimos entre 100 por ser porcentajes
cat("\n📌 Valor = Lₚ × P_q\n")
cat(sprintf("   %.2f = %.2f × %.2f / 100\n", indice_valor, laspeyres_precios, paasche_cantidad))
cat(sprintf("   %.2f = %.2f ✓\n", indice_valor, relacion1))

# Relación: Valor = Paasche Precios × Laspeyres Cantidad
relacion2 <- (paasche_precios * laspeyres_cantidad) / 100
cat("\n📌 Valor = Pₚ × L_q\n")
cat(sprintf("   %.2f = %.2f × %.2f / 100\n", indice_valor, paasche_precios, laspeyres_cantidad))
cat(sprintf("   %.2f = %.2f ✓\n", indice_valor, relacion2))

# ------------------------------------------------------------
# 8. RESUMEN EN TABLA
# ------------------------------------------------------------
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("📋 6. RESUMEN DE TODOS LOS ÍNDICES\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

resumen <- data.frame(
  `Tipo de Índice` = c(
    'Valor',
    'Laspeyres Precios',
    'Paasche Precios', 
    'Fisher Precios',
    'Laspeyres Cantidad',
    'Paasche Cantidad',
    'Fisher Cantidad'
  ),
  Valor = c(
    sprintf("%.2f", indice_valor),
    sprintf("%.2f", laspeyres_precios),
    sprintf("%.2f", paasche_precios),
    sprintf("%.2f", fisher_precios),
    sprintf("%.2f", laspeyres_cantidad),
    sprintf("%.2f", paasche_cantidad),
    sprintf("%.2f", fisher_cantidad)
  ),
  Interpretación = c(
    sprintf("El gasto total ha variado un %.1f%%", indice_valor - 100),
    sprintf("La cesta base cuesta un %.1f%% más", laspeyres_precios - 100),
    sprintf("La cesta actual cuesta un %.1f%% más", paasche_precios - 100),
    sprintf("Media no sesgada: %.1f%%", fisher_precios - 100),
    sprintf("Las cantidades (a precios base) variaron %.1f%%", laspeyres_cantidad - 100),
    sprintf("Las cantidades (a precios actuales) variaron %.1f%%", paasche_cantidad - 100),
    sprintf("Media no sesgada de cantidad: %.1f%%", fisher_cantidad - 100)
  )
)

print(kable(resumen, format = "simple", col.names = c("Tipo de Índice", "Valor", "Interpretación")))

# ------------------------------------------------------------
# 9. VISUALIZACIÓN GRÁFICA
# ------------------------------------------------------------
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("📈 7. VISUALIZACIÓN GRÁFICA\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

# Preparar datos para ggplot
indices_plot <- data.frame(
  Tipo = rep(c("Laspeyres", "Paasche", "Fisher"), 2),
  Medicion = c(rep("Precios", 3), rep("Cantidad", 3)),
  Valor = c(laspeyres_precios, paasche_precios, fisher_precios,
            laspeyres_cantidad, paasche_cantidad, fisher_cantidad)
)

# Gráfico de barras con ggplot2
p <- ggplot(indices_plot, aes(x = Tipo, y = Valor, fill = Medicion)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red", size = 0.8) +
  geom_text(aes(label = sprintf("%.1f", Valor)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  labs(title = "Comparación de Índices de Precios y Cantidad",
       subtitle = "Base 2020 = 100",
       x = "", y = "Valor del Índice") +
  scale_fill_manual(values = c("Precios" = "#FF9999", "Cantidad" = "#66B3FF")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Guardar el gráfico
ggsave("indices_comparacion_R.png", p, width = 10, height = 6, dpi = 100)
cat("✅ Gráfico guardado como 'indices_comparacion_R.png'\n")

# Mostrar el gráfico (en RStudio se muestra automáticamente)
print(p)

# ------------------------------------------------------------
# 10. EJEMPLO IPC SIMPLIFICADO (con ponderaciones)
# ------------------------------------------------------------
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("🏠 8. EJEMPLO SIMPLIFICADO DEL IPC (con ponderaciones)\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

# Simulamos ponderaciones típicas (basadas en la conversación anterior)
ponderaciones <- c(
  'Pan' = 0.17,      # 17% Alimentos
  'Leche' = 0.17,     # 17% Restauración/hoteles (aproximamos)
  'Huevos' = 0.15     # 15% Transporte (simplificamos)
)

# Normalizamos para que sumen 1
ponderaciones <- ponderaciones / sum(ponderaciones)

cat("\n📊 Ponderaciones normalizadas (simuladas):\n")
for(i in 1:length(ponderaciones)) {
  cat(sprintf("   %s: %.3f (%.1f%%)\n", names(ponderaciones)[i], 
              ponderaciones[i], ponderaciones[i] * 100))
}

# IPC como Laspeyres con ponderaciones
ipc <- 0
for(i in 1:nrow(datos)) {
  prod <- datos$Producto[i]
  indice_simple <- datos$Indice_precio_simple[i]
  ipc <- ipc + ponderaciones[prod] * indice_simple
}

cat("\n📌 IPC = ∑ (ponderación_i × índice_simple_precio_i)\n")
cat(sprintf("   IPC = %.2f\n", ipc))
cat(sprintf("   ▶️ Interpretación: Los precios han subido un %.1f%% de media ponderada\n", ipc - 100))

# ------------------------------------------------------------
# 11. COMPARATIVA FINAL
# ------------------------------------------------------------
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("🎯 COMPARATIVA: Índice de Valor vs IPC vs Fisher\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

cat("\n📊 RESULTADOS CLAVE:\n")
cat(sprintf("   • Índice de Valor (gasto total): %.2f → +%.1f%%\n", indice_valor, indice_valor - 100))
cat(sprintf("   • IPC ponderado (solo precios):  %.2f → +%.1f%%\n", ipc, ipc - 100))
cat(sprintf("   • Fisher precios (ideal):        %.2f → +%.1f%%\n", fisher_precios, fisher_precios - 100))

cat("\n💡 INTERPRETACIÓN:\n")
cat("   El gasto total (valor) ha subido más que los precios porque también\n")
cat("   han cambiado las cantidades consumidas. El IPC mide SOLO el efecto\n")
cat("   precios, manteniendo fija la cesta (Laspeyres). El Fisher corrige\n")
cat("   el sesgo de sustitución.\n")

cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("✅ FIN DEL EJEMPLO\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

# ------------------------------------------------------------
# EXTRA: Función para calcular todos los índices automáticamente
# ------------------------------------------------------------
calcular_todos_indices <- function(precios_base, cantidades_base, 
                                   precios_actual, cantidades_actual) {
  # Cálculos básicos
  total_base <- sum(precios_base * cantidades_base)
  total_actual <- sum(precios_actual * cantidades_actual)
  total_paq0 <- sum(precios_actual * cantidades_base)
  total_p0qa <- sum(precios_base * cantidades_actual)
  
  # Índices
  list(
    Laspeyres_Precios = (total_paq0 / total_base) * 100,
    Paasche_Precios = (total_actual / total_p0qa) * 100,
    Fisher_Precios = sqrt(((total_paq0 / total_base) * 100) * 
                            ((total_actual / total_p0qa) * 100)),
    Laspeyres_Cantidad = (total_p0qa / total_base) * 100,
    Paasche_Cantidad = (total_actual / total_paq0) * 100,
    Fisher_Cantidad = sqrt(((total_p0qa / total_base) * 100) * 
                             ((total_actual / total_paq0) * 100)),
    Valor = (total_actual / total_base) * 100
  )
}

# Ejemplo de uso de la función
cat("\n📦 EJEMPLO DE USO DE LA FUNCIÓN calular_todos_indices():\n")
resultado_funcion <- calcular_todos_indices(
  precios_base = datos$Precio_base,
  cantidades_base = datos$Cantidad_base,
  precios_actual = datos$Precio_actual,
  cantidades_actual = datos$Cantidad_actual
)

print(str(resultado_funcion))