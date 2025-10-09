# Cargar librerías necesarias
library(tidyverse)
library(caret)
library(cluster)
library(factoextra)
library(mice)  # Para imputación de valores faltantes
library(pROC)  # Para curva ROC

# Cargar los datos
framingham_data <- read.csv("C:/Users/USUARIO/Dropbox/Mi PC (i3)/Desktop/dia_300925 (2)/framingham (2).csv", na.strings = c("NA", ""))

# Exploración inicial de los datos
str(framingham_data)
summary(framingham_data)

# 1. PREPROCESAMIENTO DE DATOS

# Verificar valores faltantes
missing_values <- colSums(is.na(framingham_data))
print(missing_values)

# Opción A: Eliminar filas con NA (más simple)
data_clean <- na.omit(framingham_data)

# Opción B: Imputación de valores (comentada por ahora)
# set.seed(123)
# data_imputed <- mice(framingham_data, m = 1, maxit = 50, method = 'pmm', seed = 123)
# data_clean <- complete(data_imputed, 1)

# Convertir variables categóricas a factor
data_clean <- data_clean %>%
  mutate(
    male = as.factor(male),
    currentSmoker = as.factor(currentSmoker),
    BPMeds = as.factor(BPMeds),
    prevalentStroke = as.factor(prevalentStroke),
    prevalentHyp = as.factor(prevalentHyp),
    diabetes = as.factor(diabetes),
    TenYearCHD = as.factor(TenYearCHD)
  )

# 2. REGRESIÓN LOGÍSTICA

# Dividir en conjunto de entrenamiento y prueba
set.seed(123)
train_index <- createDataPartition(data_clean$TenYearCHD, p = 0.7, list = FALSE)
train_data <- data_clean[train_index, ]
test_data <- data_clean[-train_index, ]

# Entrenar modelo de regresión logística
logit_model <- glm(TenYearCHD ~ ., 
                   data = train_data, 
                   family = binomial(link = "logit"))

# Resumen del modelo
summary(logit_model)

# Predicciones en el conjunto de prueba
predictions_prob <- predict(logit_model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predictions_prob > 0.5, 1, 0)

# Matriz de confusión
conf_matrix <- table(Predicho = predicted_classes, 
                     Real = test_data$TenYearCHD)
print("Matriz de Confusión:")
print(conf_matrix)

# Métricas de evaluación
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("\nMétricas de Evaluación:\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1-Score:", round(f1_score, 3), "\n")

# Curva ROC
roc_obj <- roc(as.numeric(as.character(test_data$TenYearCHD)), predictions_prob)
plot(roc_obj, main = "Curva ROC - Regresión Logística")
auc_value <- auc(roc_obj)
cat("AUC:", round(auc_value, 3), "\n")

# 3. CLUSTERING (K-MEANS)

# Seleccionar variables numéricas para clustering
numeric_vars <- c("age", "totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")
clustering_data <- data_clean[, numeric_vars]

# Eliminar filas con NA en variables de clustering
clustering_data <- na.omit(clustering_data)

# Estandarizar las variables
scaled_data <- scale(clustering_data)

# Determinar número óptimo de clusters (método del codo)
set.seed(123)
wss <- numeric(10)
for (i in 1:10) {
  kmeans_temp <- kmeans(scaled_data, centers = i, nstart = 25)
  wss[i] <- kmeans_temp$tot.withinss
}

# Graficar método del codo
plot(1:10, wss, type = "b", 
     xlab = "Número de clusters", 
     ylab = "Suma de cuadrados within",
     main = "Método del Codo")

# Aplicar K-means con k seleccionado (ej: 3 clusters)
k <- 3
kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)

# Añadir asignación de clusters al dataset
# Primero nos aseguramos de que los índices coincidan
cluster_data <- data_clean[rownames(clustering_data), ]
cluster_data$cluster_kmeans <- as.factor(kmeans_result$cluster)

# Visualización de clusters
fviz_cluster(kmeans_result, data = scaled_data, 
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "Clustering K-means")

# Características de cada cluster
cluster_summary <- cluster_data %>%
  group_by(cluster_kmeans) %>%
  summarise(
    n = n(),
    avg_age = mean(age),
    avg_chol = mean(totChol, na.rm = TRUE),
    avg_sysBP = mean(sysBP),
    avg_BMI = mean(BMI),
    chd_rate = mean(as.numeric(as.character(TenYearCHD))) * 100
  ) %>%
  mutate(prop = n / sum(n) * 100)

print("Resumen por Cluster K-means:")
print(cluster_summary)

# 4. CLUSTERING JERÁRQUICO

# Calcular matriz de distancias
dist_matrix <- dist(scaled_data, method = "euclidean")

# Clustering jerárquico
hc_result <- hclust(dist_matrix, method = "ward.D2")

# Dendrograma
plot(hc_result, cex = 0.6, hang = -1, 
     main = "Dendrograma - Clustering Jerárquico",
     xlab = "Observaciones", ylab = "Distancia")

# Cortar dendrograma para obtener clusters
hc_clusters <- cutree(hc_result, k = k)

# Añadir clusters jerárquicos al dataset
cluster_data$cluster_hierarchical <- as.factor(hc_clusters)

# Comparar distribución de TenYearCHD entre clusters
chd_by_cluster <- cluster_data %>%
  group_by(cluster_kmeans) %>%
  summarise(
    total = n(),
    chd_cases = sum(as.numeric(as.character(TenYearCHD))),
    chd_rate = round(chd_cases / total * 100, 1)
  )

print("Tasa de CHD por Cluster:")
print(chd_by_cluster)

# Visualización de variables por cluster
# Edad por cluster
p1 <- ggplot(cluster_data, aes(x = cluster_kmeans, y = age, fill = cluster_kmeans)) +
  geom_boxplot() +
  labs(title = "Distribución de Edad por Cluster", 
       x = "Cluster", y = "Edad") +
  theme_minimal() +
  theme(legend.position = "none")

# Presión sistólica por cluster
p2 <- ggplot(cluster_data, aes(x = cluster_kmeans, y = sysBP, fill = cluster_kmeans)) +
  geom_boxplot() +
  labs(title = "Distribución de Presión Sistólica por Cluster", 
       x = "Cluster", y = "Presión Sistólica") +
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar gráficos
print(p1)
print(p2)

# 5. ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)

pca_result <- prcomp(scaled_data, scale. = TRUE)

# Visualizar PCA con clusters
fviz_pca_ind(pca_result,
             geom.ind = "point",
             col.ind = cluster_data$cluster_kmeans,
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             legend.title = "Cluster",
             title = "PCA - Visualización de Clusters"
)

# Resumen de varianza explicada
cat("\nResumen PCA:\n")
print(summary(pca_result))

# Varianza explicada por componente
fviz_eig(pca_result, 
         main = "Varianza Explicada por Componentes PCA",
         addlabels = TRUE)

# 6. COMPARACIÓN ENTRE MÉTODOS DE CLUSTERING

# Tabla de contingencia comparando ambos métodos
comparison_table <- table(Kmeans = cluster_data$cluster_kmeans, 
                          Hierarchical = cluster_data$cluster_hierarchical)
print("Comparación entre K-means y Clustering Jerárquico:")
print(comparison_table)

# Guardar resultados
results <- list(
  logistic_model = logit_model,
  kmeans_result = kmeans_result,
  hierarchical_result = hc_result,
  pca_result = pca_result,
  cluster_data = cluster_data,
  metrics = c(accuracy = accuracy, precision = precision, 
              recall = recall, f1_score = f1_score, auc = auc_value)
)

cat("\nAnálisis completado exitosamente!\n")





# Cargar librerías necesarias
library(tidyverse)
library(caret)
library(corrplot)
library(pROC)
library(ggplot2)
library(ggcorrplot)

# Cargar los datos
framingham_data <- read.csv("C:/Users/USUARIO/Dropbox/Mi PC (i3)/Desktop/dia_300925 (2)/framingham (2).csv")

# 1. MATRIZ DE CORRELACIÓN

# Seleccionar solo variables numéricas
numeric_vars <- framingham_data %>%
  select(where(is.numeric)) %>%
  select(-TenYearCHD)  # Excluir la variable objetivo

# Calcular matriz de correlación
correlation_matrix <- cor(numeric_vars, use = "complete.obs")

# Visualizar matriz de correlación - Método 1 (corrplot)
corrplot(correlation_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.8,
         tl.col = "black",
         title = "Matriz de Correlación - Variables Numéricas",
         mar = c(0, 0, 2, 0))

# Visualizar matriz de correlación - Método 2 (ggcorrplot)
ggcorrplot(correlation_matrix,
           method = "circle",
           type = "upper",
           lab = TRUE,
           lab_size = 3,
           title = "Matriz de Correlación - Variables Numéricas",
           ggtheme = theme_minimal())

# Visualizar matriz de correlación - Método 3 (heatmap detallado)
corrplot(correlation_matrix,
         method = "number",
         type = "upper",
         order = "hclust",
         number.cex = 0.7,
         tl.cex = 0.8,
         tl.col = "black",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Matriz de Correlación con Valores",
         mar = c(0, 0, 2, 0))

# Correlaciones más fuertes (abs > 0.5)
strong_correlations <- which(abs(correlation_matrix) > 0.5 & 
                               abs(correlation_matrix) < 1, arr.ind = TRUE)

if(nrow(strong_correlations) > 0) {
  strong_corr_df <- data.frame(
    Variable1 = rownames(correlation_matrix)[strong_correlations[,1]],
    Variable2 = colnames(correlation_matrix)[strong_correlations[,2]],
    Correlacion = correlation_matrix[strong_correlations]
  ) %>%
    arrange(desc(abs(Correlacion)))
  
  print("Correlaciones fuertes (|r| > 0.5):")
  print(strong_corr_df)
} else {
  print("No hay correlaciones fuertes (|r| > 0.5)")
}

# 2. PREPARACIÓN PARA MATRIZ DE CONFUSIÓN

# Preprocesamiento básico
data_clean <- na.omit(framingham_data)

# Convertir variables categóricas
data_clean <- data_clean %>%
  mutate(
    male = as.factor(male),
    currentSmoker = as.factor(currentSmoker),
    BPMeds = as.factor(BPMeds),
    prevalentStroke = as.factor(prevalentStroke),
    prevalentHyp = as.factor(prevalentHyp),
    diabetes = as.factor(diabetes),
    TenYearCHD = as.factor(TenYearCHD)
  )

# Dividir en entrenamiento y prueba
set.seed(123)
train_index <- createDataPartition(data_clean$TenYearCHD, p = 0.7, list = FALSE)
train_data <- data_clean[train_index, ]
test_data <- data_clean[-train_index, ]

# 3. MODELO DE REGRESIÓN LOGÍSTICA Y MATRIZ DE CONFUSIÓN

# Entrenar modelo
logistic_model <- glm(TenYearCHD ~ age + male + sysBP + diaBP + 
                        totChol + BMI + diabetes + prevalentHyp + 
                        currentSmoker + cigsPerDay, 
                      data = train_data, 
                      family = binomial)

# Resumen del modelo
summary(logistic_model)

# Predicciones
predictions_prob <- predict(logistic_model, newdata = test_data, type = "response")
predicted_class <- ifelse(predictions_prob > 0.5, 1, 0)

# Convertir a factores con los mismos niveles
predicted_class <- factor(predicted_class, levels = c(0, 1))
actual_class <- factor(test_data$TenYearCHD, levels = c(0, 1))

# 4. MATRIZ DE CONFUSIÓN BÁSICA
confusion_matrix_basic <- table(Predicted = predicted_class, Actual = actual_class)
print("Matriz de Confusión Básica:")
print(confusion_matrix_basic)

# 5. MATRIZ DE CONFUSIÓN DETALLADA CON CARET
confusion_matrix_detailed <- confusionMatrix(predicted_class, actual_class, positive = "1")
print("Matriz de Confusión Detallada:")
print(confusion_matrix_detailed)

# 6. VISUALIZACIÓN DE MATRIZ DE CONFUSIÓN

# Función para crear matriz de confusión visual
create_confusion_plot <- function(conf_matrix) {
  conf_df <- as.data.frame(conf_matrix)
  colnames(conf_df) <- c("Predicted", "Actual", "Freq")
  
  ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "white", size = 6) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Matriz de Confusión",
         x = "Valor Real",
         y = "Predicción",
         fill = "Frecuencia") +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, hjust = 0.5))
}

# Crear visualización
confusion_plot <- create_confusion_plot(confusion_matrix_basic)
print(confusion_plot)

# 7. MÉTRICAS DETALLADAS

# Extraer métricas de la matriz de confusión
cm <- confusion_matrix_detailed$table
accuracy <- confusion_matrix_detailed$overall['Accuracy']
precision <- confusion_matrix_detailed$byClass['Precision']
recall <- confusion_matrix_detailed$byClass['Recall']
f1 <- confusion_matrix_detailed$byClass['F1']
specificity <- confusion_matrix_detailed$byClass['Specificity']

# Crear tabla de métricas
metrics_df <- data.frame(
  Métrica = c("Accuracy", "Precision", "Recall (Sensitivity)", 
              "Specificity", "F1-Score"),
  Valor = round(c(accuracy, precision, recall, specificity, f1), 4)
)

print("Métricas de Evaluación:")
print(metrics_df)

# 8. CURVA ROC Y AUC
roc_curve <- roc(as.numeric(as.character(actual_class)), predictions_prob)
auc_value <- auc(roc_curve)

# Plot ROC
plot(roc_curve, 
     main = paste("Curva ROC - AUC =", round(auc_value, 3)),
     col = "blue", 
     lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "red")
legend("bottomright", 
       legend = paste("AUC =", round(auc_value, 3)), 
       col = "blue", 
       lwd = 2)

# 9. ANÁLISIS POR CLASES

# Proporción de clases en los datos
class_distribution <- data_clean %>%
  group_by(TenYearCHD) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

print("Distribución de Clases:")
print(class_distribution)

# Visualizar distribución de clases
ggplot(class_distribution, aes(x = TenYearCHD, y = Count, fill = TenYearCHD)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count, "\n(", Percentage, "%)")), 
            vjust = -0.5, size = 4) +
  labs(title = "Distribución de Clases - TenYearCHD",
       x = "Enfermedad Coronaria (10 años)",
       y = "Número de Pacientes") +
  theme_minimal() +
  theme(legend.position = "none")

# 10. MATRIZ DE CONFUSIÓN NORMALIZADA

# Matriz de confusión normalizada por filas (por clase real)
conf_matrix_normalized <- prop.table(confusion_matrix_basic, margin = 2) * 100

print("Matriz de Confusión Normalizada (% por clase real):")
print(round(conf_matrix_normalized, 1))

# Visualizar matriz normalizada
conf_norm_df <- as.data.frame(conf_matrix_normalized)
colnames(conf_norm_df) <- c("Predicted", "Actual", "Percentage")

ggplot(conf_norm_df, aes(x = Actual, y = Predicted, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Matriz de Confusión Normalizada (%)",
       x = "Valor Real",
       y = "Predicción",
       fill = "Porcentaje") +
  theme_minimal()

# 11. REPORTE COMPLETO
cat("\n=== REPORTE COMPLETO DEL MODELO ===\n")
cat("Tamaño del conjunto de entrenamiento:", nrow(train_data), "\n")
cat("Tamaño del conjunto de prueba:", nrow(test_data), "\n")
cat("Proporción de clases positivas en entrenamiento:", 
    round(mean(as.numeric(as.character(train_data$TenYearCHD))) * 100, 1), "%\n")
cat("Proporción de clases positivas en prueba:", 
    round(mean(as.numeric(as.character(test_data$TenYearCHD))) * 100, 1), "%\n")
cat("AUC del modelo:", round(auc_value, 3), "\n\n")

print("Matriz de Confusión:")
print(confusion_matrix_basic)
cat("\n")
print(metrics_df)