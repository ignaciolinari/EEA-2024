rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

# Leer los datos
soja_data <- read.csv("/Users/ignacio/Downloads/soja_total_serie.csv", encoding = "latin1")

#####################

# Estadística descriptiva básica
library(dplyr)
library(ggplot2)
library(tidyr)

# Resumen numérico de las variables cuantitativas
summary(select(soja_data, where(is.numeric)))

# Verificar valores ausentes
colSums(is.na(soja_data))

# Visualización: distribuciones de las variables numéricas
soja_data %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = Valor)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribuciones de Variables Numéricas", x = "", y = "Frecuencia")

# Resumen de variables categóricas
soja_data %>%
  select(where(is.character)) %>%
  summarise(across(everything(), ~ length(unique(.)))) %>%
  t() %>%
  data.frame() %>%
  setNames(c("Niveles Únicos"))

# Q-Q plots para todas las variables cuantitativas
soja_data %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(sample = Valor)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Q-Q Plots de las Variables Cuantitativas")

# Pruebas de normalidad para cada variable numérica (submuestreo de 5000 observaciones para validez de test)
soja_data %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ shapiro.test(sample(., size = min(length(.), 5000)))$p.value)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "p_value") %>%
  mutate(Normalidad = ifelse(p_value > 0.05, "Sí", "No"))

# Crear boxplot por provincia para identificar outliers en la variable "produccion_tm"
ggplot(soja_data, aes(x = provincia_nombre, y = produccion_tm)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  theme_minimal() +
  labs(title = "Distribución de la Producción por Provincia",
       x = "Provincia", 
       y = "Producción Total (tm)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Para que los nombres de las provincias sean legibles

#####################

# Dividir los datos en entrenamiento (70%) y prueba (30%)
set.seed(123)  # Para que los resultados sean reproducibles
indice_entrenamiento <- sample(1:nrow(soja_data), size = 0.7 * nrow(soja_data))
soja_entrenamiento <- soja_data[indice_entrenamiento, ]
soja_prueba <- soja_data[-indice_entrenamiento, ]

# Verificación de la distribución de los datos
ggplot(soja_entrenamiento, aes(x = produccion_tm)) +
  geom_histogram(fill = "steelblue", color = "black", alpha = 0.7) +
  ggtitle("Distribución de la Producción en el Conjunto de Entrenamiento")

ggplot(soja_prueba, aes(x = produccion_tm)) +
  geom_histogram(fill = "darkorange", color = "black", alpha = 0.7) +
  ggtitle("Distribución de la Producción en el Conjunto de Prueba")

#####################

# Se procede con el modelo lineal mixto robusto

# Cargar la librería
library(robustlmm)

# Ajustar un modelo lineal mixto robusto con los datos de entrenamiento
modelo_robusto <- rlmer(produccion_tm ~ superficie_cosechada_ha + anio + cultivo_nombre +
                          (1 | provincia_nombre/departamento_nombre),
                        data = soja_entrenamiento)

# Guardar el modelo ajustado en un archivo .RData para no tener que correrlo siempre
save(modelo_robusto, file = "/Users/ignacio/Downloads/modelo_robusto.RData")

###################

# Cargar el modelo desde el archivo .RData
load("/Users/ignacio/Downloads/modelo_robusto.RData")

# Resumen del modelo robusto
summary(modelo_robusto)

# Gráfico de los efectos fijos

# Coeficientes de efectos fijos
coef_fijos <- summary(modelo_robusto)$coefficients

# Crear un gráfico de los efectos fijos
coef_fijos_df <- data.frame(
  Variable = rownames(coef_fijos),
  Estimate = coef_fijos[, "Estimate"],
  StdError = coef_fijos[, "Std. Error"]
)

ggplot(coef_fijos_df, aes(x = Variable, y = Estimate, ymin = Estimate - StdError, ymax = Estimate + StdError)) +
  geom_pointrange() +
  theme_minimal() +
  labs(title = "Efectos Fijos", y = "Estimación de Coeficiente", x = "Variable") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Efectos aleatorios
ranef(modelo_robusto)

# Extraer los efectos aleatorios por provincia para visualizar
efectos_provincia <- ranef(modelo_robusto)$provincia_nombre
efectos_provincia_df <- data.frame(
  provincia = rownames(efectos_provincia),
  intercepto = efectos_provincia[, "(Intercept)"]
)

# Ordenar las provincias por el valor del intercepto
efectos_provincia_df <- efectos_provincia_df[order(efectos_provincia_df$intercepto), ]

# Crear el gráfico
ggplot(efectos_provincia_df, aes(x = reorder(provincia, intercepto), y = intercepto)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Para que las provincias aparezcan en el eje y
  labs(
    title = "Efectos Aleatorios por Provincia",
    x = "Provincia",
    y = "Intercepto"
  ) +
  theme_minimal()

################################

# Comparo con el modelo no robusto

# Cargar la librería lme4
library(lme4)

# Ajustar el modelo lineal mixto estándar con los datos de entrenamiento
modelo_estandar <- lmer(produccion_tm ~ superficie_cosechada_ha + anio + cultivo_nombre +
                          (1 | provincia_nombre/departamento_nombre),
                        data = soja_entrenamiento)

# Resumen del modelo estándar
summary(modelo_estandar)

# Extraer coeficientes de efectos fijos del modelo estándar
coef_estandar <- summary(modelo_estandar)$coefficients

# Unir los coeficientes de ambos modelos en un dataframe
comparacion_coef <- data.frame(
  Variable = rownames(coef_fijos),
  Estimacion_Robusta = coef_fijos[, "Estimate"],
  Estimacion_Estandar = coef_estandar[, "Estimate"]
)

# Visualizar la comparación
library(reshape2)
comparacion_long <- melt(comparacion_coef, id.vars = "Variable")

ggplot(comparacion_long, aes(x = Variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparación de Efectos Fijos: Robusto vs Estándar",
       y = "Estimación de Coeficiente", x = "Variable") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("Robusto", "Estándar"))

# Efectos aleatorios del modelo estándar
efectos_provincia_estandar <- ranef(modelo_estandar)$provincia_nombre
efectos_comparacion <- data.frame(
  provincia = rownames(efectos_provincia),
  Intercepto_Robusto = efectos_provincia[, "(Intercept)"],
  Intercepto_Estandar = efectos_provincia_estandar[, "(Intercept)"]
)

# Visualizar la comparación con un gráfico
efectos_comparacion %>%
  pivot_longer(cols = c(Intercepto_Robusto, Intercepto_Estandar), names_to = "Modelo", values_to = "Valor") %>%
  ggplot(aes(x = reorder(provincia, Valor), y = Valor, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Comparación de Efectos Aleatorios: Robusto vs Estándar",
    x = "Provincia", y = "Intercepto"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("Robusto", "Estándar"))

# Residuos de ambos modelos
residuos_robusto <- residuals(modelo_robusto)
residuos_estandar <- residuals(modelo_estandar)

# Visualización de los residuos
residuos_df <- data.frame(
  Residuos = c(residuos_robusto, residuos_estandar),
  Modelo = rep(c("Robusto", "Estándar"), each = length(residuos_robusto))
)

ggplot(residuos_df, aes(x = Residuos, fill = Modelo)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  theme_minimal() +
  labs(title = "Distribución de Residuos: Robusto vs Estándar", x = "Residuos", y = "Frecuencia") +
  scale_fill_manual(values = c("steelblue",  "darkorange"))

# Predicción y evaluación del modelo en los datos de prueba

# Realizar predicciones con el modelo robusto en el conjunto de prueba
predicciones_robusto <- predict(modelo_robusto, newdata = soja_prueba)

# Realizar predicciones con el modelo estándar en el conjunto de prueba
predicciones_estandar <- predict(modelo_estandar, newdata = soja_prueba)

# Calcular el error cuadrático medio (RMSE) para ambos modelos
rmse_robusto <- sqrt(mean((soja_prueba$produccion_tm - predicciones_robusto)^2))
rmse_estandar <- sqrt(mean((soja_prueba$produccion_tm - predicciones_estandar)^2))

# Calcular el MAE (Mean Absolute Error)
mae_robusto <- mean(abs(soja_prueba$produccion_tm - predicciones_robusto))
mae_estandar <- mean(abs(soja_prueba$produccion_tm - predicciones_estandar))

# Calcular el MSE (Mean Squared Error)
mse_robusto <- mean((soja_prueba$produccion_tm - predicciones_robusto)^2)
mse_estandar <- mean((soja_prueba$produccion_tm - predicciones_estandar)^2)

# Calcular el R² (Coeficiente de determinación)
r2_robusto <- 1 - sum((soja_prueba$produccion_tm - predicciones_robusto)^2) / sum((soja_prueba$produccion_tm - mean(soja_prueba$produccion_tm))^2)
r2_estandar <- 1 - sum((soja_prueba$produccion_tm - predicciones_estandar)^2) / sum((soja_prueba$produccion_tm - mean(soja_prueba$produccion_tm))^2)


# Mostrar los resultados
cat("RMSE del modelo robusto:", rmse_robusto, "\n")
cat("RMSE del modelo estándar:", rmse_estandar, "\n")
cat("MAE del modelo robusto:", mae_robusto, "\n")
cat("MAE del modelo estándar:", mae_estandar, "\n")
cat("MSE del modelo robusto:", mse_robusto, "\n")
cat("MSE del modelo estándar:", mse_estandar, "\n")
cat("R² del modelo robusto:", r2_robusto, "\n")
cat("R² del modelo estándar:", r2_estandar, "\n")

# Graficar las predicciones vs los valores reales
predicciones_df <- data.frame(
  Real = soja_prueba$produccion_tm,
  Prediccion_Robusta = predicciones_robusto,
  Prediccion_Estandar = predicciones_estandar
)

ggplot(predicciones_df, aes(x = Real)) +
  geom_point(aes(y = Prediccion_Robusta), color = "steelblue", alpha = 0.5) +
  geom_point(aes(y = Prediccion_Estandar), color = "darkorange", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Predicciones vs Valores Reales: Robusto vs Estándar",
    x = "Valores Reales",
    y = "Predicciones"
  )

# Comparar los residuos de las predicciones con los valores reales
residuos_robusto_test <- soja_prueba$produccion_tm - predicciones_robusto
residuos_estandar_test <- soja_prueba$produccion_tm - predicciones_estandar

# Crear un gráfico de los residuos
residuos_test_df <- data.frame(
  Residuos = c(residuos_robusto_test, residuos_estandar_test),
  Modelo = rep(c("Robusto", "Estándar"), each = length(residuos_robusto_test))
)

ggplot(residuos_test_df, aes(x = Residuos, fill = Modelo)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  theme_minimal() +
  labs(title = "Distribución de Residuos en el Conjunto de Prueba: Robusto vs Estándar", 
       x = "Residuos", y = "Frecuencia") +
  scale_fill_manual(values = c("steelblue", "darkorange"))

######################

# Ajustar un modelo lineal simple
modelo_lineal_simple <- lm(produccion_tm ~ superficie_cosechada_ha, data = soja_entrenamiento)

# Resumen del modelo lineal simple
summary(modelo_lineal_simple)

# Realizar predicciones con el modelo lineal simple en el conjunto de prueba
predicciones_lineal_simple <- predict(modelo_lineal_simple, newdata = soja_prueba)

# Calcular el RMSE (Root Mean Squared Error)
rmse_lineal_simple <- sqrt(mean((soja_prueba$produccion_tm - predicciones_lineal_simple)^2))

# Calcular el MAE (Mean Absolute Error)
mae_lineal_simple <- mean(abs(soja_prueba$produccion_tm - predicciones_lineal_simple))

# Calcular el MSE (Mean Squared Error)
mse_lineal_simple <- mean((soja_prueba$produccion_tm - predicciones_lineal_simple)^2)

# Calcular el R² (Coeficiente de determinación)
r2_lineal_simple <- 1 - sum((soja_prueba$produccion_tm - predicciones_lineal_simple)^2) / sum((soja_prueba$produccion_tm - mean(soja_prueba$produccion_tm))^2)

# Mostrar los resultados
cat("RMSE del modelo lineal simple:", rmse_lineal_simple, "\n")
cat("MAE del modelo lineal simple:", mae_lineal_simple, "\n")
cat("MSE del modelo lineal simple:", mse_lineal_simple, "\n")
cat("R² del modelo lineal simple:", r2_lineal_simple, "\n")

# Graficar las predicciones vs los valores reales del modelo lineal simple
predicciones_lineal_df <- data.frame(
  Real = soja_prueba$produccion_tm,
  Prediccion_Lineal_Simple = predicciones_lineal_simple
)

ggplot(predicciones_lineal_df, aes(x = Real)) +
  geom_point(aes(y = Prediccion_Lineal_Simple), color = "green", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Predicciones vs Valores Reales: Modelo Lineal Simple",
    x = "Valores Reales",
    y = "Predicciones"
  )

# Comparar los residuos del modelo lineal simple con los valores reales
residuos_lineal_simple_test <- soja_prueba$produccion_tm - predicciones_lineal_simple

# Crear un gráfico de los residuos
residuos_lineal_simple_df <- data.frame(
  Residuos = residuos_lineal_simple_test
)

ggplot(residuos_lineal_simple_df, aes(x = Residuos)) +
  geom_histogram(bins = 30, alpha = 0.7, fill = "green") +
  theme_minimal() +
  labs(title = "Distribución de Residuos del Modelo Lineal Simple", 
       x = "Residuos", y = "Frecuencia")

#################

library(broom)

# Ajustar el modelo lineal
modelo_lineal <- lm(produccion_tm ~ superficie_cosechada_ha, data = soja_data)

# Extraer el R²
r2 <- summary(modelo_lineal)$r.squared

# Crear el scatter plot con la regresión lineal y R²
ggplot(soja_data, aes(x = superficie_cosechada_ha, y = produccion_tm)) +
  geom_point(color = "steelblue", alpha = 0.7) +  # Puntos del scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "lightgray") +  # Línea de regresión con intervalo de confianza
  theme_minimal() +
  labs(title = paste("Producción en Función de la Superficie Cosechada (R² = ", round(r2, 3), ")", sep = ""),
       x = "Superficie Cosechada (ha)", 
       y = "Producción Total (tm)") 

###################

# Ajustar un modelo lineal por provincia
modelos_por_provincia <- soja_data %>%
  group_by(provincia_nombre) %>%   # Agrupar por provincia
  do(modelo = lm(produccion_tm ~ superficie_cosechada_ha, data = .)) # Ajustar el modelo

# Ver el resumen de los modelos ajustados por provincia
resultados_modelos <- modelos_por_provincia %>%
  summarise(
    provincia = provincia_nombre,
    coef_intercepto = coef(modelo)[1],
    coef_superficie = coef(modelo)[2],
    r2 = summary(modelo)$r.squared
  )

# Visualización de las rectas de regresión para cada provincia con colores por provincia
ggplot(soja_data, aes(x = superficie_cosechada_ha, y = produccion_tm, color = provincia_nombre)) +
  geom_point(alpha = 0.7) +  # Puntos del scatter plot
  geom_smooth(method = "lm", se = FALSE, aes(group = provincia_nombre)) + # Línea de regresión por provincia
  theme_minimal() +
  labs(title = "Producción en Función de Superficie Cosechada por Provincia",
       x = "Superficie Cosechada (ha)", 
       y = "Producción Total (tm)") +
  theme(legend.position = "right")  # Mostrar la leyenda de provincias
