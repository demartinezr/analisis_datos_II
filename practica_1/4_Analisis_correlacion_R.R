# UNIVERSIDAD DE NARIÑO
# FACULTAD DE CIENCIAS EXACTAS Y NATURALES
# PROGRAMA DE BIOLOGÍA
# Asignatura: Análisis de datos II
# Profesor: Jhon Jairo Calderón – Diego E. Martinez Revelo
#
# Análisis de correlación
#
# librerias
  library(nortest)
  #
# Correlación simple
    #
    #Cargar el dataset iris
    data(iris)
    #
    # Seleccionar las variables numéricas de interés (Sepal.Length y Sepal.Width)
    x <- iris$Sepal.Length
    y <- iris$Sepal.Width
    #
  # 1. Gráfico de puntos para visualizar el comportamiento de cada variable
    par(mfrow = c(1, 2), mar=c(4,4,2,2)) 
    plot(x, main = "Gráfico de puntos: Sepal.Length", ylab = "Sepal Length", 
       xlab = "Índice", pch = 19, col = "blue") # Gráfico de puntos de Sepal.Length
    plot(y, main = "Gráfico de puntos: Sepal.Width", ylab = "Sepal Width", 
       xlab = "Índice", pch = 19, col = "green")   # Gráfico de puntos de Sepal.Width
    #
  # 2. Evaluación de la normalidad de Sepal.Length y Sepal.Width
    #
    # Histogramas
    #
    par(mfrow = c(1, 2), mar=c(4,4,2,2))
    hist(x, probability = TRUE, main = "Histograma de Sepal.Length con Distribución Normal",
       xlab = "Sepal.Length", col = "lightblue", border = "black")
    curve(dnorm(x, mean = mean(x), sd = sd(x)), from = min(x), to = max(x), 
        add = TRUE, col = "blue", lwd = 2)
    hist(y, probability = TRUE, main = "Histograma de Sepal.Width con Distribución Normal",
       xlab = "Sepal.Width", col = "lightgreen", border = "black")
    curve(dnorm(x, mean = mean(y), sd = sd(y)), from = min(y), to = max(y), 
          add = TRUE, col = "blue", lwd = 2)
    # QQ-plots
    par(mfrow = c(1, 2), mar=c(4,4,2,2))
    qqnorm(x, main = "QQ-plot de Sepal.Length")
    qqline(x, col = "red")
    qqnorm(y, main = "QQ-plot de Sepal.Width")
    qqline(y, col = "red")
    #
  # Pruebas de normalidad
    shapiro.test(x)
    shapiro.test(y)
    ad.test(x)
    ad.test(y)
    #
    # Interprete los resultados de las graficas y pruebas: ¿Los datos son normales?
    #
  # 3. Correlación de Pearson y Spearman para Sepal.Length y Sepal.Width
    #
    pearson_corr <- cor(x, y, method = "pearson")
    cat("Correlación de Pearson: ", pearson_corr, "\n")
    r_squared_pearson <- pearson_corr^2
    print(paste("R^2 para Pearson:", r_squared_pearson))
    # Interpretación:
    # La correlación de Pearson mide la relación lineal entre las variables.
    # - r = 1: Relación lineal positiva perfecta.
    # - r = -1: Relación lineal negativa perfecta.
    # - r = 0: Sin relación lineal.
    # - 0.3 - 0.7: Correlación positiva moderada.
    # - 0.7 - 1: Correlación positiva fuerte.
    # - -0.3 - -0.7: Correlación negativa moderada.
    # - -0.7 - -1: Correlación negativa fuerte.
    spearman_corr <- cor(x, y, method = "spearman")
    cat("Correlación de Spearman: ", spearman_corr, "\n")
    r_squared_spearman <- spearman_corr^2
    print(paste("R^2 para Spearman:", r_squared_spearman))
    # Interpretación:
    # La correlación de Spearman mide la relación monotónica basada en rangos.
    # - ρ = 1: Relación monotónica positiva perfecta.
    # - ρ = -1: Relación monotónica negativa perfecta.
    # - ρ = 0: Sin relación monotónica.
    # - 0.3 - 0.7: Correlación positiva moderada en rangos.
    # - 0.7 - 1: Correlación positiva fuerte en rangos.
    # - -0.3 - -0.7: Correlación negativa moderada en rangos.
    # - -0.7 - -1: Correlación negativa fuerte en rangos.
    #
  # Gráfico de dispersión entre las dos variables
    plot(x, y, main = "Gráfico de Dispersión: Sepal.Length vs Sepal.Width",
    xlab = "Sepal Length", ylab = "Sepal Width", pch = 19, col = "blue")
    abline(lm(y ~ x), col = "red", lwd = 2)  # Línea de regresión ajustada
    #
    # Interpretación: El gráfico de dispersión muestra la relación entre 
    #Sepal.Length y Sepal.Width. La línea de regresión muestra la tendencia lineal
    # entre las dos variables.
    # Que interpretación puede hacer de la figura: Hay correlación?
    #
  # 4. Sensibilidad de la correlación
      #
    # Evaluación de la sensibilidad de la correlación a la escala de las variables
      #
      # Ajustar escala de Sepal.Width y observar la correlación
      y_scaled <- y * 10  # Cambiar la escala de Sepal.Width
      x_scaled <- log10(x)
      pearson_corr_scaled <- cor(x_scaled, y_scaled, method = "pearson")
      cat("Correlación de Pearson con escala cambiada en Sepal.Width: ", pearson_corr_scaled, "\n")
      r_pearson_corr_scaled <- pearson_corr_scaled^2
      print(paste("R^2 para Pearson scaled:", r_squared_pearson))
      # Interpretación: La correlación no debería cambiar al ajustar la escala de
      # las variables. Esto verifica que la correlación de Pearson es insensible a 
      # la escala de las variables.
      #
      # Gráfico de dispersión con la variable escalada
      par(mfrow = c(1, 2), mar=c(4,4,2,2))
      plot(x, y, main = "Gráfico de Dispersión: Sepal.Length vs Sepal.Width",
           xlab = "Sepal Length", ylab = "Sepal Width", pch = 19, col = "blue")
      abline(lm(y ~ x), col = "red", lwd = 2)  # Línea de regresión ajustada
      plot(x_scaled, y_scaled, main = "Dispersión: Sepal.Length vs Sepal.Width (Escalada)", 
          xlab = "Sepal Length", ylab = "Sepal Width Escalada", pch = 19, col = "purple")
      abline(lm(y_scaled ~ x_scaled), col = "red", lwd = 2)  # Línea de regresión ajustada
      #
    # Sensibilidad al tamaño de muestra: usar una muestra más pequeña
      sample_size <- 30  # Tomar una muestra aleatoria
      x_sample <- sample(x, sample_size)
      y_sample <- sample(y, sample_size)
      pearson_corr_sample <- cor(x_sample, y_sample, method = "pearson")
      cat("Correlación de Pearson con muestra pequeña: ", pearson_corr_sample, "\n")
      r_pearson_corr_sample <- pearson_corr_sample^2
      print(paste("R^2 para Pearson sample:", r_squared_pearson))
      # Interpretación: La correlación puede variar con diferentes tamaños de muestra. 
      # Evaluar cómo cambia la correlación con una muestra pequeña ayuda a entender 
      # la estabilidad de la correlación con respecto al tamaño de muestra.
      #
      # Gráfico de dispersión con muestra pequeña
      par(mfrow = c(1, 2), mar=c(4,4,2,2))
      plot(x, y, main = "Gráfico de Dispersión: Sepal.Length vs Sepal.Width",
           xlab = "Sepal Length", ylab = "Sepal Width", pch = 19, col = "blue")
      abline(lm(y ~ x), col = "red", lwd = 2)  # Línea de regresión ajustada
      plot(x_sample, y_sample, main = "Dispersión: Muestra Pequeña de Sepal.Length vs Sepal.Width",
         xlab = "Sepal Length", ylab = "Sepal Width", pch = 19, col = "darkgreen")
      abline(lm(y_sample ~ x_sample), col = "red", lwd = 2)  # Línea de regresión ajustada
      #
      # Interpretación: Este gráfico muestra la relación en una muestra pequeña, 
      # destacando posibles variaciones en la correlación debido al tamaño 
      # reducido de la muestra.
      #
    # Evaluación de la sensibilidad a la presencia de outliers
      x_outliers <- c(x, 8.5, 9.0, 9.5)  # Añadir valores atípicos en Sepal.Length
      y_outliers <- c(y, 4.5, 5.0, 5.5)
      pearson_corr_outliers <- cor(x_outliers, y_outliers, method = "pearson")
      cat("Correlación de Pearson con outliers: ", pearson_corr_outliers, "\n")
      r_pearson_corr_outliers <- pearson_corr_outliers^2
      print(paste("R^2 para Pearson outliers:", r_squared_pearson))
            # Interpretación: Los outliers pueden influir significativamente en la 
      # correlación. Evaluar cómo cambian los resultados con outliers ayuda a entender 
      # la robustez de la correlación.
      #
      # Gráfico de dispersión con outliers
      #
      par(mfrow = c(1, 2), mar=c(4,4,2,2))
      plot(x, y, main = "Gráfico de Dispersión: Sepal.Length vs Sepal.Width",
           xlab = "Sepal Length", ylab = "Sepal Width", pch = 19, col = "blue")
      abline(lm(y ~ x), col = "red", lwd = 2)  # Línea de regresión ajustada
      plot(x_outliers, y_outliers, main = "Dispersión: Sepal.Length vs Sepal.Width con Outliers",
         xlab = "Sepal Length", ylab = "Sepal Width", pch = 19, col = "orange")
      abline(lm(y_outliers ~ x_outliers), col = "red", lwd = 2)  # Línea de regresión ajustada
      # Interpretación: Este gráfico muestra la relación entre Sepal.Length y 
      # Sepal.Width con outliers. Los outliers pueden distorsionar la visualización 
      # y la correlación calculada.
      #
      #
# Correlación multiple
    #
    # Cargar el dataset iris
    data(iris)
    # Seleccionar las 4 variables numéricas de interés
    variables <- iris[, 1:4]  # Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
    #
  # 1. Graficar las relaciones entre las variables
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
    plot(variables$Sepal.Length, variables$Sepal.Width, main = "Sepal.Length vs Sepal.Width",
           xlab = "Sepal Length", ylab = "Sepal Width", pch = 19, col = "blue")
    plot(variables$Sepal.Length, variables$Petal.Length, main = "Sepal.Length vs Petal.Length",
           xlab = "Sepal Length", ylab = "Petal Length", pch = 19, col = "green")
    plot(variables$Sepal.Length, variables$Petal.Width, main = "Sepal.Length vs Petal.Width",
           xlab = "Sepal Length", ylab = "Petal Width", pch = 19, col = "red")
    plot(variables$Sepal.Width, variables$Petal.Length, main = "Sepal.Width vs Petal.Length",
           xlab = "Sepal Width", ylab = "Petal Length", pch = 19, col = "purple")
    #
  # 2. Evaluación de la normalidad de las variables
    #
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
    hist(variables$Sepal.Length, probability = TRUE, main = "Histograma de Sepal.Length",
         xlab = "Sepal Length", col = "lightblue", border = "black", ylim = c(0, 0.5))
    curve(dnorm(x, mean = mean(variables$Sepal.Length), sd = sd(variables$Sepal.Length)), 
          from = min(variables$Sepal.Length), to = max(variables$Sepal.Length), 
          add = TRUE, col = "blue", lwd = 3)
    hist(variables$Sepal.Width, probability = TRUE, main = "Histograma de Sepal.Width",
         xlab = "Sepal Width", col = "lightgreen", border = "black")
    curve(dnorm(x, mean = mean(variables$Sepal.Width), sd = sd(variables$Sepal.Width)), 
          from = min(variables$Sepal.Width), to = max(variables$Sepal.Width), 
          add = TRUE, col = "blue", lwd = 3)
    hist(variables$Petal.Length, probability = TRUE, main = "Histograma de Petal.Length",
         xlab = "Petal Length", col = "lightcoral", border = "black")
    curve(dnorm(x, mean = mean(variables$Petal.Length), sd = sd(variables$Petal.Length)), 
          from = min(variables$Petal.Length), to = max(variables$Petal.Length), 
          add = TRUE, col = "blue", lwd = 3)
    hist(variables$Petal.Width, probability = TRUE, main = "Histograma de Petal.Width",
         xlab = "Petal Width", col = "lightgoldenrod", border = "black")
    curve(dnorm(x, mean = mean(variables$Petal.Width), sd = sd(variables$Petal.Width)), 
          from = min(variables$Petal.Width), to = max(variables$Petal.Width), 
          add = TRUE, col = "blue", lwd = 3)
    #
    # q-q lots
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
    qqnorm(variables$Sepal.Length, main = "QQ-plot de Sepal.Length")
    qqline(variables$Sepal.Length, col = "red", lwd = 2)
    qqnorm(variables$Sepal.Width, main = "QQ-plot de Sepal.Width")
    qqline(variables$Sepal.Width, col = "red", lwd = 2)
    qqnorm(variables$Petal.Length, main = "QQ-plot de Petal.Length")
    qqline(variables$Petal.Length, col = "red", lwd = 2)
    qqnorm(variables$Petal.Width, main = "QQ-plot de Petal.Width")
    qqline(variables$Petal.Width, col = "red", lwd = 2)
    #
    # Pruebas de normalidad
    shapiro.test(variables$Sepal.Length)
    shapiro.test(variables$Sepal.Width)
    shapiro.test(variables$Petal.Length)
    shapiro.test(variables$Petal.Width)
    ad.test(variables$Sepal.Length)
    ad.test(variables$Sepal.Width)
    ad.test(variables$Petal.Length)
    ad.test(variables$Petal.Width)
    #
  # 3. Correlaciones
    #
    # Correlación de Pearson
    pearson_corr <- cor(variables, method = "pearson")
    pearson_corr  # Matriz de correlación de Pearson
    r_pearson_corr <- pearson_corr^2
    print(r_pearson_corr)
    #
    # Interpretación matriz de correlación:
    # La matriz de correlación muestra valores entre -1 y 1.
    # Valores cercanos a 1 indican una fuerte correlación positiva,
    # valores cercanos a -1 indican una fuerte correlación negativa,
    # y valores cercanos a 0 indican una débil o nula correlación.
    # En este caso, podemos analizar las correlaciones entre las variables Sepal 
    # y Petal.
    # Por ejemplo, una correlación alta entre Petal.Length y Petal.Width sugiere
    # que conforme una aumenta, la otra también lo hace (relación positiva).
    #
    # Matriz de varianza-covarianza para Pearson
    pearson_cov_matrix <- cov(variables)
    pearson_cov_matrix  # Matriz de varianza-covarianza
    #
    # Interpretación de la matriz de varianza-covarianza (Pearson):
    # La matriz de varianza-covarianza mide la dispersión conjunta de las variables.
    # Los valores diagonales representan la varianza de cada variable individual,
    # es decir, qué tanto se dispersan los datos alrededor de la media.
    # Los valores fuera de la diagonal indican la covarianza entre pares de variables,
    # que mide cómo varían juntas: un valor positivo indica que ambas variables tienden a
    # aumentar o disminuir juntas, mientras que un valor negativo indica que cuando una variable
    # aumenta, la otra tiende a disminuir.
    # Por ejemplo, una covarianza alta entre Petal.Length y Petal.Width indica que estas dos
    # variables tienden a aumentar o disminuir juntas, lo cual sugiere una relación directa.
    #
    # Correlación de Spearman
    spearman_corr <- cor(variables, method = "spearman")
    spearman_corr  # Matriz de correlación de Spearman
    r_spearman_corr <- spearman_corr^2
    print(r_spearman_corr)
    # Interpretación para Spearman:
    # Spearman mide la relación en términos de rangos y es más robusto frente a datos no normales.
    # Una correlación cercana a 1 o -1 sigue indicando relaciones fuertes (positivas o negativas), 
    # mientras que valores cercanos a 0 indican relaciones débiles o nulas.
    # A diferencia de Pearson, Spearman es menos sensible a valores atípicos
    # y puede detectar relaciones no lineales.
    #
    # Calcular la matriz de correlación de Pearson
    pearson_corr_matrix <- cor(variables, method = "pearson")
    cat("Matriz de Correlación de Pearson:\n")
    print(pearson_corr_matrix)
    #
    # Interpretación de la matriz de varianza-covarianza (Pearson):
    # La matriz de varianza-covarianza mide la dispersión conjunta de las variables.
    # Los valores diagonales representan la varianza de cada variable individual,
    # es decir, qué tanto se dispersan los datos alrededor de la media.
    # Los valores fuera de la diagonal indican la covarianza entre pares de variables,
    # que mide cómo varían juntas: un valor positivo indica que ambas variables tienden a
    # aumentar o disminuir juntas, mientras que un valor negativo indica que cuando una variable
    # aumenta, la otra tiende a disminuir.
    # Por ejemplo, una covarianza alta entre Petal.Length y Petal.Width indica que estas dos
    # variables tienden a aumentar o disminuir juntas, lo cual sugiere una relación directa.
    # Gráficos de dispersión entre las variables
    pairs(variables, main = "Gráficos de Dispersión entre las Variables",
          pch = 19, col = "blue")
    #
  # 4. Sensibilidad de la correlación multiple
    #
    # Sensibilidad al tamaño de muestra
    #
    # Crear una muestra pequeña aleatoria (por ejemplo, 50% de los datos)
    set.seed(123)  # Para reproducibilidad
    sample_indices <- sample(1:nrow(variables), size = 0.2 * nrow(variables))  # 50% de los datos
    small_sample <- variables[sample_indices, ]
    #
    # Calcular correlación y matriz de varianza-covarianza para la muestra completa
    pearson_corr_full <- cor(variables, method = "pearson")
    r_pearson_corr_full <- pearson_corr_full^2
    print(r_pearson_corr_full)
    cov_matrix_full <- cov(variables)
    print(cov_matrix_full)
    #
    # Calcular correlación y matriz de varianza-covarianza para la muestra pequeña
    pearson_corr_small <- cor(small_sample, method = "pearson")
    r_pearson_corr_small  <- pearson_corr_small ^2
    print(r_pearson_corr_small )
    cov_matrix_small <- cov(small_sample)
    print(cov_matrix_small)
    #
    # Configuración para gráficos en un solo panel
    pairs(variables, main = "Gráficos de Dispersión (Muestra Completa)",
          pch = 19, col = "blue")
    pairs(small_sample, main = "Gráficos de Dispersión (Muestra Pequeña)",
          pch = 19, col = "red")
    #
    # Mostrar las matrices de correlación y varianza-covarianza
    pearson_corr_full
    cov_matrix_full
    #
    pearson_corr_small
    cov_matrix_small
    #
    # Interpretación de la sensibilidad:
    # - **Matriz de Varianza-Covarianza**: Muestra la covarianza entre pares de  
    #  variables. Se mide en unidades cuadradas.
    #
    # - **Matriz de Correlación**: Muestra la fuerza y dirección de la relación 
    # lineal entre variables, estandarizada para eliminar el efecto de la escala.
    #
    # - **Sensibilidad al Tamaño de Muestra**: La matriz de correlación con un 
    # tamaño de muestra reducido puede diferir de la matriz calculada con la 
    # muestra completa. Un tamaño de muestra más pequeño puede introducir mayor 
    # variabilidad en las estimaciones de correlación.
  