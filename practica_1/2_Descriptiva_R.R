# UNIVERSIDAD DE NARIÑO
# FACULTAD DE CIENCIAS EXACTAS Y NATURALES
# PROGRAMA DE BIOLOGÍA
# Asignatura: Análisis de datos II
# Profesor: Jhon Jairo Calderón – Diego E. Martinez Revelo
#
# Estadistica descriptiva (medidas de tendencia central y dispersión)
#
  # Crear un vector con datos de longitudes de hojas en cm
    longitudes <- c(4.0, 4.0, 4.0, 4.0, 6.0, 5.0, 8.0, 7.0, 5.0, 9.0, 10.0)
    #
    # Calcular la moda
    frecuencias <- table(longitudes)
    moda <- as.numeric(names(frecuencias[frecuencias == max(frecuencias)]))
    #
    # Calcular la mediana
    mediana <- median(longitudes)
    #
    # Calcular el promedio
    promedio <- mean(longitudes)
    #
    # resultados
    cat("Moda:", moda, "\n") # note que se usa cat en lugar de print 
    cat("Mediana:", mediana, "\n") # solo es otra forma de visualizar resultados
    cat("Promedio:", promedio, "\n") # cat tiene menos errores que print
    #
    # Crear un grafico de histograma
    par(mar = c(3, 3, 2, 5), xpd = TRUE) 
    hist(longitudes, breaks = seq(2, 10, by = 1), col = "lightblue", main = "Histograma de Longitudes de Hojas", xlab = "Longitud (cm)", ylab = "Frecuencia")
    abline(v = promedio, col = "red", lty = 4)  # Línea del promedio
    abline(v = mediana, col = "green", lty = 4) # Línea de la mediana
    abline(v = moda, col = "purple", lty = 4)   # Línea de la moda
    # Añadir una leyenda
    legend("topright", legend = c("Promedio", "Mediana", "Moda"), col = c("red", "green", "purple"), lty = 4, bg = "white", cex = 0.8, inset = c(-0.2, 0))
    #
  # Medidas de dispersión
    #
    # Calculos
    desviacion_estandar <- sd(longitudes)
    rango_intercuartilico <- IQR(longitudes)
    cuartiles <- quantile(longitudes, probs = c(0.25, 0.50, 0.75)) # Q1, Mediana (Q2), Q3
    cuartil_inferior <- cuartiles[1]
    cuartil_medio <- cuartiles[2]
    cuartil_superior <- cuartiles[3]
    #
    # resultados
    cat("Mediana:", mediana, "\n")
    cat("Promedio:", promedio, "\n")
    cat("Desviación estándar:", desviacion_estandar, "\n")
    cat("Rango intercuartílico:", rango_intercuartilico, "\n")
    cat("Cuartil Inferior (Q1):", cuartil_inferior, "\n")
    cat("Cuartil Medio (Q2):", cuartil_medio, "\n")
    cat("Cuartil Superior (Q3):", cuartil_superior, "\n")
    #
    # gráfico boxplot
    par(mar = c(5, 4, 4, 8), xpd = TRUE)
    boxplot(longitudes, 
            main = "Box Plot de Longitudes de Hojas", 
            ylab = "Longitud (cm)", 
            col = "lightblue", 
            border = "blue", 
            horizontal = TRUE)
    #
    # Añadir líneas para el promedio
    abline(v = promedio, col = "red", lty = 2)  # Línea del promedio
    
    # Añadir líneas para los cuartiles
    abline(v = cuartil_inferior, col = "blue", lty = 2)  # Línea del cuartil inferior (Q1)
    abline(v = cuartil_medio, col = "green", lty = 2)    # Línea de la mediana (Q2)
    abline(v = cuartil_superior, col = "purple", lty = 2)   # Línea del cuartil superior (Q3)
    #
    # Añadir una leyenda al gráfico
    legend("topright", 
           inset = c(-0.5, 0), 
           legend = c("Promedio", "Cuartil Inferior (Q1)", "Mediana (Q2)", "Cuartil Superior (Q3)"), 
           col = c("red", "blue", "green", "purple"), 
           lty = 4, 
           bg = "white", 
           cex = 0.6,
           xpd = TRUE)
#
# Ejercicio
#
  # Estudiamos la longitud de las hojas y el peso seco de una especie de planta 
  # tropical para analizar la variabilidad en las poblaciones de diferentes sitios. 
  # Los datos de longitud (en cm) y peso seco (en gramos) de 20 hojas fueron 
  # recolectados manualmente usando una regla y una balanza de precisión.
  #
  #
  # ¿Cómo se distribuyen los datos de longitud de las hojas y peso seco de las  
  # plantas, y qué diferencias existen entre las medidas de tendencia central y 
  # dispersión de ambas variables?
  #
  # 1. Calcular las medidas de tendencia central
  # 2. Calcular las medidas de dispresión
  # 3. Utilizar una libreria para elaborar un resumen estadístico de las variables
  # 4. Representar las variables en graficas de histograma y boxplot
  #
  # Datos (en cm y gramos) con más dispersión en peso
    longitud_hojas <- c(5.2, 6.1, 4.8, 6.5, 7.0, 6.8, 5.9, 6.3, 5.5, 7.2, 
                          6.0, 5.7, 4.9, 6.4, 5.1, 7.1, 5.8, 6.6, 4.7, 6.9)
    #  
    peso_seco_hojas <- c(0.5, 2.3, 0.7, 2.8, 1.8, 3.5, 0.9, 1.1, 0.4, 4.0, 
                           2.2, 1.6, 0.6, 2.5, 1.0, 3.8, 1.2, 3.6, 0.3, 4.2)
    #
    # practiquemos las transformaciones de estructuras en R
    #
    # Transformar los vectores a una matriz
    matriz_hojas <- cbind(longitud_hojas, peso_seco_hojas)
    #
    # Transformar los vectores a un data frame
    df_hojas <- data.frame(Longitud = longitud_hojas, Peso_Seco = peso_seco_hojas)
    #
  # 1) Medidas de tendencia central
    #
    # Media
    media_longitud <- mean(longitud_hojas)
    media_peso <- mean(peso_seco_hojas)
    #
    # Mediana
    mediana_longitud <- median(longitud_hojas)
    mediana_peso <- median(peso_seco_hojas)
    #
    # Moda (se calcula utilizando la frecuencia más alta)
    moda_longitud <- as.numeric(names(sort(table(longitud_hojas), decreasing = TRUE)[1]))
    moda_peso <- as.numeric(names(sort(table(peso_seco_hojas), decreasing = TRUE)[1]))
    #
    tendencia_central <- data.frame(
      Estadístico = c("Media", "Mediana", "Moda"),
      Longitud = c(media_longitud, mediana_longitud, moda_longitud),
      Peso = c(media_peso, mediana_peso, moda_peso))
    #
  # Pregunta 1: 
    # Observa las medidas de tendencia central (media, mediana y moda) para la 
    # longitud y el peso seco de las hojas. ¿Cuál de estas medidas consideras más 
    # representativa para cada variable? ¿Qué diferencias encuentras entre las 
    # dos variables en cuanto a estas medidas? Explica por qué estas diferencias 
    # pueden ser importantes.
    #
    # Respuesta: _
    #
    # 
  # 2) Calculo de Medidas de Dispersión
    #  
    # Desviación Estándar
    desviacion_longitud <- sd(longitud_hojas)
    desviacion_peso <- sd(peso_seco_hojas)
    #
    # Rango Intercuartílico (IQR)
    iqr_longitud <- IQR(longitud_hojas)
    iqr_peso <- IQR(peso_seco_hojas)
    #
    # Cuartiles
    cuartiles_longitud <- quantile(longitud_hojas)
    cuartiles_peso <- quantile(peso_seco_hojas)
    cuartil_inf_longitud <- cuartiles_longitud[["25%"]]
    cuartil_mediana_longitud <- cuartiles_longitud[["50%"]]
    cuartil_sup_longitud <- cuartiles_longitud[["75%"]]
    cuartil_inf_peso <- cuartiles_peso[["25%"]]
    cuartil_mediana_peso <- cuartiles_peso[["50%"]]
    cuartil_sup_peso <- cuartiles_peso[["75%"]]
    #
    # Rango
    rango_longitud <- diff(range(longitud_hojas))
    rango_peso <- diff(range(peso_seco_hojas))
    #
    dispersion <- data.frame(
      Estadístico = c("Desviación Estándar", "Rango", "Rango Intercuartílico", "Cuartil Inferior (Q1)", "Mediana (Q2)", "Cuartil Superior (Q3)"),
      Longitud = c(desviacion_longitud, rango_longitud, iqr_longitud, cuartil_inf_longitud, cuartil_mediana_longitud, cuartil_sup_longitud),
      Peso = c(desviacion_peso, rango_peso, iqr_peso, cuartil_inf_peso, cuartil_mediana_peso, cuartil_sup_peso)
    )
    #
  # Pregunta 2:
    # Compara la dispersión (desviación estándar, rango intercuartílico y rango 
    # total) entre la longitud y el peso seco. ¿Cuál variable presenta mayor 
    # variabilidad y qué te indica esto sobre la distribución?    
    #
    # Respuesta: _
    #
  # 3) Resumen Estadístico
    #
    resumen_completo <- rbind(tendencia_central, dispersion)
    #
  # Pregunta 3: 
    # Observa el resumen estadístico completo para ambas variables. 
    # ¿Cómo se relacionan las medidas de tendencia central con las medidas de dispersión?
    #
    # Respuesta: _
    #  
  # 4) Visualización de Datos Histogramas
    #
    # Histograma para longitud de las hojas
    par(mfrow=c(1,2), mar = c(4, 4, 2, 2), xpd = TRUE)
    hist_data_longitud <- hist(longitud_hojas, 
                               main = "Longitud de las hojas",
                               xlab = "Longitud (cm)", 
                               ylab = "Frecuencia",
                               col = "lightblue", 
                               border = "black",
                               freq = TRUE, ylim = c(0, max(hist(longitud_hojas, plot = FALSE)$counts) + 2))
    #
    # Límites del eje Y
    ylim_longitud <- par("usr")[3:4]
    #
    # Añadir medidas de tendencia central
    lines(c(media_longitud, media_longitud), ylim_longitud, col = "red", lty = 2, lwd = 2)    # Media
    lines(c(mediana_longitud, mediana_longitud), ylim_longitud, col = "blue", lty = 2, lwd = 2) # Mediana
    lines(c(moda_longitud, moda_longitud), ylim_longitud, col = "green", lty = 2, lwd = 2)   # Moda
    #
    # Histograma para peso seco de las hojas
    hist_data_peso <- hist(peso_seco_hojas, 
                           main = "Peso seco de las hojas",
                           xlab = "Peso (g)",
                           ylab = "Frecuencia",
                           col = "lightgreen", 
                           border = "black",
                           freq = TRUE, ylim = c(0, max(hist(peso_seco_hojas, plot = FALSE)$counts) + 2))
    #
    # Límites del eje Y
    ylim_peso <- par("usr")[3:4]
    #
    # Añadir medidas de tendencia central
    lines(c(media_peso, media_peso), ylim_peso, col = "red", lty = 2, lwd = 2)    # Media
    lines(c(mediana_peso, mediana_peso), ylim_peso, col = "blue", lty = 2, lwd = 2) # Mediana
    lines(c(moda_peso, moda_peso), ylim_peso, col = "green", lty = 2, lwd = 2)
    #
   legend("bottomleft", inset = c(0.5, 0.6), cex = 0.8, bty = "n",
          legend = c("Media", "Mediana", "Moda"),
          col = c("red", "blue", "green"),
          lty = 4, lwd = 2)
    #
    # Pregunta 4: 
    # Observa los histogramas para la longitud y el peso seco de las hojas.
    # A) ¿Cómo se distribuyen los datos en cada variable según los histogramas?
    # B) ¿Existen diferencias notables en la forma de las distribuciones entre la 
    # longitud y el peso seco de las hojas? Describe cualquier patrón o tendencia 
    # visible.
    #
    # Respuesta: _
    #
  # 5) Visualización de Datos Histogramas Boxplot
    #
    # Histograma para longitud de las hojas
   par(mfrow=c(1,2), mar = c(2, 4, 2, 2), xpd = TRUE)
   boxplot(longitud_hojas, 
           main = "Longitud de las hojas",
           ylab = "Longitud (cm)",
           col = "lightblue", 
           border = "darkblue",
           horizontal = FALSE)
   abline(h = mean(longitud_hojas), col = "red", lty = 2, lwd = 2)    # Media
   # Boxplot para peso seco de las hojas
   boxplot(peso_seco_hojas, 
           main = "Peso seco de las hojas",
           ylab = "Peso (g)",
           col = "lightgreen", 
           border = "darkgreen",
           horizontal = FALSE)
   abline(h = mean(peso_seco_hojas), col = "red", lty = 2, lwd = 2)    # Media
   #
   #
   # Pregunta 5 
   # A) ¿Cómo se comparan las distribuciones en términos de mediana, cuartiles, 
   # y posibles valores atípicos?
   #
   # Respuesta: _
   #
   #
   # B) Qué diferencias observas en la variabilidad y el IQR entre ambas variables? 
   # ¿Qué nos indica esto sobre la distribución de los datos en cada caso?
   #
   # Respuesta: _
   #