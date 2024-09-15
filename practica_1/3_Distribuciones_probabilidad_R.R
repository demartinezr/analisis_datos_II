# UNIVERSIDAD DE NARIÑO
# FACULTAD DE CIENCIAS EXACTAS Y NATURALES
# PROGRAMA DE BIOLOGÍA
# Asignatura: Análisis de datos II
# Profesor: Jhon Jairo Calderón – Diego E. Martinez Revelo
#
# Distribuciones de probabilidad
  #
  # Configuración inicial
  set.seed(123)
  #
# 1. Distribución Normal
  #
  # Descripción
  # Simulamos alturas de plantas para estudiar la variabilidad en una población 
  # de cultivo.
  # La distribución normal se define por dos parámetros:
  # - mu (μ): Media, que representa el valor promedio alrededor del cual se 
  # distribuyen los datos.
  # - sigma (σ): Desviación estándar, que indica la cantidad de variabilidad o 
  # dispersión de los datos alrededor de la media.
  #
  # Pregunta
  # ¿Cuál es la probabilidad de que una planta tenga una altura entre 45 y 55 cm?
  #
  # Simulación del experimento
  mu <- 50       # Media de altura (cm)
  sigma <- 10    # Desviación estándar (cm)
  heights <- rnorm(100, mean = mu, sd = sigma) # se midió 100 veces
  #
  # Cálculo de probabilidad
  prob_height <- pnorm(55, mean = mu, sd = sigma) - pnorm(45, mean = mu, sd = sigma)
  #
  # Gráfico para Distribución Normal
  hist(heights, breaks = 30, main = "Alturas de Plantas (Normal)", xlab = "Altura (cm)", freq = FALSE, col = "lightgrey")
  curve(dnorm(x, mean = mu, sd = sigma), add = TRUE, col = "blue", lwd = 6)
  plot(density(heights), main = "Densidad de alturas", xlab = "Altura (cm)", ylab = "Densidad", col = "blue")
  #
  # Espacio para la respuesta
  # Respuesta: Número 1:
  #
# 2. Distribución Binomial
  #
  # Descripción
  # Simulamos el número de plantas infectadas por una plaga en un cultivo para 
  # evaluar la propagación de la enfermedad.
  # La distribución binomial se define por dos parámetros:
  # - n: Número total de experimentos o pruebas (en este caso, el número total
  # de plantas examinadas).
  # - p: Probabilidad de éxito en cada prueba (en este caso, la probabilidad de
  # que una planta esté infectada).
  #
  # Pregunta
  # ¿Cuál es la probabilidad de que exactamente 12 plantas estén infectadas en 
  # una muestra de 30?
  #
  # Simulación del experimento
  n <- 30   # Número total de plantas examinadas
  p <- 0.4  # Probabilidad de que una planta esté infectada
  infectadas <- rbinom(100, size = n, prob = p)
  #
  # Cálculo de probabilidad
  prob_infectadas <- dbinom(12, size = n, prob = p)
  #
  # Gráfico para Distribución Binomial
  hist(infectadas, breaks = 15, main = "Plantas infectadas (Binomial)", xlab = "Número de plantas infectadas", freq = FALSE, col = "lightgrey")
  barplot(dbinom(0:n, size = n, prob = p), names.arg = 0:n, main = "Plantas infectadas", xlab = "Número de Plantas Infectadas", ylab = "Probabilidad", col = "blue")
  #
  # Espacio para la respuesta
  # Respuesta: Número 2:
  #
# 3. Distribución de Poisson
  #
  # Descripción
  # Simulamos el número de avistamientos de una especie rara de insecto en un 
  # cultivo durante un mes para evaluar su impacto.
  # La distribución de Poisson se define por un parámetro:
  # - lambda (λ): Tasa promedio de ocurrencia del evento en un intervalo fijo 
  # (en este caso, el número promedio de avistamientos por mes).
  #
  # Pregunta
  # ¿Cuál es la probabilidad de observar exactamente 7 avistamientos en un mes?
  #
  # Simulación del experimento
  lambda <- 5  # Tasa promedio de avistamientos por mes
  avistamientos <- rpois(100, lambda = lambda)
  #
  # Cálculo de probabilidad
  prob_avistamientos <- dpois(7, lambda = lambda)
  #
  # Gráfico para Distribución de Poisson
  hist(avistamientos, breaks = 10, main = "Avistamientos de ave (Poisson)", xlab = "Número de avistamientos", freq = FALSE, col = "lightgrey")
  barplot(dpois(0:max(avistamientos), lambda = lambda), names.arg = 0:max(avistamientos), main = "Distribución de avistamientos", xlab = "Número de Avistamientos", ylab = "Probabilidad", col = "blue")
  #
  # Espacio para la respuesta
  # Respuesta: Número 3:
  #
# 4. Distribución Binomial Negativa
  #
  # Descripción
  # Simulamos el número de muestras necesarias hasta encontrar 3 plantas con una mutación específica.
  # La distribución binomial negativa se define por dos parámetros:
  # - r: Número de éxitos deseados (en este caso, el número de plantas con la mutación).
  # - p: Probabilidad de éxito en cada prueba (en este caso, la probabilidad de que una planta tenga la mutación).
  #
  # Pregunta
  # ¿Cuál es la probabilidad de necesitar exactamente 10 muestras para encontrar 3 plantas con la mutación?
  #
  # Simulación del experimento
  r <- 3    # Número de éxitos deseados
  p <- 0.1  # Probabilidad de tener la mutación
  muestras <- rnbinom(100, size = r, prob = p)
  #
  # Cálculo de probabilidad
  prob_muestras <- dnbinom(10, size = r, prob = p)
  #
  # Gráfico para Distribución Binomial Negativa
  hist(muestras, breaks = 20, main = "Muestras hasta Encontrar 3 Mutaciones (Binomial Negativa)", xlab = "Número de Muestras", freq = FALSE, col = "lightgrey")
  barplot(dnbinom(0:max(muestras), size = r, prob = p), names.arg = 0:max(muestras), main = "Distribución de muestras", xlab = "Número de muestras", ylab = "Probabilidad", col = "blue")
  #
  # Espacio para la respuesta
  # Respuesta: Número 4:
#  
# Ejercicios de Simulación y Visualización
  #
  # Objetivo: Simular datos utilizando las distribuciones normal, binomial y Poisson. Modificar los parámetros para observar cómo cambian las gráficas de distribución.
  #
  # Instrucciones:
  # Genera una muestra de 100 datos con una distribución normal.
  # Usa rnorm() para simular los datos.
  # Ajusta los parámetros de la media (μ) y la desviación estándar (σ).
  #
  # Parámetros
  mu <- 50       # Media (modifica este valor)
  sigma <- 10    # Desviación estándar (modifica este valor)
  #
  # Simulación
  datos_normales <- rnorm(100, mean = mu, sd = sigma)
  #
  # Gráfico
  hist(datos_normales, breaks = 20, probability = TRUE, col = "lightgrey", main = "Distribución Normal", xlab = "Valores")
  curve(dnorm(x, mean = mu, sd = sigma), add = TRUE, col = "blue", lwd = 2)
  # Modificaciones:
  # Cambia los valores de mu y sigma y observa cómo varía la forma de la curva.
  #
  # Distribución Binomial
  #
  # Simula 100 datos usando una distribución binomial.
  #Usa rbinom() para la simulación.
  #Ajusta el número de pruebas (n) y la probabilidad de éxito (p).
  n <- 30   # Número de pruebas (modifica este valor)
  p <- 0.4  # Probabilidad de éxito (modifica este valor)
  # Simulación
  datos_binomiales <- rbinom(100, size = n, prob = p)
  #
  # Gráfico
  hist(datos_binomiales, breaks = seq(-0.5, n + 0.5, by = 1), probability = TRUE, col = "lightgrey", main = "Distribución Binomial", xlab = "Número de Éxitos")
  x <- 0:n
  y <- dbinom(x, size = n, prob = p)
  lines(x, y, type = "h", lwd = 2, col = "blue")
  # Modificaciones:
  # Cambia los valores de n y p y observa cómo cambian las barras en el histograma.
  # 
  # Distribución de Poisson
  #
  # Genera una muestra de 100 datos con una distribución de Poisson.
  # Usa rpois() para simular los datos.
  # Ajusta la tasa promedio de eventos (λ).
  # Parámetro
  lambda <- 5  # Tasa promedio (modifica este valor)
  # Simulación
  datos_poisson <- rpois(100, lambda = lambda)
  # Gráfico
  hist(datos_poisson, breaks = seq(-0.5, max(datos_poisson) + 0.5, by = 1), probability = TRUE, col = "lightgrey", main = "Distribución de Poisson", xlab = "Número de Eventos")
  x <- 0:max(datos_poisson)
  y <- dpois(x, lambda = lambda)
  lines(x, y, type = "h", lwd = 2, col = "blue")
  # Modificaciones:
  # Cambia el valor de lambda y observa cómo se ajustan las barras en el histograma.
  