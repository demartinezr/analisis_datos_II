# UNIVERSIDAD DE NARIÑO
# FACULTAD DE CIENCIAS EXACTAS Y NATURALES
# PROGRAMA DE BIOLOGÍA
# Asignatura: Análisis de datos II
# Profesor: Jhon Jairo Calderón – Diego E. Martinez Revelo
#
# Introducción al entorno R Studio (variables y estructuras en R)
#
# Para crear un nuevo proyecto en RStudio:
# En RStudio, ve a 'File' -> 'New Project' -> 'New Directory' -> 'New Project'.
# Selecciona la carpeta donde se creará el proyecto y sigue las instrucciones.
#
# Consultar el directorio de trabajo actual
  getwd()  # Muestra el directorio de trabajo en el que estás actualmente

# Asignar un nuevo directorio de trabajo
# Puedes establecer el directorio de trabajo a una carpeta específica usando setwd()
# Cambia la ruta a la que necesites en tu sistema.
  setwd("/ruta/a/tu/directorio")  # Cambia "/ruta/a/tu/directorio" a la ruta deseada
#
# Verifica si el directorio se cambió correctamente
  getwd()  # Confirma que ahora estás en el nuevo directorio
#
# Indique en el espacio _ como R studio resalta los objetos, las variables, 
  # los operadores, las funciones y los errores 
  x <- 10   # 
  y = "hola"  #
  z = x + 5 #
  mean(x 10)  #
  #
  # Asignación de valores a variables: <-, ->, =, assign ()
  #  
  # una forma
    a <- 6
    a
  # otra forma
    1986 -> b
    b
  # otra forma
    c = 0.006
    c
  # otra forma
    assign("d", 513.8)
    d
  # que pasa cuando se ejecutan los siguientes objetos X y x? 
  # escriba su obsrevación: _
    x <- 2
    X 
    x
  #    
# Tipos de variables en R
  #
  # En los siguentes espacios _ escriba el tipo de variable en R
  # variable _
    x <- 10.5
    #class(x)
    #
    # variable _
    x <- 1000L
    #class(x)
    #
    # variable _
    x <- 9i + 3
    #class(x)
    #
    # variable _
    x <- "R es emocionante"
    #class(x)
    #
    # variable _
    x <- TRUE
    #class(x)
    #
    # transformaciones
    edad <- c(10L,15L, 10L, 14L, 12L, 9L, 15L)
    print(edad)
    class(edad)
    edad_numeric <- as.numeric(edad)
    print(edad_numeric)
    class(edad_numeric)
    edad_factor <- as.factor(edad)
    print(edad_factor)
    class(edad_factor)
  #
# Estrucuras de datos en R
  #    
  # Vectores: Datos unidimensionales del mismo tipo
    # Ejemplo: Longitud de hojas de una planta (en cm)
    longitud_hojas <- c(5.1, 6.3, 4.8, 7.2, 5.9)
    print(longitud_hojas)
    class(longitud_hojas)
    #
  # Matrices: Datos bidimensionales del mismo tipo
    # Ejemplo: Matriz de conteo de especies en diferentes hábitats
    # Filas: Especies, Columnas: Hábitats (Bosque, Pradera, Cultivo)
    conteo_especies <- matrix(c(10, 5, 2,
                                8, 3, 4,
                                6, 9, 7),
                                nrow = 3, byrow = TRUE)
    rownames(conteo_especies) <- c("Especie_A", "Especie_B", "Especie_C")
    colnames(conteo_especies) <- c("Bosque", "Pradera", "Cultivo")
    print(conteo_especies)
    class(conteo_especies)
    #
  # Arreglos: Datos multidimensionales del mismo tipo
    # Ejemplo: Arreglo de biomasa en diferentes hábitats y años
    # Dimensiones: 3 hábitats x 4 años x 2 especies
    biomasa_arreglo <- array(c(15, 18, 20, 22,  # Bosque, Especie 1
                               12, 14, 13, 17,  # Pradera, Especie 1
                               9, 8, 7, 10,     # Cultivo, Especie 1
                               25, 28, 30, 35,  # Bosque, Especie 2
                               20, 23, 22, 24,  # Pradera, Especie 2
                               14, 16, 15, 18), # Cultivo, Especie 2
                             dim = c(3, 4, 2), 
                             dimnames = list(c("Bosque", "Pradera", "Cultivo"),
                                             c("Año_1", "Año_2", "Año_3", "Año_4"),
                                             c("Especie_1", "Especie_2")))
    print(biomasa_arreglo)
    class(biomasa_arreglo)
    #
  # Data Frames: Datos tabulares con columnas de diferentes tipos
    # Ejemplo: Datos de individuos capturados (especie, peso, hábitat)
    captura_individuos <- data.frame(
      Especie = c("Especie_A", "Especie_B", "Especie_C", "Especie_A", "Especie_B"),
      Peso_gramos = c(120, 85, 100, 95, 110),
      Habitat = c("Bosque", "Pradera", "Bosque", "Cultivo", "Pradera")
    )
    print(captura_individuos)
    class(captura_individuos)
    #
    # Exportar como tabla (archivo de texto)
    write.table(captura_individuos, file = "captura_individuos.txt", sep = "\t", row.names = FALSE)
    #
    # Exportar como CSV
    write.csv(captura_individuos, file = "captura_individuos.csv", row.names = FALSE)
    #
    # Exportar como archivo RDS (formato nativo de R)
    saveRDS(captura_individuos, file = "captura_individuos.rds")
    #
    # Para cargar el archivo RDS posteriormente puedes usar:
    # captura_individuos_cargado <- readRDS("captura_individuos.rds")
    #
  # Listas: Datos heterogéneos, colecciones de objetos
    # Ejemplo: Lista que contiene información biológica variada
    datos_biologicos <- list(
      Longitud_hojas = longitud_hojas,               # Vector
      Conteo_especies = conteo_especies,             # Matriz
      Biomasa_arreglo = biomasa_arreglo,             # Arreglo
      Captura_individuos = captura_individuos        # Data Frame
    )
    print(datos_biologicos)
    class(datos_biologicos)
    #
  # Acceso a elementos de la lista
    print(paste("Primera longitud de hoja:", datos_biologicos$Longitud_hojas[1]))
    print(paste("Conteo de Especie_A en Pradera:", datos_biologicos$Conteo_especies["Especie_A", "Pradera"]))
    #
    # Transformaciones
    #
    # Vector original de longitudes de hojas en centímetros
    print(longitud_hojas)
    # Convertir el vector a un arreglo (array)
    longitud_hojas_array <- as.array(longitud_hojas)
    print(longitud_hojas_array)
    class(longitud_hojas_array)
    #
    # Matriz original de conteo de especies
    print(conteo_especies)
    # Convertir la matriz a data frame
    conteo_especies_df <- as.data.frame(conteo_especies)
    print(conteo_especies_df)
    class(conteo_especies_df)
        