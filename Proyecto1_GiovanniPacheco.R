#Los datos utilizados para el análisis corresponden a información del destace de ganado para consumo interno y externo.Los datos corresponden a los publicados en la página oficial del Instituto Nacional de Estadística (INE). Únicamente cuentan con dos bases de datos correspondientes al año 2023 y 2022. En los años anteriores el INE solo cuenta con las publicaciones de resultados a partir de los datos.
#Para acceder a los datos ingrese en el siguiente enlace: https://www.ine.gob.gt/estadisticas-agropecuarias/ y seleccione la opción "Base de datos", el año (para este proyecto 2023 y 2022) y el periodo "Anual".

#Para ejecutar el proyecto primero se instalaran las librerías a utilizar.
install.packages("arules")
library(arules)

#Dado que los datos vienen en un formato xlsx, el siguiente paso es descargar una libreria que permita la lectura de ese tipo de archivos
install.packages("readxl")
library(readxl)

#El siguiente paso es cargar los datasets

data_2023 <- read_excel("C:\\Users\\geplo\\OneDrive\\Escritorio\\MAESTRIA\\Ciclo 4\\Minería\\Proyecto 1\\Dataset\\2023\\data_2023.xlsx", sheet = "Basededatosdeganado")

data_2022 <- read_excel("C:\\Users\\geplo\\OneDrive\\Escritorio\\MAESTRIA\\Ciclo 4\\Minería\\Proyecto 1\\Dataset\\2022\\data_2022.xlsx", sheet = "paso6 baseParaUsuario")

#Hacer una expliracion rapida de los data sets

View(data_2022)
View(data_2023)

#Los dos datasets tienen 19 columnas con los mismos datos. El siguiente paso es unir las dos bases de datos para contar con un solo dataset para el analisis. Para ello es necesario eliminar la columna de correlativo; que corresponde únicamente al número de observación. Se eliminar ya que no aporta información relevante y puede dificultar la unión de los datasets.

data_2022 <- data_2022[, -c(1)]
data_2023 <- data_2023[,-c(1)]

#Al cargar los datos, algunas variables muestran las observaciones con numeros decimales; informacion que no forma parte de la fuente original. Por lo que es necesario transformar los valores de esas variables a numeros enteros.
data_2022$`Peso total en libras`<- round(data_2022$`Peso total en libras`)
data_2022$`Peso total del número de cabezas (quintales)`<- round(data_2022$`Peso total del número de cabezas (quintales)`)
data_2022$`Carne y hueso` <- round(data_2022$`Carne y hueso`)
data_2022$Sebo <- round(data_2022$Sebo)
data_2022$Total <- round(data_2022$Total)
data_2022$Vísceras <- round(data_2022$Vísceras)
data_2022$Cuero <- round(data_2022$Cuero)
data_2022$Sangre <- round(data_2022$Sangre)
data_2022$Desperdicio <- round(data_2022$Desperdicio)

data_2023$`Peso total en libras` <- round(data_2023$`Peso total en libras`)
data_2023$`Peso total del número de cabezas (quintales)`<- round(data_2023$`Peso total del número de cabezas (quintales)`)
data_2023$`Carne y Hueso`<- round(data_2023$`Carne y Hueso`)
data_2023$Sebo <- round(data_2023$Sebo)
data_2023$Total <- round(data_2023$Total)
data_2023$Vísceras <- round(data_2023$Vísceras)
data_2023$Cuero <- round(data_2023$Cuero)
data_2023$Sangre <- round(data_2023$Sangre)
data_2023$Desperdicio <- round(data_2023$Desperdicio)

#La variable "Peso vivo promedio (peso de cada cabeza)" muestra sus valores unicamente en dos decimales. Al momento de cargar el dataset de 2022, esta columna muestra valores con mas de cuatro decimales; y el dataset de 2023 con tres decimales. Por lo que es necesario hacer la transformacion de los datos.
data_2022$`Peso vivo promedio (peso de cada cabeza)`<- round(data_2022$`Peso vivo promedio (peso de cada cabeza)`, 2)
data_2023$`Peso vivo promedio (Peso de cada cabeza)`<- round(data_2023$`Peso vivo promedio (Peso de cada cabeza)`, 2)

#Antes de hacer la unión de los dos datasets es importante verificar que no hay datos nulos o vacíos.
anyNA(data_2022)
anyNA(data_2023)

#Al identificar los datos nulos, el siguiente paso es saber cuántos datos nulos hay en los datasets.
sum(is.na(data_2022))
sum(is.na(data_2023))

#Se identificó que los valores nulos corresponden a la columna "Peso total en libras". Sin embargo, estos valores corresponden al dato del tipo de carne de exportación, que no contiene información en esta variable, por lo que únicamente se llenarán los vacíos con ceros.
data_2022[is.na(data_2022)] <- 0
data_2023[is.na(data_2023)] <- 0

#A partir de esto, se procede a hacer la union de los dos datasets. Para ello es necesario instalar el paquete dplyr.
install.packages("dplyr")
library(dplyr)

#Para no tener problemas con la unión, es necesario que las columnas tengan el mismo nombre en los encabezados. Por lo que a continuación se procede a modificarlos.
View(data_2022)
View(data_2023)

colnames(data_2022)[colnames(data_2022) == "Número de cabezas"] <- "Número de Cabezas"
colnames(data_2022)[colnames(data_2022) == "Peso vivo promedio (peso de cada cabeza)"] <- "Peso vivo promedio (Peso de cada cabeza)"
colnames(data_2022)[colnames(data_2022) == "Carne y hueso"] <- "Carne y Hueso"

#A continuación se unen las tablas combinando las filas y se asigna la combinación a una nueva variable.
data_ganado <- bind_rows(data_2022, data_2023)

View(data_ganado)

###Aplicación de Minería de datos:
##Reglas de asociación Apriori

#A partir del set de datos transformado, se aplicará la reglas de asociación apriori solo aquellas columnas con datos categóricos o nominales. Por lo que se creará un set de datos a partir del transformado.

nuevo_data_ganado <- data_ganado[, c('Año','Tipo de Carne', 'Mes', 'Departamento', 'Municipio', 'Clase', 'Sexo (subclase)')]

#Verificar el nuevo set de datos creado solo con las columnas con variables categoricas o discretas
View(nuevo_data_ganado)

#En el set de datos se identificaron variables en la columna "Municipio" con valores decimales, por lo que tanto en el dataset fuente como en el de "nuevo_data_ganado" se reduciran a numeros enteros.
data_ganado$Municipio <- round(data_ganado$Municipio)
nuevo_data_ganado$Municipio <- round(nuevo_data_ganado$Municipio)

#Verificar el set editado
View(nuevo_data_ganado)

#A continuacion, se aplican las reglas de asociacion al set creado
reglas_apriori <- apriori(nuevo_data_ganado, parameter = list(support=0.2, confidence=0.5))

#Inspeccionar las reglas
inspect(reglas_apriori[0:100])


#Al aplicar las reglas, salen advertencias de que no hay valores logicos de factor. Por lo que se explora la estructura de dataset creado
str(nuevo_data_ganado)

#Se identifica que las categorías están identificadas como números y no como factores. Por lo que se procede a transformar los datos a factores.
nuevo_data_ganado[,1:7] <- lapply(nuevo_data_ganado[,1:7], as.factor)

#Se verifica el dataset
View(nuevo_data_ganado)

#Con la base limpia, se aplican ahora el algoritmo apriori nuevamente.
reglas_apriori <- apriori(nuevo_data_ganado, parameter = list(support=0.2, confidence=0.5))

#Se identificaron 22 reglas, por lo que se inspeccionan.
inspect(reglas_apriori[0:22])

##Reglas de asociación FP-Growth
# Instalar Rtool, verificar la versión de Rstudio para instalar la versión correcta. Lo puede descargar en el siguiente enlace:https://cran.r-project.org/bin/windows/Rtools/
#Una vez se ha instalado la versión de Rtools, verificar que esté insatalado en ruta de Rstudio
system("where make")

#A continuación se aplica el algoritmo sobre el set de variables discretas generado
reglas_fp_growth <- fim4r(nuevo_data_ganado, method = "fpgrowth", target = "rules", supp = 0.2, conf = 0.5)

#Para visualizar las reglas como un frame, se aplica el siguiente código
reglasframe <- as(reglas_fp_growth, "data.frame")
View(reglasframe)

#Se cambió el soporte a 0.1 para verificar si aparecen otro tipo de asociaciones que señales ubicación o tiempo (mes)
reglas_fp_growth <- fim4r(nuevo_data_ganado, method = "fpgrowth", target = "rules", supp = 0.1, conf = 0.5)

#Para visualizar las reglas como un frame, se aplica el siguiente código
reglasframe <- as(reglas_fp_growth, "data.frame")
View(reglasframe)
