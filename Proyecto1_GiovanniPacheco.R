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

#Se identificó que los valores nulos corresponden a la columna "Peso total en libras". Para no tener problemas posteriormente se eliminan esas observaciones.
data_2022_limpio <- na.omit(data_2022)
data_2023_limpio <- na.omit(data_2023)

#A partir de esto, se procede a hacer la union de los dos datasets. Para ello es necesario instalar el paquete dplyr.
install.packages("dplyr")
library(dplyr)

#Para no tener problemas con la unión, es necesario que las columnas tengan el mismo nombre en los encabezados. Por lo que a continuación se procede a modificarlos.
View(data_2022_limpio)
View(data_2023_limpio)

colnames(data_2022_limpio)[colnames(data_2022_limpio) == "Número de cabezas"] <- "Número de Cabezas"
colnames(data_2022_limpio)[colnames(data_2022_limpio) == "Peso vivo promedio (peso de cada cabeza)"] <- "Peso vivo promedio (Peso de cada cabeza)"
colnames(data_2022_limpio)[colnames(data_2022_limpio) == "Carne y hueso"] <- "Carne y Hueso"

data_ganado <- bind_rows(data_2022_limpio, data_2023_limpio)

View(data_ganado)
