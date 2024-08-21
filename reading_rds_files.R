# directory "C:/Users/mperez12/Documents/GitHub/Hydra_selftest_OM"
library(tidyverse)
library(dplyr)
# Definir la ruta donde están los archivos .rds
dataListRuta<- "OM_MUST/data"

# Crear una lista con los nombres de los archivos .rds
files <- list.files(path = dataListRuta, pattern = "\\.rds$", full.names = TRUE)

# Leer los 10 primeros archivos .rds
# (ajustar el número si hay menos de 10 archivos en la carpeta)
filestoread <- files[1:36]

# Leer los archivos y almacenarlos en una lista
dataList <- lapply(filestoread, readRDS)

# Si deseas acceder a un archivo en particular, por ejemplo, el primero:
File001 <- dataList[[1]]

names(File001)

# Inicializar una lista para almacenar las variables extraídas
list <- vector("list", length(dataList))
#list_2 <- vector("list", length(dataList[[1]]$Fyrspecies))

#mylist <- list((list_1), (list_2))

#variables <- vector("list", length(mylist))


# Usar un bucle for para extraer la columna 'mi_variable' de cada matriz
list_Fyr<- for (i in seq_along(dataList)) {
  list[[1]] <- dataList[[i]][["Fyrspecies"]][[1]]
}

list[[1]] <- dataList[[i]][["Fyrspecies"]][[1]]
write.csv(list[[1]],"Fyrs.csv", row.name=T)
