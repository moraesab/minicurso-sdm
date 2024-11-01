# -------------------------------------------------------------------------
# Minicurso Introdução a Modelagem de Nicho - XII CBC
#
# Objetivo: instalar pacotes necessários
#
# Autor: Alex Barbosa de Moraes
#
# Última atualização: 2024-10-29
#
# Descrição: Nesse script eu verifico se todos os pacotes necessários
# para rodar esse projeto estão instalados. Em caso negativo, ele instala
# o pacote e todos as suas dependencias. 
# -------------------------------------------------------------------------

# 1. Instalar pacotes -----------------------------------------------------

# Java

# install java - windows - https://www.java.com/pt_BR/download/
if(!require(rJava)) install.packages("rJava")

# Maxent

# maxent
download.file(url = "https://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download",
              destfile = paste0(system.file("java", package = "dismo"), "/maxent.zip"), mode = "wb")
unzip(zipfile = paste0(system.file("java", package = "dismo"), "/maxent.zip"),
      exdir = system.file("java", package = "dismo"), junkpaths = TRUE)
# dir(system.file("java", package = "dismo")) # verifica se todos os arquivos estão certinhos

# Base para download de pacotes fora do CRAN
if(!require(devtools)) install.packages("devtools", dependencies = TRUE)


# Visualizacao e manipulacao
if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE) # Global manipulation package - include dplyr and ggplot2
if(!require(readr)) install.packages("readr", dependencies = TRUE) # Read data
if(!require(inspectdf)) install.packages("inspectdf", dependencies = TRUE) # Inspect dataframe
if(!require(rbokeh)) install.packages("rbokeh", dependencies = TRUE) # Creation of interactive maps using figure function
if(!require(forcats)) install.packages("forcats", dependencies = TRUE) # Manager factors
if(!require(lubridate)) install.packages("lubridate", dependencies = TRUE) # Deal with dates

# Down(load) de camadas ambientais
if(!require(sdmpredictors)) install.packages("sdmpredictors", dependencies = TRUE) #Enviromment layers
if(!require(biooracler)) devtools::install_github("bio-oracle/biooracler") # Enviromment layers updated

# Funções para ENM
if(!require(sdm)) install.packages("sdm", dependencies = TRUE)
sdm::installAll() # instala todos os pacotes necessários para todas as funções presentes no pacote SDM 
if(!require(ssdm)) install.packages("ssdm")

# Manipulacao e visualizacao de variaveis - raster
if(!require(ggspatial)) install.packages("ggspatial", dependencies = TRUE)
if(!require(landscapetools)) install.packages("landscapetools", dependencies = TRUE)
if(!require(raster)) install.packages("raster", dependencies = TRUE)
if(!require(sf)) install.packages("sf", dependencies = TRUE)
if(!require(rgdal)) install.packages("rgdal", dependencies = TRUE)


# Limpeza espacial
if(!require(spThin)) install.packages("spThin", dependencies = TRUE)
if(!require(sf)) install.packages("sf", dependencies = TRUE)

# Limites geograficos
if(!require(rnaturalearth)) install.packages("rnaturalearth", dependencies = TRUE)
if(!require(maptools)) install.packages("maptools", dependencies = TRUE)

# Estatística
if(!require(corrr)) install.packages("corrr", dependencies = TRUE) #Correlation
if(!require(caret)) install.packages("caret", dependencies = TRUE)
if(!require(usdm)) install.packages("usdm", dependencies = TRUE) # VIF

# Algoritmos

# Bioclim, Domain, Mahalanobis e Maxent
if(!require(dismo)) install.packages("dismo", dependencies = TRUE)
if(!require(rJava)) install.packages("rJava", dependencies = TRUE)

# SVM
if(!require(kernlab)) install.packages("kernlab", dependencies = TRUE)

# Random Forest
if(!require(randomForest)) install.packages("randomForest", dependencies = TRUE)

# Consenso de modelos
if(!require(vegan)) install.packages("vegan", dependencies = TRUE)

# som de notificacao
if(!require(beepr)) install.packages("beepr", dependencies = TRUE)

# Barra de escala e Rosa dos ventos
if(!require(ggspatial)) install.packages("ggspatial", dependencies = TRUE) # Scale bar and North arrow

# Utilitários para desenhar os mapas
if(!require(showtext)) install.packages("showtext", dependencies = TRUE) # Import external font
if(!require(RColorBrewer)) install.packages("RColorBrewer", dependencies = TRUE) # Color Palette
if(!require(wesanderson)) devtools::install_github("karthik/wesanderson") # Palette Generator
if(!require(ggnewscale)) install.packages("ggnewscale", dependencies = TRUE) # permite incluir mais de uma escala de cor em mapas do ggplot
if(!require(patchwork)) install.packages("patchwork", dependencies = TRUE) # plot grids

# end ---------------------------------------------------------------------
