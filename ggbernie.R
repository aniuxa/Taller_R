

# Paquetes                        ----------------------------------------------
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, readxl,
               janitor) #carga los paquetes necesarios


# Un paquete en desarrollo        ----------------------------------------------

if (!require("devtools")) {
  install.packages("devtools")
 }
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("R-CoderDotCom/ggbernie")

library(ggbernie)
# Datos                   -----------------------------------------------

# índice de competitividad estatal (no internacional)
url <- "https://github.com/aniuxa/CursoR-posgrado-pol/raw/master/datos/ICE_2018.xlsx"
destfile <- "ICE_2018.xlsx"
curl::curl_download(url, destfile)

ICE_2018 <- read_excel(destfile, sheet = "para_importar")
ICE_2018 <- clean_names(ICE_2018) # limpia los nombres

# Gráficos multivariados  -----------------------------------------------

#Iniciamos con un ggplot "scatter"

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_point()


ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_bernie(bernie="sitting")


ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_bernie(bernie="stand")
