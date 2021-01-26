# ==============================================================================
# Fecha: 2021-01-22 
# Trabajo final de EACSII
# Autores: AQUÍ SUS NOMBRES
# ==============================================================================


# Paquetes                        ----------------------------------------------
if (!require("pacman")) install.packages("pacman")#instala pacman si se requiere
#                                       no me paso de aquí al escribir --------->
pacman::p_load(tidyverse, 
              readxl,writexl,googlesheets4, # importar hojas de cálculo
              haven, foreign, # importación de dta y sav
              sjlabelled, GGally, RColorBrewer, # etiquetas
              janitor, skimr, #limpieza y verificación
              DescTools, # Paquete para estimaciones y pruebas
              infer, # tidy way 
              broom,  # Una escobita para limpiar (pero es para arreglar)
              estimatr, car, stargazer, ggpubr) # Para la regresión



# Datos                   -----------------------------------------------

# índice de competitividad estatal (no internacional)
url <- "https://github.com/aniuxa/Taller_R/raw/master/datos/ICE_2018.xlsx"
destfile <- "ICE_2018.xlsx"
curl::curl_download(url, destfile)

ICE_2018 <- read_excel(destfile, sheet = "para_importar")
ICE_2018 <- clean_names(ICE_2018) # limpia los nombres

# Análisis descriptivo    -----------------------------------------------

# De una variable

ICE_2018 %>% 
  ggplot(aes(homicidios)) +
  geom_density()

ICE_2018 %>% 
  ggplot(aes(homicidios)) +
  geom_histogram()

summary(ICE_2018$homicidios)

ICE_2018 %>% 
  tabyl(region2) %>% 
  adorn_totals() 


# Gráficos multivariados  -----------------------------------------------

#Iniciamos con un ggplot "scatter"

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_point()

# geometría "jitter"

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_jitter()

# geometría "text"

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_text(aes(label=edo2))

# geometría "text"

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_text(aes(label=edo2)) +
  geom_smooth(method="lm") 

# geometría "label"

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_label(aes(label=edo2))



## Muchos

ICE_2018 %>% 
  select(homicidios:competencia_en_servicios_notariales) %>% 
ggpairs(title="correlogram with ggpairs()") 

# Inferencia  -----------------------------------------------

ICE_2018 %>% 
  with(
  t.test(competencia_en_servicios_notariales))



# Modelo  -----------------------------------------------


modelo<-ICE_2018 %>% 
  with(
    lm(competencia_en_servicios_notariales~pobreza)
    )

# o

modelo <-lm(competencia_en_servicios_notariales~pobreza, data=ICE_2018)

summary(modelo)
anova(modelo)
plot(modelo)

