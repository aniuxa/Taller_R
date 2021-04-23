# ============================================================================ #
# Fecha: 2021-04-23 
# Sesión lúdica
# Mapas, memes y más
# ============================================================================ #


# Paquetes                        ----------------------------------------------
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, readxl,janitor,
               haven, sjlabelled, 
               remotes) #carga los paquetes necesarios


# Paquetes en desarrollo      ----------------------------------------------

#if (!require("remotes")) {
#  install.packages("remotes")
# }

remotes::install_github("diegovalle/mxmaps") # mapas
#https://www.diegovalle.net/mxmaps/

remotes::install_github("sctyner/memer") # memes
#https://github.com/sctyner/memer

remotes::install_github("R-CoderDotCom/ggcats@main") # ggcats
#https://github.com/R-CoderDotCom/ggcats

remotes::install_github("R-CoderDotCom/ggbernie") # ggbernie
#https://github.com/R-CoderDotCom/ggbernie

# Hay que cargar los instalados

library(mxmaps)
library(memer)
library(ggcats)
library(ggbernie)


# Datos                   -----------------------------------------------

# índice de competitividad estatal

ICE_2018 <- read_excel("datos/ICE_2018.xlsx", sheet = "para_importar")
ICE_2018 <- clean_names(ICE_2018) # limpia los nombres


# Mapas                   -----------------------------------------------

data("df_mxstate_2020") # carga la base estatal del paquete
glimpse(df_mxstate_2020)


# Cómo opera el paquete
df_mxstate_2020$value <- df_mxstate_2020$pop # paso esencial

glimpse(df_mxstate_2020$region)


mxstate_choropleth(df_mxstate_2020,
                   title = "Total población, por Estado") 

# Mientras haya una variable de region estatal
ICE_2018$value<- ICE_2018$homicidios

mxstate_choropleth(ICE_2018,
                   title = "Tasa de homicidios") 

mapa<-mxstate_choropleth(ICE_2018,
                         title = "Tasa de homicidios") 

mapa

ggsave(plot=mapa, #objeto donde está el gráfico
       device="png", # formato del gráfico
       filename = "mapa.png") # nombre el archivo de salida

# lo guardará en nuestro directorio y en formato ".png" de imagen


## Mapas municipales

data("df_mxmunicipio_2020")
df_mxmunicipio_2020$value <-df_mxmunicipio_2020$indigenous_language/df_mxmunicipio_2020$pop * 100

mxmunicipio_choropleth(df_mxmunicipio_2020, 
                       num_colors = 1,
                       title = "Porcentaje de la población que habla\nuna lengua indígena",
                       legend = "%")


# Si queremos hacer un "zoom"

mxmunicipio_choropleth(df_mxmunicipio_2020, 
                       num_colors = 1,
                       zoom = subset(df_mxmunicipio_2020, state_name %in% 
                                       c("Yucatán", "Veracruz"))$region,
                       title = "Porcentaje de la población que habla\nuna lengua indígena en Yucatány Veracruz",
                       show_states = FALSE,
                       legend = "%")

rm(df_mxmunicipio_2020, mapa,df_mxstate_2020) # nos roba mucha memoria

# Es importante liberar nuestro ambiente porque estamos limitado en rstudio.cloud
# sólo nos quedamos con el objeto ICE
detach("package:mxmaps", unload = TRUE)

# Memes en R ----

meme_list() # lista todos los memes disponibles


## Distracted Boyfriend ----

meme_explain("DistractedBf") # explica un meme en específico

meme_get("DistractedBf") # lo imprime en Viewer

meme_get("DistractedBf")%>% # ojo este es un pipe
  meme_text_distbf("Outlier", "Media", "Mediana") # coloca el texto necesario


meme_get("DistractedBf")%>% # ojo este es un pipe
  meme_text_distbf(newgirl = "Outlier", 
                   guy="Media", 
                   oldgirl = "Mediana",
                   font="Times")


#Ojo que la distribución del texto y la cantidad de elementos depende del meme

## Is this a Pigeon ----


meme_explain("IsThisAPigeon")

meme_get("IsThisAPigeon")

meme_get("IsThisAPigeon")%>% # ojo este es un pipe
  meme_text_pigeon("¿Es esto data science?", "Gente","mean()" )


meme_get("IsThisAPigeon")%>% # ojo este es un pipe
  meme_text_pigeon(isthis="¿Es esto data science?",
                   humanoid = "Gente",
                   butterfly="mean()" )

## Otros memes ----

meme_get("HotlineDrake")%>% # ojo este es un pipe
  meme_text_drake("Aceptar la H", "No rechazar la Ho")


meme_get("CheersLeo")%>% # ojo este es un pipe
  meme_text_top("¡SALUD!") %>% 
  meme_text_bottom("Por los memes en R", size=50)


# Gatitos en R ----

# Primero necesitamos hacer un scatter plot
# Por eso cargamos nuestros datos

#Iniciamos con un ggplot "scatter"

#El lienzo, sólo pone las coordenadas
ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad))


#Luego podemos poner las geometrías

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_point() # obvio, es un puntito

# ggcats se usa con ggplot
# lo que nos da es cómo cambiar esos puntitos por GATITOS
# Esa geometría se llama geom_cat

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_cat() # obvio es un gatito, pero uno específico


ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_cat(cat="nyancat") 


ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_cat(cat="nyancat", size=5) # se puede modificar el tamaño

# Los gatitos disponibles son: 
# c("nyancat", "bongo", "colonel", "grumpy", "hipster",
# "lil_bub", "maru", "mouth", "pop", "pop_close", 
#"pusheen", "pusheen_pc", "toast", "venus", "shironeko")


ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_cat(cat="pusheen", size=2) # se puede modificar el tamaño



ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_cat(cat="shironeko", size=2) 




# Bernie en R ----

# Funciona muy parecido. Igual es una extensión de ggplot2
# Tendremos una geometría nueva geom_bernie

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_bernie() # por default nos da Bernie sentadito


ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_bernie(bernie="sitting", size=2)


ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_bernie(bernie="stand") #Bernie parado


ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_bernie(bernie="head") #la cabeza de bernie


ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_bernie(bernie="asking") #bernie preguntando