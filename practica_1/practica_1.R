# Script Practice 1 -  Creacion de Ambientes de cultivo
# Source of Weather data: NASA Prediction Of Worldwide Energy Resources https://power.larc.nasa.gov/
# Source of soil data: https://www.soilgrids.org/  
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/cropmodel_guatemala2022
# 2022

### Objetivo: 
### Generar ambientes de cultivo en modelos de cultivo mediante la creación de archivos de clima y suelo.


#################################################
### 1 Cargar Paquetes
#################################################

library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)
library(naniar)
library(jsonlite)
library(sirad)
library(soiltexture)

## 1.1 Algunas funciones en desarrollo
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/get_data/get_data_nasapower.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/get_data/get_data_soilgrids.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/write_files/write_wth_aquacrop.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/write_files/write_soil_aquacrop.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/utils/utils_crop_model.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/utils/soil_PTF.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/utils/clim_tools.R", encoding = "UTF-8")




#################################################
### 2 Definir directorios de trabajo y localidad
#################################################

#Periodo (Año-mes-dia yyyymmdd)
fecha_inicial <- ymd("1990-01-01")
fecha_final <- ymd("2019-12-01")

#Localidad
localidad <- "VegaArriba"
latitud <- 14.76
longitud <- -89.56
altitud <- get_elevation(latitud, longitud) 

#Directorios de trabajo
directorio <- paste0(getwd(), "/practica_1/")
directorio_resultados <- paste0(directorio, "/data/")
dir.create(directorio_resultados)

#https://power.larc.nasa.gov/docs/v1/
#Precipitacion, Radiacion Solar, Humedad Relativa, Temp. Max, Temp. Min, Velocidad viento.
#variables_clima <- c("PRECTOT", "ALLSKY_SFC_SW_DWN","RH2M", "T2M_MAX", "T2M_MIN","WS2M")

#https://www.isric.org/explore/soilgrids/faq-soilgrids
#Densidad Aparente, %Arcillas, %Arenas, %Grava, Carbono Organico, (Contenido agua a marchitez, Capacidad de campo, Saturacion) 
#variables_suelo <- c("BLDFIE","CLYPPT","SNDPPT","CRFVOL","ORCDRC","WWP","AWCh1","AWCtS")
#profundidades <- c("sl1", "sl2", "sl3", "sl4", "sl5")  # 60cm

  
#################################################
### 3. Descargar datos de clima y suelo
#################################################

datos_clima_nasa <- get_data_nasapower(latitud, longitud, fecha_inicial, fecha_final)

datos_suelo_soilgrids <- get_data_soilgrids(latitud, longitud)


#################################################
### 4. Explorar y Organizar data
#################################################

#Analisis exploratorio con skimr
skim(datos_clima_nasa)
#skim(datos_suelo_crudos)


#Graficar clima
datos_clima_nasa %>%  
  group_by(year = year(date), month = month(date)) %>%
  summarise(rain = sum(rain), 
            tmin = mean(tmin), 
            tmax = mean(tmax), 
            srad = mean(srad), 
            rhum = mean(rhum),
            wspd = mean(wspd)) %>% #write.csv("climate_data_monthly.csv")
  ungroup() %>% gather(var, value, -c(year, month)) %>%
  ggplot(aes(factor(month), value)) + 
  geom_boxplot() + 
  facet_wrap(~var, scales = "free") + 
  labs(x = "mes", title = paste("promedios mesuales de", localidad)) +
  theme_bw()

#calcular la ETo

datos_clima <- datos_clima_nasa %>% mutate(
  ETo = ETo_cal(., latitud, altitud))

# organizar suelo a Aquacrop
datos_suelo <- soilgrids_to_aquacrop(datos_suelo_soilgrids)


#################################################
### 5. Convertir a formato Aquacrop
#################################################


write_wth_aquacrop(directorio_resultados, localidad, datos_clima, latitud, altitud)

write_soil_aquacrop(directorio_resultados, localidad,datos_suelo$data, datos_suelo$CN, datos_suelo$REW)


#################################################
### 6. Guardar datos clima y suelo en formato *.csv
#################################################

write_csv(datos_clima, paste0(directorio_resultados, localidad, "_clima.csv"))
write_csv(datos_suelo$data, paste0(directorio_resultados, localidad, "_suelo.csv"))





