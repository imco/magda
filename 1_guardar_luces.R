# Oscar Ruiz, IMCO 
# CDMX, 12 de enero de 2018

# 1. Importar SHAPEFILES del INEGI de Municipios y Localidades Urbanas. 
# 2. Importar RASTER de NOAA con la foto de luminosidad. 
# 3A La herramienta de RASTER CALCULATOR se usa para modificar los pixeles
#    de luminosidad >= 175.  
# 3B Utiliza ZONAL STATISTICS para resumir los 
#    datos del raster en los shapefiles.  
#    Si el Zonal Statistics aparece en blanco, se corrigen las 
#    proyecciones del shapefile y ráster para que coincidan. 
# 4. Continuar con este script. 

#Correr el script 1_guardar_luces_aux.R para unificar claves de municipios para el QGIS

library(rgdal)
library(dplyr)
library(stringr)
library(readr)
library(foreign)

#Obtenemos la info de los rasters de luminosidad

datos_municipios<-read.dbf("/home/oscar/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/inegi/marco_geo/processed/datos_municipios.dbf") %>% 
  mutate(CVE_GEO=paste(CVE_ENT,CVE_MUN,sep=""))

zonal_statistcs_2014<-read_csv("~/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/viirs/processed/raster_statistics_2014.csv") %>% 
  mutate(x175=Mean*`pixel count`) %>% select(CVE_GEO=`Zone ID`,x175)
zonal_statistcs_2015<-read_csv("~/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/viirs/processed/raster_statistics_2015.csv") %>% 
  mutate(x175_2015=Mean*`pixel count`) %>% select(CVE_GEO =`Zone ID`,x175_2015)
zonal_statistcs_2016<-read_csv("~/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/viirs/processed/raster_statistics_2016.csv") %>% 
  mutate(x175_2016=Mean*`pixel count`) %>% select(CVE_GEO =`Zone ID`,x175_2016)
zonal_statistcs_2017<-read_csv("~/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/viirs/processed/raster_statistics_2017.csv") %>% 
  mutate(x175_2017=Mean*`pixel count`) %>% select(CVE_GEO =`Zone ID`,x175_2017)
zonal_statistcs_2018<-read_csv("~/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/viirs/processed/raster_statistics_2018.csv") %>% 
  mutate(x175_2018=Mean*`pixel count`) %>% select(CVE_GEO =`Zone ID`,x175_2018)
zonal_statistcs_2019<-read_csv("~/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/viirs/processed/raster_statistics_2019.csv") %>% 
  mutate(x175_2019=Mean*`pixel count`) %>% select(CVE_GEO =`Zone ID`,x175_2019)

zonal_statistcs<-zonal_statistcs_2014 %>% 
  inner_join(zonal_statistcs_2015) %>% 
  inner_join(zonal_statistcs_2016) %>% 
  inner_join(zonal_statistcs_2017) %>% 
  inner_join(zonal_statistcs_2018) %>% 
  inner_join(zonal_statistcs_2019)

datos_municipios<-datos_municipios %>% inner_join(zonal_statistcs)

write.dbf(file = "/home/oscar/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/inegi/marco_geo/processed/datos_municipios.dbf",
          dataframe = datos_municipios)

#Repetimos ahora para localidades

datos_localidades<-read.dbf("/home/oscar/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/inegi/marco_geo/processed/datos_localidades.dbf") %>% 
  mutate(CVE_GEO=paste(CVE_ENT,CVE_MUN,CVE_LOC,sep=""))

zonal_statistcs_locs_2014<-read_csv("~/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/viirs/processed/raster_statistics_locs_2014.csv") %>% 
  mutate(x175=Mean*`pixel count`) %>% select(CVE_GEO=`Zone ID`,x175)
zonal_statistcs_locs_2015<-read_csv("~/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/viirs/processed/raster_statistics_locs_2015.csv") %>% 
  mutate(x175_2015=Mean*`pixel count`) %>% select(CVE_GEO =`Zone ID`,x175_2015)
zonal_statistcs_locs_2016<-read_csv("~/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/viirs/processed/raster_statistics_locs_2016.csv") %>% 
  mutate(x175_2016=Mean*`pixel count`) %>% select(CVE_GEO =`Zone ID`,x175_2016)
zonal_statistcs_locs_2017<-read_csv("~/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/viirs/processed/raster_statistics_locs_2017.csv") %>% 
  mutate(x175_2017=Mean*`pixel count`) %>% select(CVE_GEO =`Zone ID`,x175_2017)
zonal_statistcs_locs_2018<-read_csv("~/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/viirs/processed/raster_statistics_locs_2018.csv") %>% 
  mutate(x175_2018=Mean*`pixel count`) %>% select(CVE_GEO =`Zone ID`,x175_2018)
zonal_statistcs_locs_2019<-read_csv("~/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/viirs/processed/raster_statistics_locs_2019.csv") %>% 
  mutate(x175_2019=Mean*`pixel count`) %>% select(CVE_GEO =`Zone ID`,x175_2019)

zonal_statistcs_locs<-zonal_statistcs_locs_2014 %>% 
  inner_join(zonal_statistcs_locs_2015) %>% 
  inner_join(zonal_statistcs_locs_2016) %>% 
  inner_join(zonal_statistcs_locs_2017) %>% 
  inner_join(zonal_statistcs_locs_2018) %>% 
  inner_join(zonal_statistcs_locs_2019)

datos_localidades<-datos_localidades %>% inner_join(zonal_statistcs_locs)

write.dbf(file = "/home/oscar/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/inegi/marco_geo/processed/datos_localidades.dbf",
          dataframe = datos_localidades)

areas_shape <- function (shape) {  
  # Los applies se ven complicators.
  # Shape es una estructura, de listas de estructuras.
  areas_ <- shape@polygons %>% 
    lapply(. %>% slot("Polygons") %>% 
      sapply(. %>% slot("area")) ) %>% 
    sapply(sum)
  return (areas_) }

datos_shape <- function (shape, nivel = "localidad") {  # 
  # shape = locs_shp; nivel = "localidad"
  # shape = muns_shp; nivel = "municipio"
  datos_ <- shape@data %>% 
    mutate(area = areas_shape(shape))

  switch (nivel, 
    estado = {
      datos = datos_ %>% 
        mutate(CVEGEO = str_c(CVE_ENT)) %>% 
        select(-c(CVE_ENT)) %>% 
        rename(nombre = NOM_ENT)
    },
    municipio = { 
      datos = datos_ %>% 
        mutate(CVEGEO = str_c(CVE_ENT, CVE_MUN)) %>% 
        select(-c(CVE_ENT, CVE_MUN)) %>% 
        rename(nombre = NOM_MUN)
    },
    localidad = { 
      datos = datos_ %>% 
        mutate(CVEGEO = str_c(CVE_ENT, CVE_MUN, CVE_LOC)) %>% 
        select(-c(CVE_ENT, CVE_MUN, CVE_LOC)) %>% 
        rename(nombre = NOM_LOC)
    })
  return (datos)
}


# 1. Leer Shapefiles

locs_shp <- readOGR("../data/inegi/marco_geo/processed", 
  "datos_localidades", stringsAsFactors = FALSE)

# Los shapefiles de municipios y estados no son necesarios para el
# análisis.  Para correrlos, hay que antes cargarlos en QGIS como se 
# hizo con las localidades. 

muns_shp <- readOGR("../data/inegi/marco_geo/processed",
  "datos_municipios",  stringsAsFactors = FALSE)

# edos_shp <- readOGR("../data/inegi/marco_geo/processed",
#   "datos_estados",     stringsAsFactors = FALSE)


# 2. Sacar Datos

locs_data <- datos_shape(locs_shp, "localidad") %>% 
  rename(CVELOC = CVEGEO) %>% 
  select(CVELOC, nombre, area, x175,x175_2015,x175_2016,x175_2017,x175_2018,x175_2019) 

# Descomentar, 
# edos_data <- datos_shape(edos_shp, "estado") %>% 
#   rename(CVEENT = CVEGEO) %>% 
#   select(CVEENT, nombre, LUMEN = luz_sum, area, x175)
# 
muns_data <- datos_shape(muns_shp, "municipio") %>%
  rename(CVEMUN = CVEGEO) %>%
  select(CVEMUN, nombre, area, x175,x175_2015,x175_2016,x175_2017,x175_2018,x175_2019)


# 3. Juntar datos. 

municipios_datos <- muns_data %>%
  left_join(by = "CVEMUN", locs_data %>%
    mutate(CVEMUN = str_sub(CVELOC, 1, 5)) %>%
    group_by(CVEMUN) %>%
    summarize_at(vars(area, x175, x175_2015, x175_2016,x175_2017,x175_2018,x175_2019), funs(loc = sum))
  )
#     
# estados_datos <- edos_data %>% 
#   left_join(by = "CVEENT", locs_data %>% 
#     mutate(CVEENT = str_sub(CVELOC, 1, 2)) %>% 
#     group_by(CVEENT) %>% 
#     summarize_at(vars(LUMEN, area, x175), funs(loc = sum))
#   )


# 4. Escribir tablas

write_csv(locs_data, 
    "../data/viirs/processed/locs_luces_175.csv")

write_csv(municipios_datos, 
      "../data/viirs/processed/mun_luces_175.csv")

# write_csv(estados_datos, "../data/viirs/processed_tables/edos_luces_175.csv")
