library(foreign)

datos_localidades<-read.dbf("/home/oscar/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/inegi/marco_geo/processed/datos_localidades.dbf") %>% 
  mutate(CVE_GEO_2=paste(CVE_ENT,CVE_MUN,CVE_LOC,sep=""))

write.dbf(file = "/home/oscar/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/inegi/marco_geo/processed/datos_localidades_Prueba.dbf",
          dataframe = datos_localidades)

datos_municipios<-read.dbf("/home/oscar/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/inegi/marco_geo/processed/datos_municipios.dbf") %>% 
  mutate(CVE_GEO=paste(CVE_ENT,CVE_MUN,sep=""))

write.dbf(file = "/home/oscar/Documentos/IMCO/Proyectos/Fondos_propios/MAGDA/Recalcula_MAGDA_marzo_2020/data/inegi/marco_geo/processed/datos_municipios_Prueba.dbf",
          dataframe = datos_municipios)
