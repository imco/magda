library(readr)
library(lubridate)
library(chron) 
library(reshape2)
grupos_municipios_prex11 <- MB_x11

View(grupos_municipios_prex11)

cnbv_muns<-grupos_municipios_prex11
cnbv_muns$ENT<-substr(cnbv_muns$CVEMUN,start = 0,stop = 2)

cnbv_muns_edo<-cnbv_muns %>% group_by(ENT,fecha) %>% summarise(bancomer=sum(bancomer),
                                                           banamex=sum(banamex),
                                                           valor=sum(valor),
                                                           otros=sum(otros)) %>% 
  filter(month(fecha)==12,year(fecha)>=2014)

ggplot(cnbv_muns_edo, aes(fecha, bancomer,color=ENT)) + geom_line()
ggplot(cnbv_muns_edo, aes(fecha, banamex,color=ENT)) + geom_line()
ggplot(cnbv_muns_edo, aes(fecha, valor,color=ENT)) + geom_line()
ggplot(cnbv_muns_edo, aes(fecha, otros,color=ENT)) + geom_line()

cnbv_edo_bancomer<-cnbv_muns_edo %>% select(ENT,fecha,bancomer) %>% dcast(ENT~fecha)
names(cnbv_edo_bancomer)<-c("CVEENT","bancomer_2014","bancomer_2015","bancomer_2016",
                            "bancomer_2017","bancomer_2018","bancomer_2019")

cnbv_edo_banamex<-cnbv_muns_edo %>% select(ENT,fecha,banamex) %>% dcast(ENT~fecha)
names(cnbv_edo_banamex)<-c("CVEENT","banamex_2014","banamex_2015","banamex_2016",
                           "banamex_2017","banamex_2018","banamex_2019")

cnbv_edo_valor<-cnbv_muns_edo %>% select(ENT,fecha,valor) %>% dcast(ENT~fecha)
names(cnbv_edo_valor)<-c("CVEENT","valor_2014","valor_2015","valor_2016",
                         "valor_2017","valor_2018","valor_2019")

cnbv_edo_otros<-cnbv_muns_edo %>% select(ENT,fecha,otros) %>% dcast(ENT~fecha)
names(cnbv_edo_otros)<-c("CVEENT","otros_2014","otros_2015","otros_2016",
                         "otros_2017","otros_2018","otros_2019")

#Por municipios

cnbv_muns<-cnbv_muns %>% group_by(ENT,CVEMUN,fecha) %>% summarise(bancomer=sum(bancomer),
                                                               banamex=sum(banamex),
                                                               valor=sum(valor),
                                                               otros=sum(otros)) %>% 
  filter(month(fecha)==12,year(fecha)>=2014)

cnbv_muns_bancomer<-cnbv_muns %>% select(ENT,CVEMUN,fecha,bancomer) %>% dcast(CVEMUN~fecha)
#names(cnbv_muns_bancomer)<-c("CVEENT","bancomer_2014","bancomer_2015","bancomer_2016")

cnbv_muns_banamex<-cnbv_muns %>% select(ENT,CVEMUN,fecha,banamex) %>% dcast(CVEMUN~fecha)
#names(cnbv_muns_banamex)<-c("CVEENT","banamex_2014","banamex_2015","banamex_2016")

cnbv_muns_valor<-cnbv_muns %>% select(ENT,CVEMUN,fecha,valor) %>% dcast(CVEMUN~fecha)
#names(cnbv_muns_valor)<-c("CVEENT","valor_2014","valor_2015","valor_2016")

cnbv_muns_otros<-cnbv_muns %>% select(ENT,CVEMUN,fecha,otros) %>% dcast(CVEMUN~fecha)
#names(cnbv_muns_otros)<-c("CVEENT","otros_2014","otros_2015","otros_2016")


