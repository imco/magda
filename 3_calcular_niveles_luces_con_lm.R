# Oscar Ruiz, IMCO
# CDMX, 17 de julio de 2020

library(ggrepel)
library(RColorBrewer)
library(magrittr)
library(readr)
library(dplyr)
library(stringr)
library(car)
library(ggthemes)

filter <- dplyr::filter

# Los datos de luces llegan a variar con QGIS. 
# USAR_REFERENCIA para utilizar el dato original.

pibe_14 <- read_csv("../data/bie/processed/pibe.csv") %>% 
  filter(año == "2014-12-01")

muns_acteco <- read_csv("../data/viirs/processed/mun_luces_175.csv",
      col_types = "ccdddddddddddddd") %>% 
  mutate(CVEENT = CVEMUN %>% str_sub(1, 2)) %>% 
  left_join(by="CVEENT", 
      pibe_14 %>% select(CVEENT, Estado, pibe)) %>% 
  group_by(CVEENT, Estado) %>% 
  mutate(ae_175 = ifelse((pibe*x175_loc/sum(x175_loc))>=0,yes = pibe*x175_loc/sum(x175_loc),no = 0)) %>%
  ungroup %>% filter(x175>0)

write_csv(muns_acteco %>% select(-pibe, -CVEENT), 
  "../data/resultados/acteco/por_municipio.csv")


## Gráfica de Luminosidad vs, PIBE, juntando ZMVM

edo_acteco <- muns_acteco %>% 
  select(CVEMUN, x175_loc, CVEENT, Estado, pibe) %>% 
  group_by(CVEENT, Estado) %>% 
  summarize(x175_loc = sum(x175_loc), 
            pibe = max(pibe)) %>% 
  inner_join(cnbv_edo_bancomer) %>% 
  inner_join(cnbv_edo_banamex) %>% 
  inner_join(cnbv_edo_otros) %>% 
  inner_join(cnbv_edo_valor)

edo_zmvm <- edo_acteco %>%
  mutate(zmvm = ifelse(str_detect(Estado, "México"), 
      "ZMVM", "no_ZMVM")) %>% 
  group_by(zmvm) %>% 
  summarize_at(vars(x175_loc, pibe,bancomer_2014,banamex_2014,otros_2014,valor_2014,
                    valor_2015), list(sum)) %>% 
  filter(zmvm == "ZMVM") %>% rename(Estado = zmvm) %>% 
  mutate(CVEENT = "09_15")

edo_acteco_ <- edo_acteco %>%  
  filter(!str_detect(Estado, "México")) %>% 
  bind_rows(edo_zmvm) 

 
gg_lineup <- edo_acteco_ %>% 
  ggplot(aes(x175_loc, pibe, color = Estado)) + 
    geom_point() + 
    geom_smooth(method = "lm", color = "cornflowerblue", se=FALSE, size=0.2) +
    scale_x_log10("Luminosidad (escala log)", 
        labels = . %>% divide_by(1e3)) +
    scale_y_log10("PIBE (escala log)", labels = . %>% divide_by(1e3)) + 
    scale_color_manual(values = brewer.pal(11, "BrBG")[c(1,2,3,9,10,11)] %>% rep(6)) +
    geom_text_repel(aes(label = Estado)) +
    theme_fivethirtyeight()+
    theme(legend.position = "none") 
print(gg_lineup)

plot(log(edo_acteco_$pibe),log(edo_acteco_$bancomer_2014))
plot(log(edo_acteco_$pibe),log(edo_acteco_$banamex_2014))
plot(log(edo_acteco_$pibe),log(edo_acteco_$otros_2014))
plot(log(edo_acteco_$pibe),log(edo_acteco_$valor_2014))


#Aquí se selecciona el modelo en función del lineup con el pib de 2014
lm_acteco<-lm(log(pibe)~log(x175_loc)+log(bancomer_2014)+log(banamex_2014)+
                log(otros_2014)+log(valor_2014),data=edo_acteco_)

lm_acteco<-lm(log(pibe)~log(valor_2014),data=edo_acteco_)
lm_acteco<-lm(log(pibe)~log(otros_2014),data=edo_acteco_)
lm_acteco<-lm(log(pibe)~log(bancomer_2014),data=edo_acteco_)

lm_acteco<-lm(log(pibe)~log(x175_loc)+log(banamex_2014),data=edo_acteco_) ####Usado en última versión
lm_acteco<-lm(log(pibe)~log(x175_loc)+log(valor_2014),data=edo_acteco_)

lm_acteco<-lm(log(pibe)~log(banamex_2014),data=edo_acteco_)

summary(lm_acteco)
vif(lm_acteco)

coefs<-coef(lm_acteco)


#tomamos solo los municipios de las ZM

muns_metro <- read_csv("../data/referencias" %>% file.path(
  "zonas_metro_estado_2020.csv")) %>% 
  mutate(CVEMET = CVEMET %>% str_pad(3, "left", "0"),
         CVEENT = CVEENT %>% str_pad(2, "left", "0"),
         CVEMUN = CVEMUN %>% str_pad(5, "left", "0")) %>% 
  select(CVEMET, CVEENT, CVEMUN, zona_metro = zona_met)

#Regresión con banamex
muns_acteco_lm<-muns_acteco %>% inner_join(muns_metro,by = c("CVEENT","CVEMUN")) %>%
  inner_join(filter(cnbv_muns_banamex,`2014-12-01`!=0,`2015-12-01`!=0,`2016-12-01`!=0,
                    `2017-12-01`!=0,`2018-12-01`!=0,`2019-12-01`!=0)) %>% 
  na.exclude() 

#Regresión con valor (todos los bancos)
muns_acteco_lm<-muns_acteco %>% inner_join(muns_metro,by = c("CVEENT","CVEMUN")) %>%
  inner_join(filter(cnbv_muns_valor,`2014-12-01`!=0,`2015-12-01`!=0,`2016-12-01`!=0,
                    `2017-12-01`!=0,`2018-12-01`!=0,`2019-12-01`!=0)) %>% 
  na.exclude() 

#Se puede escoger entre opción 1 (sin pivote en 2014) y opción 2 (con pivote 2014), 
#Despúes se escoge el tipo de derivada en función de as variables que componen el modelo (lm_contagio)

#Modelo escogido: lm_acteco<-lm(log(pibe)~log(x175_loc)+log(banamex_2014),data=edo_acteco_)
#Opción escogida: Opción 2

#Opción 1. Esta opción obtiene la actividad económica únicamente con los coeficientes
# y el intercepto de el modelo

muns_acteco_lm$ae_175_lm<-
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_loc)+coefs[3]*log(1+muns_acteco_lm$`2014-12-01`))
muns_acteco_lm$ae_175_2015_lm<-
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2015_loc)+coefs[3]*log(1+muns_acteco_lm$`2015-12-01`))
muns_acteco_lm$ae_175_2016_lm<-
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2016_loc)+coefs[3]*log(1+muns_acteco_lm$`2016-12-01`))
muns_acteco_lm$ae_175_2017_lm<-
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2017_loc)+coefs[3]*log(1+muns_acteco_lm$`2017-12-01`))
muns_acteco_lm$ae_175_2018_lm<-
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2018_loc)+coefs[3]*log(1+muns_acteco_lm$`2018-12-01`))
muns_acteco_lm$ae_175_2019_lm<-
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2019_loc)+coefs[3]*log(1+muns_acteco_lm$`2019-12-01`))

#Opción 2. Esta opción obtiene la actividad económica con el coeficiente asociado 
# a la luminosidad del modelo pero toma como punto inicial actividad económica de 2014 que es 
# el pibe 2014 repartido en localidades urbanas.
#Los años subsecuentes se calculan con la derivada de las variables

#Derivada de pibe_est sobre luminosidad

muns_acteco_lm$ae_175_2015_lm<-muns_acteco_lm$ae_175+
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_loc))*
  (0+coefs[2]*(1/muns_acteco_lm$x175_loc)*(muns_acteco_lm$x175_2015_loc-muns_acteco_lm$x175_loc))
  
muns_acteco_lm$ae_175_2016_lm<-muns_acteco_lm$ae_175_2015_lm+
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2015_loc))*
  (0+coefs[2]*(1/muns_acteco_lm$x175_2015_loc)*(muns_acteco_lm$x175_2016_loc-muns_acteco_lm$x175_2015_loc))

muns_acteco_lm$ae_175_2017_lm<-muns_acteco_lm$ae_175_2016_lm+
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2016_loc))*
  (0+coefs[2]*(1/muns_acteco_lm$x175_2016_loc)*(muns_acteco_lm$x175_2017_loc-muns_acteco_lm$x175_2016_loc))

muns_acteco_lm$ae_175_2018_lm<-muns_acteco_lm$ae_175_2017_lm+
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2017_loc))*
  (0+coefs[2]*(1/muns_acteco_lm$x175_2017_loc)*(muns_acteco_lm$x175_2018_loc-muns_acteco_lm$x175_2017_loc))

muns_acteco_lm$ae_175_2019_lm<-muns_acteco_lm$ae_175_2018_lm+
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2018_loc))*
  (0+coefs[2]*(1/muns_acteco_lm$x175_2018_loc)*(muns_acteco_lm$x175_2019_loc-muns_acteco_lm$x175_2018_loc))

#Derivada de pibe_est sobre valor (todos los bancos)

muns_acteco_lm$ae_175_2015_lm<-muns_acteco_lm$ae_175+
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$`2014-12-01`))*
  (0+coefs[2]*(1/muns_acteco_lm$`2014-12-01`)*(muns_acteco_lm$`2015-12-01`-muns_acteco_lm$`2014-12-01`))

muns_acteco_lm$ae_175_2016_lm<-muns_acteco_lm$ae_175_2015_lm+
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$`2015-12-01`))*
  (0+coefs[2]*(1/muns_acteco_lm$`2015-12-01`)*(muns_acteco_lm$`2016-12-01`-muns_acteco_lm$`2015-12-01`))

muns_acteco_lm$ae_175_2017_lm<-muns_acteco_lm$ae_175_2016_lm+
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$`2016-12-01`))*
  (0+coefs[2]*(1/muns_acteco_lm$`2016-12-01`)*(muns_acteco_lm$`2017-12-01`-muns_acteco_lm$`2016-12-01`))

muns_acteco_lm$ae_175_2018_lm<-muns_acteco_lm$ae_175_2017_lm+
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$`2017-12-01`))*
  (0+coefs[2]*(1/muns_acteco_lm$`2017-12-01`)*(muns_acteco_lm$`2018-12-01`-muns_acteco_lm$`2017-12-01`))

muns_acteco_lm$ae_175_2019_lm<-muns_acteco_lm$ae_175_2018_lm+
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$`2018-12-01`))*
  (0+coefs[2]*(1/muns_acteco_lm$`2018-12-01`)*(muns_acteco_lm$`2019-12-01`-muns_acteco_lm$`2018-12-01`))


#Derivada del pibe_est sobre luminosidad y banco (se pone el banco dependiendo del usado en 
#el modelo, esto se decide en la línea donde se define "muns_acteco_lm")

muns_acteco_lm$ae_175_2015_lm<- muns_acteco_lm$ae_175+

exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_loc)+coefs[3]*log(1+muns_acteco_lm$`2014-12-01`))*
  (coefs[2]*(1/muns_acteco_lm$x175_loc)*(muns_acteco_lm$x175_2015_loc-muns_acteco_lm$x175_loc))+ 

exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_loc)+coefs[3]*log(1+muns_acteco_lm$`2014-12-01`))*
  (coefs[3]*(1/(1+muns_acteco_lm$`2014-12-01`))*(muns_acteco_lm$`2015-12-01`- muns_acteco_lm$`2014-12-01`))

muns_acteco_lm$ae_175_2016_lm<- muns_acteco_lm$ae_175_2015_lm+
  
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2015_loc)+coefs[3]*log(1+muns_acteco_lm$`2015-12-01`))*
  (coefs[2]*(1/muns_acteco_lm$x175_2015_loc)*(muns_acteco_lm$x175_2016_loc-muns_acteco_lm$x175_2015_loc))+

  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2015_loc)+coefs[3]*log(1+muns_acteco_lm$`2015-12-01`))*
  (coefs[3]*(1/(1+muns_acteco_lm$`2015-12-01`))*(muns_acteco_lm$`2016-12-01`- muns_acteco_lm$`2015-12-01`))

muns_acteco_lm$ae_175_2017_lm<- muns_acteco_lm$ae_175_2016_lm+
  
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2016_loc)+coefs[3]*log(1+muns_acteco_lm$`2016-12-01`))*
  (coefs[2]*(1/muns_acteco_lm$x175_2016_loc)*(muns_acteco_lm$x175_2017_loc-muns_acteco_lm$x175_2016_loc))+
  
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2016_loc)+coefs[3]*log(1+muns_acteco_lm$`2016-12-01`))*
  (coefs[3]*(1/(1+muns_acteco_lm$`2016-12-01`))*(muns_acteco_lm$`2017-12-01`- muns_acteco_lm$`2016-12-01`))

muns_acteco_lm$ae_175_2018_lm<- muns_acteco_lm$ae_175_2017_lm+
  
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2017_loc)+coefs[3]*log(1+muns_acteco_lm$`2017-12-01`))*
  (coefs[2]*(1/muns_acteco_lm$x175_2017_loc)*(muns_acteco_lm$x175_2018_loc-muns_acteco_lm$x175_2017_loc))+
  
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2017_loc)+coefs[3]*log(1+muns_acteco_lm$`2017-12-01`))*
  (coefs[3]*(1/(1+muns_acteco_lm$`2017-12-01`))*(muns_acteco_lm$`2018-12-01`- muns_acteco_lm$`2017-12-01`))

muns_acteco_lm$ae_175_2019_lm<- muns_acteco_lm$ae_175_2018_lm+
  
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2018_loc)+coefs[3]*log(1+muns_acteco_lm$`2018-12-01`))*
  (coefs[2]*(1/muns_acteco_lm$x175_2018_loc)*(muns_acteco_lm$x175_2019_loc-muns_acteco_lm$x175_2018_loc))+
  
  exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_2018_loc)+coefs[3]*log(1+muns_acteco_lm$`2018-12-01`))*
  (coefs[3]*(1/(1+muns_acteco_lm$`2018-12-01`))*(muns_acteco_lm$`2019-12-01`- muns_acteco_lm$`2018-12-01`))


#muns_acteco ya trae info de actividad económica por municipio (tomando en cuenta localidades dentro del mismo) 
#ahora hay que calcular con base en la información de luminosidad el crecimiento de esa actividad económica

#sacamos crecimientos porcentuales en luminosidad por municipio y aplicamos esos crecimientos 
#a la actividad económica basada en el pibe 2014

muns_acteco_lm<-muns_acteco_lm %>% 
  mutate(crec_lumin_14_15=(x175_2015_loc - x175_loc)/x175_loc,
         crec_lumin_15_16=(x175_2016_loc - x175_2015_loc)/x175_2015_loc,
         crec_lumin_16_17=(x175_2017_loc - x175_2016_loc)/x175_2016_loc,
         crec_lumin_17_18=(x175_2018_loc - x175_2017_loc)/x175_2017_loc,
         crec_lumin_18_19=(x175_2019_loc - x175_2018_loc)/x175_2018_loc,
         ae_175_2015=ae_175*(1+crec_lumin_14_15),
         ae_175_2016=ae_175_2015*(1+crec_lumin_15_16),
         ae_175_2017=ae_175_2016*(1+crec_lumin_16_17),
         ae_175_2018=ae_175_2017*(1+crec_lumin_17_18),
         ae_175_2019=ae_175_2018*(1+crec_lumin_18_19)) 

#Esta opción la usamos cuando no pivoteamos (Opción 1) y sacamos el cálculo solo del lm
acteco_metro <- muns_acteco_lm %>% select(-Estado) %>% 
  group_by(CVEENT, CVEMET, zona_metro) %>% 
  summarize_at(.funs = funs(sum), 
               .vars = vars(ae_175,ae_175_2015,ae_175_2016,ae_175_2017,ae_175_2018,
                            ae_175_lm,ae_175_2015_lm,ae_175_2016_lm,ae_175_2017_lm,
                            ae_175_2018_lm,ae_175_2019_lm,
                            area, area_loc)) %>% 
  group_by(CVEMET, zona_metro) %>% 
  summarize(magda_2014 = sum(ae_175, na.rm = TRUE),
            magda_2015 = sum(ae_175_2015, na.rm = TRUE),
            magda_2016 = sum(ae_175_2016, na.rm = TRUE),
            magda_2017 = sum(ae_175_2017, na.rm = TRUE),
            magda_2018 = sum(ae_175_2018, na.rm = TRUE),
            magda_2019 = sum(ae_175_2019, na.rm = TRUE),
            magda_2014_lm = sum(ae_175_lm, na.rm = TRUE),
            magda_2015_lm = sum(ae_175_2015_lm, na.rm = TRUE),
            magda_2016_lm = sum(ae_175_2016_lm, na.rm = TRUE),
            magda_2017_lm = sum(ae_175_2017_lm, na.rm = TRUE),
            magda_2018_lm = sum(ae_175_2018_lm, na.rm = TRUE),
            magda_2019_lm = sum(ae_175_2019_lm, na.rm = TRUE)) 

#Esta opción se usa cuando pivoteamos el cálculo con la repartición de luminosidad 2014 (Opción 2)
acteco_metro <- muns_acteco_lm %>% select( -Estado) %>% 
  group_by(CVEENT, CVEMET, zona_metro) %>% 
  summarize_at(.funs = funs(sum), 
    .vars = vars(ae_175,ae_175_2015,ae_175_2016,ae_175_2017,ae_175_2018,ae_175_2019,
                 ae_175_2015_lm,ae_175_2016_lm,ae_175_2017_lm,ae_175_2018_lm,ae_175_2019_lm,
                 area,area_loc)) %>% 
  group_by(CVEMET, zona_metro) %>% 
  summarize(magda_2014 = sum(ae_175, na.rm = TRUE),
            magda_2015 = sum(ae_175_2015, na.rm = TRUE),
            magda_2016 = sum(ae_175_2016, na.rm = TRUE),
            magda_2017 = sum(ae_175_2017, na.rm = TRUE),
            magda_2018 = sum(ae_175_2018, na.rm = TRUE),
            magda_2019 = sum(ae_175_2019, na.rm = TRUE),
            magda_2014_lm = sum(ae_175, na.rm = TRUE),
            magda_2015_lm = sum(ae_175_2015_lm, na.rm = TRUE),
            magda_2016_lm = sum(ae_175_2016_lm, na.rm = TRUE),
            magda_2017_lm = sum(ae_175_2017_lm, na.rm = TRUE),
            magda_2018_lm = sum(ae_175_2018_lm, na.rm = TRUE),
            magda_2019_lm = sum(ae_175_2019_lm, na.rm = TRUE)) 

write_csv(acteco_metro, 
  "../data/resultados/acteco/por_zonas_metro_lum_banamex_piv.csv")

write_csv(acteco_metro, 
          "../data/resultados/acteco/por_zonas_metro_lum_valor_piv.csv")











