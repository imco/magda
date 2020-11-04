# Oscar Ruiz, IMCO
# CDMX, 12 de enero de 2018

library(ggrepel)
library(RColorBrewer)
library(magrittr)
library(readr)
library(dplyr)

filter <- dplyr::filter

# Los datos de luces llegan a variar con QGIS. 
# USAR_REFERENCIA para utilizar el dato original.

pibe_14 <- read_csv("../data/bie/processed/pibe.csv") %>% 
  filter(año == "2014-12-01")

muns_acteco <- read_csv("../data/viirs/processed/mun_luces_175.csv",
      col_types = "ccdddddddd") %>% 
  mutate(CVEENT = CVEMUN %>% str_sub(1, 2)) %>% 
  left_join(by="CVEENT", 
      pibe_14 %>% select(CVEENT, Estado, pibe)) %>% 
  group_by(CVEENT, Estado) %>% 
  mutate(ae_175 = pibe*x175_loc/sum(x175_loc)) %>%
  ungroup 



write_csv(muns_acteco %>% select(-pibe, -CVEENT), 
  "../data/resultados/acteco/por_municipio.csv")


## Gráfica de Luminosidad vs, PIBE, juntando ZMVM

edo_acteco <- muns_acteco %>% 
  select(CVEMUN, x175_loc, CVEENT, Estado, pibe) %>% 
  group_by(CVEENT, Estado) %>% 
  summarize(x175_loc = sum(x175_loc), 
            pibe = max(pibe)) 

edo_zmvm <- edo_acteco %>%
  mutate(zmvm = ifelse(str_detect(Estado, "México"), 
      "ZMVM", "no_ZMVM")) %>% 
  group_by(zmvm) %>% 
  summarize_at(vars(x175_loc, pibe), funs(sum)) %>% 
  filter(zmvm == "ZMVM") %>% rename(Estado = zmvm) %>% 
  mutate(CVEENT = "09_15")

edo_acteco_ <- edo_acteco %>% 
  filter(!str_detect(Estado, "México")) %>% 
  bind_rows(edo_zmvm)

 
gg_lineup <- edo_acteco_ %>% 
  ggplot(aes(x175_loc, pibe, color = Estado)) + 
    geom_point() + 
    geom_smooth(method = "lm", color = "cornflowerblue", se=FALSE) +
    scale_x_log10("Luminosidad (escala log)", 
        labels = . %>% divide_by(1e3)) +
    scale_y_log10("PIBE (escala log)", labels = . %>% divide_by(1e3)) + 
    scale_color_manual(values = brewer.pal(8, "Dark2") %>% rep(4)) +
    geom_text_repel(aes(label = Estado)) + 
    theme(legend.position = "none")
print(gg_lineup)

lm_acteco<-lm(log(pibe)~log(x175_loc),data=edo_acteco_)
summary(lm_acteco)

coefs<-coef(lm_acteco)

muns_acteco_lm<-muns_acteco

muns_acteco_lm$ae_175_lm<-exp(coefs[1]+coefs[2]*log(muns_acteco_lm$x175_loc))

#ggsave(plot = gg_lineup, 
#    "../visualization/figures/scatter_luces_log.png", 
#    width = 16, height = 9, dpi = 100)

muns_metro <- read_csv("../data/referencias" %>% file.path(
      "zonas_metro_estado_ok.csv")) %>% 
  mutate(CVEMET = CVEMET %>% str_pad(3, "left", "0"),
      CVEENT = CVEENT %>% str_pad(2, "left", "0"),
      CVEMUN = CVEMUN %>% str_pad(5, "left", "0")) %>% 
  select(CVEMET, CVEENT, CVEMUN, zona_metro = nombre_corto)

#muns_acteco ya trae info de actividad económica por municipio (tomando en cuenta localidades dentro del mismo) 
#ahora hay que calcular con base en la información de luminosidad el crecimiento de esa actividad económica

#sacamos crecimientos porcentuales en luminosidad por municipio y aplicamos esos crecimientos 
#a la actividad económica basada en el pibe 2014

muns_acteco<-muns_acteco %>% 
  mutate(crec_lumin_14_15=(x175_2015_loc - x175_loc)/x175_loc,
         crec_lumin_15_16=(x175_2016_loc - x175_2015_loc)/x175_2015_loc,
         ae_175_2015=ae_175*(1+crec_lumin_14_15),
         ae_175_2016=ae_175_2015*(1+crec_lumin_15_16)) 

acteco_metro <- muns_acteco %>% select(-CVEENT, -Estado) %>% 
  right_join(muns_metro, by="CVEMUN") %>% 
  group_by(CVEENT, CVEMET, zona_metro) %>% 
  summarize_at(.funs = funs(sum), 
    .vars = vars(ae_175,ae_175_2015,ae_175_2016, area, area_loc)) %>% 
  group_by(CVEMET, zona_metro) %>% 
  summarize(magda_2014 = sum(ae_175, na.rm = TRUE),
            magda_2015 = sum(ae_175_2015, na.rm = TRUE),
            magda_2016 = sum(ae_175_2016, na.rm = TRUE)) 

write_csv(acteco_metro, 
  "../data/resultados/acteco/por_zonas_metro.csv")











