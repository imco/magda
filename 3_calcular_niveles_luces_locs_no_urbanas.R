#Saca la luminosidad y su crecimiento de localidades no urbanas definidas como
#toda el área de los municipios que no está clasificada como localdad urbana

muns_lumin <- read_csv("../data/viirs/processed/mun_luces_175.csv",
                        col_types = "ccdddddddddd") %>% 
  mutate(CVEENT = CVEMUN %>% str_sub(1, 2)) %>% 
  left_join(by="CVEENT", 
            pibe_14 %>% select(CVEENT, Estado, pibe)) %>% 
  group_by(CVEENT, Estado) %>% 
  mutate(ae_175 = ifelse((pibe*x175_loc/sum(x175_loc))>=0,yes = pibe*x175_loc/sum(x175_loc),no = 0)) %>%
  ungroup %>% filter(x175>0)

# x175 sale primero de topar el valor de cada pixel a 175 y luego sumar (via zonal statistics de QGIS)
# los valores de los pixeles que están dentro de los políginos municipales

#luminosidad total
sum(muns_lumin$x175)
sum(muns_lumin$x175_2015)
sum(muns_lumin$x175_2016)
sum(muns_lumin$x175_2017)


#luminosidad urbana total
sum(muns_lumin$x175_loc)
sum(muns_lumin$x175_2015_loc)
sum(muns_lumin$x175_2016_loc)
sum(muns_lumin$x175_2017_loc)

#luminosidad total - urbanal
sum(muns_lumin$x175-muns_lumin$x175_loc)
sum(muns_lumin$x175_2015-muns_lumin$x175_2015_loc)
sum(muns_lumin$x175_2016-muns_lumin$x175_2016_loc)

#Veracruz

#luminosidad total
sum(muns_lumin$x175[which(muns_lumin$Estado=="Veracruz")])
sum(muns_lumin$x175_2015[which(muns_lumin$Estado=="Veracruz")])
sum(muns_lumin$x175_2016[which(muns_lumin$Estado=="Veracruz")])

#luminosidad urbana total
sum(muns_lumin$x175_loc[which(muns_lumin$Estado=="Veracruz")])
sum(muns_lumin$x175_2015_loc[which(muns_lumin$Estado=="Veracruz")])
sum(muns_lumin$x175_2016_loc[which(muns_lumin$Estado=="Veracruz")])

#luminosidad total - urbana
sum(muns_lumin$x175[which(muns_lumin$Estado=="Veracruz")]-muns_lumin$x175_loc[which(muns_lumin$Estado=="Veracruz")])
sum(muns_lumin$x175_2015[which(muns_lumin$Estado=="Veracruz")]-muns_lumin$x175_2015_loc[which(muns_lumin$Estado=="Veracruz")])
sum(muns_lumin$x175_2016[which(muns_lumin$Estado=="Veracruz")]-muns_lumin$x175_2016_loc[which(muns_lumin$Estado=="Veracruz")])



#Una hipótesis es que el entorno no urbano estuviera disminuyendo y yéndose a 
#ciudades cercanas aumentando así la actividad de algunas ciudades que no pintaban
#(Checar con manuel molano)


muns_acteco_lm_piv<-muns_acteco_lm

