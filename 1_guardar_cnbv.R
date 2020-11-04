# Oscar Ruiz, IMCO
# CDMX, 17 de julio de 2017

filter <- dplyr::filter
melt <- reshape2::melt

dir_cnbv <- "../data/cnbv"

# posible diferencia por:  
#   empieza <- "2011-03-01"
empieza <- "2011-03-01" %>% as.Date
termina <- "2020-05-01" %>% as.Date


# archivo, pestaña, output, bancos = NULL
leer_multibanca <- function (pestagna_id, periodo, 
      output = FALSE, bancos = NULL) {
  require(zoo)
  require(readxl)
  
  # Lee archivo de Banca Múltiple
  nombre_ <- pestagna_id
  if (output) cat(sprintf("%-32s %s\n", nombre_, periodo))
      
    archivo_ <- file.path(dir_cnbv, "raw", "BM_Operativa_%s.xls") %>% 
      sprintf(periodo)
    if (file.exists(archivo_)) {
    mb_ <- read_excel(archivo_, sheet = nombre_, skip = 1) %>%
      { names(.)[1:4] <- c("entidad", "cvegeo", "colonia", "valor")
        as_data_frame(.)} %>%  
      filter(!is.na(colonia)) %>%  # Los totales por estado
      slice(-1) %>%  # La primera fila es un total también
      mutate(
          entidad = na.locf(entidad),
          cvegeo  = na.locf(cvegeo) %>% str_sub(4, 13),
          fecha   = periodo %>% sprintf("%s01", .) %>% ymd,
          tipo    = nombre_)
    
    bancos_ <- intersect(bancos, names(mb_)) %>% sprintf("`%s`", .)
    mb <- select_(mb_, .dots = union(bancos_, 
        c("tipo","cvegeo","colonia","fecha","valor")))
  } else {
    # pestagna_id <- "Num de Transac en Cajeros Aut"
    # periodo <- "201612"
    # output <- FALSE
    # bancos <- c("BBVA Bancomer", "Banamex")
    
    pestañas_rnm <- c(
       "Número de Transacciones en Cajeros Automáticos" = 
            "Num de Transac en Cajeros Aut")
    
    archivo_ <- file.path(dir_cnbv, "raw", "BM_Operativa_%s.xlsx") %>% 
      sprintf(periodo)
    
    if ("Hoja1" %in% excel_sheets(archivo_)) {
      mb_0 <- read_excel(archivo_, sheet = "Hoja1", col_types = 
          c("text", "text", "text", "text", "text", "text", "skip",
            "text", "numeric", "skip", "skip"),range = cell_cols("A:K")) %>% 
        select(tipo  = dl_producto_financiero, 
            cvegeo  = cve_inegi, 
            colonia = dl_localidad,  # Lo usamos para consistencia. 
            fecha = cve_periodo, 
            valor = dat_num_total,
            entidad = dl_estado, 
            banco = nombre_publicacion) %>% 
        mutate(fecha = fecha %>% str_c("01") %>% as.Date("%Y%m%d"),
            cvegeo = cvegeo %>% str_sub(4, 13), 
            tipo = tipo %>% str_replace_all(pestañas_rnm)) %>% 
        filter(tipo == pestagna_id, cvegeo != "L")
    } else {
      mb_0 <- read_excel(archivo_, sheet = "Datos", col_types = 
          c("text", "text", "text", "text", "text", "text", "skip",
            "text", "numeric", "skip", "skip", "skip"),range = cell_cols("A:L")) %>% 
        select(tipo = `Producto Financiero`, 
            cvegeo  = `Cve Localidad`, 
            colonia = Localidad,  # Lo usamos para consistencia. 
            fecha = `Periodo (clave)`, 
            valor = Total,
            entidad = Estado, 
            banco = `Institución`) %>% 
        mutate(fecha = fecha %>% str_c("01") %>% as.Date("%Y%m%d"),
            cvegeo = cvegeo %>% str_sub(4, 13), 
            tipo = tipo %>% str_replace_all(pestañas_rnm)) %>% 
        filter(tipo == pestagna_id, cvegeo != "L")
    }
    
    mb_1 <- mb_0 %>% 
      group_by(tipo, cvegeo, colonia, fecha, entidad) %>% 
      summarize_at("valor", . %>% sum(na.rm = TRUE)) %>% 
      mutate(banco = "valor")
    
    mb_2 <- mb_0 %>% 
      filter(banco %in% bancos) %>% 
      group_by(tipo, cvegeo, colonia, fecha, entidad, banco) %>% 
      summarize_at("valor", . %>% sum(na.rm = TRUE))
    
    mb <- bind_rows(mb_1, mb_2) %>% 
      spread(banco, valor)
  }
  return (mb)
}


pestagnas_df <- read_csv("../data/referencias/pestañas_cnbv.csv", 
  locale = locale(encoding = "latin1")) %>% 
  filter(Alias == "transacciones_atm")
periodos <- seq(empieza, termina, by = "1 month") %>% 
  format("%Y%m")


MB_frame_ <- expand.grid(pestagnas_df$Nombre, periodos, 
      stringsAsFactors=FALSE) %>%
  apply(1, . %>% {leer_multibanca(.[1], .[2], output=TRUE, 
      bancos = c("BBVA Bancomer", "Banamex") )}) 

MB_frame <- MB_frame_ %>% 
  bind_rows %>% 
  mutate(tipo = pestagnas_df %$% Alias[match(tipo, Nombre)]) %>% 
            rename(bancomer = `BBVA Bancomer`, banamex = Banamex) %>% 
  filter(!(cvegeo %>% str_detect("blanco")))

MB_x11 <- MB_frame %>% # filter(tipo == "transacciones_atm") %>% 
  mutate(CVEMUN = str_sub(cvegeo, 1, 5)) %>% 
  select(-tipo, -colonia, -cvegeo) %>% 
  group_by(CVEMUN, fecha) %>% 
  summarize_at(vars(bancomer, banamex, valor),
               funs(. %>% sum(na.rm = T))) %>%
  mutate(otros = valor - bancomer - banamex) 

write_csv(MB_x11, "../data/cnbv/processed/grupos_municipios_prex11.csv")

muns_metro <- read_csv("../data/referencias" %>% file.path(
  "zonas_metro_estado_2020.csv")) %>% 
  mutate(CVEMET = CVEMET %>% str_pad(3, "left", "0"),
         CVEENT = CVEENT %>% str_pad(2, "left", "0"),
         CVEMUN = CVEMUN %>% str_pad(5, "left", "0")) %>% 
  select(CVEMET, CVEENT, CVEMUN, zona_metro = nombre_corto)

mdf<- MB_x11 %>% filter(CVEMUN %in% muns_metro$CVEMUN[which(muns_metro$zona_metro=="ZMVM")]) %>% 
  group_by(fecha) %>% 
  summarise(bancomer=sum(bancomer),
            otros=sum(otros),
            banamex=sum(banamex),
            valor=sum(valor)) %>% 
  melt(id.vars="fecha")

ggplot(mdf,aes(fecha,value,color=variable)) + geom_line()



