# Diego Villamil, OPI
# CDMX, 4 de mayo de 2017
# May the fourth be with you


# Verificar cuál es el último mes disponible en 
# http://portafolioinfo.cnbv.gob.mx/PUBLICACIONES/IO/Paginas/bm.aspx
ultimo_mes <- as.Date("2019-12-01")


tags_meses <- seq(as.Date("2011-03-01"), ultimo_mes, by = "1 month") %>% 
  format("%Y%m")
bm_raw_dir <- "../data/cnbv/raw/"


descarga_bm <- function (fecha_tag, folder) {
  url_0 <- "https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BM_Operativa_%s.xls" %>% sprintf(fecha_tag) 
  download.file(url_0, method="curl", 
      destfile = file.path(folder, basename(url_0)))

  # Algunos archivos cambiaron de manera desconocida.
  # se identifican por el tamaño del archivo, y la corrección es en
  # la versión de Excel del archivo .xls por .xlsx
  if (file.size(file.path(folder, basename(url_0))) < 1000) {
    url_1 <- str_replace(url_0, ".xls$", ".xlsx")
    download.file(url_1, method="curl", 
        destfile = file.path(folder, basename(url_1)))
    file.remove(file.path(folder, basename(url_0)))
  }
}

tmp_var <- lapply(tags_meses, descarga_bm, bm_raw_dir)




