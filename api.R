library(plumber)
#* @apiTitle AEMET
#* @apiDescription graficos de temp y prec


#* @param altitud_ Selecciona la altitud minima
#* @param fecha_inic Selecciona la fecha de referencia yyyy-mm-dd
#* @param nmeses Selecciona numero de meses para seleccionar
#* @param provincia_ Selecciona provincia en mayusculas
#* @serializer png
#* @get /precipitaciones

consulta_aemet<- function(altitud_, fecha_inic,nmeses, provincia_) {
  library(climaemet)
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  nmeses<- as.numeric(nmeses)
  altitud_=as.numeric(altitud_)
  apikey = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJibXVuaXpAbW9uZHJhZ29uLmVkdSIsImp0aSI6IjM5N2Y0Zjg2LTE0N2MtNGFjYy05ZjI4LTExYjZiODk3NzE4MCIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNjQ3MjY0NzA4LCJ1c2VySWQiOiIzOTdmNGY4Ni0xNDdjLTRhY2MtOWYyOC0xMWI2Yjg5NzcxODAiLCJyb2xlIjoiIn0.h4cBR_lOro719PbcSJ_4J3DtggMDV2dCcyolWDWv6TQ"
  aemet_api_key(apikey, overwrite=TRUE, install=TRUE)
  datos_diarios <- aemet_daily_clim(
    station = "all",
    start = as.Date(fecha_inic)-months(nmeses-1),
    end = as.Date(fecha_inic),
    verbose = FALSE,
    return_sf = FALSE
  )
  
  datos_diarios$prec <- as.numeric(gsub(",",".",datos_diarios$prec))
  datos_diarios$mes <- month(datos_diarios$fecha)
  
  datos_diarios_filt <- datos_diarios %>%
    filter(provincia==provincia_ & altitud<altitud_) %>%
    group_by (mes) %>%
    summarize(tmed_media=mean(tmed, na.rm=TRUE), prec_media=mean(prec, na.rm=TRUE))
  
  temperatura<-ggplot(datos_diarios_filt, aes(x= mes, y =tmed_media))+ geom_line()+ theme_bw()
  precipitaciones<-ggplot(datos_diarios_filt, aes(x= mes, y =prec_media))+ geom_line()+ theme_bw()
  graficos<-grid.arrange(precipitaciones, temperatura,nrow=1,ncol=2 )
}
