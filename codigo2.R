consulta_aemet<- function(altitud_, nmeses, fecha_inic, provincia_) {
  
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
consulta_aemet(altitud_=1000, nmeses=6, fecha_inic="2022-03-31", provincia_="MADRID")

 