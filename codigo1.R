library(climaemet)
apikey = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJibXVuaXpAbW9uZHJhZ29uLmVkdSIsImp0aSI6IjM5N2Y0Zjg2LTE0N2MtNGFjYy05ZjI4LTExYjZiODk3NzE4MCIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNjQ3MjY0NzA4LCJ1c2VySWQiOiIzOTdmNGY4Ni0xNDdjLTRhY2MtOWYyOC0xMWI2Yjg5NzcxODAiLCJyb2xlIjoiIn0.h4cBR_lOro719PbcSJ_4J3DtggMDV2dCcyolWDWv6TQ"
aemet_api_key(apikey, overwrite = T, install = T)

datos<-aemet_daily_clim(
  station = "all",
  start = '2022-02-13',
  end = '2022-06-13',
  verbose = FALSE,
  return_sf = FALSE
)
library(dplyr)
cadiz<-datos%>%
  filter(provincia=='CADIZ' & altitud<500)
library(lubridate)
cadiz$fecha<-month(cadiz$fecha)
cadiz$prec <- as.numeric(gsub(",",".",cadiz$prec))

cadiz<-cadiz%>%
  group_by(provincia, fecha)%>%
  summarise(pre_med=mean(prec, na.rm=T),tem_med=mean(tmed,na.rm=T))

library(ggplot2)
precipitaciones<-ggplot(cadiz, aes(x=fecha, y=pre_med))+geom_line()+theme_bw()
temperatura<-ggplot(cadiz, aes(x=fecha, y=tem_med))+geom_line()+theme_bw()
library(gridExtra)
graficos<-grid.arrange(precipitaciones, temperatura,nrow=1,ncol=2 )

