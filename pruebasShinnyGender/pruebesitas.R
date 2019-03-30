library(readxl)
library(dplyr)



datos_2009<-read_excel("/home/isa/Escritorio/Enlace hacia dataScience/proyecto/DataScienceGenderEquityStudy/pruebasShinnyGender/wbl2019paneldata.xlsx",sheet="WBL2009")

hist(datos_2009$"HAVING_CHILDREN")

datos<-read_excel("/home/isa/Escritorio/Enlace hacia dataScience/proyecto/DataScienceGenderEquityStudy/pruebasShinnyGender/wbl2019paneldata.xlsx",sheet="WBL_panel_long")


datos_2010<-datos %>% filter(reportyr =="2010")
datos_2011<-datos %>% filter(reportyr =="2011")

hist(datos_2010$`GOING PLACES`)
