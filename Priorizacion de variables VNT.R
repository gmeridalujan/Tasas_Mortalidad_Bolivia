## PRIORIZACION DE VARIABLES  VENTANILLAS

# Librerias ####

library(RODBC)
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(reshape2)
library(nortest)
library(RVAideMemoire)
library(exact2x2)

# Base Dimension ACH ####

cnn_dmd<-odbcDriverConnect(connection="Driver={SQL Server Native Client 11.0};
                                  server=btbdmd00;
                                  database=BD_DATA_CANALES_2;
                                  trusted_connection=yes;")

Q1<-"SELECT A.CIC as CIC_D, A.PERIODO as PERIODO_D, MADURACION,Producto,B.* 
     FROM [BD_DATA_CANALES_2].[cfi].[BASE_CFO_POBLACION] A
     left join [BD_DATA_CANALES_2].[cfi].[PROP_VNT] B on A.CIC = B.CIC and A.PERIODO = B.PERIODO
     where A.PERIODO != '202205'"

bd<-sqlQuery(channel = cnn_dmd,query = Q1,as.is=T)

for (i in 8:41) {
  bd[,i]<-as.numeric(bd[,i])
}

odbcClose(cnn_dmd)

# Descipcion de datos ####

summary(bd)

# Tratamiento de datos ####

COLS<-c("CIC_D","PERIODO_D","MADURACION","Producto","CIC","PERIODO")

VAR_i<-colnames(bd)[!colnames(bd) %in%COLS ]

INDICADORES_FLG<-VAR_i[str_detect(string = VAR_i,pattern = "flg")]
INDICADORES_NUM<-VAR_i[!str_detect(string = VAR_i,pattern = "flg")]
INDICADORES_NUM<-INDICADORES_NUM[!str_detect(string = INDICADORES_NUM,pattern = "fec_ult_")]
INDICADORES_NUM<-INDICADORES_NUM[!str_detect(string = INDICADORES_NUM,pattern = "PERIODO_0106")]
INDICADORES_IMPUT_MAX<-INDICADORES_NUM[str_detect(string = INDICADORES_NUM,pattern = "rec_")]


for (i1 in INDICADORES_NUM) {
  bd[,i1]<- as.numeric(bd[,i1])
}

# Imput Maaximo

for (i in INDICADORES_IMPUT_MAX) {
  bd[is.na(bd[,i]),i]<-max(bd[,i],na.rm = T)
}

# IMPUT 0

bd[is.na(bd)]<-0

# Priorizacion ####

alpha<-0.005
Resultados<-data.frame()
k<-"VNT"

# Pruebas de Numericas ####
for (i in 1:6){
  for (h in -6:-1) {
    for (j in INDICADORES_NUM){
      
      # Filtrando solo Clientes Pareadas
      bd_i<-bd %>% filter(MADURACION==h | MADURACION==i) %>% select("CIC_D","MADURACION",j)
      
      # Pivoteando el Antes y Despúes
      bd_i$MADURACION<-ifelse(bd_i$MADURACION>0,"i_despues","i_antes")
      bd_i<-bd_i %>% filter(CIC_D %in% names(table(bd_i$CIC_D)[table(bd_i$CIC_D)==2]))
      bd_p<-dcast(bd_i, CIC_D ~ MADURACION)
      
      # Pruebas estadisticas
      
      w<-wilcox.test(bd_p$i_antes,bd_p$i_despues)$p.value # Wilcoxon
      
      w<-ifelse(is.na(w),1,w)
      
      t<-t.test(bd_p$i_antes,bd_p$i_despues)$p.value      # t student
      
      t<-ifelse(is.na(t),1,t)
      
      #dif <- bd_p$i_antes - bd_p$i_despues                   # Lilifors
      #lf  <-if (sd(dif)==0) w else lillie.test(dif)$p.value  # Lilifors
      
      if (w<alpha & t<alpha ) {
        Resultados<-rbind(Resultados,c(k,h,i,j,w,t,mean(bd_p$i_antes)-mean(bd_p$i_despues)))
      }
    }
  }
}

names(Resultados)<-c("Dimension","Maduracion_Negativa","Maduracion","Indicador","Wilcoxon","T_student","Promedio")
table(Resultados$Maduracion,Resultados$Maduracion_Negativa)

unique(Resultados$Variable)

Resultados

write.csv(x = Resultados,file = "D:/Guillermo/Credifondo (Proyecto)/General/EXCELS/Priorizacion_Estaditica/Resultados_numericos_VNT.csv",row.names = FALSE)
