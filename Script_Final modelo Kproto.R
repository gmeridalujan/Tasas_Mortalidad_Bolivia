# Kprototypes Final

# Librerias ####

library(dplyr)
library(RODBC)
library(stringr)
library(ggplot2)
library(clustMixType)
library(tidyverse)

# Datos ####

cnn_dmd<-odbcDriverConnect(connection="Driver={SQL Server Native Client 11.0};
                                  server=btbdmd00;
                                  database=BD_DATA_CANALES_2;
                                  trusted_connection=yes;")

Q1=("
SELECT A.CIC as CIC_D, A.PERIODO as PERIODO_D, MADURACION,Producto
,ACH_ENV_CNT_CTA_TOT--
,ACH_REC_AVG_MTO_TOT_0106--
,ACH_REC_SD_MTO_TOT_0106--
,D.cnt_cred_cta_bol_0106       AS MOV_PAS_cnt_cred_cta_bol_0106
,D.cnt_deb_cta_bol_0106    AS MOV_PAS_cnt_deb_cta_bol_0106
,D.mnt_max_cred_cta_bol_0106 AS MOV_PAS_mnt_max_cred_cta_bol_0106
,D.mnt_max_deb_cta_bol_0106  AS MOV_PAS_mnt_max_deb_cta_bol_0106
,D.tot_mnt_cred_cta_bol_0106 AS MOV_PAS_tot_mnt_cred_cta_bol_0106
,MTO_CMP_POS_06
,PROP_DEM_EDAD
,PROP_DEM_FLG_DIR_GER = CASE WHEN C.PROP_DEM_FLG_DIR_M =1 OR C.PROP_DEM_FLG_GER_M =1 THEN 1 ELSE 0 END
,C.PROP_DEM_FLG_ENALTA
,C.PROP_DEM_FLG_PREMIUM
,C.PROP_DEM_FLG_EXCLUSIVO
,C.PROP_DEM_FLG_VIGENTE
,H.mnt_tot_depaho_bol_0106 AS VNT_mnt_tot_depaho_bol_0106
,H.mnt_tot_retaho_bol_0106 AS VNT_mnt_tot_retaho_bol_0106
,DWH_SLD_CIERRE_DOL
FROM [BD_DATA_CANALES_2].[cfi].[BASE_CFO_POBLACION] A
left join [BD_DATA_CANALES_2].[cfi].[PROP_ACH] B on A.CIC = B.CIC and A.PERIODO = B.PERIODO
left join [BD_DATA_CANALES_2].[cfi].[PROP_DEM] C on A.CIC = C.CIC and A.PERIODO = C.PERIODO_CORTE
left join [BD_DATA_CANALES_2].[cfi].[PROP_MOVPAS] D on A.CIC = D.CIC and A.PERIODO = D.PERIODO
--left join [BD_DATA_CANALES_2].[cfi].[PROP_ATM] E on A.CIC = E.CIC and A.PERIODO = E.PERIODO_CORTE
left join [BD_DATA_CANALES_2].[cfi].[PROP_CMP] F on A.CIC = F.CIC and A.PERIODO = F.PERIODO
--left join [BD_DATA_CANALES_2].[cfi].[PROP_BMOV_IBK] G on A.CIC = G.CIC and A.PERIODO = G.PERIODO
left join [BD_DATA_CANALES_2].[cfi].[PROP_VNT] H on A.CIC = H.CIC and A.PERIODO = H.PERIODO
left join [BD_DATA_CANALES_2].[cfi].[PROP_SALD_PAS] I on A.CIC = I.CIC and A.PERIODO = I.PERIODO
where A.PERIODO = '202205' and left(A.CIC,2)='  ' and Producto != 'BCP_PAS'
")

bd<-sqlQuery(channel = cnn_dmd,query = Q1,as.is=T)

odbcClose(cnn_dmd)

# Resumen de Datos

summary(bd)

# Tratamiento de datos ####

COLS<-c("CIC_D","PERIODO_D","MADURACION","Producto")
VAR_i<-colnames(bd)[!colnames(bd) %in%COLS ]

INDICADORES_FLG<-VAR_i[str_detect(string = VAR_i,pattern = "FLG")]
INDICADORES_NUM<-VAR_i[!str_detect(string = VAR_i,pattern = "FLG")]

# Transformando a numericos y factores
for (i in INDICADORES_NUM) {
  bd[,i]<-as.numeric(bd[,i])
}

for (i in INDICADORES_FLG) {
  bd[,i]<-as.factor(bd[,i])
}

# variable edad

bd$PROP_DEM_EDAD<-ifelse(test = bd$PROP_DEM_EDAD >=99,yes = 99,no = bd$PROP_DEM_EDAD)
mean(bd$PROP_DEM_EDAD,na.rm = T) # 50 Años
bd[is.na(bd[,"PROP_DEM_EDAD"]),"PROP_DEM_EDAD"]<-50

# imputando con 0
bd[is.na(bd)]<-0

# Base Estandarizada
bd_fin <- bd[,VAR_i]

for (i in INDICADORES_NUM) {
  bd_fin[,i]<-scale(bd_fin[,i])
}

# Iteracion1 Cantidad de Perfiles ####

x_1 <- numeric(10)
for(i in 1:10){
  set.seed(12345)
  kpres <- kproto(bd_fin, k = i, nstart = 5)
  x_1[i] <- kpres$tot.withinss
}

plot(1:10, x_1, type = "b", ylab = "Objective Function", xlab = "# Clusters",main = "Scree Plot")

# Modelo 1 ####

set.seed(12345)
kpres <- kproto(bd_fin, k = 8, nstart = 5)

bd$cluster_iter1 <- kpres$cluster

bd %>% group_by(cluster_iter1) %>% summarise(n())

# Excluyendo los datos Atipicos Iter 1 ####

row.names(bd)<-bd$CIC_D
bd_fin_2 <- bd %>% filter(!cluster_iter1 %in% 7) %>% select(VAR_i)

bd_fin_2_MEAN<-apply(bd_fin_2[,INDICADORES_NUM], 2, mean)
bd_fin_2_SD<-apply(bd_fin_2[,INDICADORES_NUM], 2, sd)

# estandarizando

bd_fin_2[,INDICADORES_NUM]<-scale(bd_fin_2[,INDICADORES_NUM],center = bd_fin_2_MEAN,scale = bd_fin_2_SD)

# Iteracion2 Cantidad de Perfiles ####

x_1 <- numeric(8)
for(i in 1:8){
  set.seed(12345)
  kpres <- kproto(bd_fin_2, k = i, nstart = 3, iter.max = 1000)
  x_1[i] <- kpres$tot.withinss
}

plot(1:8, x_1, type = "b", ylab = "Objective Function", xlab = "# Clusters",main = "Scree Plot")

# Modelo 2 ####

set.seed(1234)
kpres<-kproto(bd_fin_2,k=7,nstart=3,iter.max = 1000)
table(kpres$cluster)
bd_fin_2$cluster_iter2<-kpres$cluster

x_Cluster<-data.frame(CIC_D=row.names(bd_fin_2),cluster_iter2=bd_fin_2$cluster_iter2)

bd<-left_join(bd,x_Cluster,by = c("CIC_D"="CIC_D"))
row.names(bd)<-bd$CIC_D
bd %>% group_by(cluster_iter2) %>% summarise(n())

# Predicción  ####
bd_fin_test<-bd %>% select(VAR_i)

# estandarizando

bd_fin_test[,INDICADORES_NUM]<-scale(bd_fin_test[,INDICADORES_NUM],center = bd_fin_2_MEAN,scale = bd_fin_2_SD)

bd$CLUSTER_PREDICT<-predict(object = kpres,newdata = bd_fin_test)$cluster

bd_fin_test$CLUSTER_PREDICT <-predict(object = kpres,newdata = bd_fin_test)$cluster

table(bd$CLUSTER_PREDICT)
table(bd$cluster_iter1)
table(bd$cluster_iter2)

# Guardando datos ####

# Modelo 

# Entrenamiento y centroides

# Base entrenamiento 
write.csv(x = bd %>% filter(!cluster_iter1 %in% 7) %>% select(VAR_i),file = "D:/Guillermo/Credifondo (Proyecto)/General/R/Modelos/Datos Finales/BASE_ENTRENAMIENTO.csv")

# Media y Desviacion Estandar
write.csv(x = bd_fin_2_MEAN,file = "D:/Guillermo/Credifondo (Proyecto)/General/R/Modelos/Datos Finales/Media_VAR_NUMERICAS.csv")
write.csv(x = bd_fin_2_SD,file = "D:/Guillermo/Credifondo (Proyecto)/General/R/Modelos/Datos Finales/DESVIAC_VAR_NUMERICAS.csv")

# Modelo 
saveRDS(object = kpres,file = "D:/Guillermo/Credifondo (Proyecto)/General/R/Modelos/Datos Finales/Modelo_Final_kpro.rds")

# Centroides
write.csv(x = kpres$centers,file = "D:/Guillermo/Credifondo (Proyecto)/General/R/Modelos/Datos Finales/Centroides.csv")

# Interpretacion del Modelo ####

# Grafico de Barras
for (i in INDICADORES_NUM) { 
  print(ggplot(bd) + 
          geom_boxplot(mapping = aes(x=as.factor(CLUSTER_PREDICT),y=bd[,i],fill=as.factor(CLUSTER_PREDICT))) + 
          ggtitle(label = i) +
          theme(legend.position = "none") +labs(x = "Segmento"), y = names(bd[i])) 
}

# Grafico de medianas

bd_cluster_medias<-bd_fin_test %>% group_by(CLUSTER_PREDICT) %>% 
  summarise(ACH_ENV_CNT_CTA_TOT=mean(ACH_ENV_CNT_CTA_TOT),
            ACH_REC_AVG_MTO_TOT_0106=mean(ACH_REC_AVG_MTO_TOT_0106),
            ACH_REC_SD_MTO_TOT_0106=mean(ACH_REC_SD_MTO_TOT_0106),
            MOV_PAS_cnt_cred_cta_bol_0106=mean(MOV_PAS_cnt_cred_cta_bol_0106),
            MOV_PAS_mnt_max_cred_cta_bol_0106=mean(MOV_PAS_mnt_max_cred_cta_bol_0106),
            MOV_PAS_mnt_max_deb_cta_bol_0106=mean(MOV_PAS_mnt_max_deb_cta_bol_0106),
            MOV_PAS_tot_mnt_cred_cta_bol_0106=mean(MOV_PAS_tot_mnt_cred_cta_bol_0106),
            MTO_CMP_POS_06=mean(MTO_CMP_POS_06),
            PROP_DEM_EDAD=mean(PROP_DEM_EDAD),
            VNT_mnt_tot_depaho_bol_0106=mean(VNT_mnt_tot_depaho_bol_0106),
            VNT_mnt_tot_retaho_bol_0106=mean(VNT_mnt_tot_retaho_bol_0106),
            DWH_SLD_CIERRE_DOL=mean(DWH_SLD_CIERRE_DOL),)

bd_cluster_medianas<-bd_fin_test %>% group_by(CLUSTER_PREDICT) %>% 
  summarise(ACH_ENV_CNT_CTA_TOT=median(ACH_ENV_CNT_CTA_TOT),
            ACH_REC_AVG_MTO_TOT_0106=median(ACH_REC_AVG_MTO_TOT_0106),
            ACH_REC_SD_MTO_TOT_0106=median(ACH_REC_SD_MTO_TOT_0106),
            MOV_PAS_cnt_cred_cta_bol_0106=median(MOV_PAS_cnt_cred_cta_bol_0106),
            MOV_PAS_mnt_max_cred_cta_bol_0106=median(MOV_PAS_mnt_max_cred_cta_bol_0106),
            MOV_PAS_mnt_max_deb_cta_bol_0106=median(MOV_PAS_mnt_max_deb_cta_bol_0106),
            MOV_PAS_tot_mnt_cred_cta_bol_0106=median(MOV_PAS_tot_mnt_cred_cta_bol_0106),
            MTO_CMP_POS_06=median(MTO_CMP_POS_06),
            PROP_DEM_EDAD=median(PROP_DEM_EDAD),
            VNT_mnt_tot_depaho_bol_0106=median(VNT_mnt_tot_depaho_bol_0106),
            VNT_mnt_tot_retaho_bol_0106=median(VNT_mnt_tot_retaho_bol_0106),
            DWH_SLD_CIERRE_DOL=median(DWH_SLD_CIERRE_DOL))

bd_cluster_medianas %>% filter(!CLUSTER_PREDICT %in% c(3,7))%>%  
  gather(key = "Variables",value = "Valores",ACH_ENV_CNT_CTA_TOT:DWH_SLD_CIERRE_DOL,factor_key = TRUE) %>% 
  ggplot(mapping = aes(as.factor(x=Variables),y=Valores,group=as.factor(CLUSTER_PREDICT),col=as.factor(CLUSTER_PREDICT))) +
  #stat_summary(mapping=aes(shape=CLUSTER_PREDICT),fun=mean,geom = "pointrange",size=1) +
  stat_summary(geom = "line") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))



# Presentacion ####
COLUMNAS<-c("CIC_D","PERIODO_D","MADURACION","Producto","cluster_iter1","cluster_iter2")
y<-bd %>% select(-COLUMNAS)%>%
    group_by(CLUSTER_PREDICT) %>%
    do(the_summary = summary(.)) 

y$the_summary

write.csv(x = y,file = "D:/Guillermo/Credifondo (Proyecto)/General/R/Modelos/Datos Finales/SEGMENTOS.csv" )


names(bd)


bd %>% group_by(CLUSTER_PREDICT) %>% summarise(tot=n(),por=n()/nrow(bd)*100,p_ger_dir=(mean(as.numeric(PROP_DEM_FLG_DIR_GER))-1)*100,
                                               p_PREMIUM=(mean(as.numeric(PROP_DEM_FLG_PREMIUM))-1)*100,
                                               p_EXCLUSIVO=(mean(as.numeric(PROP_DEM_FLG_EXCLUSIVO))-1)*100,
                                               p_VIG=(mean(as.numeric(PROP_DEM_FLG_VIGENTE))-1)*100,
                                               sld_prom=median(DWH_SLD_CIERRE_DOL))


write.csv(x =  bd ,file = "D:/Guillermo/Credifondo (Proyecto)/General/R/Modelos/Datos Finales/base_test.csv" )


bd %>% group_by(CLUSTER_PREDICT) %>% summarise(median(DWH_SLD_CIERRE_DOL))



