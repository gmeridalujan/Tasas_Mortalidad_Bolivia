# Analisis de correspondencia

library(networkD3)
library(ade4)
library(FactoMineR)
library(factoextra)
library(explor)
library(ca)

# Bases CFO ####

cnn_dmd<-odbcDriverConnect(connection="Driver={SQL Server Native Client 11.0};
                                       server=btbdmd00;
                                       database=BD_DATA_CANALES_2;
                                       trusted_connection=yes;")

Q1<-" SELECT DWH_CLI_CIC,Mes,Producto,Perfil_CFO,Perfil_BCP,Saldo_USD_MAX
      FROM [cfi].[BASE_CFO_POBLACION_PRED]"

bd<-sqlQuery(channel = cnn_dmd,query = Q1,as.is=T)

odbcClose(cnn_dmd)

# Bases de Correspondencia ####

for (i in c("Producto","Perfil_CFO","Perfil_BCP")) {
  bd[,i]<-as.factor(bd[,i])
}

bd_correspondencia_BCP<-bd %>% filter(Mes=="202206") %>% select(Producto,Perfil_BCP)
bd_correspondencia_CFO<-bd %>% filter(Mes=="202206") %>% select(Producto,Perfil_CFO)

prop.table(table(bd_correspondencia$CLUSTER_PREDICT,bd_correspondencia$Producto),1)

# Análisis de correspondencia BCP####

ACS_BCP <- CA(table(bd_correspondencia_BCP$Perfil_BCP,bd_correspondencia_BCP$Producto), graph = FALSE)

explor(ACS_BCP)

# Análisis de correspondencia CFO####

ACS_CFO <- CA(table(bd_correspondencia_CFO$Perfil_CFO,bd_correspondencia_BCP$Producto), graph = FALSE)

prop.table(table(bd_correspondencia_CFO$Perfil_CFO,bd_correspondencia_BCP$Producto),1)*100

explor(ACS_CFO)

# Análisis de correspondencia Multiple ####

# MCA ####

uni.mca <- MCA(bd %>% filter(Mes=="202206") %>% select(-DWH_CLI_CIC,-Mes), graph = FALSE)
print(uni.mca)

explor(uni.mca)

# Diagrama de sankey ####

bd_03<-bd %>% filter(Mes=="202203") %>% select(DWH_CLI_CIC,Perfil_BCP)
names(bd_03)<-c("CIC","Perfil_BCP_MAR")
bd_06<-bd %>% filter(Mes=="202206") %>% select(DWH_CLI_CIC,Perfil_BCP,Saldo_USD_MAX)
names(bd_06)<-c("CIC","Perfil_BCP_JUN","SALD_MAX")

bd_fin_san<-full_join(bd_06,bd_03)
bd_fin_san<-as.data.frame(bd_fin_san %>% group_by(Perfil_BCP_MAR,Perfil_BCP_JUN) %>% summarise(CTEO=n()))
bd_fin_san <- as.data.frame(bd %>% filter(Mes=="202206")%>% group_by(CLUSTER_MAR,CLUSTER_PREDICT) %>% summarise(CTEO=n()))

bd_fin_san$Perfil_BCP_MAR<-paste0("Mar_",bd_fin_san$Perfil_BCP_MAR)
bd_fin_san$Perfil_BCP_JUN<-paste0("Jun_",bd_fin_san$Perfil_BCP_JUN)

colnames(bd_fin_san)<-c("source","target","value")

links<-bd_fin_san
links$source<-ifelse(test = links$source=="Mar_NA","NUEVOS",links$source)
links$target<-ifelse(test = links$target=="Jun_NA","CERRADOS",links$target)
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE,fontSize = 12,nodeWidth = 20)
p 

# Metricas de Estabilidad ####

# transiciones

addmargins(table(bd_fin_san$Perfil_BCP_MAR,bd_fin_san$Perfil_BCP_JUN))




sum(is.na(bd_fin_san))

for (i in c("Perfil_BCP_JUN","Perfil_BCP_MAR")) {
  bd_fin_san[,i]<-as.numeric(bd_fin_san[,i])
}
bd_fin_san[is.na(bd_fin_san)]<-99
bd_fin_san$SALD_MAX<-as.numeric(bd_fin_san$SALD_MAX)

bd_fin_san %>% filter(Perfil_BCP_JUN==Perfil_BCP_MAR) %>% group_by(Perfil_BCP_MAR,Perfil_BCP_JUN) %>% summarise(CONTEO_IGUAL=n(),n()/,median(SALD_MAX,na.rm = T))

for (i in c(1:7,99)) {
  print("------------------------------------------------------------------------------------------")
  print(bd_fin_san %>% filter(Perfil_BCP_JUN!=Perfil_BCP_MAR & Perfil_BCP_MAR==i) %>% group_by(Perfil_BCP_MAR,Perfil_BCP_JUN) %>% summarise(CONTEO_IGUAL=n(),SALDO_MEDIANO_JUN=median(SALD_MAX),SALDO_PONDERADO=(n()/nrow(bd_fin_san %>% filter(Perfil_BCP_MAR==i)))*median(SALD_MAX)) %>% arrange(CONTEO_IGUAL))
}


bd_fin_san %>% group_by(Perfil_BCP_MAR) %>% summarise(n())

# relacion CFO, BCP

x<-bd %>% filter(Mes=="202206") %>% select(Perfil_CFO,Perfil_BCP)

x[is.na(x)]<-99

table(x$Perfil_CFO,x$Perfil_BCP)

x$Perfil_BCP<-paste0("BCP_",x$Perfil_CFO)
x$Perfil_CFO<-paste0("CFO_",x$Perfil_BCP)
