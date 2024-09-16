#Carga de datos de estructura de variables score
library(tidyverse)
library(data.table)
dir.p<-getwd()
list.files()

#Carga de tablas de desempenio
Op1<-fread("EstructuraVariables/EstructuraVariables/OperacionesTabla1.txt")

Tc1<-fread("EstructuraVariables/EstructuraVariables/TarjetasTabla1.txt")


colnames(Op1)
colnames(Tc1)
Op1[,.N,by=TIPO_SISTEMA]
head(Op1[NUMERO_DIAS_MOROSIDAD_OP_M2>0])#Dias de mora 


head(Op1[NUMERO_DIAS_MOROSIDAD_OP_M2>0 & NUMERO_DIAS_MOROSIDAD_OP_M2<=90 & NUM_OPERACION_OP=="MC0022027225020220702"])#saldo vencido y no devenga interes


#Carga de tablas al punto de observacion
#Sacar las identificaciones de los individuos que van a ser parte del modelo 

Op2<-fread("EstructuraVariables/EstructuraVariables/OperacionesTabla2.txt")

Tc2<-fread("EstructuraVariables/EstructuraVariables/TarjetasTabla2.txt")

#Sumar Saldos y tomar el mayor numero de dias de mora, quitar las de 2023(fechas de corte)son 5 puntos de observacion ,fecha de concession la mas antigua
#Cruar fecha e identificacio n
ids<-unique(rbindlist(list(Op2[,.(FECHA_CORTE_PUNTO_CONTROL,IDENTIFICACION_SCORE)],
Tc2[,.(FECHA_CORTE_PUNTO_CONTROL,IDENTIFICACION_SCORE)])))

ids
ids |> filter(FECHA_CORTE_PUNTO_CONTROL=="2023-06-30") |> nrow()
ids |> filter(FECHA_CORTE_PUNTO_CONTROL=="2023-09-30") |> nrow()
#DEBER
#Seleccionando las identificaciones segun las especificaciones enunciadas en clase 
idsfiltro<-ids |> filter(FECHA_CORTE_PUNTO_CONTROL!="2023-06-30" & FECHA_CORTE_PUNTO_CONTROL!="2023-09-30" )
idsfiltro[,.N,by=FECHA_CORTE_PUNTO_CONTROL]

#preparacion de la base de datos con informacion a posteriori

Tabla1<-Op1 |> select(-TIPO_PERSONA,-TIPO_IDENTIFICACION,-TIPO_SISTEMA,-NUM_OPERACION_OP) |> mutate(CATEGORIA="OP")
Tabla2<-Tc1 |> select(-TIPO_IDENTIFICACION,-TIPO_PERSONA,-TIPO_SISTEMA,-NUM_OPERACION_OP)|> mutate(CATEGORIA="TS")
colnames(Tabla1)
Tabla1[Tabla1$FECHA_CONCESION=="1900-01-31"]
Tabla2[Tabla2$FECHA_CONCESION=="1900-01-31"]
colnames(Tabla2)
nombres<-colnames(Tabla1)
Tabla2<-Tabla2 |> rename_with(~nombres,everything())
Consolidada<-rbind(Tabla1,Tabla2)
colnames(Consolidada)
NombresTablaFinal<-gsub("OP","_",nombres)
Consolidada<-Consolidada |> rename_with(~NombresTablaFinal,everything()) |> mutate(FECHA_CONCESION=as.Date(FECHA_CONCESION))
colnames(Consolidada)
min(Consolidada$FECHA_CONCESION)
#Consolidada |> group_by(IDENTIFICACION_SCORE) |> summarise(FECHA_CONCESION_consoli=min(FECHA_CONCESION))
#Consolidada[Consolidada$IDENTIFICACION_SCORE=="0000C1D3A2BF6B164894B1DBDC8B6013"] NA
#tail(Consolidada[Consolidada$IDENTIFICACION_SCORE=="77CB027928AC85B5EA4E7452E8892C77"]) 1900 xd
ConsolidadaAgrupada<-Consolidada |> group_by(IDENTIFICACION_SCORE) |> summarise(FECHA_CONCESION_FINAL=min(FECHA_CONCESION,na.rm = T),across(starts_with("SALDO"),sum),across(starts_with("NUM"),max))
#Consolidada[Consolidada$FECHA_CONCESION=="1900-01-31"]

#innerjoin con ids

Resultado<-inner_join(idsfiltro,ConsolidadaAgrupada,by="IDENTIFICACION_SCORE")
Resultado[,.N,by=FECHA_CORTE_PUNTO_CONTROL]



#08/05/2024

load("INDICADORES.RData")
setDT(d)[,list(Min=min(score001),
               Max=max(score001)),
               by=segScore002]

d[,FECHA_CORTE_PUNTO_CONTROL:=as.Date(FECHA_CORTE_PUNTO_CONTROL)]
str(d)
#Aniadiendo indicadores por fecha de corte e identificacion 
setnames(d,old=c("fechaCorte","identificacionSujeto"),new = c("FECHA_CORTE_PUNTO_CONTROL","IDENTIFICACION_SCORE"))


Resultado_2<-merge(Resultado,d, by = c("FECHA_CORTE_PUNTO_CONTROL","IDENTIFICACION_SCORE"), all = FALSE)
colnames(Resultado_2)
dim(Resultado_2)

#Profe diego 15/05/224

#info al punto de observacion 
info<-fread("OneDrive_1_11-5-2024/DataInicial_01022024_123459.txt")
dim(info)
info<-info[Muestra==1]
dim(info)
info[,list(Registros=.N),by=IDENTIFICACION[order(desc(Registros))]]
info[Muestra==1][list(Registros=.N),by=IDENTIFICACION[order(desc(Registros))]]
info[Muestra==1][,N,by=Fecha_Corte]
