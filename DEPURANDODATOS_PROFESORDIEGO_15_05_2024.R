#profediego 15/05/2024
#info al punto de observacion 
library(data.table)
library(tidyverse)
info<-fread("OneDrive_1_11-5-2024/DataInicial_01022024_123459.txt")
dim(info)
info<-info[Muestra==1]
dim(info)

#Carga de tablas de desempenio
Op1<-fread("EstructuraVariables/EstructuraVariables/OperacionesTabla1.txt")[,c(1,3,4,2,5:111)]
Tc1<-fread("EstructuraVariables/EstructuraVariables/TarjetasTabla1.txt")
colnames(Tc1)<-colnames(Op1)
des<-rbindlist(list(Op1,Tc1))
rm(list=c("Op1","Tc1"))
dim(des)
des[,list(Registros=.N),by=IDENTIFICACION_SCORE][order(desc(Registros))]
des[,list(Registros=.N),by=IDENTIFICACION_SCORE][Registros==5]
des[IDENTIFICACION_SCORE=="6BA88E9367878A8BD564B7F6EE907DE2"]
s1<-des[,lapply(.SD,max),.SDcols=paste0("NUMERO_DIAS_MOROSIDAD_OP_M",1:13),by="IDENTIFICACION_SCORE"]
s2<-des[,lapply(.SD,sum),.SDcols=paste0("SALDO_DEUDA_OP_M",1:13),by="IDENTIFICACION_SCORE"]
s3<-des[,lapply(.SD,sum),.SDcols=paste0("SALDO_VENCIDO_OP_M",1:13),by="IDENTIFICACION_SCORE"]
s4<-des[,lapply(.SD,sum),.SDcols=paste0("SALDO_CCASTIGADA_OP_M",1:13),by="IDENTIFICACION_SCORE"]
s5<-des[,lapply(.SD,sum),.SDcols=paste0("SALDO_DJUDICIAL_OP_M",1:13),by="IDENTIFICACION_SCORE"]
res<-s1[s2,on="IDENTIFICACION_SCORE"]
res<-s3[res,on="IDENTIFICACION_SCORE"]
res<-s4[res,on="IDENTIFICACION_SCORE"]
res<-s5[res,on="IDENTIFICACION_SCORE"]
rm(list=paste0("s",1:5))
#Cruce de informacion (desempe;o + pto de observacion )
colnames(res)[1]<-"IDENTIFICACION"
dim(res[info,on="IDENTIFICACION"])
info<-res[info,on="IDENTIFICACION"]
rm(list=c("res"))

dim(info)

#Sacar las identificaciones de los individuos que van a ser parte del modelo 
#Carga de tablas al punto de observacion(solo era para ir practicando) 
Op2<-fread("EstructuraVariables/EstructuraVariables/OperacionesTabla2.txt")
Tc2<-fread("EstructuraVariables/EstructuraVariables/TarjetasTabla2.txt")


#Carga de tabals historicas (estas si usamos)
Op3<-fread("EstructuraVariables/EstructuraVariables/OperacionesTabla3.txt")
Tc3<-fread("EstructuraVariables/EstructuraVariables/TarjetaTabla3.txt")
colnames(Op3)[c(3,1)]<-c("IDENTIFICACION","FECHA_CORTE")
colnames(Tc3)[c(3,1)]<-c("IDENTIFICACION","FECHA_CORTE")

info<-Op3[info,on=c("IDENTIFICACION","FECHA_CORTE")]
info<-Tc3[info,on=c("IDENTIFICACION","FECHA_CORTE")]
rm(list=c("Op3","Tc3"))
dim(info)
colnames(info)

#GEneracion de variables acumuladas(SF+SCE)
#Revisar el codigo donde esta la funcion 
#info[,NOPE_APERT_SF_OP_3M:=NOPE_APERT_SBS_OP_3M+NOPE_APERT_SC_OP_3M]
#info[,NOPE_APERT_SCE_OP_3M:=NOPE_APERT_SBS_OP_3M+NOPE_APERT_SC_OP_3M+NOPE_APERT_SICOM_OP_3M+NOPE_APERT_OTROS_OP_3M]
#info[,MVAL_DEMANDA_SCE_OP_3M:=pmax(MVAL_DEMANDA_SBS_OP_3M,MVAL_DEMANDA_SC_OP_3M,MVAL_DEMANDA_SICOM_OP_3M),]
#dim(info)
#Carolyn udla , kerly uce

info[,.(IDENTIFICACION,FECHA_CORTE)]
Op3[,.(IDENTIFICACION_SCORE,FECHA_CORTE_PUNTO_CONTROL)]
dim(Op3)
dim(Tc3)
Op3[,list(Registros=.N),by=IDENTIFICACION_SCORE][order(desc(Registros))]
info[IDENTIFICACION=="F81329F437E438FC89A5F705BC1D50D5"]
Op3[IDENTIFICACION_SCORE=="F81329F437E438FC89A5F705BC1D50D5"]

Op3[,NOPE_APERT_SF_OP_3M:=NOPE_APERT_SBS_OP_3M+NOPE_APERT_SC_OP_3M]
Op3[,NOPE_APERT_SCE_OP_3M:=NOPE_APERT_SBS_OP_3M+NOPE_APERT_SC_OP_3M+NOPE_APERT_SICOM_OP_12M+NOPE_APERT_OTROS_OP_3M]

summary(Tc3$ANTIGUEDAD_TC_SBS)
summary(Tc3$ANTIGUEDAD_TC_SC)
summary(Tc3$ANTIGUEDAD_TC_SICOM)
quantile(Tc3$ANTIGUEDAD_TC_SICOM,probs=seq(0,1,by=0.1))
quantile(Tc3$ANTIGUEDAD_TC_SC,probs=seq(0,1,by=0.1))
quantile(Tc3$ANTIGUEDAD_TC_SBS,probs=seq(0,1,by=0.1))


#Base de indicadores

load("INDICADORES.RData")
dim(d)
colnames(d)
vars<-c("fechaCalificacion","tipoIdentificacionSujeto","tipoIdentificacionSujetoDescripcion",
        "identificacionSujeto","marcaPrinTC090","emisorPrinTC091","gastoPersonal093","salEntidad112",
        "salEntidad110","entidad111","entidad113","salEntidad116","buenoMaloBancos342","buenoMaloCoops343","buenoMaloTarjetas344",
        "Covid19OpTc345","ID","entidad","identificacionSujeto","ID4","ingreso136_Actual")
d<-setDT(d)[,-vars,with=FALSE]

colnames(d)[1:2]<-c("IDENTIFICACION","FECHA_CORTE")
d[,.N,by=FECHA_CORTE]
d[,FECHA_CORTE:=lubridate::ymd(FECHA_CORTE)]
dim(d)
dim(info)
info<-d[info,on=c("IDENTIFICACION","FECHA_CORTE")]
dim(info)
save(list=c("info"),file="InfoConsolidada.Rdata",envir=.GlobalEnv)
