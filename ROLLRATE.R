#VARIABLE DEPENDIENTE(MARCA BUENO /MALO)
library(data.table)
library(tidyverse)
library(expss)
load("InfoConsolidada.Rdata")
#grep(colnames(info),pattern="_OP_M",value = TRUE)
#DESEMPENIO EN LA INSTITUCION
info[,DESEMPENO:=count_row_if(gt(0),SALDO_DEUDA_OP_M2,SALDO_DEUDA_OP_M3,SALDO_DEUDA_OP_M4,SALDO_DEUDA_OP_M5,
                                      SALDO_DEUDA_OP_M6,SALDO_DEUDA_OP_M7,SALDO_DEUDA_OP_M8,SALDO_DEUDA_OP_M9,SALDO_DEUDA_OP_M10,SALDO_DEUDA_OP_M11,
                                      SALDO_DEUDA_OP_M12,SALDO_DEUDA_OP_M13)]
info[,.N,by=DESEMPENO][order(DESEMPENO)]
info[,SIN_DESEMPENO:=ifelse(DESEMPENO<6,"SIN_DESEMPENO","CON_DESEMPENO")][order(DESEMPENO)]
info[,.N,by=SIN_DESEMPENO][order(SIN_DESEMPENO)]

info[,.(SALDO_DEUDA_OP_M2,SALDO_DEUDA_OP_M3,SALDO_DEUDA_OP_M4,SALDO_DEUDA_OP_M5,
        SALDO_DEUDA_OP_M6,SALDO_DEUDA_OP_M7,SALDO_DEUDA_OP_M8,SALDO_DEUDA_OP_M9,SALDO_DEUDA_OP_M10,SALDO_DEUDA_OP_M11,
        SALDO_DEUDA_OP_M12,SALDO_DEUDA_OP_M13)]

#MARCA BANCARIZADA
info[,MARCA_BANCARIZADO:=ifelse(score419==0|is.na(score419),"NO BANCARIZADO","BANCARIZADO")]
info[,table(SIN_DESEMPENO)]

#ANALISIS ROLL RATE
info[,MARCA_VENCIDO:=ifelse(NUMERO_DIAS_MOROSIDAD_OP_M1> 180,1,0)]
info[,.N,by=MARCA_VENCIDO]
GB<-info[MARCA_VENCIDO==0 & MARCA_BANCARIZADO=="BANCARIZADO" & SIN_DESEMPENO=="CON_DESEMPENO"][,c("IDENTIFICACION","FECHA_CORTE",grep(colnames(info),pattern="_OP_M",value = TRUE))]
fun_cortes<-function(vector,cortes){
  
  res<-cut(vector,breaks = cortes,label=seq(1:(length(cortes)-1)))
  return(as.numeric(as.character(res)))
}
cortes_inf<-c(-1,0,30,60,120,150,180,1000000)
cortes_sup<-c(-1,0,30,60,120,150,180,210,1000000)
#Mes 0 vs Mes1
t1<-1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M1,cortes_inf)<fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M2,cortes_sup))

data.table(Rango=fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M1,cortes_inf),Marca=t1)
#para cuando Es a menos de 30  dias , digamos , a 15  dias se debe modificar los cortes 


head(cut(info$NUMERO_DIAS_MOROSIDAD_OP_M1,breaks=c(-1,0,30,60,120,150,180,1000000)),4)
head(cut(info$NUMERO_DIAS_MOROSIDAD_OP_M2,breaks=c(-1,0,30,60,120,150,180,1000000)),4)
