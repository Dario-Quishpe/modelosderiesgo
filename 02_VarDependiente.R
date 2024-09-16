# Variable dependiente (Marca Bueno/Malo)

dir.p <- getwd()
dir.b <- paste0(dir.p, "/BDD")
dir.s <- paste0(dir.p, "/Scripts")
list.files()

setwd(dir.b)
list.files()
load("InfoConsolidad.RData")

# Desempeño en la institución
library(expss)
info[,DESEMPENO := count_row_if(gt(0), SALDO_DEUDA_OP_M2, SALDO_DEUDA_OP_M3, SALDO_DEUDA_OP_M4, SALDO_DEUDA_OP_M5,
        SALDO_DEUDA_OP_M6, SALDO_DEUDA_OP_M7, SALDO_DEUDA_OP_M8, SALDO_DEUDA_OP_M9, SALDO_DEUDA_OP_M10,
        SALDO_DEUDA_OP_M11, SALDO_DEUDA_OP_M12, SALDO_DEUDA_OP_M13)]
info[,.N,by=DESEMPENO][order(DESEMPENO)]
info[, SIN_DESEMPENO := ifelse(DESEMPENO < 6, "SIN_DESEMPENO", "CON_DESEMPENO")]
info[,.N,by=SIN_DESEMPENO][order(SIN_DESEMPENO)]

# Marca Bancarizado
info[, MARCA_BANCARIZADO := ifelse(score419 == 0 | is.na(score419), "NO BANCARIZADO", "BANCARIZADO")]
info[,.N,by=MARCA_BANCARIZADO]
info[,.N,by=c("MARCA_BANCARIZADO","FECHA_CORTE")][order(FECHA_CORTE)]
info[,.N,by=c("SIN_DESEMPENO","FECHA_CORTE")][order(FECHA_CORTE)]
info[,table(PROVINCIA,FECHA_CORTE)]
info[,table(INSTRUCCION,FECHA_CORTE)]
info[,table(ESTADO_CIVIL,FECHA_CORTE)]
info[,table(RELACION_DEPENDENCIA,FECHA_CORTE)]
info[,table(OFICINA,FECHA_CORTE)]

# Cruce Bancarizado y Desempeño
info[,table(MARCA_BANCARIZADO, SIN_DESEMPENO)]

info[DESEMPENO == 1][,.(SALDO_DEUDA_OP_M1, SALDO_DEUDA_OP_M2, SALDO_DEUDA_OP_M3, SALDO_DEUDA_OP_M4, SALDO_DEUDA_OP_M5,
                        SALDO_DEUDA_OP_M6, SALDO_DEUDA_OP_M7, SALDO_DEUDA_OP_M8, SALDO_DEUDA_OP_M9, SALDO_DEUDA_OP_M10,
                        SALDO_DEUDA_OP_M11, SALDO_DEUDA_OP_M12, SALDO_DEUDA_OP_M13)]

# Análisis Roll Rate
info[, MARCA_VENCIDO := ifelse(NUMERO_DIAS_MOROSIDAD_OP_M1 > 180, 1, 0)]
info[,.N,by=c("MARCA_VENCIDO","MARCA_BANCARIZADO","SIN_DESEMPENO")]
GB <- info[MARCA_VENCIDO == 0 & MARCA_BANCARIZADO == "BANCARIZADO" & SIN_DESEMPENO == "CON_DESEMPENO"][, c("IDENTIFICACION", "FECHA_CORTE", grep(colnames(info), pattern = "_OP_M", value = TRUE)), with = FALSE]

#cortes_inf <- c(-1,0,30,60,90,120,150,180,100000)
#cortes_sup <- c(-1,0,30,60,90,120,150,180,210,100000)

cortes_inf <- c(-1,0,15,30,45,60,75,90,100000)
cortes_sup <- c(-1,0,15,30,45,60,75,90,105,100000)

fun_cortes <- function(vector, cortes){
      res <- cut(vector, breaks = cortes, labels = seq(1:(length(cortes)-1)))
      return(as.numeric(as.character(res)))
}

# Mes 0 vs Mes 1
t1 <- 1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M1, cortes_inf) < fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M2, cortes_sup))
data.table(Rango = fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M1, cortes_inf), Marca = t1)[,table(Rango, Marca)]

# Mes 1 vs Mes 2
t2 <- 1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M2, cortes_inf) < fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M3, cortes_sup))
data.table(Rango = fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M2, cortes_inf), Marca = t2)[,table(Rango, Marca)]

# Mes 2 vs Mes 3
t3 <- 1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M3, cortes_inf) < fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M4, cortes_sup))
data.table(Rango = fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M3, cortes_inf), Marca = t3)[,table(Rango, Marca)]

# Mes 3 vs Mes 4
t4 <- 1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M4, cortes_inf) < fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M5, cortes_sup))
data.table(Rango = fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M4, cortes_inf), Marca = t4)[,table(Rango, Marca)]

# Mes 4 vs Mes 5
t5 <- 1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M5, cortes_inf) < fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M6, cortes_sup))
data.table(Rango = fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M5, cortes_inf), Marca = t5)[,table(Rango, Marca)]

# Mes 5 vs Mes 6
t6 <- 1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M6, cortes_inf) < fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M7, cortes_sup))
data.table(Rango = fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M6, cortes_inf), Marca = t6)[,table(Rango, Marca)]


# Mes 6 vs Mes 7
t7 <- 1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M7, cortes_inf) < fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M8, cortes_sup))
data.table(Rango = fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M7, cortes_inf), Marca = t7)[,table(Rango, Marca)]


# Mes 7 vs Mes 8
t8 <- 1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M8, cortes_inf) < fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M9, cortes_sup))
data.table(Rango = fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M8, cortes_inf), Marca = t8)[,table(Rango, Marca)]


# Mes 8 vs Mes 9
t9 <- 1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M9, cortes_inf) < fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M10, cortes_sup))
data.table(Rango = fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M9, cortes_inf), Marca = t9)[,table(Rango, Marca)]


# Mes 9 vs Mes 10
t10 <- 1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M10, cortes_inf) < fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M11, cortes_sup))
data.table(Rango = fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M10, cortes_inf), Marca = t10)[,table(Rango, Marca)]


# Mes 10 vs Mes 11
t11 <- 1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M11, cortes_inf) < fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M12, cortes_sup))
data.table(Rango = fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M11, cortes_inf), Marca = t11)[,table(Rango, Marca)]


# Mes 11 vs Mes 12
t12 <- 1*(fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M12, cortes_inf) < fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M13, cortes_sup))
data.table(Rango = fun_cortes(GB$NUMERO_DIAS_MOROSIDAD_OP_M12, cortes_inf), Marca = t12)[,table(Rango, Marca)]

#MAXIMO NUMERO DE DIAS DE MOROSIDAD EN TODA LA VENTANA DE DESEMPENO
info[,MAX_DIA_MOROSIDAD:=pmax(NUMERO_DIAS_MOROSIDAD_OP_M2,NUMERO_DIAS_MOROSIDAD_OP_M3,NUMERO_DIAS_MOROSIDAD_OP_M4,
                              NUMERO_DIAS_MOROSIDAD_OP_M5,NUMERO_DIAS_MOROSIDAD_OP_M6,NUMERO_DIAS_MOROSIDAD_OP_M7,
                              NUMERO_DIAS_MOROSIDAD_OP_M8,NUMERO_DIAS_MOROSIDAD_OP_M9,NUMERO_DIAS_MOROSIDAD_OP_M10,
                              NUMERO_DIAS_MOROSIDAD_OP_M11,NUMERO_DIAS_MOROSIDAD_OP_M12,NUMERO_DIAS_MOROSIDAD_OP_M13)]

#DEFINICION DE VARIABLE DEPENDIENTE 
info[,VarDep:=ifelse(MARCA_BANCARIZADO=="NO BANCARIZADO",5,
                     ifelse(SIN_DESEMPENO=="SIN_DESEMPENO",4,
                            ifelse(NUMERO_DIAS_MOROSIDAD_OP_M1>180,3,
                                   ifelse(MAX_DIA_MOROSIDAD>60,1,
                                          ifelse(MAX_DIA_MOROSIDAD==0,0,2)))))]

#AQUI POR FAVOR CALCULA TODAS LAS VARIABLES EXTRAS .!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#sSEGMENTACION CLEAN DIRTY SCE

#MARCAS para clean Dirty 
info[,M_DEMANDA_CASTIGO_SCE_36M:=ifelse(MVAL_DEMANDA_CASTIGO_SCE_36M==0,0,1)]
info[,SEGMENTACION_SCE:=ifelse(M_DEMANDA_CASTIGO_SCE_36M == 0 & MAX_DVEN_SCE_12M <= 30,0,1)]
info[,.N,by=SEGMENTACION_SCE]
dim(info)
#elimnamos las variabels de la ventana de desempenio
colsremover <- grep("OP_M\\d+", colnames(info), value = TRUE)
info <- info[, !..colsremover, with = FALSE]
dim(info)
info[,.N,by=c("VarDep")]
info[,.N,by=c("MARCA_BANCARIZADO","SEGMENTACION","FECHA_CORTE")][order(FECHA_CORTE)]

#BAse de datos clean

dim(info[SEGMENTACION_SCE==0])
info[SEGMENTACION_SCE==0,.N,by=VarDep][order(VarDep)]


#BAse de datos Dirty
dim(info[SEGMENTACION_SCE==1])
info[SEGMENTACION_SCE==1,.N,by=VarDep][order(VarDep)]



#SEGMENTACION CLEAN DIRTY SF
arbol<-info[VarDep%in%c(0,1)][,.(VarDep,MVAL_DEMANDA_CASTIGO_SF_24M,MAX_DVEN_SF_6M,MVAL_DEMANDA_CASTIGO_SF_36M,MAX_DVEN_SF_12M )]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
dim(info)
info[,.N,by=VarDep][order(VarDep)]
info[VarDep%in%c(0,1)][,.N,by=VarDep]
#MARCAS para clean Dirty 
info[,M_DEMANDA_CASTIGO_SF_24M:=ifelse(MVAL_DEMANDA_CASTIGO_SF_24M==0,0,1)]
info[,SEGMENTACION_SF:=ifelse(M_DEMANDA_CASTIGO_SF_24M == 0 & MAX_DVEN_SF_12M <= 10,0,1)]
info[,.N,by=SEGMENTACION_SF]
dim(info)
#elimnamos las variabels de la ventana de desempenio
colsremover <- grep("OP_M\\d+", colnames(info), value = TRUE)
info <- info[, !..colsremover, with = FALSE]
dim(info)
info[,.N,by=c("VarDep")]
info[,.N,by=c("MARCA_BANCARIZADO","SEGMENTACION","FECHA_CORTE")][order(FECHA_CORTE)]

#BAse de datos clean

dim(info[SEGMENTACION_SF==0])
info[SEGMENTACION_SF==0,.N,by=VarDep][order(VarDep)]


#BAse de datos Dirty
dim(info[SEGMENTACION_SF==1])
info[SEGMENTACION_SF==1,.N,by=VarDep][order(VarDep)]




info[,.N,by=VarDep][order(VarDep)]
#MUESTRAS DE MODELAMIENTO modelamiento 80% --- modelamiento 20%
info_model<-info[MARCA_BANCARIZADO == "BANCARIZADO" & SIN_DESEMPENO == "CON_DESEMPENO"]

set.seed(1723951065)
marca<-sample(1:nrow(info_model),size=floor(0.2*nrow(info_model)),replace = FALSE)
info_model[,ModVal:= 1:nrow(info_model)]
info_model[,ModVal:=ifelse(ModVal %in% marca,1,0)]
info_model[,.N,by=ModVal]
info_model[,table(VarDep,ModVal)]
dim(info_model)


#elimnamos las variabels de la ventana de desempenio
colsremover <- grep("OP_M\\d+", colnames(info_model), value = TRUE)
info_model <- info_model[, !..colsremover, with = FALSE]
dim(info_model)
info_model[,.N,by=c("VarDep")]
info_model[,.N,by=c("MARCA_BANCARIZADO","FECHA_CORTE")][order(FECHA_CORTE)]


save(list=c("info_model"),file="InfoModelamiento_2.RData",envir=.GlobalEnv)


#MUESTRAS DE MODELAMIENTO modelamiento 80% --- modelamiento 20% PERo ya con el modelo ajustado
#info_model<-info[MARCA_BANCARIZADO == "BANCARIZADO" & SIN_DESEMPENO == "CON_DESEMPENO"]

set.seed(1723951065)
marca<-sample(1:nrow(info),size=floor(0.2*nrow(info)),replace = FALSE)
info[,ModVal:= 1:nrow(info)]
info[,ModVal:=ifelse(ModVal %in% marca,1,0)]
info[,.N,by=ModVal]
info[,table(VarDep,ModVal)]
dim(info)


#elimnamos las variabels de la ventana de desempenio
colsremover <- grep("OP_M\\d+", colnames(info), value = TRUE)
info <- info[, !..colsremover, with = FALSE]
dim(info)
info[,.N,by=c("VarDep")]
info[,.N,by=c("MARCA_BANCARIZADO","FECHA_CORTE")][order(FECHA_CORTE)]

save(list=c("info"),file="InfoModelamiento_1.RData",envir=.GlobalEnv)





#MUESTRAS DE MODELAMIENTO modelamiento 80% --- modelamiento 20% dentro de CLEAN 
info_clean<-info[SEGMENTACION_SF==0]
set.seed(1723951065)
marca<-sample(1:nrow(info_clean),size=floor(0.2*nrow(info_clean)),replace = FALSE)
info_clean[,ModVal:= 1:nrow(info_clean)]
info_clean[,ModVal:=ifelse(ModVal %in% marca,1,0)]
info_clean[,.N,by=ModVal]
info_clean[,table(VarDep,ModVal)]
dim(info_clean)




#elimnamos las variabels de la ventana de desempenio
colsremover <- grep("OP_M\\d+", colnames(info_clean), value = TRUE)
info_clean <- info_clean[, !..colsremover, with = FALSE]
dim(info_clean)
info_clean[,.N,by=c("VarDep")]
info_clean[,.N,by=c("MARCA_BANCARIZADO","FECHA_CORTE")][order(FECHA_CORTE)]


save(list=c("info_clean"),file="InfoModelamiento_clean.RData",envir=.GlobalEnv)



#MUESTRAS DE MODELAMIENTO modelamiento 80% --- modelamiento 20% dentro de Dirty
info_dirty<-info[ SEGMENTACION_SF==1]
set.seed(1723951065)
marca<-sample(1:nrow(info_dirty),size=floor(0.2*nrow(info_dirty)),replace = FALSE)
info_dirty[,ModVal:= 1:nrow(info_dirty)]
info_dirty[,ModVal:=ifelse(ModVal %in% marca,1,0)]
info_dirty[,.N,by=ModVal]
info_dirty[,table(VarDep,ModVal)]
dim(info_dirty)


#elimnamos las variabels de la ventana de desempenio
colsremover <- grep("OP_M\\d+", colnames(info_dirty), value = TRUE)
info_dirty <- info_dirty[, !..colsremover, with = FALSE]
dim(info_dirty)
info_dirty[,.N,by=c("VarDep")]
info_dirty[,.N,by=c("MARCA_BANCARIZADO","FECHA_CORTE")][order(FECHA_CORTE)]


save(list=c("info_dirty"),file="InfoModelamiento_dirty.RData",envir=.GlobalEnv)

load(file ="InfoModelamiento_dirty.RData" )
load(file="InfoModelamiento_clean.RData")
load(file="InfoModelamiento_1.RData")



#SACANDO MUESTRITA ALEATORIA PARA APP
variables_necesarias<-c("VarDep",#variabledependiente
                        "MAX_DVEN_SBS_OP_6M","MAX_DVEN_SC_OP_6M","MAX_DVEN_SICOM_OP_6M","MAX_DVEN_OTROS_SIS_OP_6M",#var1clean # var2dirty
                        "MAX_DVEN_SBS_TC_6M","MAX_DVEN_SC_TC_6M","MAX_DVEN_SICOM_TC_6M","MAX_DVEN_OTROS_SIS_TC_6M",#var1clean # var2dirty
                        "numMesesSinVenDesdeUltVenD334",#var2clean # var5dirty
                        "numOpsAperturadas12M142",#var3clean
                        "numOpsVencidas3MD336",#var4clean
                        "NOPE_APERT_SBS_OP_6M","NOPE_APERT_SC_OP_6M","NOPE_APERT_SICOM_OP_6M","NOPE_APERT_OTROS_OP_6M",#Var5clean
                        "salTotOp040","maxMontoOp096",#var6clean #var3clean
                        "SALDO_PROMEDIO_AHORRO",#var7clean
                        "edad",#var8clean
                        "maySalVenD24M275",#var9clean
                        "ANTIGUEDAD_OP_OTROS",#var10clean #var9dirty
                        "ANTIGUEDAD_OP_SICOM",#var11clean
                        "moraOps307",#var12clean
                        "numAcreedoresOpBanCooComD414",#var13clean
                        "ANTIG_LABORAL",#var14clean #var8dirty
                        
                        "MAX_DVEN_SBS_OP_3M","MAX_DVEN_SC_OP_3M",#var1dirty
                        "maxMorosidadCoo133",#var4dirty
                        "peorNivelRiesgoValorOpBanCooComD415",#var6dirty
                        "MVALVEN_OTROS_TC_3M" ,"MVALVEN_SBS_TC_3M","MVALVEN_SC_TC_3M","MVALVEN_SICOM_TC_3M",#Var7dirty
                        "MVALVEN_OTROS_OP_3M" ,"MVALVEN_SBS_OP_3M","MVALVEN_SC_OP_3M","MVALVEN_SICOM_OP_3M",#Var7dirty
                        "PROM_MAX_DVEN_SBS_OP_3M","PROM_MAX_DVEN_SBS_OP_6M",#Var10dirty
                        "cuotaVencidos302",#var11dirty
                        "numOpsVencidas3M102",#var12dirty
                        
                        
                        "MVAL_CASTIGO_SBS_TC_24M","MVAL_CASTIGO_SC_TC_24M",#para segmentacion SF
                        "MVAL_CASTIGO_SBS_OP_24M","MVAL_CASTIGO_SC_OP_24M",#para segmentacion SF
                        "MVAL_DEMANDA_SBS_TC_24M", "MVAL_DEMANDA_SC_TC_24M",#para segmentacion SF
                        "MVAL_DEMANDA_SBS_OP_24M", "MVAL_DEMANDA_SC_OP_24M",#para segmentacion SF
                        "MAX_DVEN_SBS_OP_12M","MAX_DVEN_SC_OP_12M",#para segmentacion SF
                        "MAX_DVEN_SBS_TC_12M","MAX_DVEN_SC_TC_12M",#para segmentacion SF
                        
                        
                        "salTCDia031",#regla1
                        "numOpsAperturadas106",#regla2
                        "DEUDA_TOTAL_SC_OP_12M","DEUDA_TOTAL_SC_OP_24M", #regla3
                        "antiguedadOpBanCoo388",#regla4
                        "ANTIGUEDAD_OP_SC",#regla5
                        "numMesesInfoCredBanCoopD36M421"#regla6
                        
                        )




set.seed(1723951065)

muestrita<-info[sample(.N,5000)]
muestrita<-muestrita[,variables_necesarias,with = FALSE]
fwrite(muestrita, file = "muestrita.txt", sep = "\t")
