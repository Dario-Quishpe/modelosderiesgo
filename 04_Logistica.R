# Regresión Logística

dir.p <- getwd()
dir.b <- paste0(dir.p, "/BDD")
dir.s <- paste0(dir.p, "/Scripts")
list.files()
setwd(dir.b)
list.files()
load("InfoModelamiento.RData")
# Construcción de variables y discretizaciones
info[, LN_DEUDA_TOTAL_SBS_OP_24M := ifelse(DEUDA_TOTAL_SBS_OP_24M > 1, log(DEUDA_TOTAL_SBS_OP_24M), 0)]
info[, LN_DEUDA_TOTAL_SBS_OP_3M := ifelse(DEUDA_TOTAL_SBS_OP_3M > 1, log(DEUDA_TOTAL_SBS_OP_3M), 0)]
info[, LN_PROM_XVEN_SBS_OP_6M := ifelse(PROM_XVEN_SBS_OP_6M > 1, log(PROM_XVEN_SBS_OP_6M), 0)]

info[, INGRESOS_mod := ifelse(INGRESOS > 10000, 10000, INGRESOS)]
info[, LN_DEUDA_TOTAL_SCE_OP_24M := ifelse(DEUDA_TOTAL_SBS_OP_24M > 1, log(DEUDA_TOTAL_SBS_OP_24M), 0)]
info[, GASTOS_MOD := ifelse(GASTOS > 4772 ,4772,GASTOS)]
info[,cuotaEstimadaD24M416_MOD:= ifelse(cuotaEstimadaD24M416 > 12000, 12000,cuotaEstimadaD24M416)]
info[, LN_DEUDA_TOTAL_SCE_TC_24M := ifelse(DEUDA_TOTAL_SBS_TC_24M > 1, log(DEUDA_TOTAL_SBS_TC_24M), 0)]
info[,MAX_DVEN_SCE_OP_3M_MOD:=ifelse(MAX_DVEN_SCE_OP_3M > 360 ,360,MAX_DVEN_SCE_OP_3M)]
#info[,MAX_DVEN_SCE_OP_6M_MOD:=ifelse(MAX_DVEN_SCE_OP_3M > 360 ,360,MAX_DVEN_SCE_OP_3M)]
info[,MAX_DVEN_SF_OP_12M_MOD:=ifelse(MAX_DVEN_SF_OP_12M > 270 ,270,MAX_DVEN_SF_OP_12M)]
info[,maySalVenD6M314_MOD:=ifelse(is.na(maySalVenD6M314) ,0,ifelse(maySalVenD6M314>1469.6005,1469.6005,maySalVenD6M314))]
info[,numOpsVigD332_MOD:=ifelse(numOpsVigD332>14,14,numOpsVigD332)]
info[,NOPE_APERT_SCE_OP_24M_MOD:=ifelse(NOPE_APERT_SCE_OP_24M>13,13,NOPE_APERT_SCE_OP_24M)]
info[,NOPE_XVEN_OP_12M_MOD:=ifelse(NOPE_XVEN_OP_12M>13,13,NOPE_XVEN_OP_12M)]
info[,SALDO_PROMEDIO_AHORRO_MOD:=ifelse(SALDO_PROMEDIO_AHORRO>5748.8698,5748.8698,SALDO_PROMEDIO_AHORRO)]
info[,cuota052_MOD:= ifelse(cuota052 > 6400.5525, 6400.5525,cuota052)]
info[,d_numOpsVen_prem:=ifelse(numOpsVencidas3M102==0,1,0)]#premia
info[,MAX_DVEN_SCE_12M_MOD:=ifelse(MAX_DVEN_SCE_12M > 180, 180, MAX_DVEN_SCE_12M)]
info[,DEUDA_TOTAL_SCE_6M_MOD:=ifelse(DEUDA_TOTAL_SCE_6M > 636804.183, 636804.183, DEUDA_TOTAL_SCE_6M)]
info[,LN_DEUDA_TOTAL_SCE_6M:=ifelse(DEUDA_TOTAL_SCE_6M > 1, log(DEUDA_TOTAL_SCE_6M), 0)]
info[,LN_PROM_XVEN_SBS_OP_6M:=ifelse(PROM_XVEN_SBS_OP_6M > 1, log(PROM_XVEN_SBS_OP_6M), 0)]
info[,consultasUlt12M098_MOD:=ifelse(consultasUlt12M098 > 6, 6,consultasUlt12M098)]
info[,peorNivelRiesgoValorOpBanCooComD415_MOD:=ifelse(peorNivelRiesgoValorOpBanCooComD415>5,5,peorNivelRiesgoValorOpBanCooComD415)]
info[,numOpsVencidas24MD338_MOD:=ifelse(numOpsVencidas24MD338>4,4,numOpsVencidas24MD338)]
info[,numAcreedoresOpBanCooComD414_MOD:=ifelse(numAcreedoresOpBanCooComD414>4,4,numAcreedoresOpBanCooComD414)]
info[,numAcreedores348_MOD:=ifelse(numAcreedores348>9,9,numAcreedores348)]
info[,numOpsAperturadas12M142_MOD:=ifelse(numOpsAperturadas12M142>8,8,numOpsAperturadas12M142)]
info[,ANTIGUEDAD_OP_OTROS_MOD:=ifelse(ANTIGUEDAD_OP_OTROS>133.900,133.900,ANTIGUEDAD_OP_OTROS)]

info[,ANTIG_LABORAL_MOD:=ifelse(ANTIG_LABORAL>267,267,ANTIG_LABORAL)]
info[,MAX_DVEN_SCE_6M_MOD:=ifelse(MAX_DVEN_SCE_6M>180.00,180.00,MAX_DVEN_SCE_6M)]
info[,numOpsVencidas36MD339_MOD:=ifelse(numOpsVencidas36MD339>4,4,numOpsVencidas36MD339)]
info[,ANTIG_DOMICILIARIA_MOD:=ifelse(ANTIG_DOMICILIARIA>25,25,ANTIG_DOMICILIARIA)]


#AMAS
info[,NOPE_VENC_31AMAS_OP_36M:=NOPE_VENC_31A90_OP_36M+NOPE_VENC_91A180_OP_36M+
       NOPE_VENC_181A360_OP_36M+NOPE_VENC_MAYOR360_OP_36M]
info[,NOPE_VENC_31AMAS_OP_36M_MOD:=ifelse(NOPE_VENC_31AMAS_OP_36M>6,6,NOPE_VENC_31AMAS_OP_36M)]
info[,CARGAS_MOD:=ifelse(CARGAS>3,3,CARGAS)]

#RATIOS

info[,R_cuotaTotOp059_INGRESOS:=cuotaTotOp059/INGRESOS] #1.41909685
info[,R_cuotaTotOp059_INGRESOS_MOD:=ifelse(R_cuotaTotOp059_INGRESOS > 1.41909685,1.41909685,R_cuotaTotOp059_INGRESOS)]


info[,R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_36M:=ifelse(PROM_MAX_DVEN_SC_OP_36M==0,0,PROM_MAX_DVEN_SC_OP_12M/PROM_MAX_DVEN_SC_OP_36M)]

info[,R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M:=ifelse(PROM_MAX_DVEN_SC_OP_24M==0,0,PROM_MAX_DVEN_SC_OP_12M/PROM_MAX_DVEN_SC_OP_24M)]

info[,R_NOPE_VENC_OP_36M_NOPE_APERT_SBS_36M:=ifelse(NOPE_APERT_SBS_OP_36M==0,0,NOPE_VENC_OP_36M/NOPE_APERT_SBS_OP_36M)]

info[,R_NOPE_VENC_OP_24M_NOPE_VENC_OP_36M:=ifelse(NOPE_VENC_OP_36M==0,0,NOPE_VENC_OP_24M/NOPE_VENC_OP_36M)]


info[,R_DEUDA_TOTAL_SC_6M_DEUDA_TOTAL_SCE_6M:=ifelse(DEUDA_TOTAL_SCE_6M==0,0,DEUDA_TOTAL_SF_6M/DEUDA_TOTAL_SCE_6M)]
info[,R_DEUDA_TOTAL_SC_6M_DEUDA_TOTAL_SCE_6M_MOD:=ifelse(R_DEUDA_TOTAL_SC_6M_DEUDA_TOTAL_SCE_6M>0.9895973,0.9895973,R_DEUDA_TOTAL_SC_6M_DEUDA_TOTAL_SCE_6M)]


info[,R_MaxMontoOpD24M417_salTotOpBCoo036:=ifelse(salTotOpBCoo036==0,0,MaxMontoOpD24M417/salTotOpBCoo036)]
info[,R_MaxMontoOpD24M417_salTotOpBCoo036_MOD:=ifelse(R_MaxMontoOpD24M417_salTotOpBCoo036>13.2493156,13.2493156,R_MaxMontoOpD24M417_salTotOpBCoo036)]

info[,r_cuota052sINGRESOS := ifelse(INGRESOS > 0, cuota052/INGRESOS, 0)]
info[,r_salTotOp040smaxMontoOp096:= ifelse(maxMontoOp096>0,salTotOp040/maxMontoOp096, 0)]

info[,r_PROM_MAX_DVEN_SC_OP_12s24M_MOD:=ifelse(r_PROM_MAX_DVEN_SC_OP_12s24M>1.9914530,1.9914530,r_PROM_MAX_DVEN_SC_OP_12s24M)]

#info[,NOPEAPERTSCE:=ifelse(NOPE_APERT_SCE_24M > 9, 9, NOPE_APERT_SCE_24M)]
#Variables de probabilkidad de malo
info[,prbm_DEUDA_TOTAL_SBS_OP_3M:=ifelse(DEUDA_TOTAL_SBS_OP_3M==0,0.32246,
                                         ifelse(DEUDA_TOTAL_SBS_OP_3M<=8414.460,0.46632,
                                                ifelse(DEUDA_TOTAL_SBS_OP_3M<=38653.780,0.51463,0.57863)))]

info[,prbm_MAX_DVEN_SCE_6M:=ifelse(MAX_DVEN_SCE_6M==0,0.24733,
                                         ifelse(MAX_DVEN_SCE_6M<=20,0.75751,
                                                ifelse(MAX_DVEN_SCE_6M<=78,0.87549,0.92936)))]


info[,prbb_MAX_DVEN_SCE_3M:=ifelse(MAX_DVEN_SCE_3M==0,0.75460,
                                   ifelse(MAX_DVEN_SCE_3M<=20,0.24874,
                                          ifelse(MAX_DVEN_SCE_3M<=88,0.11353,0.0755)))]

#prbb_MAX_DVEN_SCE_3M
info[,prbb_MAX_DVEN_SCE_6M:=ifelse(MAX_DVEN_SCE_6M==0,0.77623,
                                   ifelse(MAX_DVEN_SCE_6M<=9,0.36210,
                                          ifelse(MAX_DVEN_SCE_6M<=30,0.28457,0.13418)))]


info[,prbm_maySalVenD24M275:=ifelse(maySalVenD24M275==0,0.29590,
                                   ifelse(maySalVenD24M275<=81.760,0.60773,
                                          ifelse(maySalVenD24M275<=273.920,0.69922,0.77257)))]

info[,prbm_cuotaTotOp059:=ifelse(cuotaTotOp059<=209.470,0.30735,
                                    ifelse(cuotaTotOp059<=466.930,0.35849,
                                           ifelse(cuotaTotOp059<=774.740,0.42013,
                                                  ifelse(cuotaTotOp059<=1071.880,0.49683,
                                                         ifelse(cuotaTotOp059<=1645.570,0.54619,0.62377)))))]

info[,prbm_cuotaTotOp059:=ifelse(cuotaTotOp059<=209.470,1,
                                 ifelse(cuotaTotOp059<=466.930,2,
                                        ifelse(cuotaTotOp059<=774.740,3,
                                               ifelse(cuotaTotOp059<=1071.880,4,
                                                      ifelse(cuotaTotOp059<=1645.570,5,6)))))]

info[,prbm_DEUDA_TOTAL_SCE_6M:=ifelse(DEUDA_TOTAL_SCE_6M<=25433.970,0.35840,
                                 ifelse(DEUDA_TOTAL_SCE_6M<=71015.920,0.3844,
                                        ifelse(DEUDA_TOTAL_SCE_6M<=100378.870,0.42259,
                                               ifelse(DEUDA_TOTAL_SCE_6M<=149911.940,0.45759,
                                                      ifelse(DEUDA_TOTAL_SCE_6M<=262627.480,0.51232,0.58817)))))]



info[,prbb_DEUDA_TOTAL_SBS_OP_12M:=ifelse(DEUDA_TOTAL_SBS_OP_12M==0,0.31925,
                                         ifelse(DEUDA_TOTAL_SBS_OP_12M<=33041.530,0.44646,
                                                ifelse(DEUDA_TOTAL_SBS_OP_12M<=140867.310,0.50758,0.57828)))]


info[,prbm_NOPE_VENC_31AMAS_OP_36M:=ifelse(NOPE_VENC_31AMAS_OP_36M==0,0.32385,
                                          ifelse(NOPE_VENC_31AMAS_OP_36M<=1,0.54850,
                                                 ifelse(NOPE_VENC_31AMAS_OP_36M<=3,0.62552,0.75411)))]

info[,prbb_antiguedadOpBanCoo388:=ifelse(antiguedadOpBanCoo388<=40,0.53417,
                                           ifelse(antiguedadOpBanCoo388<=71,0.57397,0.63769))]



info[,prbm_numTCVig36M125:=ifelse(numTCVig36M125==0,0.37395,
                                           ifelse(numTCVig36M125<=2,0.46261,0.52134))]

info[,prbm_salEntidad118:=ifelse(salEntidad118==0,0.36358,
                                  ifelse(salEntidad118<=389.570,0.59181,0.69954))]



info[,prbb_ESTADO_CIVIL:=ifelse(is.na(ESTADO_CIVIL)|ESTADO_CIVIL=="Union Libre",0.7037718,
                                           ifelse(ESTADO_CIVIL=="Casado (a)"|ESTADO_CIVIL=="Viudo (a)",0.6404544,
                                                  ifelse(ESTADO_CIVIL=="Divorciado (a)",0.5593992,0.5288934)))]


info[,prbm_INSTRUCCION:=ifelse(is.na(INSTRUCCION)|INSTRUCCION=="Primario",0.34394,0.43786)]


info[,prbb_numMesesSinVenDesdeUltVen100:=ifelse(numMesesSinVenDesdeUltVen100==0,0.03053,
                                      ifelse(numMesesSinVenDesdeUltVen100<=8,0.33152,
                                             ifelse(numMesesSinVenDesdeUltVen100<=19,0.53599,
                                                    ifelse(numMesesSinVenDesdeUltVen100<=35,0.56963,0.71457))))]

info[,prbb_numMesesSinVenDesdeUltVenD334:=ifelse(numMesesSinVenDesdeUltVenD334==0,0.03018,
                                                ifelse(numMesesSinVenDesdeUltVenD334<=8,0.33146,
                                                       ifelse(numMesesSinVenDesdeUltVenD334<=18,0.53440,
                                                              ifelse(numMesesSinVenDesdeUltVenD334<=35,0.57607,0.72027))))]


info[,prbb_SALDO_PROMEDIO_AHORRO:=ifelse(SALDO_PROMEDIO_AHORRO==0,0.36993,
                                      ifelse(SALDO_PROMEDIO_AHORRO<=0.370,0.50097,
                                             ifelse(SALDO_PROMEDIO_AHORRO<=4.950,0.55063,
                                                    ifelse(SALDO_PROMEDIO_AHORRO<=6.780,0.62196,
                                                           ifelse(SALDO_PROMEDIO_AHORRO<=12.410,0.63512,
                                                                  ifelse(SALDO_PROMEDIO_AHORRO<=37.330,0.67489,
                                                                         ifelse(SALDO_PROMEDIO_AHORRO<=169.660,0.71303,0.73464)))))))]


info[,prbb_ANTIG_LABORAL:=ifelse(ANTIG_LABORAL<=43,0.53338,
                                                ifelse(ANTIG_LABORAL<=71,0.56523,
                                                       ifelse(ANTIG_LABORAL<=149,0.60026,
                                                              ifelse(ANTIG_LABORAL<=219,0.62670,0.66514))))]

info[,prbm_NOPE_VENC_31AMAS_OP_36M:=ifelse(NOPE_VENC_31AMAS_OP_36M<=0,0.32362,
                                 ifelse(NOPE_VENC_31AMAS_OP_36M<=1,0.55207,
                                        ifelse(NOPE_VENC_31AMAS_OP_36M<=3,0.62301,0.74499)))]


info[,prbm_peorNivelRiesgoValorOpBanCooComD415:=ifelse(peorNivelRiesgoValorOpBanCooComD415<=1,0.32636,
                                           ifelse(peorNivelRiesgoValorOpBanCooComD415<=2,0.93458,0.9755))]

                                                      
#SEGMENTACION CLEAN DIRTY
#MARCAS para clean Dirty 

info[,M_DEMANDA_CASTIGO_SCE_24M:=ifelse(MVAL_DEMANDA_CASTIGO_SCE_24M==0,0,1)]
info[,SEGMENTACION:=ifelse(M_DEMANDA_CASTIGO_SCE_24M == 0 & MAX_DVEN_SCE_12M <= 30,0,1)]
info[,.N,by=SEGMENTACION]
dim(info)
#DISCRETIZACIONDE  VARIABLES 
arbol<-mod[,.(VarDep,DEUDA_TOTAL_SBS_OP_3M,DEUDA_TOTAL_SBS_OP_6M)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,MAX_DVEN_SCE_3M)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,maySalVenD24M275)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,cuotaTotOp059)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,DEUDA_TOTAL_SCE_6M)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,DEUDA_TOTAL_SBS_OP_12M)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,NOPE_VENC_31AMAS_OP_36M)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,numTCVig36M125)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,ANTIGUEDAD_TC_SICOM)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,salEntidad118)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,ESTADO_CIVIL)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,INSTRUCCION)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,ACTIVIDAD_ECONOMICA)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,numMesesSinVenDesdeUltVen100)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,numAcreedoresOpBanCooComD414)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,numOpsVencidas3MD336)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,numOpsVencidas3MD336)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,MAX_DVEN_SCE_6M)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,SALDO_PROMEDIO_AHORRO)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,numMesesSinVenDesdeUltVenD334)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,ANTIG_LABORAL,numMesesSinVenDesdeUltVenD334)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,antiguedadOpBanCoo388)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,NOPE_VENC_31AMAS_OP_36M,peorNivelRiesgoValorOpBanCooComD415)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
#ARBOLITO CLEAN DIRTY 
arbol<-mod[,.(VarDep,MVAL_DEMANDA_CASTIGO_SCE_24M,MAX_DVEN_SCE_12M,numMesesSinVenDesdeUltVenD334,
              NOPE_VENC_31AMAS_OP_36M,ANTIG_LABORAL,SALDO_PROMEDIO_AHORRO,
              antiguedadOpBanCoo388,MAX_DVEN_SCE_6M)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)



# Entrenamiento del modelo

#mod <- info[ModVal == 0 & VarDep %in% c(0,1)]
dim(info)
mod[,.N,by=VarDep]
mod <- info[ModVal == 0 & VarDep %in% c(0,1)]
mod[,.N,by=VarDep]
dim(mod)
mod$pr
prop.table(mod[,table(prbb_ESTADO_CIVIL, VarDep)],1)

# Gráfico de distribuciones
ggplot(mod, aes(x =maySalVenD36M299)) + geom_density(aes(group = as.factor(VarDep), fill = as.factor(VarDep)), alpha = 0.5)

# Cá# Cá# Cálculo de percentiles
quantile(mod$maySalVenD36M299, probs = seq(0,1,by=0.01),na.rm = TRUE)
quantile(mod$ANTIGUEDAD_TC_SICOM, probs = seq(0,1,by=0.01),na.rm = TRUE)

#info[]
#formula <- "VarDep ~ LN_DEUDA_TOTAL_SBS_OP_24M + INGRESOS_mod"
#973
#formula <- "VarDep ~ prbm_maySalVenD24M275+cuotaTotOp059+prbm_DEUDA_TOTAL_SBS_OP_3M+
#            numAcreedoresOpBanCooComD414+NOPE_APERT_SCE_OP_24M_MOD+prbm_MAX_DVEN_SCE_3M"
##903

#EL segundo mejor
#formula <- "VarDep ~ prbm_maySalVenD24M275+LN_DEUDA_TOTAL_SCE_6M+LN_DEUDA_TOTAL_SCE_OP_24M+
#            numAcreedoresOpBanCooComD414+NOPE_APERT_SCE_OP_24M_MOD+prbm_MAX_DVEN_SCE_3M+
#            R_cuotaTotOp059_INGRESOS_MOD+R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M+
#            R_DEUDA_TOTAL_SC_6M_DEUDA_TOTAL_SCE_6M+prbm_NOPE_VENC_31AMAS_OP_36M+
#            prbm_numTCVig36M125+R_maxMontoOp096_salTotOpBCoo036_MOD"


#R_maxMontoOp096_salTotOpBCoo036_MOD
#formula <- "VarDep ~ 
#                      R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_36M+
#                      numAcreedoresOpBanCooComD414+
#                      prbm_NOPE_VENC_31AMAS_OP_36M+d_numOpsVen_prem+
#                      prbb_numMesesSinVenDesdeUltVen100+antiguedadOpBanCoo388+
#                      prbm_MAX_DVEN_SCE_3M"



#EL MEjor
#formula <- "VarDep ~ numOpsVencidas36MD339_MOD + prbb_numMesesSinVenDesdeUltVenD334+
#prbb_SALDO_PROMEDIO_AHORRO + peorNivelRiesgoValorOpBanCooComD415_MOD+r_salTotOp040smaxMontoOp096 + 
#prbb_MAX_DVEN_SCE_6M+antiguedadOpBanCoo388+numAcreedoresOpBanCooComD414_MOD+
#+NOPE_APERT_SCE_OP_24M_MOD+LN_DEUDA_TOTAL_SCE_OP_24M+NOPE_APERT_SCE_OP_24M_MOD"


#El MEjor
formula <- "VarDep ~  R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M+prbb_numMesesSinVenDesdeUltVenD334+
prbb_MAX_DVEN_SCE_6M+numAcreedoresOpBanCooComD414_MOD+
r_salTotOp040smaxMontoOp096 + 
NOPE_APERT_SCE_OP_24M_MOD+prbm_NOPE_VENC_31AMAS_OP_36M+
prbb_ANTIG_LABORAL+prbb_SALDO_PROMEDIO_AHORRO+
prbb_antiguedadOpBanCoo388+peorNivelRiesgoValorOpBanCooComD415_MOD"
#formula <- "VarDep ~ numOpsVencidas36MD339_MOD + prbb_numMesesSinVenDesdeUltVenD334+
#prbb_SALDO_PROMEDIO_AHORRO + peorNivelRiesgoValorOpBanCooComD415_MOD+r_salTotOp040smaxMontoOp096 + 
#prbb_MAX_DVEN_SCE_6M+antiguedadOpBanCoo388+numAcreedoresOpBanCooComD414_MOD+
#+NOPE_APERT_SCE_OP_24M_MOD+LN_DEUDA_TOTAL_SCE_6M+NOPE_APERT_SCE_OP_24M_MOD+prbb_ANTIG_LABORAL"

#info$prb

#prbm_cuotaTotOp059
#cuotaTotOp059
#d_numOpsVen_prem
#NOPE_XVEN_OP_12M_MOD
#+numOpsVigD332_MOD
#numMesesSinVenDesdeUltVenD334
modelo <- glm(formula = as.formula(formula), family = binomial("logit"), data = mod)
summary(modelo)

# Validaciones y contrastes
res <- cor(mod[,c("prbb_numMesesSinVenDesdeUltVenD334","prbb_SALDO_PROMEDIO_AHORRO",
                  "peorNivelRiesgoValorOpBanCooComD415_MOD", "r_salTotOp040smaxMontoOp096",
                  "prbb_MAX_DVEN_SCE_6M", "prbb_antiguedadOpBanCoo388",
                  "numAcreedoresOpBanCooComD414_MOD", "NOPE_APERT_SCE_OP_24M_MOD",
                  "prbb_ANTIG_LABORAL", "prbm_NOPE_VENC_31AMAS_OP_36M",
                  "R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M")])
#view(res)
#write.table(res,"Tabla_Correlacion.xls",sep="\t",fileEncoding = "UTF-8")

sqrt(max(eigen(res)$values)/min(eigen(res)$values))

# Rango de Score
rango_score <- function(vector){
      index <- aux <- seq(1:length(vector))
      res <- data.frame(id=index, val=vector)
      res <- res[order(res$val, decreasing = TRUE),]
      res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
      res <- res[order(res$id),]
      return(as.numeric(res$aux))
}

# Estimación Probabilidades para la base de modelamiento
mod <- info_model[ModVal == 0 & VarDep %in% c(0,1)]
dim(mod)
res_mod <- data.table(Var=mod$VarDep, Score=1000-floor(1000*predict(modelo, mod, type="response")))
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]

# Rango de Score
rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 21),0), labels = seq(1,20))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}

# Estimación Probabilidades para la base de mod
mod <- info_model[ModVal == 0 & VarDep %in% c(0,1)]
dim(mod)
res_mod <- data.table(Var=mod$VarDep, Score=1000-floor(1000*predict(modelo, mod, type="response")))
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]


# Estimación Probabilidades para la base de evaluacion
mod <- info_model[ModVal == 1 & VarDep %in% c(0,1)]
dim(mod)
res_mod <- data.table(Var=mod$VarDep, Score=1000-floor(1000*predict(modelo, mod, type="response")))
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]




# Rango de Score
rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}

# Estimación Probabilidades para la base de evaluacion
mod <- info_model[ModVal == 1 & VarDep %in% c(0,1)]
dim(mod)
res_mod <- data.table(Var=mod$VarDep, Score=1000-floor(1000*predict(modelo, mod, type="response")))
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]




#CON LA MUESTRA CON TODOS 0 1 2 3 4 5 


# Rango de Score
rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}

# Estimación Probabilidades para la base de modelamiento
mod <- info[ModVal == 0 & VarDep %in% c(0,1,2,3,4)]
dim(mod)
res_mod <- data.table(Var=mod$VarDep, Score=1000-floor(1000*predict(modelo, mod, type="response")))
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]

# Rango de Score
rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 21),0), labels = seq(1,20))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}

# Estimación Probabilidades para la base de evaluacion
mod <- info[ModVal == 0 & VarDep %in% c(0,1,2,3,4)]
dim(mod)
res_mod <- data.table(Var=mod$VarDep, Score=1000-floor(1000*predict(modelo, mod, type="response")))
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]




# Rango de Score
rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}

# Estimación Probabilidades para la base de evaluacion
mod <- info[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
dim(mod)
res_mod <- data.table(Var=mod$VarDep, Score=1000-floor(1000*predict(modelo, mod, type="response")))
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]

# Rango de Score
rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 21),0), labels = seq(1,20))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}

# Estimación Probabilidades para la base de evaluacion
mod <- info[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
dim(mod)
res_mod <- data.table(Var=mod$VarDep, Score=1000-floor(1000*predict(modelo, mod, type="response")))
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]





#Aplicando el modelo a la base de 79 mil 
dim(info)
# Ejecución de la fórmula sobre toda la base
info[, Y := 6.060151 +
       0.223242 * R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M +
       (-1.548924) * prbb_numMesesSinVenDesdeUltVenD334 +
       (-2.582596) * prbb_MAX_DVEN_SCE_6M +
       0.198832 * numAcreedoresOpBanCooComD414_MOD +
       0.697172 * r_salTotOp040smaxMontoOp096 +
       0.077996 * NOPE_APERT_SCE_OP_24M_MOD +
       0.406974 * prbm_NOPE_VENC_31AMAS_OP_36M +
       (-4.431135) * prbb_ANTIG_LABORAL +
       (-1.941352) * prbb_SALDO_PROMEDIO_AHORRO +
       (-5.547708) * prbb_antiguedadOpBanCoo388 +
       1.096508 * peorNivelRiesgoValorOpBanCooComD415_MOD]
info[, SCORE := ceiling(1000/(1 + exp(Y)))]
info[, SEGMENTO := ifelse(SCORE <= 6, "5. C",
                          ifelse(SCORE <= 242, "4. B",
                                 ifelse(SCORE <= 740, "3. A",
                                        ifelse(SCORE <= 878, "2. AA", "1. AAA"))))]
info[ModVal == 0 & VarDep %in% c(0,1,2,3,4)][,.N,by=SEGMENTO][order(SEGMENTO)]
info[ModVal == 1 & VarDep %in% c(0,1,2,3,4)][,.N,by=SEGMENTO][order(SEGMENTO)]


#VAlidacion score de la base para comparacion 

# Rango de Score
rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}

# Estimación Probabilidades para la base de evaluacion
mod <- info[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
dim(mod)
res_mod <- data.table(Var=mod$VarDep, Score=mod$score419)
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]



# Rango de Score
rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 21),0), labels = seq(1,20))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}


# Estimación Probabilidades para la base de evaluacion
mod <- info[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
dim(mod)
res_mod <- data.table(Var=mod$VarDep, Score=mod$score419)
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]
