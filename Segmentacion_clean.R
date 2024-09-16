# modelos de segmentacion 

load("InfoModelamiento_clean.RData")
options(scipen = 999999)


# Construcción de variables y discretizaciones

info_clean[,prbb_ESTADO_CIVIL_clean:=ifelse(is.na(ESTADO_CIVIL)|ESTADO_CIVIL=="Union Libre"|ESTADO_CIVIL=="Viudo (a)"|ESTADO_CIVIL=="Casado (a)",0.80738,0.66103)]

info_clean[,prbb_numMesesSinVenDesdeUltVenD334_clean:=ifelse(numMesesSinVenDesdeUltVenD334<=6,0.23128,
                                                 ifelse(numMesesSinVenDesdeUltVenD334<=18,0.57277,
                                                        ifelse(numMesesSinVenDesdeUltVenD334<=35,0.62441,0.75057)))]

info_clean[,prbb_SALDO_PROMEDIO_AHORRO_clean:=ifelse(SALDO_PROMEDIO_AHORRO<=10.080,0.65641,0.79324)]

info_clean[,prbb_MAX_DVEN_SCE_6M_clean:=ifelse(MAX_DVEN_SCE_6M==0,0.78726,0.29703)]


info_clean[,prbb_ANTIG_LABORAL_clean:=ifelse(ANTIG_LABORAL<=41,0.60012,
                                 ifelse(ANTIG_LABORAL<=88,0.66183,
                                        ifelse(ANTIG_LABORAL<=217,0.71028,0.77620)))]


info_clean[,prbb_numAcreedoresOpBanCooComD414_clean:=ifelse(numAcreedoresOpBanCooComD414<=2,0.79253,0.57003)]





info_clean[,prbb_numOpsAperturadas12M142_clean:=ifelse(numOpsAperturadas12M142<=0,0.85560,
                                                 ifelse(numOpsAperturadas12M142<=1,0.78316,
                                                        ifelse(numOpsAperturadas12M142<=2,0.65963,
                                                               ifelse(numOpsAperturadas12M142<=3,0.54720,
                                                                      ifelse(numOpsAperturadas12M142<=5,0.45044,0.38797)))))]


info_clean[,prbb_edad_clean:=ifelse(edad<=26,0.49573,
                                                 ifelse(edad<=29,0.62236,
                                                        ifelse(edad<=40,0.65287,
                                                               ifelse(edad<=49,0.68951,
                                                                      ifelse(edad<=62,0.72767,0.78170)))))]


info_clean[,prbb_INSTRUCCION_clean:=ifelse(is.na(INSTRUCCION)|INSTRUCCION=="Primario"|INSTRUCCION=="Sin estudios",0.73043,0.42574)]

info_clean[,prbb_maySalVenD36M299_clean:=ifelse(maySalVenD36M299<=22.6,0.73043,0.42574)]

info_clean[,prbb_NOPE_APERT_SCE_OP_12M_clean:=ifelse(NOPE_APERT_SCE_OP_12M<=2,1-0.19399,1-0.48354)]

info_clean[,prbb_numOpsVigD332_clean:=ifelse(numOpsVigD332<=5,0.7506,0.4521)]
info_clean[,prbb_numOpsVig36MD333_clean:=ifelse(numOpsVig36MD333<=10,0.76113,0.57143)]


info_clean[,d_maySalVenD36M299_clean:=ifelse(maySalVenD36M299==0,0,1)]
info_clean[,d_cuotaVencidos302_clean:=ifelse(cuotaVencidos302==0,0,1)]
info_clean[,d_moraPersona305_clean:=ifelse(moraPersona305==0,0,1)]
info_clean[,d_moraOps307_clean:=ifelse(moraOps307==0,0,1)]
info_clean[,d_moraTC306_clean:=ifelse(moraTC306==0,0,1)]
info_clean[,d_numMesesSinVenDesdeUltVenD334_clean:=ifelse(numMesesSinVenDesdeUltVenD334>35,0,1)]
info_clean[,d_numOpsVencidas3MD336_clean:=ifelse(numOpsVencidas3MD336<=0,0,1)]
info_clean[,d_numOpsAperturadas12M142_clean:=ifelse(numOpsAperturadas12M142<=2,0,1)]
info_clean[,d_peorNivelRiesgoValorOpBanCooComD415_clean:=ifelse(peorNivelRiesgoValorOpBanCooComD415<=1,0,1)]
info_clean[,d_NENT_VEN_SCE_12M_clean:=ifelse(NENT_VEN_SCE_12M<=1,0,1)]
info_clean[,d_maySalVen078_clean:=ifelse(maySalVen078<=0,0,1)]
info_clean[,d_antiguedadOpBanCoo388_clean:=ifelse(antiguedadOpBanCoo388>=37,0,1)]

info_clean[,d_consultasUlt12M098_clean:=ifelse(consultasUlt12M098<=1,0,1)]
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

###################################################
#write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,ESTADO_CIVIL,INSTRUCCION,ANTIG_LABORAL,numOpsVencidas3M102,numOpsVigD332,maySalVenD24M275,maySalVenD36M299,maySalVen078,
              numMesesSinVenDesdeUltVenD334,numAcreedoresOpBanCooComD414,numMesesSinVenDesdeUltVenD334,
              SALDO_PROMEDIO_AHORRO,ACTIVIDAD_ECONOMICA,numOpsVencidas3MD336,NOPE_APERT_SCE_OP_12M,numOpsAperturadas6M141,
              antiguedadOpBanCoo388,MAX_DVEN_SCE_6M,NOPE_VENC_31AMAS_OP_36M,numOpsVencidas3M102,NENT_VEN_SCE_12M,ANTIGUEDAD_OP_OTROS,
              peorNivelRiesgoValorOpBanCooComD415,numOpsAperturadas12M142,moraTC306,moraOps307,moraPersona305,
              NENT_VEN_SCE_12M,ANTIGUEDAD_OP_OTROS,maySalVenBan073,maySalVenCoo074,
              maySalVen3M221,maySalVen12M245,maySalVenD24M275,maySalVenD36M299,edad,consultasUlt12M098,numTCVig36M125,maxCupoTC089,
              numMesesInfoCredBanCoopD36M421,consumoTC087
              )]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)


arbol<-mod[,.(VarDep,salPromD6M316,salPromD6M316,salProm36M303,salPromD36M319,numOpsVig36MD333,
              NOPE_XVEN_OP_12M,NOPE_XVEN_OP_24M,numOpsVig36M099,maxMontoOp096,cuotaCoo055,
              DEUDA_TOTAL_SC_OP_12M,PROM_XVEN_SC_OP_24M,salOpDia008,salTotOp040,
              salOpDiaCoo004,salTotOpBCoo036,NOPE_APERT_SF_OP_3M,NOPE_APERT_SCE_OP_3M,
              DEUDA_TOTAL_SCE_OP_3M,DEUDA_TOTAL_SF_OP_3M,ANTIGUEDAD_OP_SC,GASTOS,PROM_XVEN_SCE_24M


)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)



###################################################
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)
arbol<-mod[,.(VarDep,numOpsAperturadas12M142, numOpsVigD332, numMesesSinVenDesdeUltVenD334, maySalVenD36M299, moraPersona305, numAcreedoresOpBanCooComD414, NENT_VEN_SCE_12M, NOPE_APERT_SF_OP_6M, SALDO_PROMEDIO_AHORRO, ANTIG_LABORAL, antiguedadOpTcBanCoo390, edad, r_salTotOp040smaxMontoOp096, cuotaVencidos302, NOPE_APERT_SCE_OP_12M, numOpsAperturadas6M141, NOPE_APERT_SCE_OP_6M, MAX_DVEN_SCE_OP_6M, ANTIGUEDAD_OP_OTROS, MAX_DVEN_SF_6M, ESTADO_CIVIL, INSTRUCCION, ANTIGUEDAD_OP_SICOM, numOpsVencidas3M102
)]
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
arbol<-mod[,.(VarDep,NOPE_VENC_31AMAS_OP_36M)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)

# Entrenamiento del modelo
#mod <- info[ModVal == 0 & VarDep %in% c(0,1)]
#dim(info)
#mod[,.N,by=VarDep]
mod <- info_clean[ModVal == 0 & VarDep %in% c(0,1)]
mod[,.N,by=VarDep]
dim(mod)
prop.table(mod[,table(numTCAperturadas6M144, VarDep)],1)
# Gráfico de distribuciones
ggplot(mod, aes(x =DEUDA_TOTAL_SBS_OP_6M)) + geom_density(aes(group = as.factor(VarDep), fill = as.factor(VarDep)), alpha = 0.5)

# Cá# Cá# Cálculo de percentiles
info_clean$DEUDA_TOTAL_SBS_OP_6M
quantile(mod$DEUDA_TOTAL_SBS_OP_6M, probs = seq(0,1,by=0.01),na.rm = TRUE)
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
#+NOPE_APERT_SCE_OP_24M_MOD+LN_DEUDA_TOTAL_SCE_OP_24M+NOPE_APERT_SCE_OP_2
#El MEjor
formula <- "VarDep ~prbb_ESTADO_CIVIL+prbb_numMesesSinVenDesdeUltVenD334+
prbb_SALDO_PROMEDIO_AHORRO+prbb_MAX_DVEN_SCE_6M+prbb_ANTIG_LABORAL+
r_salTotOp040smaxMontoOp096+R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M+
prbb_numAcreedoresOpBanCooComD414+prbb_numOpsAperturadas12M142+PROM_VEN_SCE_6M"

formula <- "VarDep ~prbb_ESTADO_CIVIL_clean+prbb_numMesesSinVenDesdeUltVenD334_clean+
prbb_SALDO_PROMEDIO_AHORRO_clean+prbb_MAX_DVEN_SCE_6M_clean+prbb_ANTIG_LABORAL_clean+
r_salTotOp040smaxMontoOp096+R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M+
prbb_numAcreedoresOpBanCooComD414_clean+prbb_numOpsAperturadas12M142_clean+d_cuotaVencidos302_clean+
r_DEUDA_TOTAL_SBS_OP_6s24M+d_moraPersona305_clean+d_numMesesSinVenDesdeUltVenD334_clean+d_numOpsVencidas3MD336_clean+
prbb_NOPE_APERT_SCE_OP_12M_clean+d_numOpsAperturadas12M142_clean+d_moraOps307_clean+
d_peorNivelRiesgoValorOpBanCooComD415_clean+prbb_numOpsVigD332_clean+d_NENT_VEN_SCE_12M_clean+d_maySalVen078_clean+
maySalVenCoo075
"



#R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M+prbb_numMesesSinVenDesdeUltVenD334+
#prbb_MAX_DVEN_SCE_6M+numAcreedoresOpBanCooComD414_MOD+
#r_salTotOp040smaxMontoOp096 + 
#NOPE_APERT_SCE_OP_24M_MOD+prbm_NOPE_VENC_31AMAS_OP_36M+
#prbb_ANTIG_LABORAL+prbb_SALDO_PROMEDIO_AHORRO+
#prbb_antiguedadOpBanCoo388+peorNivelRiesgoValorOpBanCooComD415_MOD"
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
res <- cor(mod[,c("prbb_ESTADO_CIVIL", "prbb_numMesesSinVenDesdeUltVenD334", 
                  "prbb_SALDO_PROMEDIO_AHORRO", "prbb_MAX_DVEN_SCE_6M", "prbb_ANTIG_LABORAL", 
                  "r_salTotOp040smaxMontoOp096", "R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M", 
                  "prbb_numAcreedoresOpBanCooComD414",
                  "prbb_numOpsAperturadas12M142", "d_cuotaVencidos302", "r_DEUDA_TOTAL_SBS_OP_6s24M")])

#view(res)
write.table(res,"Tabla_Correlacion.xls",sep="\t",fileEncoding = "UTF-8")

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
mod <- info_clean[ModVal == 0 & VarDep %in% c(0,1)]
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
mod <- info_clean[ModVal == 0 & VarDep %in% c(0,1)]
dim(mod)
res_mod <- data.table(Var=mod$VarDep, Score=1000-floor(1000*predict(modelo, mod, type="response")))
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]


# Estimación Probabilidades para la base de evaluacion
mod <- info_clean[ModVal == 1 & VarDep %in% c(0,1)]
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
mod <- info_clean[ModVal == 1 & VarDep %in% c(0,1)]
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
mod <- info_clean[ModVal == 0 & VarDep %in% c(0,1,2,3,4)]
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
val <- info_clean[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
dim(val)
res_val <- data.table(Var=val$VarDep, Score=1000-floor(1000*predict(modelo, val, type="response")))
res_val$Rango <- rango_score(res_val$Score)
res_val[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_val[,table(Rango, Var)]




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
mod <- info_clean[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
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
info[, SEGMENTO := ifelse(SCORE <= 2, "5. C",
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
mod <- info_clean[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
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