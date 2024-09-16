# modelos de segmentacion 

load("InfoModelamiento_dirty.RData")
options(scipen = 999999)
#dim(info_dirty)



info_dirty[,prbb_SALDO_PROMEDIO_AHORRO_dirty:=ifelse(SALDO_PROMEDIO_AHORRO<=0.410,0.17925,
                                               ifelse(SALDO_PROMEDIO_AHORRO<=8.300,0.28366,
                                                      ifelse(SALDO_PROMEDIO_AHORRO<=222.580,0.34197,0.38665)))]



info_dirty[,prbm_MVALVEN_SCE_3M_dirty:=ifelse(MVALVEN_SCE_3M<=0,0.6568,
                         ifelse(MVALVEN_SCE_3M<=170.700,0.9091,0.95857))]


info_dirty[,prbb_peorNivelRiesgoValorOpBanCooComD415_dirty:=ifelse(peorNivelRiesgoValorOpBanCooComD415<=2,0.3324,0.02152)]
info_dirty[,prbm_peorNivelRiesgoValorOpBanCooComD415_dirty:=ifelse(peorNivelRiesgoValorOpBanCooComD415<=2,0.6656,0.97848)]

info_dirty[,prbb_numOpsAperturadas12M142_dirty:=ifelse(numOpsAperturadas12M142<=0,0.85560,
                                                 ifelse(numOpsAperturadas12M142<=1,0.78316,
                                                        ifelse(numOpsAperturadas12M142<=2,0.65963,
                                                               ifelse(numOpsAperturadas12M142<=3,0.54720,
                                                                      ifelse(numOpsAperturadas12M142<=5,0.45044,0.38797)))))]


info_dirty[,prbb_antiguedadOpBanCoo388_dirty:=ifelse(antiguedadOpBanCoo388<=40,0.53417,
                                         ifelse(antiguedadOpBanCoo388<=71,0.57397,0.63769))]

info_dirty[,prbb_numMesesSinVenDesdeUltVenD334_dirty:=ifelse(numMesesSinVenDesdeUltVenD334<=0,0.02305,
           ifelse(numMesesSinVenDesdeUltVenD334<=1,0.15427,
                  ifelse(numMesesSinVenDesdeUltVenD334<=350,0.21706,0.22394)))]

#info_dirty[,prbb_numOpsVencidasD335:=ifelse(numOpsVencidasD335==0,0.26074,0.02450)]

info_dirty[,MAX_DVEN_SCE_3M_MOD_dirty:=ifelse(MAX_DVEN_SCE_3M>270,270,MAX_DVEN_SCE_3M)]

info_dirty[,MAX_DVEN_SF_3M_MOD_dirty:=ifelse(MAX_DVEN_SF_3M>180,180,MAX_DVEN_SF_3M)]

info_dirty[,prbm_maxMorosidadCoo133_dirty:=ifelse(maxMorosidadCoo133<=0,0.66131,0.96117)]

info_dirty[,prbm_PROM_NDI_SCE_3M_dirty:=ifelse(PROM_NDI_SCE_3M<=0,0.70644,0.96516)]

info_dirty[,prbb_numOpsVencidasD335_dirty:=ifelse(numOpsVencidasD335<=0,0.26074,0.02450)]

info_dirty[,prbm_maySalVenD24M275_dirty:=ifelse(maySalVenD24M275<=1,0.6838,0.9683)]
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
arbol<-mod[,.(VarDep,maxMorosidadCoo133,ESTADO_CIVIL,MVALVEN_SCE_3M,INSTRUCCION,ANTIG_LABORAL,MAX_DVEN_SCE_3M,numMesesSinVenDesdeUltVen100,
              numMesesSinVenDesdeUltVenD334,numAcreedoresOpBanCooComD414,numOpsVencidas101,numOpsVencidasD335,PROM_NDI_SCE_3M,
              SALDO_PROMEDIO_AHORRO,ACTIVIDAD_ECONOMICA,numOpsVencidas3MD336,numOpsVigD332,PROM_NDI_SCE_3M,maySalVenD24M275,SALDO_PROMEDIO_AHORRO,
              antiguedadOpBanCoo388,MAX_DVEN_SF_3M,NOPE_VENC_31AMAS_OP_36M,peorNivelRiesgoValorOpBanCooComD415,numOpsAperturadas12M142,
              MVALVEN_SCE_3M,maxMorosidadCoo133)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)

###################################################
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
arbol<-mod[,.(VarDep,NOPE_VENC_31AMAS_OP_36M)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)

# Entrenamiento del modelo
#mod <- info[ModVal == 0 & VarDep %in% c(0,1)]
#dim(info)
#mod[,.N,by=VarDep]
mod <- info_dirty[ModVal == 0 & VarDep %in% c(0,1)]
mod[,.N,by=VarDep]
dim(mod)
prop.table(mod[,table(MAX_DVEN_SF_3M, VarDep)],1)
# Gráfico de distribuciones
ggplot(mod, aes(x =r_PROM_MAX_DVEN_SC_OP_3s6M)) + geom_density(aes(group = as.factor(VarDep), fill = as.factor(VarDep)), alpha = 0.5)

# Cá# Cá# Cálculo de percentiles

quantile(mod$r_PROM_MAX_DVEN_SC_OP_3s6M, probs = seq(0,1,by=0.01),na.rm = TRUE)
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
#formula <- "VarDep ~prbb_ESTADO_CIVIL+prbb_numMesesSinVenDesdeUltVenD334+
#prbb_SALDO_PROMEDIO_AHORRO+prbb_MAX_DVEN_SCE_6M+prbb_ANTIG_LABORAL+
#r_salTotOp040smaxMontoOp096+R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M+
#prbb_numAcreedoresOpBanCooComD414+prbb_numOpsAperturadas12M142+PROM_VEN_SCE_6M"

formula <- "VarDep ~prbb_peorNivelRiesgoValorOpBanCooComD415_dirty+
r_PROM_MAX_DVEN_M_OP_3s6M+prbm_maxMorosidadCoo133_dirty+
numAcreedoresOpBanCooComD414_MOD+prbm_MVALVEN_SCE_3M_dirty+r_salTotOp040smaxMontoOp096+
prbb_SALDO_PROMEDIO_AHORRO_dirty+r_PROM_MAX_DVEN_SC_OP_3s6M+prbm_PROM_NDI_SCE_3M_dirty"

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
res <- cor(mod[,c("prbb_peorNivelRiesgoValorOpBanCooComD415",
                  "r_PROM_MAX_DVEN_M_OP_3s6M",
                  "prbm_maxMorosidadCoo133",
                  "numAcreedoresOpBanCooComD414_MOD",
                  "prbm_MVALVEN_SCE_3M",
                  "r_salTotOp040smaxMontoOp096",
                  "prbb_SALDO_PROMEDIO_AHORRO",
                  "r_PROM_MAX_DVEN_SC_OP_3s6M",
                  "prbm_PROM_NDI_SCE_3M")])

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
mod <- info_dirty[ModVal == 0 & VarDep %in% c(0,1)]
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
mod <- info_dirty[ModVal == 1 & VarDep %in% c(0,1)]
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
mod <- info_dirty[ModVal == 1 & VarDep %in% c(0,1)]
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
mod <- info_dirty[ModVal == 0 & VarDep %in% c(0,1,2,3,4)]
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
val<- info_dirty[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
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
mod <- info_dirty[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
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