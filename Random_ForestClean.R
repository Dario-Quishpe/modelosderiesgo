install.packages("ranger")

# Cargar la librería ranger
library(ranger)
library(data.table)
library(tidyverse)
# Entrenamiento del modelo
mod <- info_clean[ModVal == 0 & VarDep %in% c(0,1)]
mod[,.N,by=VarDep]
dim(mod)
dim(info_clean)
# Cálculo de percentiles
quantile(mod$numOpsVencidas3M102, probs = seq(0,1,by=0.01))
prop.table(mod[,table(NOPE_APERT_SCE_24M_MOd, VarDep)],1)
info$numOpsAperturadas12M142

# Random Forest
formula_rf<- 
  "VarDep ~prbb_ESTADO_CIVIL+prbb_numMesesSinVenDesdeUltVenD334+
prbb_SALDO_PROMEDIO_AHORRO+prbb_MAX_DVEN_SCE_6M+
prbb_ANTIG_LABORAL+r_salTotOp040smaxMontoOp096+
R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M+prbb_numAcreedoresOpBanCooComD414+
prbb_numOpsAperturadas12M142+PROM_VEN_SCE_6M+
MAX_DVEN_SCE_12M+MAX_DVEN_SCE_24M+
NOPE_APERT_SCE_OP_24M+maySalVenD24M275+
ANTIGUEDAD_OP_OTROS+MAX_DVEN_SCE_36M+
ANTIGUEDAD_OP_SC+
maySalVen36M293
"


#formula_rf<-"VarDep ~
#MAX_DVEN_SCE_12M+MAX_DVEN_SCE_6M+
#numOpsAperturadas12M142+NOPE_APERT_SCE_OP_24M+
#numOpsVigD332+numMesesSinVenDesdeUltVenD334+
#MVALVEN_SCE_36M+maySalVenD36M299+moraPersona305+
#numAcreedoresOpBanCooComD414+R_DEUDA_TOTAL_SC_6M_DEUDA_TOTAL_SCE_6M+
#NOPE_XVEN_OP_12M+NENT_VEN_SCE_12M+NOPE_APERT_SF_OP_3M+
#NOPE_APERT_SF_OP_6M+SALDO_PROMEDIO_AHORRO+ANTIGUEDAD_OP_SICOM+
#consultasUlt12M098+
#ANTIG_LABORAL+antiguedadOpTcBanCoo390+edad+cuotaCom056+r_salTotOp040smaxMontoOp096+
#peorNivelRiesgoValorOpBanCooComD415+salEntidad118+ANTIGUEDAD_OP_SC+moraOps307
#
#"

#formula_rf<-"VarDep ~
#numOpsAperturadas12M142+
#numOpsVigD332+numMesesSinVenDesdeUltVenD334+maySalVenD36M299+moraPersona305+
#numAcreedoresOpBanCooComD414+NENT_VEN_SCE_12M+
#NOPE_APERT_SF_OP_6M+SALDO_PROMEDIO_AHORRO+
#ANTIG_LABORAL+antiguedadOpTcBanCoo390+edad+r_salTotOp040smaxMontoOp096+cuotaVencidos302+
#+NOPE_APERT_SCE_OP_12M+numOpsVigD332+numOpsAperturadas6M141+
#NOPE_APERT_SCE_OP_6M+MAX_DVEN_SCE_OP_6M+
#ANTIGUEDAD_OP_OTROS+NOPE_APERT_SF_OP_6M+MAX_DVEN_SF_6M
#"


formula_rf<-"VarDep ~
prbb_numOpsAperturadas12M142_clean+
numOpsVigD332+prbb_maySalVenD36M299_clean+d_moraPersona305_clean+
prbb_numAcreedoresOpBanCooComD414_clean+NENT_VEN_SCE_12M+
NOPE_APERT_SF_OP_6M+prbb_SALDO_PROMEDIO_AHORRO_clean+
prbb_ANTIG_LABORAL_clean+antiguedadOpTcBanCoo390+prbb_edad_clean+r_salTotOp040smaxMontoOp096+cuotaVencidos302+
+NOPE_APERT_SCE_OP_12M+numOpsAperturadas6M141+
NOPE_APERT_SCE_OP_6M+MAX_DVEN_SCE_OP_6M+
ANTIGUEDAD_OP_OTROS+MAX_DVEN_SF_6M+prbb_ESTADO_CIVIL_clean+prbb_INSTRUCCION_clean+ANTIGUEDAD_OP_SICOM+
numOpsVencidas3M102+d_numMesesSinVenDesdeUltVenD334_clean+d_numOpsVencidas3MD336_clean+prbb_NOPE_APERT_SCE_OP_12M_clean+
d_numOpsAperturadas12M142_clean+d_moraOps307_clean+prbb_numOpsVigD332_clean
"



#formula_rf <- "VarDep ~
#d_moraPersona305_clean+
#prbb_numMesesSinVenDesdeUltVenD334_clean+
#prbb_SALDO_PROMEDIO_AHORRO_clean+  #
#prbb_MAX_DVEN_SCE_6M_clean+
#r_salTotOp040smaxMontoOp096+
#R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M+
#prbb_numAcreedoresOpBanCooComD414_clean+
#prbb_numOpsAperturadas12M142_clean+
#d_cuotaVencidos302_clean+
#r_DEUDA_TOTAL_SBS_OP_6s24M+
#d_moraPersona305_clean+
#d_numMesesSinVenDesdeUltVenD334_clean+
#d_numOpsVencidas3MD336_clean+
#prbb_NOPE_APERT_SCE_OP_12M_clean+
#d_numOpsAperturadas12M142_clean+
#ANTIGUEDAD_OP_OTROS+
#d_moraOps307_clean+
#prbb_numOpsVigD332_clean+
#NENT_VEN_SCE_12M+
#MAX_DVEN_SF_12M+
#antiguedadOpTcBanCoo390+
#ANTIGUEDAD_OP_SICOM+
#maySalVen12M245+
#maySalVenD24M275+
#maySalVenD36M299+
#edad+NOPE_APERT_SCE_OP_6M+
#d_antiguedadOpBanCoo388_clean"


formula_rf <- "VarDep ~
NOPE_APERT_SCE_OP_6M+
moraOps307+
prbb_MAX_DVEN_SCE_6M_clean+
r_salTotOp040smaxMontoOp096+
edad+
maySalVenD24M275+
ANTIGUEDAD_OP_OTROS+
ANTIGUEDAD_OP_SICOM+ #
prbb_numOpsAperturadas12M142_clean+
numAcreedoresOpBanCooComD414+
d_numOpsVencidas3MD336_clean+
numMesesSinVenDesdeUltVenD334+
SALDO_PROMEDIO_AHORRO+
numOpsAperturadas12M142+
ANTIG_LABORAL
"
#numOpsVig36M099
#formula_rf <- "VarDep ~ MAX_DVEN_SCE_12M + NOPE_APERT_SCE_24M + numMesesSinVenDesdeUltVenD334 + 
#habCC085 + numOpsVig092 + numOpsVencidas101 + DEUDA_TOTAL_SBS_OP_3M + DEUDA_TOTAL_SBS_OP_12M +
#DEUDA_TOTAL_SC_OP_3M + DEUDA_TOTAL_SC_OP_12M + maySalVenD24M275 + maySalVenD36M299 + numOpsVencidas24MD338 + 
#numOpsVencidas36MD339 + numOpsAperturadas12M142 + INSTRUCCION + CARGAS + ANTIG_LABORAL + RELACION_DEPENDENCIA + 
#ESTADO_CIVIL + INGRESOS + MVAL_CASTIGO_SBS_OP_6M + DEUDA_TOTAL_SICOM_OP_3M + NENT_VEN_SBS_OP_12M"
rf <- ranger(formula = as.formula(formula_rf), data = mod, num.trees = 300, mtry = 5, importance = "impurity", write.forest = T, probability = T, 
             min.node.size = floor(0.03*nrow(mod)), seed = 1723951065)

summary(rf)

imp <- data.table(Variable=names(ranger::importance(rf)), Valor=unname(ranger::importance(rf)))[order(Valor)]
imp[order(desc(Valor))]

# Rango de Score
rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}
# Función de reemplazo NA's
reemplazo_col = function(dt, vars, valor){ 
  na.replace = function(v, value=valor) { v[is.na(v)] = value; v }
  for (i in vars)
    eval(parse(text=paste("dt[,",i,":=na.replace(",i,")]")))
}

num_nas <- sum(is.na(info_clean$numOpsAperturadas12M142))
quantile(mod$numOpsVencidas3M102, probs = seq(0,1,by=0.01),na.rm = TRUE)
print(num_nas)

#reemplazo_col(info_clean, c("antiguedadOpTcBanCoo390","numOpsVigD332","numAcreedoresOpBanCooComD414","numOpsAperturadas3M140",
#                      "numAcreedoresOpBanCooComD414_MOD", "consultasUlt12M098","peorNivelRiesgoValorOpBanCooComD415
#" ,"salEntidad118","moraPersona305","moraOps307","consumoTC087","numAcreedores348","numOpsVig36MD333",
#                      "r_salTotOp040smaxMontoOp096","numMesesInfoCredBanCoopD36M421","maxCupoTC089",
#                      "prbb_antiguedadOpBanCoo388","numTCVig36M125","saldoOpVenSfrTCS36M379",
#                      "prbm_maySalVenD24M275", "ingreso136","numOpsAperturadas6M141","maySalVenD36M299",
#                      "r_cuota052sINGRESOS","cuotaCom056","cuotaVencidos302","numOpsVencidas3M102",
#                      "maxMorosidadCoo133","prbb_numAcreedoresOpBanCooComD414","numMesesSinVenDesdeUltVenD334",
#                      "numOpsAperturadas12M142", "maySalVenD24M275","maySalVen3M221","habCC085"), 0)

reemplazo_col(info_clean, c("antiguedadOpTcBanCoo390","numOpsVigD332","numAcreedoresOpBanCooComD414","numOpsAperturadas3M140",
                            "numAcreedoresOpBanCooComD414_MOD", "consultasUlt12M098","peorNivelRiesgoValorOpBanCooComD415
" ,"salEntidad118","moraPersona305","moraOps307","consumoTC087","numAcreedores348","numOpsVig36MD333","maySalVenSer12M243",
                            "r_salTotOp040smaxMontoOp096","numMesesInfoCredBanCoopD36M421","maxCupoTC089","salOpDiaCom005",
                            "numTCVig36M125","saldoOpVenSfrTCS36M379","maySalVen078","numOpsVencidas3MD336","numOpsVencidas36M105",
                             "ingreso136","numOpsAperturadas6M141","maySalVenD36M299","maySalVen12M245","salProm6M095","d_numOpsVencidas3MD336_clean",
                            "r_cuota052sINGRESOS","cuotaCom056","cuotaVencidos302","numOpsVencidas3M102","d_antiguedadOpBanCoo388_clean",
                            "maxMorosidadCoo133","numMesesSinVenDesdeUltVenD334","prbb_numAcreedoresOpBanCooComD414_clean", "d_moraPersona305_clean",
                            "numOpsAperturadas12M142","salPromD6M316","SalTotOpD383", "GASTOS",
                            "maySalVenD24M275","maySalVen3M221","habCC085","d_numOpsVencidas3MD336_clean",
                            "salProm36M303", "salPromD36M319", "numOpsVig36M099","maxMontoOp096", "cuotaCoo055", "salOpDia008", "salTotOp040", "salOpDiaCoo004", "salTotOpBCoo036"), 0)

reemplazo_col(info_clean,c("prbm_maxMorosidadCoo133_dirty","prbm_MVALVEN_SCE_3M_dirty","prbb_numMesesSinVenDesdeUltVenD334_dirty",
                          "prbm_peorNivelRiesgoValorOpBanCooComD415_dirty"),0)
reemplazo_col(info_clean,c("edad"),19)

reemplazo_col(info,c("moraPersona305","prbm_peorNivelRiesgoValorOpBanCooComD415_dirty"),0)
reemplazo_col(info,c("moraOps307", "numAcreedoresOpBanCooComD414", "d_numOpsVencidas3MD336_clean"),0)
#reemplazo_col(info, c("numOpsVig092", "numOpsVencidas101", "maySalVenD24M275", "maySalVenD36M299", "numOpsVencidas24MD338", "numOpsVencidas36MD339", "numOpsAperturadas12M142"), 0)
#reemplazo_col(info, c("numMesesSinVenDesdeUltVenD334"), 36)
#reemplazo_col(info, c("habCC085"), "NO")

# Ejecución de la fórmula sobre toda la base
info_clean[, SCORE_RF_clean := ceiling(1000*predict(object=rf, data=info_clean, predict.all = FALSE, type = "response")$predictions[,1])]
info_dirty[, SCORE_RF_clean := ceiling(1000*predict(object=rf, data=info_dirty, predict.all = FALSE, type = "response")$predictions[,1])]
info[, SCORE_RF_clean := ceiling(1000*predict(object=rf, data=info, predict.all = FALSE, type = "response")$predictions[,1])]
info_clean[, SEGMENTO := ifelse(SCORE <= 477, "5. C",
                          ifelse(SCORE <= 672, "4. B",
                                 ifelse(SCORE <= 842, "3. A",
                                        ifelse(SCORE <= 938, "2. AA", "1. AAA"))))]


info_clean[ModVal == 1][,.N,by=SEGMENTO][order(SEGMENTO)]
info_clean[ModVal==1][,.N,by=VarDep]
info_clean[SEGMENTO=="1. AAA"]

info_clean[ModVal == 0 & VarDep %in% c(0,1,2,3,4)][,.N,by=SEGMENTO][order(SEGMENTO)]
info_clean[ModVal == 1 & VarDep %in% c(0,1,2,3,4)][,.N,by=SEGMENTO][order(SEGMENTO)]

# Estimación Probabilidades para la base de modelamiento
mod <- info_clean[ModVal == 0 & VarDep %in% c(0,1,2,3,4)]
res_mod <- data.table(Var=mod$VarDep, Score=mod$SCORE_RF_clean)
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]

  # Estimación Probabilidades para la base de validación 
  val <- info_clean[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
  res_val <- data.table(Var=val$VarDep, Score=val$SCORE_RF_clean)
  res_val$Rango <- rango_score(res_val$Score)
  res_val[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
  res_val[,table(Rango, Var)]


# Estimación Probabilidades para la base de validación Score 419
val <- info_clean[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
res_val <- data.table(Var=val$VarDep, Score=val$score419)
res_val$Rango <- rango_score(res_val$Score)
res_val[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_val[,table(Rango, Var)]



## Optimización de hiperparámetros
formula_rf <- "VarDep ~ MAX_DVEN_SCE_12M + NOPE_APERT_SCE_24M + numMesesSinVenDesdeUltVenD334 + 
habCC085 + numOpsVig092 + numOpsVencidas101 + DEUDA_TOTAL_SBS_OP_3M + DEUDA_TOTAL_SBS_OP_12M +
DEUDA_TOTAL_SC_OP_3M + DEUDA_TOTAL_SC_OP_12M + maySalVenD24M275 + maySalVenD36M299 + numOpsVencidas24MD338 + 
numOpsVencidas36MD339 + numOpsAperturadas12M142 + INSTRUCCION + CARGAS + ANTIG_LABORAL + RELACION_DEPENDENCIA + 
ESTADO_CIVIL + INGRESOS + MVAL_CASTIGO_SBS_OP_6M + DEUDA_TOTAL_SICOM_OP_3M + NENT_VEN_SBS_OP_12M"

mod <- info_clean[ModVal == 0 & VarDep %in% c(0,1)]
val <- info_clean[ModVal == 1 & VarDep %in% c(0,1)]

# Número de árboles
num_trees <- seq(10, 500, 10)
train_errors <- rep(NA, times = length(num_trees))
oob_errors   <- rep(NA, times = length(num_trees))

for (i in seq_along(num_trees)){
  modelo  <- ranger(formula   = as.formula(formula_rf), data = mod, 
                    num.trees = num_trees[i], oob.error = TRUE, seed = 123)
  predicciones_train <- predict(modelo, data = mod)
  predicciones_train <- predicciones_train$predictions
  
  train_error <- mean((predicciones_train - mod$VarDep)^2)
  oob_error   <- modelo$prediction.error
  
  train_errors[i] <- sqrt(train_error)
  oob_errors[i]   <- sqrt(oob_error)
}

# Gráfico
resultados <- data.frame(n_arboles = num_trees, train_errors, oob_errors)
ggplot(data = resultados) +
  geom_line(aes(x = num_trees, y = train_errors, color = "train rmse")) + 
  geom_line(aes(x = num_trees, y = oob_errors, color = "oob rmse")) +
  geom_vline(xintercept = num_trees[which.min(oob_errors)],
             color = "firebrick", linetype = "dashed") +
  labs(title = "Evolución del OOB vs número árboles", x = "número de árboles",
       y = "OOB-error (rmse)", color = "") + theme_bw() +
  theme(legend.position = "bottom")

# Número de variables para construir cada árbol
num_mtry <- seq(1, 20, 1)
train_errors <- rep(NA, times = length(num_mtry))
oob_errors   <- rep(NA, times = length(num_mtry))

for (i in seq_along(num_mtry)){
  modelo  <- ranger(formula   = as.formula(formula_rf), data = mod, num.trees = 200, 
                    mtry = num_mtry[i], oob.error = TRUE, seed = 123)
  predicciones_train <- predict(modelo, data = mod)
  predicciones_train <- predicciones_train$predictions
  
  train_error <- mean((predicciones_train - mod$VarDep)^2)
  oob_error   <- modelo$prediction.error
  
  train_errors[i] <- sqrt(train_error)
  oob_errors[i]   <- sqrt(oob_error)
}
# Gráfico
resultados <- data.frame(mtry = num_mtry, train_errors, oob_errors)
ggplot(data = resultados) +
  geom_line(aes(x = num_mtry, y = train_errors, color = "train rmse")) + 
  geom_line(aes(x = num_mtry, y = oob_errors, color = "oob rmse")) +
  geom_vline(xintercept =  num_mtry[which.min(oob_errors)],
             color = "firebrick", linetype = "dashed") +
  labs(title = "Evolución del OOB-error vs mtry", x = "mtry", y = "OOB-error (rmse)",
       color = "" ) + theme_bw() + theme(legend.position = "bottom")


# Mínimo de registros en los nodos finales
num_size <- floor((1:30/100)*nrow(mod))
train_errors <- rep(NA, times = length(num_size))
oob_errors   <- rep(NA, times = length(num_size))

for (i in seq_along(num_size)){
  modelo  <- ranger(formula   = as.formula(formula_rf), data = mod, num.trees = 200, 
                    mtry = 7, min.node.size = num_size[i], oob.error = TRUE, seed = 123)
  predicciones_train <- predict(modelo, data = mod)
  predicciones_train <- predicciones_train$predictions
  
  train_error <- mean((predicciones_train - mod$VarDep)^2)
  oob_error   <- modelo$prediction.error
  
  train_errors[i] <- sqrt(train_error)
  oob_errors[i]   <- sqrt(oob_error)
}
# Gráfico
resultados <- data.frame(size = num_size, train_errors, oob_errors)
ggplot(data = resultados) +
  geom_line(aes(x = num_size, y = train_errors, color = "train rmse")) + 
  geom_line(aes(x = num_size, y = oob_errors, color = "oob rmse")) +
  geom_vline(xintercept =  num_size[which.min(oob_errors)],
             color = "firebrick", linetype = "dashed") +
  labs(title = "Evolución del OOB-error vs min.size", x = "min.size", y = "OOB-error (rmse)",
       color = "" ) + theme_bw() + theme(legend.position = "bottom")





