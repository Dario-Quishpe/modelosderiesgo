# Entrenamiento del modelo
install.packages("ranger")

# Cargar la librería ranger
library(ranger)
library(data.table)

# Entrenamiento del modelo
mod <- info[ModVal == 0 & VarDep %in% c(0,1)]
mod[,.N,by=VarDep]
dim(mod)
dim(info)
# Cálculo de percentiles
quantile(mod$numOpsVencidas3M102, probs = seq(0,1,by=0.01))
prop.table(mod[,table(NOPE_APERT_SCE_24M_MOd, VarDep)],1)
info$numOpsAperturadas12M142

# Random Forest
formula_rf<- 
"VarDep ~ R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M + 
prbb_numMesesSinVenDesdeUltVenD334 +
prbb_MAX_DVEN_SCE_6M + 
numAcreedoresOpBanCooComD414_MOD +
r_salTotOp040smaxMontoOp096 +
NOPE_APERT_SCE_OP_24M_MOD +
prbm_NOPE_VENC_31AMAS_OP_36M +
prbb_ANTIG_LABORAL +
prbb_SALDO_PROMEDIO_AHORRO + 
prbb_antiguedadOpBanCoo388 +
prbm_peorNivelRiesgoValorOpBanCooComD415 +
prbb_ESTADO_CIVIL+
prbm_maySalVenD24M275 + 
r_cuota052sINGRESOS +
PROM_VEN_SCE_6M +
r_PROM_MAX_DVEN_M_OP_3s6M +
r_PROM_MAX_DVEN_SC_OP_3s6M  +
maxMorosidadCoo133 +
r_PROM_MAX_DVEN_SC_OP_24s36M +
numOpsAperturadas12M142+
numOpsVigD332+
maySalVenD36M299+edad+
cuotaVencidos302+ANTIGUEDAD_OP_OTROS+ANTIGUEDAD_OP_SICOM"

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

num_nas <- sum(is.na(info$prbb_numMesesSinVenDesdeUltVenD334))
print(num_nas)

reemplazo_col(info, c("prbb_numMesesSinVenDesdeUltVenD334", 
                      "numAcreedoresOpBanCooComD414_MOD", 
                      "r_salTotOp040smaxMontoOp096", 
                      "prbb_antiguedadOpBanCoo388", 
                      "prbm_peorNivelRiesgoValorOpBanCooComD415", 
                      "prbm_maySalVenD24M275", "cuotaVencidos302",
                      "r_cuota052sINGRESOS","numOpsVigD332", 
                      "maxMorosidadCoo133"), 0)
reemplazo_col(info,c("edad"),19)
#reemplazo_col(info, c("numOpsVig092", "numOpsVencidas101", "maySalVenD24M275", "maySalVenD36M299", "numOpsVencidas24MD338", "numOpsVencidas36MD339", "numOpsAperturadas12M142"), 0)
#reemplazo_col(info, c("numMesesSinVenDesdeUltVenD334"), 36)
#reemplazo_col(info, c("habCC085"), "NO")

# Ejecución de la fórmula sobre toda la base
info[, SCORE_RF := ceiling(1000*predict(object=rf, data=info, predict.all = FALSE, type = "response")$predictions[,1])]
info[, SEGMENTO := ifelse(SCORE <= 42, "5. C",
                          ifelse(SCORE <= 298, "4. B",
                                 ifelse(SCORE <= 685, "3. A",
                                        ifelse(SCORE <= 868, "2. AA", "1. AAA"))))]
info[ModVal == 1][,.N,by=SEGMENTO][order(SEGMENTO)]

info[ModVal == 0 & VarDep %in% c(0,1,2,3,4)][,.N,by=SEGMENTO][order(SEGMENTO)]
info[ModVal == 1 & VarDep %in% c(0,1,2,3,4)][,.N,by=SEGMENTO][order(SEGMENTO)]

# Estimación Probabilidades para la base de modelamiento
mod <- info[ModVal == 0 & VarDep %in% c(0,1,2,3,4)]
res_mod <- data.table(Var=mod$VarDep, Score=mod$SCORE_RF)
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]

# Estimación Probabilidades para la base de validación 
val <- info[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
res_val <- data.table(Var=val$VarDep, Score=val$SCORE_RF)
res_val$Rango <- rango_score(res_val$Score)
res_val[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_val[,table(Rango, Var)]


# Estimación Probabilidades para la base de validación Score 419
val <- info[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
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

mod <- info[ModVal == 0 & VarDep %in% c(0,1)]
val <- info[ModVal == 1 & VarDep %in% c(0,1)]

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





