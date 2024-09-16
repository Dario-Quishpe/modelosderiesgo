# Random Forest

dir.p <- getwd()
dir.b <- paste0(dir.p, "/BDD")
dir.s <- paste0(dir.p, "/Scripts")
dir.r <- paste0(dir.p, "/Resultados")
list.files()

setwd(dir.b)
list.files()
load("InfoModelamiento.RData")

# Construcción de variables y discretizaciones
info[, NOPE_APERT_SCE_24M := (NOPE_APERT_SBS_OP_24M + NOPE_APERT_SC_OP_24M + NOPE_APERT_SICOM_OP_24M + NOPE_APERT_OTROS_OP_24M ), ]
info[, MAX_DVEN_SCE_12M := pmax(MAX_DVEN_SBS_OP_12M, MAX_DVEN_SC_OP_12M, MAX_DVEN_SICOM_OP_12M, MAX_DVEN_OTROS_SIS_OP_12M,
                                MAX_DVEN_SBS_TC_12M, MAX_DVEN_SC_TC_12M, MAX_DVEN_SICOM_TC_12M, MAX_DVEN_OTROS_SIS_TC_12M)]

#library(h2oEnsemble)
library(h2o)

h2o.init(ip = "localhost", nthreads = -1, max_mem_size = "4G")
mod <- info[ModVal == 0 & VarDep %in% c(0,1)]
# Import a sample binary outcome train/test set into H2O
vars <- c("VarDep","R_PROM_MAX_DVEN_SC_OP_12M_PROM_MAX_DVEN_SC_OP_24M",
          "prbb_numMesesSinVenDesdeUltVenD334",
          "prbb_MAX_DVEN_SCE_6M",
          "numAcreedoresOpBanCooComD414_MOD",
          "r_salTotOp040smaxMontoOp096",
          "NOPE_APERT_SCE_OP_24M_MOD",
          "prbm_NOPE_VENC_31AMAS_OP_36M",
          "prbb_ANTIG_LABORAL",
          "prbb_SALDO_PROMEDIO_AHORRO",
          "prbb_antiguedadOpBanCoo388",
          "prbm_peorNivelRiesgoValorOpBanCooComD415",
          "prbb_ESTADO_CIVIL",
          "ANTIGUEDAD_OP_SICOM",
          "prbm_maySalVenD24M275",
          "numOpsVigD332",
          "r_cuota052sINGRESOS",
          "PROM_VEN_SCE_6M",
          "r_PROM_MAX_DVEN_M_OP_3s6M",
          "r_PROM_MAX_DVEN_SC_OP_3s6M",
          "PROM_NDI_SCE_3M",
          "maxMorosidadCoo133",
          "r_PROM_MAX_DVEN_SC_OP_24s36M",
          "numOpsAperturadas12M142",
          "maySalVenD36M299","edad","cuotaVencidos302","ANTIGUEDAD_OP_OTROS"
)

mod_em <- as.h2o(x = setDT(mod)[, vars, with=FALSE])

# Identify predictors and response
y_em <- "VarDep"; x_em <- setdiff(names(mod_em), y_em)

# For binary classification, response should be a factor
mod_em[,y_em] <- as.factor(mod_em[,y_em])

# Number of CV folds (to generate level-one data for stacking)
nfolds <- 6

# 1. Generate a 4-model ensemble (RF + GLM + GBM + XGBoost)
# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x_em,
                          y = y_em,
                          model_id = "RF",
                          training_frame = mod_em,
                          ntrees = 300,
                          min_rows = 852,
                          mtries = 5,
                          nfolds = nfolds,
                          fold_assignment = "Stratified",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1723951065)

# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = x_em,
                  y = y_em,
                  model_id = "GBM",
                  training_frame = mod_em,
                  distribution = "bernoulli",
                  ntrees = 300,
                  max_depth = 5,
                  min_rows = 852,
                  learn_rate = 0.02,
                  nfolds = nfolds,
                  fold_assignment = "Stratified",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1723951065)

# Train & Cross-validate a GLM
my_glm <- h2o.glm(x = x_em, 
                  y = y_em, 
                  model_id = "GLM",
                  training_frame = mod_em, 
                  alpha = 0,
                  remove_collinear_columns = TRUE, 
                  nfolds = nfolds, 
                  fold_assignment = "Stratified",
                  keep_cross_validation_predictions = TRUE, 
                  seed = 1723951065
)


# Train & Cross-validate a XGBoost
my_xgb <- h2o.xgboost(x = x_em, 
                      y = y_em, 
                      model_id = "XGB",
                      training_frame = mod_em, 
                      ntrees = 200, 
                      learn_rate = 0.02,
                      max_depth = 5, 
                      min_rows = 531, 
                      nfolds = nfolds, 
                      fold_assignment = "Stratified", 
                      keep_cross_validation_predictions = TRUE, 
                      seed = 1723951065, 
                      stopping_rounds = 50,
                      stopping_metric = "RMSE", 
                      stopping_tolerance = 0
)


# Train a stacked ensemble using the GBM and RF above
# Train a stacked ensemble using the GBM and RF above

e1m <- h2o.stackedEnsemble(x = x_em,
                           y = y_em,
                           training_frame = mod_em,
                           model_id = "Ensamble_1m",
                           metalearner_algorithm = "glm",
                           base_models = list(my_rf, my_gbm))

e2m <- h2o.stackedEnsemble(x = x_em,
                           y = y_em,
                           training_frame = mod_em,
                           model_id = "Ensamble_2m",
                           metalearner_algorithm = "glm",
                           base_models = list(my_rf, my_glm))

e3m <- h2o.stackedEnsemble(x = x_em,
                           y = y_em,
                           training_frame = mod_em,
                           model_id = "Ensamble_3m",
                           metalearner_algorithm = "glm",
                           base_models = list(my_rf, my_glm,my_gbm))

e4m <- h2o.stackedEnsemble(x = x_em,
                           y = y_em,
                           training_frame = mod_em,
                           model_id = "Ensamble_4m",
                           metalearner_algorithm = "glm",
                           base_models = list( my_glm,my_gbm))


# Guardamos los modelos
h2o.saveModel(object = my_rf, path = "graficas/", force = TRUE)

# Carga de modelo para estimación
my_rf2 <- h2o.loadModel(path = "graficas/RF")

info_em <- as.h2o(x = setDT(info)[, vars, with=FALSE])
info_em[,y_em] <- as.factor(info_em[,y_em])
info[, Score_RF := ceiling(1000*as.data.frame(h2o.predict(my_rf2, newdata = info_em))[,2])]
summary(info$Score_RF)

# Realizamos las predicciones sobre la base de modelamiento/validación
setwd(dir.p)

mod <- info[ModVal == 0 & VarDep %in% c(0,1,2,3,4)]
val <- info[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]


mod_em <- as.h2o(x = setDT(mod)[, vars, with=FALSE])
val_em <- as.h2o(x = setDT(val)[, vars, with=FALSE])
mod_em[,y_em] <- as.factor(mod_em[,y_em])
val_em[,y_em] <- as.factor(val_em[,y_em])


rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}
res_fun <- function(valida, resultado){
  res <- data.frame(VarDep=valida$VarDep,
                    Score=1000-round(1000*as.data.frame(resultado)[,3],0), 
                    Rango=rango_score(1000-round(1000*as.data.frame(resultado)[,3],0)))
  return(res)
}
res_funX <- function(valida, resultado){
  res <- data.frame(VarDep=valida$VarDep,
                    Score=1000-round(1000*as.data.frame(resultado)[,1],0), 
                    Rango=rango_score(1000-round(1000*as.data.frame(resultado)[,1],0)))
  return(res)
}

# Random Forest
mod_rf <- setDT(res_fun(mod, h2o.predict(my_rf, newdata = mod_em)))
mod_rf[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
mod_rf[,table(Rango, VarDep)]
val_rf <- setDT(res_fun(val, h2o.predict(my_rf, newdata = val_em)))
val_rf[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
val_rf[,table(Rango, VarDep)]

# Regresión Logística
mod_glm <- setDT(res_fun(mod, h2o.predict(my_glm, newdata = mod_em)))
mod_glm[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
mod_glm[,table(Rango, VarDep)]
val_glm <- setDT(res_fun(val, h2o.predict(my_glm, newdata = val_em)))
val_glm[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
val_glm[,table(Rango, VarDep)]

# Gradient Boosting
mod_gbm <- setDT(res_fun(mod, h2o.predict(my_gbm, newdata = mod_em)))
mod_gbm[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
mod_gbm[,table(Rango, VarDep)]
val_gbm <- setDT(res_fun(val, h2o.predict(my_gbm, newdata = val_em)))
val_gbm[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
val_gbm[,table(Rango, VarDep)]

# XGBoost
mod_xgb <- setDT(res_fun(mod, h2o.predict(my_xgb, newdata = mod_em)))
mod_xgb[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
mod_xgb[,table(Rango, VarDep)]
val_xgb <- setDT(res_fun(val, h2o.predict(my_xgb, newdata = val_em)))
val_xgb[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
val_xgb[,table(Rango, VarDep)]


# Ensamble 1m
system.time(mod_e1m <- setDT(res_fun(mod, h2o.predict(e1m, newdata = mod_em))))
mod_e1m[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
mod_e1m[,table(Rango, VarDep)]
val_e1m <- setDT(res_fun(val, h2o.predict(e1m, newdata = val_em)))
val_e1m[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
val_e1m[,table(Rango, VarDep)]

# Ensamble 2m
system.time(mod_e2m <- setDT(res_fun(mod, h2o.predict(e2m, newdata = mod_em))))
mod_e2m[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
mod_e2m[,table(Rango, VarDep)]
val_e2m <- setDT(res_fun(val, h2o.predict(e2m, newdata = val_em)))
val_e2m[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
val_e2m[,table(Rango, VarDep)]



# Ensamble 3m
system.time(mod_e3m <- setDT(res_fun(mod, h2o.predict(e3m, newdata = mod_em))))
mod_e3m[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
mod_e3m[,table(Rango, VarDep)]
val_e3m <- setDT(res_fun(val, h2o.predict(e3m, newdata = val_em)))
val_e3m[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
val_e3m[,table(Rango, VarDep)]


# Ensamble 4m
system.time(mod_e4m <- setDT(res_fun(mod, h2o.predict(e4m, newdata = mod_em))))
mod_e4m[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
mod_e4m[,table(Rango, VarDep)]
val_e4m <- setDT(res_fun(val, h2o.predict(e4m, newdata = val_em)))
val_e4m[,list(Min=min(Score), Max=max(Score)),by=Rango][order(Rango)]
val_e4m[,table(Rango, VarDep)]
