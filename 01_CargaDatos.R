# Carga de datos de la estructura de variables Score

dir.p <- getwd()
dir.b <- paste0(dir.p, "/BDD")
dir.s <- paste0(dir.p, "/Scripts")
list.files()
library(data.table)
# Info al punto de observación
info <- fread("OneDrive_1_11-5-2024/DataInicial_01022024_123459.txt")
info <- info[Muestra == 1]
importante<-colnames(info)

# Carga de tablas+ de desempeño
Op1 <- fread("EstructuraVariables/EstructuraVariables/OperacionesTabla1.txt")[,c(1,3,4,2,5:111)]
Tc1 <- fread("EstructuraVariables/EstructuraVariables/TarjetasTabla1.txt")
colnames(Tc1) <- colnames(Op1)
des <- rbindlist(list(Op1, Tc1))
rm(list = c("Op1", "Tc1"))

s1 <- des[, lapply(.SD, max), .SDcols = paste0("NUMERO_DIAS_MOROSIDAD_OP_M", 1:13), by = "IDENTIFICACION_SCORE"]
s2 <- des[, lapply(.SD, sum), .SDcols = paste0("SALDO_DEUDA_OP_M", 1:13), by = "IDENTIFICACION_SCORE"]
s3 <- des[, lapply(.SD, sum), .SDcols = paste0("SALDO_VENCIDO_OP_M", 1:13), by = "IDENTIFICACION_SCORE"]
s4 <- des[, lapply(.SD, sum), .SDcols = paste0("SALDO_CCASTIGADA_OP_M", 1:13), by = "IDENTIFICACION_SCORE"]
s5 <- des[, lapply(.SD, sum), .SDcols = paste0("SALDO_DJUDICIAL_OP_M", 1:13), by = "IDENTIFICACION_SCORE"]
res <- s1[s2, on = "IDENTIFICACION_SCORE"]
res <- s3[res, on = "IDENTIFICACION_SCORE"]
res <- s4[res, on = "IDENTIFICACION_SCORE"]
res <- s5[res, on = "IDENTIFICACION_SCORE"]
rm(list = paste0("s", 1:5))

# Cruce de información (Desempeño + Pto Obs)
colnames(res)[1] <- "IDENTIFICACION"
info <- res[info, on = "IDENTIFICACION"]
rm(list = c("res"))

# Carga de tablas históricas
Op3 <- fread("EstructuraVariables/EstructuraVariables/OperacionesTabla3.txt")
Tc3 <- fread("EstructuraVariables/EstructuraVariables/TarjetaTabla3.txt")
colnames(Op3)[c(3,1)] <- c("IDENTIFICACION", "FECHA_CORTE")
colnames(Tc3)[c(3,1)] <- c("IDENTIFICACION", "FECHA_CORTE")

info <- Op3[info, on = c("IDENTIFICACION", "FECHA_CORTE")]
info <- Tc3[info, on = c("IDENTIFICACION", "FECHA_CORTE")]
rm(list = c("Op3", "Tc3"))

# Generación de variable acumuladas (SF + SCE)
info[, NOPE_APERT_SF_OP_3M := NOPE_APERT_SBS_OP_3M + NOPE_APERT_SC_OP_3M]
info[, NOPE_APERT_SCE_OP_3M := NOPE_APERT_SBS_OP_3M + NOPE_APERT_SC_OP_3M + 
          NOPE_APERT_SICOM_OP_3M + NOPE_APERT_OTROS_OP_3M]

info[, MVAL_DEMANDA_SCE_OP_3M := max(MVAL_DEMANDA_SBS_OP_3M, MVAL_DEMANDA_SC_OP_3M, MVAL_DEMANDA_SICOM_OP_3M), by = "IDENTIFICACION"]

info[, DEUDA_TOTAL_SF_24M := sum(DEUDA_TOTAL_SBS_OP_24M, DEUDA_TOTAL_SC_OP_24M, DEUDA_TOTAL_SBS_TC_24M, 
                                 DEUDA_TOTAL_SC_TC_24M), by="IDENTIFICACION"]
info[, DEUDA_TOTAL_SCE_24M := sum(DEUDA_TOTAL_SBS_OP_24M, DEUDA_TOTAL_SC_OP_24M, DEUDA_TOTAL_SICOM_OP_24M,
                                  DEUDA_TOTAL_OTROS_OP_24M, DEUDA_TOTAL_SBS_TC_24M, DEUDA_TOTAL_SC_TC_24M,
                                  DEUDA_TOTAL_SICOM_TC_24M, DEUDA_TOTAL_OTROS_TC_24M), by="IDENTIFICACION"]

# Base indicadores
load("INDICADORES.RData")
dim(d)

vars <- c("fechaCalificacion", "tipoIdentificacionSujeto",
          "tipoIdentificacionSujetoDescripcion", "identificacionSujeto",
          "marcaPrinTC090", "emisorPrinTC091", "gastoPersonal093",
          "disponibleEst094", "entidad109", "salEntidad110", "entidad111",
          "salEntidad112", "entidad113", "salEntidad114","entidad115",
          "salEntidad116", "buenoMaloBancos342", "buenoMaloCoops343",
          "buenoMaloTarjetas344", "Covid19OpTc345", "ID", "entidad",
          "identificacionSujeto", "ID4", "ingreso136_Actual")

d <- setDT(d)[, -vars, with=FALSE]
colnames(d)[1:2] <- c("IDENTIFICACION", "FECHA_CORTE")
d[, FECHA_CORTE := lubridate::ymd(FECHA_CORTE)]

info <- d[info, on = c("IDENTIFICACION", "FECHA_CORTE")]
#setwd(dir.b)
save(list = c("info"), file = "InfoConsolidad.RData", envir = .GlobalEnv)
load("InfoConsolidad.RData")


