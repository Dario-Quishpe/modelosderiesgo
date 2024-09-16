#FUNCION PARA VARIABLES SF Y SCE
info[,NOPE_APERT_SF_OP_3M:=NOPE_APERT_SBS_OP_3M+NOPE_APERT_SC_OP_3M]
info[,NOPE_APERT_SCE_OP_3M:=NOPE_APERT_SBS_OP_3M+NOPE_APERT_SC_OP_3M+NOPE_APERT_SICOM_OP_3M+NOPE_APERT_OTROS_OP_3M]
info[,MVAL_DEMANDA_SCE_OP_3M:=pmax(MVAL_DEMANDA_SBS_OP_3M,MVAL_DEMANDA_SC_OP_3M,MVAL_DEMANDA_SICOM_OP_3M),]

info_aux<-info

columnas<-colnames(info)
columnas_12M <- grep("DEUDA_TOTAL_.*12M$", columnas, value = TRUE)
columnas_12M_TC_OP <- grep("DEUDA_TOTAL_.*(TC|OP).*_12M$", columnas, value = TRUE)
columnas_12M_TC <- grep("^DEUDA_TOTAL_.*TC.*_12M$", columnas, value = TRUE)
xd<-"a"
#info_aux[,xd:=do.call(pmax, .SD), .SDcols = columnas_a_combinar]
info_aux[, xd := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_12M_TC]
info_aux$xd
colnames(info)
columnas<-colnames(info)



#Deuda Total
temporalidad<-c("3M","6M","12M","24M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("DEUDA_TOTAL_.*(TC).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("DEUDA_TOTAL_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("DEUDA_TOTAL_SCE_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("DEUDA_TOTAL_SF_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("DEUDA_TOTAL_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("DEUDA_TOTAL_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("DEUDA_TOTAL_SCE_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("DEUDA_TOTAL_SF_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas<-colnames(info)
  columnas_SCE<-grep(paste0("DEUDA_TOTAL_.*(SCE).*_",i,"$"), columnas, value = TRUE)
  columnas_SF<-grep(paste0("DEUDA_TOTAL_.*(SF).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("DEUDA_TOTAL_SCE_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SCE]
  info[,paste0("DEUDA_TOTAL_SF_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SF]
}
print(colnames(info),max=100000)
temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("MVALVEN_.*(TC).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("MVALVEN_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("MVALVEN_SCE_TC_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("MVALVEN_SF_TC_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("MVALVEN_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("MVALVEN_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("MVALVEN_SCE_OP_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("MVALVEN_SF_OP_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas<-colnames(info)
  columnas_SCE<-grep(paste0("MVALVEN_.*(SCE).*_",i,"$"), columnas, value = TRUE)
  columnas_SF<-grep(paste0("MVALVEN_.*(SF).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("MVALVEN_SCE_",i)  := do.call(pmax, .SD), .SDcols = columnas_SCE]
  info[,paste0("MVALVEN_SF_",i)  := do.call(pmax, .SD), .SDcols = columnas_SF]
}
print(colnames(info),max=100000)

temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("MVAL_CASTIGO_.*(TC).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("MVAL_CASTIGO_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("MVAL_CASTIGO_SCE_TC_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("MVAL_CASTIGO_SF_TC_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("MVAL_CASTIGO_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("MVAL_CASTIGO_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("MVAL_CASTIGO_SCE_OP_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("MVAL_CASTIGO_SF_OP_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas<-colnames(info)
  columnas_SCE<-grep(paste0("MVAL_CASTIGO_.*(SCE).*_",i,"$"), columnas, value = TRUE)
  columnas_SF<-grep(paste0("MVAL_CASTIGO_.*(SF).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("MVAL_CASTIGO_SCE_",i)  := do.call(pmax, .SD), .SDcols = columnas_SCE]
  info[,paste0("MVAL_CASTIGO_SF_",i)  := do.call(pmax, .SD), .SDcols = columnas_SF]
}
colnames(info)

temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("MVAL_DEMANDA_.*(TC).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("MVAL_DEMANDA_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("MVAL_DEMANDA_SCE_TC_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("MVAL_DEMANDA_SF_TC_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("MVAL_DEMANDA_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("MVAL_DEMANDA_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("MVAL_DEMANDA_SCE_OP_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("MVAL_DEMANDA_SF_OP_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas<-colnames(info)
  columnas_SCE<-grep(paste0("MVAL_DEMANDA_.*(SCE).*_",i,"$"), columnas, value = TRUE)
  columnas_SF<-grep(paste0("MVAL_DEMANDA_.*(SF).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("MVAL_DEMANDA_SCE_",i)  := do.call(pmax, .SD), .SDcols = columnas_SCE]
  info[,paste0("MVAL_DEMANDA_SF_",i)  := do.call(pmax, .SD), .SDcols = columnas_SF]
}
colnames(info)

temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("NENT_VEN_.*(TC).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("NENT_VEN_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("NENT_VEN_SCE_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("NENT_VEN_SF_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("NENT_VEN_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("NENT_VEN_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("NENT_VEN_SCE_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("NENT_VEN_SF_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas<-colnames(info)
  columnas_SCE<-grep(paste0("NENT_VEN_.*(SCE).*_",i,"$"), columnas, value = TRUE)
  columnas_SF<-grep(paste0("NENT_VEN_.*(SF).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("NENT_VEN_SCE_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SCE]
  info[,paste0("NENT_VEN_SF_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SF]
}
colnames(info)

temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("NTC_APERT_.*(TC).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("NTC_APERT_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("NTC_APERT_SCE_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("NTC_APERT_SF_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("NTC_APERT_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("NTC_APERT_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("NTC_APERT_SCE_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("NTC_APERT_SF_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas<-colnames(info)
  columnas_SCE<-grep(paste0("NTC_APERT_.*(SCE).*_",i,"$"), columnas, value = TRUE)
  columnas_SF<-grep(paste0("NTC_APERT_.*(SF).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("NTC_APERT_SCE_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SCE]
  info[,paste0("NTC_APERT_SF_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SF]
}
colnames(info)



#i<-"3M"
columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("NTC_CASTIGO_.*(TC).*_",i,"$"), columnas, value = TRUE)
columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("NTC_CASTIGO_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)



#FALTAN ALGUNOS NTC

temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("PROM_CAS_.*(TC).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("PROM_CAS_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_CAS_SCE_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("PROM_CAS_SF_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("PROM_CAS_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("PROM_CAS_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_CAS_SCE_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("PROM_CAS_SF_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas<-colnames(info)
  columnas_SCE<-grep(paste0("PROM_CAS_.*(SCE).*_",i,"$"), columnas, value = TRUE)
  columnas_SF<-grep(paste0("PROM_CAS_.*(SF).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_CAS_SCE_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SCE]
  info[,paste0("PROM_CAS_SF_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SF]
}
print(colnames(info),max=100000)

temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("PROM_DEM_.*(TC).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("PROM_DEM_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_DEM_SCE_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("PROM_DEM_SF_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("PROM_DEM_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("PROM_DEM_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_DEM_SCE_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("PROM_DEM_SF_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas<-colnames(info)
  columnas_SCE<-grep(paste0("PROM_DEM_.*(SCE).*_",i,"$"), columnas, value = TRUE)
  columnas_SF<-grep(paste0("PROM_DEM_.*(SF).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_DEM_SCE_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SCE]
  info[,paste0("PROM_DEM_SF_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SF]
}
print(colnames(info),max=100000)


temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("PROM_NDI_.*(TC).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("PROM_NDI_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_NDI_SCE_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("PROM_NDI_SF_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("PROM_NDI_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("PROM_NDI_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_NDI_SCE_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("PROM_NDI_SF_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas<-colnames(info)
  columnas_SCE<-grep(paste0("PROM_NDI_.*(SCE).*_",i,"$"), columnas, value = TRUE)
  columnas_SF<-grep(paste0("PROM_NDI_.*(SF).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_NDI_SCE_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SCE]
  info[,paste0("PROM_NDI_SF_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SF]
}
print(colnames(info),max=100000)

temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("PROM_VEN_.*(TC).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("PROM_VEN_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_VEN_SCE_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("PROM_VEN_SF_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("PROM_VEN_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("PROM_VEN_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_VEN_SCE_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("PROM_VEN_SF_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas<-colnames(info)
  columnas_SCE<-grep(paste0("PROM_VEN_.*(SCE).*_",i,"$"), columnas, value = TRUE)
  columnas_SF<-grep(paste0("PROM_VEN_.*(SF).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_VEN_SCE_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SCE]
  info[,paste0("PROM_VEN_SF_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SF]
}
print(colnames(info),max=100000)


temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("PROM_XVEN_.*(TC).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("PROM_XVEN_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_XVEN_SCE_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("PROM_XVEN_SF_TC_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("PROM_XVEN_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("PROM_XVEN_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_XVEN_SCE_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("PROM_XVEN_SF_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas<-colnames(info)
  columnas_SCE<-grep(paste0("PROM_XVEN_.*(SCE).*_",i,"$"), columnas, value = TRUE)
  columnas_SF<-grep(paste0("PROM_XVEN_.*(SF).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("PROM_XVEN_SCE_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SCE]
  info[,paste0("PROM_XVEN_SF_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_SF]
}
print(colnames(info),max=100000)


#REVISAR O PREGUNTAR VAL_REFINAN_tc_12M
#CONTINUO CON LOS MAX
temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("MAX_DVEN_.*(TC).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("MAX_DVEN_.*(SBS|SC).*(TC).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("MAX_DVEN_SCE_TC_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("MAX_DVEN_SF_TC_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("MAX_DVEN_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("MAX_DVEN_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("MAX_DVEN_SCE_OP_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("MAX_DVEN_SF_OP_",i)  := do.call(pmax, .SD), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
  columnas<-colnames(info)
  columnas_SCE<-grep(paste0("MAX_DVEN_.*(SCE).*_",i,"$"), columnas, value = TRUE)
  columnas_SF<-grep(paste0("MAX_DVEN_.*(SF).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("MAX_DVEN_SCE_",i)  := do.call(pmax, .SD), .SDcols = columnas_SCE]
  info[,paste0("MAX_DVEN_SF_",i)  := do.call(pmax, .SD), .SDcols = columnas_SF]
}
print(colnames(info),max=100000)

#REVISAR NOPE_REFIN

temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_DEUDA_TOTAL_TC_SCE <- grep(paste0("NOPE_APERT_.*(OP).*_",i,"$"), columnas, value = TRUE)
  columnas_DEUDA_TOTAL_TC_SF <- grep(paste0("NOPE_APERT_.*(SBS|SC).*(OP).*_",i,"$"), columnas, value = TRUE)
  info[,paste0("NOPE_APERT_SCE_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SCE]
  info[,paste0("NOPE_APERT_SF_OP_",i)  := rowSums(.SD, na.rm = TRUE), .SDcols = columnas_DEUDA_TOTAL_TC_SF]
}
colnames(info)
dim(info)




  data <- data.table(info)
  # Ratios de Variacion OP y TC ----------------------------------------------------
  data[, r_NOPE_REFIN_OP_3s6M := ifelse(NOPE_REFIN_OP_6M > 0, NOPE_REFIN_OP_3M/NOPE_REFIN_OP_6M, 0), ]
  data[, r_NOPE_XVEN_OP_3s6M := ifelse(NOPE_XVEN_OP_6M > 0, NOPE_XVEN_OP_3M/NOPE_XVEN_OP_6M, 0), ]
  data[, r_NOPE_VENC_OP_3s6M := ifelse(NOPE_VENC_OP_6M > 0, NOPE_VENC_OP_3M/NOPE_VENC_OP_6M, 0), ]
  data[, r_NOPE_NDI_OP_3s6M := ifelse(NOPE_NDI_OP_6M > 0, NOPE_NDI_OP_3M/NOPE_NDI_OP_6M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_3s6M := ifelse(NOPE_VENC_1A30_OP_6M > 0, NOPE_VENC_1A30_OP_3M/NOPE_VENC_1A30_OP_6M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_3s6M := ifelse(NOPE_VENC_31A90_OP_6M > 0, NOPE_VENC_31A90_OP_3M/NOPE_VENC_31A90_OP_6M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_3s6M := ifelse(NOPE_VENC_91A180_OP_6M > 0, NOPE_VENC_91A180_OP_3M/NOPE_VENC_91A180_OP_6M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_3s6M := ifelse(NOPE_VENC_181A360_OP_6M > 0, NOPE_VENC_181A360_OP_3M/NOPE_VENC_181A360_OP_6M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_3s6M := ifelse(NOPE_VENC_MAYOR360_OP_6M > 0, NOPE_VENC_MAYOR360_OP_3M/NOPE_VENC_MAYOR360_OP_6M, 0), ]
  data[, r_NOPE_DEMANDA_OP_3s6M := ifelse(NOPE_DEMANDA_OP_6M > 0, NOPE_DEMANDA_OP_3M/NOPE_DEMANDA_OP_6M, 0), ]
  data[, r_NOPE_CASTIGO_OP_3s6M := ifelse(NOPE_CASTIGO_OP_6M > 0, NOPE_CASTIGO_OP_3M/NOPE_CASTIGO_OP_6M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_3s6M := ifelse(NOPE_APERT_SBS_OP_6M > 0, NOPE_APERT_SBS_OP_3M/NOPE_APERT_SBS_OP_6M, 0), ]
  data[, r_NOPE_APERT_SC_OP_3s6M := ifelse(NOPE_APERT_SC_OP_6M > 0, NOPE_APERT_SC_OP_3M/NOPE_APERT_SC_OP_6M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_3s6M := ifelse(NOPE_APERT_SICOM_OP_6M > 0, NOPE_APERT_SICOM_OP_3M/NOPE_APERT_SICOM_OP_6M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_3s6M := ifelse(NOPE_APERT_OTROS_OP_6M > 0, NOPE_APERT_OTROS_OP_3M/NOPE_APERT_OTROS_OP_6M, 0), ]
  data[, r_MVALVEN_SBS_OP_3s6M := ifelse(MVALVEN_SBS_OP_6M > 0, MVALVEN_SBS_OP_3M/MVALVEN_SBS_OP_6M, 0), ]
  data[, r_MVALVEN_SC_OP_3s6M := ifelse(MVALVEN_SC_OP_6M > 0, MVALVEN_SC_OP_3M/MVALVEN_SC_OP_6M, 0), ]
  data[, r_MVALVEN_SICOM_OP_3s6M := ifelse(MVALVEN_SICOM_OP_6M > 0, MVALVEN_SICOM_OP_3M/MVALVEN_SICOM_OP_6M, 0), ]
  data[, r_MVALVEN_OTROS_OP_3s6M := ifelse(MVALVEN_OTROS_OP_6M > 0, MVALVEN_OTROS_OP_3M/MVALVEN_OTROS_OP_6M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_OP_3s6M := ifelse(DEUDA_TOTAL_SBS_OP_6M > 0, DEUDA_TOTAL_SBS_OP_3M/DEUDA_TOTAL_SBS_OP_6M, 0), ]
  data[, r_DEUDA_TOTAL_SC_OP_3s6M := ifelse(DEUDA_TOTAL_SC_OP_6M > 0, DEUDA_TOTAL_SC_OP_3M/DEUDA_TOTAL_SC_OP_6M, 0), ]
  data[, r_DEUDA_TOTAL_SICOM_OP_3s6M := ifelse(DEUDA_TOTAL_SICOM_OP_6M > 0, DEUDA_TOTAL_SICOM_OP_3M/DEUDA_TOTAL_SICOM_OP_6M, 0), ]
  data[, r_DEUDA_TOTAL_OTROS_OP_3s6M := ifelse(DEUDA_TOTAL_OTROS_OP_6M > 0, DEUDA_TOTAL_OTROS_OP_3M/DEUDA_TOTAL_OTROS_OP_6M, 0), ]
  data[, r_NENT_VEN_SBS_OP_3s6M := ifelse(NENT_VEN_SBS_OP_6M > 0, NENT_VEN_SBS_OP_3M/NENT_VEN_SBS_OP_6M, 0), ]
  data[, r_NENT_VEN_SC_OP_3s6M := ifelse(NENT_VEN_SC_OP_6M > 0, NENT_VEN_SC_OP_3M/NENT_VEN_SC_OP_6M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_3s6M := ifelse(NENT_VEN_SICOM_OP_6M > 0, NENT_VEN_SICOM_OP_3M/NENT_VEN_SICOM_OP_6M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_3s6M := ifelse(NENT_VEN_OTROS_OP_6M > 0, NENT_VEN_OTROS_OP_3M/NENT_VEN_OTROS_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_3s6M := ifelse(PROM_MAX_DVEN_N_OP_6M > 0, PROM_MAX_DVEN_N_OP_3M/PROM_MAX_DVEN_N_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_3s6M := ifelse(PROM_MAX_DVEN_M_OP_6M > 0, PROM_MAX_DVEN_M_OP_3M/PROM_MAX_DVEN_M_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_3s6M := ifelse(PROM_MAX_DVEN_C_OP_6M > 0, PROM_MAX_DVEN_C_OP_3M/PROM_MAX_DVEN_C_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_3s6M := ifelse(PROM_MAX_DVEN_V_OP_6M > 0, PROM_MAX_DVEN_V_OP_3M/PROM_MAX_DVEN_V_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_3s6M := ifelse(PROM_MAX_DVEN_P_OP_6M > 0, PROM_MAX_DVEN_P_OP_3M/PROM_MAX_DVEN_P_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_3s6M := ifelse(PROM_MAX_DVEN_OTROS_OP_6M > 0, PROM_MAX_DVEN_OTROS_OP_3M/PROM_MAX_DVEN_OTROS_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_3s6M := ifelse(PROM_MAX_DVEN_SBS_OP_6M > 0, PROM_MAX_DVEN_SBS_OP_3M/PROM_MAX_DVEN_SBS_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_3s6M := ifelse(PROM_MAX_DVEN_SC_OP_6M > 0, PROM_MAX_DVEN_SC_OP_3M/PROM_MAX_DVEN_SC_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_3s6M := ifelse(PROM_MAX_DVEN_SICOM_OP_6M > 0, PROM_MAX_DVEN_SICOM_OP_3M/PROM_MAX_DVEN_SICOM_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_3s6M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_6M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_3M/PROM_MAX_DVEN_OTROS_SIS_OP_6M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_3s6M := ifelse(PROM_XVEN_SBS_OP_6M > 0, PROM_XVEN_SBS_OP_3M/PROM_XVEN_SBS_OP_6M, 0), ]
  data[, r_PROM_NDI_SBS_OP_3s6M := ifelse(PROM_NDI_SBS_OP_6M > 0, PROM_NDI_SBS_OP_3M/PROM_NDI_SBS_OP_6M, 0), ]
  data[, r_PROM_VEN_SBS_OP_3s6M := ifelse(PROM_VEN_SBS_OP_6M > 0, PROM_VEN_SBS_OP_3M/PROM_VEN_SBS_OP_6M, 0), ]
  data[, r_PROM_DEM_SBS_OP_3s6M := ifelse(PROM_DEM_SBS_OP_6M > 0, PROM_DEM_SBS_OP_3M/PROM_DEM_SBS_OP_6M, 0), ]
  data[, r_PROM_CAS_SBS_OP_3s6M := ifelse(PROM_CAS_SBS_OP_6M > 0, PROM_CAS_SBS_OP_3M/PROM_CAS_SBS_OP_6M, 0), ]
  data[, r_PROM_XVEN_SC_OP_3s6M := ifelse(PROM_XVEN_SC_OP_6M > 0, PROM_XVEN_SC_OP_3M/PROM_XVEN_SC_OP_6M, 0), ]
  data[, r_PROM_NDI_SC_OP_3s6M := ifelse(PROM_NDI_SC_OP_6M > 0, PROM_NDI_SC_OP_3M/PROM_NDI_SC_OP_6M, 0), ]
  data[, r_PROM_VEN_SC_OP_3s6M := ifelse(PROM_VEN_SC_OP_6M > 0, PROM_VEN_SC_OP_3M/PROM_VEN_SC_OP_6M, 0), ]
  data[, r_PROM_DEM_SC_OP_3s6M := ifelse(PROM_DEM_SC_OP_6M > 0, PROM_DEM_SC_OP_3M/PROM_DEM_SC_OP_6M, 0), ]
  data[, r_PROM_CAS_SC_OP_3s6M := ifelse(PROM_CAS_SC_OP_6M > 0, PROM_CAS_SC_OP_3M/PROM_CAS_SC_OP_6M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_3s6M := ifelse(PROM_XVEN_SICOM_OP_6M > 0, PROM_XVEN_SICOM_OP_3M/PROM_XVEN_SICOM_OP_6M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_3s6M := ifelse(PROM_NDI_SICOM_OP_6M > 0, PROM_NDI_SICOM_OP_3M/PROM_NDI_SICOM_OP_6M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_3s6M := ifelse(PROM_VEN_SICOM_OP_6M > 0, PROM_VEN_SICOM_OP_3M/PROM_VEN_SICOM_OP_6M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_3s6M := ifelse(PROM_DEM_SICOM_OP_6M > 0, PROM_DEM_SICOM_OP_3M/PROM_DEM_SICOM_OP_6M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_3s6M := ifelse(PROM_CAS_SICOM_OP_6M > 0, PROM_CAS_SICOM_OP_3M/PROM_CAS_SICOM_OP_6M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_3s6M := ifelse(PROM_XVEN_OTROS_OP_6M > 0, PROM_XVEN_OTROS_OP_3M/PROM_XVEN_OTROS_OP_6M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_3s6M := ifelse(PROM_NDI_OTROS_OP_6M > 0, PROM_NDI_OTROS_OP_3M/PROM_NDI_OTROS_OP_6M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_3s6M := ifelse(PROM_VEN_OTROS_OP_6M > 0, PROM_VEN_OTROS_OP_3M/PROM_VEN_OTROS_OP_6M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_3s6M := ifelse(PROM_DEM_OTROS_OP_6M > 0, PROM_DEM_OTROS_OP_3M/PROM_DEM_OTROS_OP_6M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_3s6M := ifelse(PROM_CAS_OTROS_OP_6M > 0, PROM_CAS_OTROS_OP_3M/PROM_CAS_OTROS_OP_6M, 0), ]
  
  data[, r_NOPE_REFIN_OP_3s12M := ifelse(NOPE_REFIN_OP_12M > 0, NOPE_REFIN_OP_3M/NOPE_REFIN_OP_12M, 0), ]
  data[, r_NOPE_XVEN_OP_3s12M := ifelse(NOPE_XVEN_OP_12M > 0, NOPE_XVEN_OP_3M/NOPE_XVEN_OP_12M, 0), ]
  data[, r_NOPE_VENC_OP_3s12M := ifelse(NOPE_VENC_OP_12M > 0, NOPE_VENC_OP_3M/NOPE_VENC_OP_12M, 0), ]
  data[, r_NOPE_NDI_OP_3s12M := ifelse(NOPE_NDI_OP_12M > 0, NOPE_NDI_OP_3M/NOPE_NDI_OP_12M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_3s12M := ifelse(NOPE_VENC_1A30_OP_12M > 0, NOPE_VENC_1A30_OP_3M/NOPE_VENC_1A30_OP_12M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_3s12M := ifelse(NOPE_VENC_31A90_OP_12M > 0, NOPE_VENC_31A90_OP_3M/NOPE_VENC_31A90_OP_12M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_3s12M := ifelse(NOPE_VENC_91A180_OP_12M > 0, NOPE_VENC_91A180_OP_3M/NOPE_VENC_91A180_OP_12M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_3s12M := ifelse(NOPE_VENC_181A360_OP_12M > 0, NOPE_VENC_181A360_OP_3M/NOPE_VENC_181A360_OP_12M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_3s12M := ifelse(NOPE_VENC_MAYOR360_OP_12M > 0, NOPE_VENC_MAYOR360_OP_3M/NOPE_VENC_MAYOR360_OP_12M, 0), ]
  data[, r_NOPE_DEMANDA_OP_3s12M := ifelse(NOPE_DEMANDA_OP_12M > 0, NOPE_DEMANDA_OP_3M/NOPE_DEMANDA_OP_12M, 0), ]
  data[, r_NOPE_CASTIGO_OP_3s12M := ifelse(NOPE_CASTIGO_OP_12M > 0, NOPE_CASTIGO_OP_3M/NOPE_CASTIGO_OP_12M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_3s12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, NOPE_APERT_SBS_OP_3M/NOPE_APERT_SBS_OP_12M, 0), ]
  data[, r_NOPE_APERT_SC_OP_3s12M := ifelse(NOPE_APERT_SC_OP_12M > 0, NOPE_APERT_SC_OP_3M/NOPE_APERT_SC_OP_12M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_3s12M := ifelse(NOPE_APERT_SICOM_OP_12M > 0, NOPE_APERT_SICOM_OP_3M/NOPE_APERT_SICOM_OP_12M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_3s12M := ifelse(NOPE_APERT_OTROS_OP_12M > 0, NOPE_APERT_OTROS_OP_3M/NOPE_APERT_OTROS_OP_12M, 0), ]
  data[, r_MVALVEN_SBS_OP_3s12M := ifelse(MVALVEN_SBS_OP_12M > 0, MVALVEN_SBS_OP_3M/MVALVEN_SBS_OP_12M, 0), ]
  data[, r_MVALVEN_SC_OP_3s12M := ifelse(MVALVEN_SC_OP_12M > 0, MVALVEN_SC_OP_3M/MVALVEN_SC_OP_12M, 0), ]
  data[, r_MVALVEN_SICOM_OP_3s12M := ifelse(MVALVEN_SICOM_OP_12M > 0, MVALVEN_SICOM_OP_3M/MVALVEN_SICOM_OP_12M, 0), ]
  data[, r_MVALVEN_OTROS_OP_3s12M := ifelse(MVALVEN_OTROS_OP_12M > 0, MVALVEN_OTROS_OP_3M/MVALVEN_OTROS_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_OP_3s12M := ifelse(DEUDA_TOTAL_SBS_OP_12M > 0, DEUDA_TOTAL_SBS_OP_3M/DEUDA_TOTAL_SBS_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_SC_OP_3s12M := ifelse(DEUDA_TOTAL_SC_OP_12M > 0, DEUDA_TOTAL_SC_OP_3M/DEUDA_TOTAL_SC_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_SICOM_OP_3s12M := ifelse(DEUDA_TOTAL_SICOM_OP_12M > 0, DEUDA_TOTAL_SICOM_OP_3M/DEUDA_TOTAL_SICOM_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_OTROS_OP_3s12M := ifelse(DEUDA_TOTAL_OTROS_OP_12M > 0, DEUDA_TOTAL_OTROS_OP_3M/DEUDA_TOTAL_OTROS_OP_12M, 0), ]
  data[, r_NENT_VEN_SBS_OP_3s12M := ifelse(NENT_VEN_SBS_OP_12M > 0, NENT_VEN_SBS_OP_3M/NENT_VEN_SBS_OP_12M, 0), ]
  data[, r_NENT_VEN_SC_OP_3s12M := ifelse(NENT_VEN_SC_OP_12M > 0, NENT_VEN_SC_OP_3M/NENT_VEN_SC_OP_12M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_3s12M := ifelse(NENT_VEN_SICOM_OP_12M > 0, NENT_VEN_SICOM_OP_3M/NENT_VEN_SICOM_OP_12M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_3s12M := ifelse(NENT_VEN_OTROS_OP_12M > 0, NENT_VEN_OTROS_OP_3M/NENT_VEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_3s12M := ifelse(PROM_MAX_DVEN_N_OP_12M > 0, PROM_MAX_DVEN_N_OP_3M/PROM_MAX_DVEN_N_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_3s12M := ifelse(PROM_MAX_DVEN_M_OP_12M > 0, PROM_MAX_DVEN_M_OP_3M/PROM_MAX_DVEN_M_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_3s12M := ifelse(PROM_MAX_DVEN_C_OP_12M > 0, PROM_MAX_DVEN_C_OP_3M/PROM_MAX_DVEN_C_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_3s12M := ifelse(PROM_MAX_DVEN_V_OP_12M > 0, PROM_MAX_DVEN_V_OP_3M/PROM_MAX_DVEN_V_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_3s12M := ifelse(PROM_MAX_DVEN_P_OP_12M > 0, PROM_MAX_DVEN_P_OP_3M/PROM_MAX_DVEN_P_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_3s12M := ifelse(PROM_MAX_DVEN_OTROS_OP_12M > 0, PROM_MAX_DVEN_OTROS_OP_3M/PROM_MAX_DVEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_3s12M := ifelse(PROM_MAX_DVEN_SBS_OP_12M > 0, PROM_MAX_DVEN_SBS_OP_3M/PROM_MAX_DVEN_SBS_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_3s12M := ifelse(PROM_MAX_DVEN_SC_OP_12M > 0, PROM_MAX_DVEN_SC_OP_3M/PROM_MAX_DVEN_SC_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_3s12M := ifelse(PROM_MAX_DVEN_SICOM_OP_12M > 0, PROM_MAX_DVEN_SICOM_OP_3M/PROM_MAX_DVEN_SICOM_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_3s12M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_12M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_3M/PROM_MAX_DVEN_OTROS_SIS_OP_12M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_3s12M := ifelse(PROM_XVEN_SBS_OP_12M > 0, PROM_XVEN_SBS_OP_3M/PROM_XVEN_SBS_OP_12M, 0), ]
  data[, r_PROM_NDI_SBS_OP_3s12M := ifelse(PROM_NDI_SBS_OP_12M > 0, PROM_NDI_SBS_OP_3M/PROM_NDI_SBS_OP_12M, 0), ]
  data[, r_PROM_VEN_SBS_OP_3s12M := ifelse(PROM_VEN_SBS_OP_12M > 0, PROM_VEN_SBS_OP_3M/PROM_VEN_SBS_OP_12M, 0), ]
  data[, r_PROM_DEM_SBS_OP_3s12M := ifelse(PROM_DEM_SBS_OP_12M > 0, PROM_DEM_SBS_OP_3M/PROM_DEM_SBS_OP_12M, 0), ]
  data[, r_PROM_CAS_SBS_OP_3s12M := ifelse(PROM_CAS_SBS_OP_12M > 0, PROM_CAS_SBS_OP_3M/PROM_CAS_SBS_OP_12M, 0), ]
  data[, r_PROM_XVEN_SC_OP_3s12M := ifelse(PROM_XVEN_SC_OP_12M > 0, PROM_XVEN_SC_OP_3M/PROM_XVEN_SC_OP_12M, 0), ]
  data[, r_PROM_NDI_SC_OP_3s12M := ifelse(PROM_NDI_SC_OP_12M > 0, PROM_NDI_SC_OP_3M/PROM_NDI_SC_OP_12M, 0), ]
  data[, r_PROM_VEN_SC_OP_3s12M := ifelse(PROM_VEN_SC_OP_12M > 0, PROM_VEN_SC_OP_3M/PROM_VEN_SC_OP_12M, 0), ]
  data[, r_PROM_DEM_SC_OP_3s12M := ifelse(PROM_DEM_SC_OP_12M > 0, PROM_DEM_SC_OP_3M/PROM_DEM_SC_OP_12M, 0), ]
  data[, r_PROM_CAS_SC_OP_3s12M := ifelse(PROM_CAS_SC_OP_12M > 0, PROM_CAS_SC_OP_3M/PROM_CAS_SC_OP_12M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_3s12M := ifelse(PROM_XVEN_SICOM_OP_12M > 0, PROM_XVEN_SICOM_OP_3M/PROM_XVEN_SICOM_OP_12M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_3s12M := ifelse(PROM_NDI_SICOM_OP_12M > 0, PROM_NDI_SICOM_OP_3M/PROM_NDI_SICOM_OP_12M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_3s12M := ifelse(PROM_VEN_SICOM_OP_12M > 0, PROM_VEN_SICOM_OP_3M/PROM_VEN_SICOM_OP_12M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_3s12M := ifelse(PROM_DEM_SICOM_OP_12M > 0, PROM_DEM_SICOM_OP_3M/PROM_DEM_SICOM_OP_12M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_3s12M := ifelse(PROM_CAS_SICOM_OP_12M > 0, PROM_CAS_SICOM_OP_3M/PROM_CAS_SICOM_OP_12M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_3s12M := ifelse(PROM_XVEN_OTROS_OP_12M > 0, PROM_XVEN_OTROS_OP_3M/PROM_XVEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_3s12M := ifelse(PROM_NDI_OTROS_OP_12M > 0, PROM_NDI_OTROS_OP_3M/PROM_NDI_OTROS_OP_12M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_3s12M := ifelse(PROM_VEN_OTROS_OP_12M > 0, PROM_VEN_OTROS_OP_3M/PROM_VEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_3s12M := ifelse(PROM_DEM_OTROS_OP_12M > 0, PROM_DEM_OTROS_OP_3M/PROM_DEM_OTROS_OP_12M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_3s12M := ifelse(PROM_CAS_OTROS_OP_12M > 0, PROM_CAS_OTROS_OP_3M/PROM_CAS_OTROS_OP_12M, 0), ]
  
  data[, r_NOPE_REFIN_OP_6s12M := ifelse(NOPE_REFIN_OP_12M > 0, NOPE_REFIN_OP_6M/NOPE_REFIN_OP_12M, 0), ]
  data[, r_NOPE_XVEN_OP_6s12M := ifelse(NOPE_XVEN_OP_12M > 0, NOPE_XVEN_OP_6M/NOPE_XVEN_OP_12M, 0), ]
  data[, r_NOPE_VENC_OP_6s12M := ifelse(NOPE_VENC_OP_12M > 0, NOPE_VENC_OP_6M/NOPE_VENC_OP_12M, 0), ]
  data[, r_NOPE_NDI_OP_6s12M := ifelse(NOPE_NDI_OP_12M > 0, NOPE_NDI_OP_6M/NOPE_NDI_OP_12M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_6s12M := ifelse(NOPE_VENC_1A30_OP_12M > 0, NOPE_VENC_1A30_OP_6M/NOPE_VENC_1A30_OP_12M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_6s12M := ifelse(NOPE_VENC_31A90_OP_12M > 0, NOPE_VENC_31A90_OP_6M/NOPE_VENC_31A90_OP_12M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_6s12M := ifelse(NOPE_VENC_91A180_OP_12M > 0, NOPE_VENC_91A180_OP_6M/NOPE_VENC_91A180_OP_12M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_6s12M := ifelse(NOPE_VENC_181A360_OP_12M > 0, NOPE_VENC_181A360_OP_6M/NOPE_VENC_181A360_OP_12M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_6s12M := ifelse(NOPE_VENC_MAYOR360_OP_12M > 0, NOPE_VENC_MAYOR360_OP_6M/NOPE_VENC_MAYOR360_OP_12M, 0), ]
  data[, r_NOPE_DEMANDA_OP_6s12M := ifelse(NOPE_DEMANDA_OP_12M > 0, NOPE_DEMANDA_OP_6M/NOPE_DEMANDA_OP_12M, 0), ]
  data[, r_NOPE_CASTIGO_OP_6s12M := ifelse(NOPE_CASTIGO_OP_12M > 0, NOPE_CASTIGO_OP_6M/NOPE_CASTIGO_OP_12M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_6s12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, NOPE_APERT_SBS_OP_6M/NOPE_APERT_SBS_OP_12M, 0), ]
  data[, r_NOPE_APERT_SC_OP_6s12M := ifelse(NOPE_APERT_SC_OP_12M > 0, NOPE_APERT_SC_OP_6M/NOPE_APERT_SC_OP_12M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_6s12M := ifelse(NOPE_APERT_SICOM_OP_12M > 0, NOPE_APERT_SICOM_OP_6M/NOPE_APERT_SICOM_OP_12M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_6s12M := ifelse(NOPE_APERT_OTROS_OP_12M > 0, NOPE_APERT_OTROS_OP_6M/NOPE_APERT_OTROS_OP_12M, 0), ]
  data[, r_MVALVEN_SBS_OP_6s12M := ifelse(MVALVEN_SBS_OP_12M > 0, MVALVEN_SBS_OP_6M/MVALVEN_SBS_OP_12M, 0), ]
  data[, r_MVALVEN_SC_OP_6s12M := ifelse(MVALVEN_SC_OP_12M > 0, MVALVEN_SC_OP_6M/MVALVEN_SC_OP_12M, 0), ]
  data[, r_MVALVEN_SICOM_OP_6s12M := ifelse(MVALVEN_SICOM_OP_12M > 0, MVALVEN_SICOM_OP_6M/MVALVEN_SICOM_OP_12M, 0), ]
  data[, r_MVALVEN_OTROS_OP_6s12M := ifelse(MVALVEN_OTROS_OP_12M > 0, MVALVEN_OTROS_OP_6M/MVALVEN_OTROS_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_OP_6s12M := ifelse(DEUDA_TOTAL_SBS_OP_12M > 0, DEUDA_TOTAL_SBS_OP_6M/DEUDA_TOTAL_SBS_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_SC_OP_6s12M := ifelse(DEUDA_TOTAL_SC_OP_12M > 0, DEUDA_TOTAL_SC_OP_6M/DEUDA_TOTAL_SC_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_SICOM_OP_6s12M := ifelse(DEUDA_TOTAL_SICOM_OP_12M > 0, DEUDA_TOTAL_SICOM_OP_6M/DEUDA_TOTAL_SICOM_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_OTROS_OP_6s12M := ifelse(DEUDA_TOTAL_OTROS_OP_12M > 0, DEUDA_TOTAL_OTROS_OP_6M/DEUDA_TOTAL_OTROS_OP_12M, 0), ]
  data[, r_NENT_VEN_SBS_OP_6s12M := ifelse(NENT_VEN_SBS_OP_12M > 0, NENT_VEN_SBS_OP_6M/NENT_VEN_SBS_OP_12M, 0), ]
  data[, r_NENT_VEN_SC_OP_6s12M := ifelse(NENT_VEN_SC_OP_12M > 0, NENT_VEN_SC_OP_6M/NENT_VEN_SC_OP_12M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_6s12M := ifelse(NENT_VEN_SICOM_OP_12M > 0, NENT_VEN_SICOM_OP_6M/NENT_VEN_SICOM_OP_12M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_6s12M := ifelse(NENT_VEN_OTROS_OP_12M > 0, NENT_VEN_OTROS_OP_6M/NENT_VEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_6s12M := ifelse(PROM_MAX_DVEN_N_OP_12M > 0, PROM_MAX_DVEN_N_OP_6M/PROM_MAX_DVEN_N_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_6s12M := ifelse(PROM_MAX_DVEN_M_OP_12M > 0, PROM_MAX_DVEN_M_OP_6M/PROM_MAX_DVEN_M_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_6s12M := ifelse(PROM_MAX_DVEN_C_OP_12M > 0, PROM_MAX_DVEN_C_OP_6M/PROM_MAX_DVEN_C_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_6s12M := ifelse(PROM_MAX_DVEN_V_OP_12M > 0, PROM_MAX_DVEN_V_OP_6M/PROM_MAX_DVEN_V_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_6s12M := ifelse(PROM_MAX_DVEN_P_OP_12M > 0, PROM_MAX_DVEN_P_OP_6M/PROM_MAX_DVEN_P_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_6s12M := ifelse(PROM_MAX_DVEN_OTROS_OP_12M > 0, PROM_MAX_DVEN_OTROS_OP_6M/PROM_MAX_DVEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_6s12M := ifelse(PROM_MAX_DVEN_SBS_OP_12M > 0, PROM_MAX_DVEN_SBS_OP_6M/PROM_MAX_DVEN_SBS_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_6s12M := ifelse(PROM_MAX_DVEN_SC_OP_12M > 0, PROM_MAX_DVEN_SC_OP_6M/PROM_MAX_DVEN_SC_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_6s12M := ifelse(PROM_MAX_DVEN_SICOM_OP_12M > 0, PROM_MAX_DVEN_SICOM_OP_6M/PROM_MAX_DVEN_SICOM_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_6s12M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_12M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_6M/PROM_MAX_DVEN_OTROS_SIS_OP_12M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_6s12M := ifelse(PROM_XVEN_SBS_OP_12M > 0, PROM_XVEN_SBS_OP_6M/PROM_XVEN_SBS_OP_12M, 0), ]
  data[, r_PROM_NDI_SBS_OP_6s12M := ifelse(PROM_NDI_SBS_OP_12M > 0, PROM_NDI_SBS_OP_6M/PROM_NDI_SBS_OP_12M, 0), ]
  data[, r_PROM_VEN_SBS_OP_6s12M := ifelse(PROM_VEN_SBS_OP_12M > 0, PROM_VEN_SBS_OP_6M/PROM_VEN_SBS_OP_12M, 0), ]
  data[, r_PROM_DEM_SBS_OP_6s12M := ifelse(PROM_DEM_SBS_OP_12M > 0, PROM_DEM_SBS_OP_6M/PROM_DEM_SBS_OP_12M, 0), ]
  data[, r_PROM_CAS_SBS_OP_6s12M := ifelse(PROM_CAS_SBS_OP_12M > 0, PROM_CAS_SBS_OP_6M/PROM_CAS_SBS_OP_12M, 0), ]
  data[, r_PROM_XVEN_SC_OP_6s12M := ifelse(PROM_XVEN_SC_OP_12M > 0, PROM_XVEN_SC_OP_6M/PROM_XVEN_SC_OP_12M, 0), ]
  data[, r_PROM_NDI_SC_OP_6s12M := ifelse(PROM_NDI_SC_OP_12M > 0, PROM_NDI_SC_OP_6M/PROM_NDI_SC_OP_12M, 0), ]
  data[, r_PROM_VEN_SC_OP_6s12M := ifelse(PROM_VEN_SC_OP_12M > 0, PROM_VEN_SC_OP_6M/PROM_VEN_SC_OP_12M, 0), ]
  data[, r_PROM_DEM_SC_OP_6s12M := ifelse(PROM_DEM_SC_OP_12M > 0, PROM_DEM_SC_OP_6M/PROM_DEM_SC_OP_12M, 0), ]
  data[, r_PROM_CAS_SC_OP_6s12M := ifelse(PROM_CAS_SC_OP_12M > 0, PROM_CAS_SC_OP_6M/PROM_CAS_SC_OP_12M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_6s12M := ifelse(PROM_XVEN_SICOM_OP_12M > 0, PROM_XVEN_SICOM_OP_6M/PROM_XVEN_SICOM_OP_12M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_6s12M := ifelse(PROM_NDI_SICOM_OP_12M > 0, PROM_NDI_SICOM_OP_6M/PROM_NDI_SICOM_OP_12M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_6s12M := ifelse(PROM_VEN_SICOM_OP_12M > 0, PROM_VEN_SICOM_OP_6M/PROM_VEN_SICOM_OP_12M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_6s12M := ifelse(PROM_DEM_SICOM_OP_12M > 0, PROM_DEM_SICOM_OP_6M/PROM_DEM_SICOM_OP_12M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_6s12M := ifelse(PROM_CAS_SICOM_OP_12M > 0, PROM_CAS_SICOM_OP_6M/PROM_CAS_SICOM_OP_12M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_6s12M := ifelse(PROM_XVEN_OTROS_OP_12M > 0, PROM_XVEN_OTROS_OP_6M/PROM_XVEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_6s12M := ifelse(PROM_NDI_OTROS_OP_12M > 0, PROM_NDI_OTROS_OP_6M/PROM_NDI_OTROS_OP_12M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_6s12M := ifelse(PROM_VEN_OTROS_OP_12M > 0, PROM_VEN_OTROS_OP_6M/PROM_VEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_6s12M := ifelse(PROM_DEM_OTROS_OP_12M > 0, PROM_DEM_OTROS_OP_6M/PROM_DEM_OTROS_OP_12M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_6s12M := ifelse(PROM_CAS_OTROS_OP_12M > 0, PROM_CAS_OTROS_OP_6M/PROM_CAS_OTROS_OP_12M, 0), ]
  
  data[, r_NOPE_REFIN_OP_6s24M := ifelse(NOPE_REFIN_OP_24M > 0, NOPE_REFIN_OP_6M/NOPE_REFIN_OP_24M, 0), ]
  data[, r_NOPE_XVEN_OP_6s24M := ifelse(NOPE_XVEN_OP_24M > 0, NOPE_XVEN_OP_6M/NOPE_XVEN_OP_24M, 0), ]
  data[, r_NOPE_VENC_OP_6s24M := ifelse(NOPE_VENC_OP_24M > 0, NOPE_VENC_OP_6M/NOPE_VENC_OP_24M, 0), ]
  data[, r_NOPE_NDI_OP_6s24M := ifelse(NOPE_NDI_OP_24M > 0, NOPE_NDI_OP_6M/NOPE_NDI_OP_24M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_6s24M := ifelse(NOPE_VENC_1A30_OP_24M > 0, NOPE_VENC_1A30_OP_6M/NOPE_VENC_1A30_OP_24M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_6s24M := ifelse(NOPE_VENC_31A90_OP_24M > 0, NOPE_VENC_31A90_OP_6M/NOPE_VENC_31A90_OP_24M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_6s24M := ifelse(NOPE_VENC_91A180_OP_24M > 0, NOPE_VENC_91A180_OP_6M/NOPE_VENC_91A180_OP_24M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_6s24M := ifelse(NOPE_VENC_181A360_OP_24M > 0, NOPE_VENC_181A360_OP_6M/NOPE_VENC_181A360_OP_24M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_6s24M := ifelse(NOPE_VENC_MAYOR360_OP_24M > 0, NOPE_VENC_MAYOR360_OP_6M/NOPE_VENC_MAYOR360_OP_24M, 0), ]
  data[, r_NOPE_DEMANDA_OP_6s24M := ifelse(NOPE_DEMANDA_OP_24M > 0, NOPE_DEMANDA_OP_6M/NOPE_DEMANDA_OP_24M, 0), ]
  data[, r_NOPE_CASTIGO_OP_6s24M := ifelse(NOPE_CASTIGO_OP_24M > 0, NOPE_CASTIGO_OP_6M/NOPE_CASTIGO_OP_24M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_6s24M := ifelse(NOPE_APERT_SBS_OP_24M > 0, NOPE_APERT_SBS_OP_6M/NOPE_APERT_SBS_OP_24M, 0), ]
  data[, r_NOPE_APERT_SC_OP_6s24M := ifelse(NOPE_APERT_SC_OP_24M > 0, NOPE_APERT_SC_OP_6M/NOPE_APERT_SC_OP_24M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_6s24M := ifelse(NOPE_APERT_SICOM_OP_24M > 0, NOPE_APERT_SICOM_OP_6M/NOPE_APERT_SICOM_OP_24M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_6s24M := ifelse(NOPE_APERT_OTROS_OP_24M > 0, NOPE_APERT_OTROS_OP_6M/NOPE_APERT_OTROS_OP_24M, 0), ]
  data[, r_MVALVEN_SBS_OP_6s24M := ifelse(MVALVEN_SBS_OP_24M > 0, MVALVEN_SBS_OP_6M/MVALVEN_SBS_OP_24M, 0), ]
  data[, r_MVALVEN_SC_OP_6s24M := ifelse(MVALVEN_SC_OP_24M > 0, MVALVEN_SC_OP_6M/MVALVEN_SC_OP_24M, 0), ]
  data[, r_MVALVEN_SICOM_OP_6s24M := ifelse(MVALVEN_SICOM_OP_24M > 0, MVALVEN_SICOM_OP_6M/MVALVEN_SICOM_OP_24M, 0), ]
  data[, r_MVALVEN_OTROS_OP_6s24M := ifelse(MVALVEN_OTROS_OP_24M > 0, MVALVEN_OTROS_OP_6M/MVALVEN_OTROS_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_OP_6s24M := ifelse(DEUDA_TOTAL_SBS_OP_24M > 0, DEUDA_TOTAL_SBS_OP_6M/DEUDA_TOTAL_SBS_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_SC_OP_6s24M := ifelse(DEUDA_TOTAL_SC_OP_24M > 0, DEUDA_TOTAL_SC_OP_6M/DEUDA_TOTAL_SC_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_SICOM_OP_6s24M := ifelse(DEUDA_TOTAL_SICOM_OP_24M > 0, DEUDA_TOTAL_SICOM_OP_6M/DEUDA_TOTAL_SICOM_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_OTROS_OP_6s24M := ifelse(DEUDA_TOTAL_OTROS_OP_24M > 0, DEUDA_TOTAL_OTROS_OP_6M/DEUDA_TOTAL_OTROS_OP_24M, 0), ]
  data[, r_NENT_VEN_SBS_OP_6s24M := ifelse(NENT_VEN_SBS_OP_24M > 0, NENT_VEN_SBS_OP_6M/NENT_VEN_SBS_OP_24M, 0), ]
  data[, r_NENT_VEN_SC_OP_6s24M := ifelse(NENT_VEN_SC_OP_24M > 0, NENT_VEN_SC_OP_6M/NENT_VEN_SC_OP_24M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_6s24M := ifelse(NENT_VEN_SICOM_OP_24M > 0, NENT_VEN_SICOM_OP_6M/NENT_VEN_SICOM_OP_24M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_6s24M := ifelse(NENT_VEN_OTROS_OP_24M > 0, NENT_VEN_OTROS_OP_6M/NENT_VEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_6s24M := ifelse(PROM_MAX_DVEN_N_OP_24M > 0, PROM_MAX_DVEN_N_OP_6M/PROM_MAX_DVEN_N_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_6s24M := ifelse(PROM_MAX_DVEN_M_OP_24M > 0, PROM_MAX_DVEN_M_OP_6M/PROM_MAX_DVEN_M_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_6s24M := ifelse(PROM_MAX_DVEN_C_OP_24M > 0, PROM_MAX_DVEN_C_OP_6M/PROM_MAX_DVEN_C_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_6s24M := ifelse(PROM_MAX_DVEN_V_OP_24M > 0, PROM_MAX_DVEN_V_OP_6M/PROM_MAX_DVEN_V_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_6s24M := ifelse(PROM_MAX_DVEN_P_OP_24M > 0, PROM_MAX_DVEN_P_OP_6M/PROM_MAX_DVEN_P_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_6s24M := ifelse(PROM_MAX_DVEN_OTROS_OP_24M > 0, PROM_MAX_DVEN_OTROS_OP_6M/PROM_MAX_DVEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_6s24M := ifelse(PROM_MAX_DVEN_SBS_OP_24M > 0, PROM_MAX_DVEN_SBS_OP_6M/PROM_MAX_DVEN_SBS_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_6s24M := ifelse(PROM_MAX_DVEN_SC_OP_24M > 0, PROM_MAX_DVEN_SC_OP_6M/PROM_MAX_DVEN_SC_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_6s24M := ifelse(PROM_MAX_DVEN_SICOM_OP_24M > 0, PROM_MAX_DVEN_SICOM_OP_6M/PROM_MAX_DVEN_SICOM_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_6s24M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_24M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_6M/PROM_MAX_DVEN_OTROS_SIS_OP_24M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_6s24M := ifelse(PROM_XVEN_SBS_OP_24M > 0, PROM_XVEN_SBS_OP_6M/PROM_XVEN_SBS_OP_24M, 0), ]
  data[, r_PROM_NDI_SBS_OP_6s24M := ifelse(PROM_NDI_SBS_OP_24M > 0, PROM_NDI_SBS_OP_6M/PROM_NDI_SBS_OP_24M, 0), ]
  data[, r_PROM_VEN_SBS_OP_6s24M := ifelse(PROM_VEN_SBS_OP_24M > 0, PROM_VEN_SBS_OP_6M/PROM_VEN_SBS_OP_24M, 0), ]
  data[, r_PROM_DEM_SBS_OP_6s24M := ifelse(PROM_DEM_SBS_OP_24M > 0, PROM_DEM_SBS_OP_6M/PROM_DEM_SBS_OP_24M, 0), ]
  data[, r_PROM_CAS_SBS_OP_6s24M := ifelse(PROM_CAS_SBS_OP_24M > 0, PROM_CAS_SBS_OP_6M/PROM_CAS_SBS_OP_24M, 0), ]
  data[, r_PROM_XVEN_SC_OP_6s24M := ifelse(PROM_XVEN_SC_OP_24M > 0, PROM_XVEN_SC_OP_6M/PROM_XVEN_SC_OP_24M, 0), ]
  data[, r_PROM_NDI_SC_OP_6s24M := ifelse(PROM_NDI_SC_OP_24M > 0, PROM_NDI_SC_OP_6M/PROM_NDI_SC_OP_24M, 0), ]
  data[, r_PROM_VEN_SC_OP_6s24M := ifelse(PROM_VEN_SC_OP_24M > 0, PROM_VEN_SC_OP_6M/PROM_VEN_SC_OP_24M, 0), ]
  data[, r_PROM_DEM_SC_OP_6s24M := ifelse(PROM_DEM_SC_OP_24M > 0, PROM_DEM_SC_OP_6M/PROM_DEM_SC_OP_24M, 0), ]
  data[, r_PROM_CAS_SC_OP_6s24M := ifelse(PROM_CAS_SC_OP_24M > 0, PROM_CAS_SC_OP_6M/PROM_CAS_SC_OP_24M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_6s24M := ifelse(PROM_XVEN_SICOM_OP_24M > 0, PROM_XVEN_SICOM_OP_6M/PROM_XVEN_SICOM_OP_24M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_6s24M := ifelse(PROM_NDI_SICOM_OP_24M > 0, PROM_NDI_SICOM_OP_6M/PROM_NDI_SICOM_OP_24M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_6s24M := ifelse(PROM_VEN_SICOM_OP_24M > 0, PROM_VEN_SICOM_OP_6M/PROM_VEN_SICOM_OP_24M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_6s24M := ifelse(PROM_DEM_SICOM_OP_24M > 0, PROM_DEM_SICOM_OP_6M/PROM_DEM_SICOM_OP_24M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_6s24M := ifelse(PROM_CAS_SICOM_OP_24M > 0, PROM_CAS_SICOM_OP_6M/PROM_CAS_SICOM_OP_24M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_6s24M := ifelse(PROM_XVEN_OTROS_OP_24M > 0, PROM_XVEN_OTROS_OP_6M/PROM_XVEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_6s24M := ifelse(PROM_NDI_OTROS_OP_24M > 0, PROM_NDI_OTROS_OP_6M/PROM_NDI_OTROS_OP_24M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_6s24M := ifelse(PROM_VEN_OTROS_OP_24M > 0, PROM_VEN_OTROS_OP_6M/PROM_VEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_6s24M := ifelse(PROM_DEM_OTROS_OP_24M > 0, PROM_DEM_OTROS_OP_6M/PROM_DEM_OTROS_OP_24M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_6s24M := ifelse(PROM_CAS_OTROS_OP_24M > 0, PROM_CAS_OTROS_OP_6M/PROM_CAS_OTROS_OP_24M, 0), ]
  
  data[, r_NOPE_REFIN_OP_12s24M := ifelse(NOPE_REFIN_OP_24M > 0, NOPE_REFIN_OP_12M/NOPE_REFIN_OP_24M, 0), ]
  data[, r_NOPE_XVEN_OP_12s24M := ifelse(NOPE_XVEN_OP_24M > 0, NOPE_XVEN_OP_12M/NOPE_XVEN_OP_24M, 0), ]
  data[, r_NOPE_VENC_OP_12s24M := ifelse(NOPE_VENC_OP_24M > 0, NOPE_VENC_OP_12M/NOPE_VENC_OP_24M, 0), ]
  data[, r_NOPE_NDI_OP_12s24M := ifelse(NOPE_NDI_OP_24M > 0, NOPE_NDI_OP_12M/NOPE_NDI_OP_24M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_12s24M := ifelse(NOPE_VENC_1A30_OP_24M > 0, NOPE_VENC_1A30_OP_12M/NOPE_VENC_1A30_OP_24M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_12s24M := ifelse(NOPE_VENC_31A90_OP_24M > 0, NOPE_VENC_31A90_OP_12M/NOPE_VENC_31A90_OP_24M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_12s24M := ifelse(NOPE_VENC_91A180_OP_24M > 0, NOPE_VENC_91A180_OP_12M/NOPE_VENC_91A180_OP_24M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_12s24M := ifelse(NOPE_VENC_181A360_OP_24M > 0, NOPE_VENC_181A360_OP_12M/NOPE_VENC_181A360_OP_24M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_12s24M := ifelse(NOPE_VENC_MAYOR360_OP_24M > 0, NOPE_VENC_MAYOR360_OP_12M/NOPE_VENC_MAYOR360_OP_24M, 0), ]
  data[, r_NOPE_DEMANDA_OP_12s24M := ifelse(NOPE_DEMANDA_OP_24M > 0, NOPE_DEMANDA_OP_12M/NOPE_DEMANDA_OP_24M, 0), ]
  data[, r_NOPE_CASTIGO_OP_12s24M := ifelse(NOPE_CASTIGO_OP_24M > 0, NOPE_CASTIGO_OP_12M/NOPE_CASTIGO_OP_24M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_12s24M := ifelse(NOPE_APERT_SBS_OP_24M > 0, NOPE_APERT_SBS_OP_12M/NOPE_APERT_SBS_OP_24M, 0), ]
  data[, r_NOPE_APERT_SC_OP_12s24M := ifelse(NOPE_APERT_SC_OP_24M > 0, NOPE_APERT_SC_OP_12M/NOPE_APERT_SC_OP_24M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_12s24M := ifelse(NOPE_APERT_SICOM_OP_24M > 0, NOPE_APERT_SICOM_OP_12M/NOPE_APERT_SICOM_OP_24M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_12s24M := ifelse(NOPE_APERT_OTROS_OP_24M > 0, NOPE_APERT_OTROS_OP_12M/NOPE_APERT_OTROS_OP_24M, 0), ]
  data[, r_MVALVEN_SBS_OP_12s24M := ifelse(MVALVEN_SBS_OP_24M > 0, MVALVEN_SBS_OP_12M/MVALVEN_SBS_OP_24M, 0), ]
  data[, r_MVALVEN_SC_OP_12s24M := ifelse(MVALVEN_SC_OP_24M > 0, MVALVEN_SC_OP_12M/MVALVEN_SC_OP_24M, 0), ]
  data[, r_MVALVEN_SICOM_OP_12s24M := ifelse(MVALVEN_SICOM_OP_24M > 0, MVALVEN_SICOM_OP_12M/MVALVEN_SICOM_OP_24M, 0), ]
  data[, r_MVALVEN_OTROS_OP_12s24M := ifelse(MVALVEN_OTROS_OP_24M > 0, MVALVEN_OTROS_OP_12M/MVALVEN_OTROS_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_OP_12s24M := ifelse(DEUDA_TOTAL_SBS_OP_24M > 0, DEUDA_TOTAL_SBS_OP_12M/DEUDA_TOTAL_SBS_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_SC_OP_12s24M := ifelse(DEUDA_TOTAL_SC_OP_24M > 0, DEUDA_TOTAL_SC_OP_12M/DEUDA_TOTAL_SC_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_SICOM_OP_12s24M := ifelse(DEUDA_TOTAL_SICOM_OP_24M > 0, DEUDA_TOTAL_SICOM_OP_12M/DEUDA_TOTAL_SICOM_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_OTROS_OP_12s24M := ifelse(DEUDA_TOTAL_OTROS_OP_24M > 0, DEUDA_TOTAL_OTROS_OP_12M/DEUDA_TOTAL_OTROS_OP_24M, 0), ]
  data[, r_NENT_VEN_SBS_OP_12s24M := ifelse(NENT_VEN_SBS_OP_24M > 0, NENT_VEN_SBS_OP_12M/NENT_VEN_SBS_OP_24M, 0), ]
  data[, r_NENT_VEN_SC_OP_12s24M := ifelse(NENT_VEN_SC_OP_24M > 0, NENT_VEN_SC_OP_12M/NENT_VEN_SC_OP_24M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_12s24M := ifelse(NENT_VEN_SICOM_OP_24M > 0, NENT_VEN_SICOM_OP_12M/NENT_VEN_SICOM_OP_24M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_12s24M := ifelse(NENT_VEN_OTROS_OP_24M > 0, NENT_VEN_OTROS_OP_12M/NENT_VEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_12s24M := ifelse(PROM_MAX_DVEN_N_OP_24M > 0, PROM_MAX_DVEN_N_OP_12M/PROM_MAX_DVEN_N_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_12s24M := ifelse(PROM_MAX_DVEN_M_OP_24M > 0, PROM_MAX_DVEN_M_OP_12M/PROM_MAX_DVEN_M_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_12s24M := ifelse(PROM_MAX_DVEN_C_OP_24M > 0, PROM_MAX_DVEN_C_OP_12M/PROM_MAX_DVEN_C_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_12s24M := ifelse(PROM_MAX_DVEN_V_OP_24M > 0, PROM_MAX_DVEN_V_OP_12M/PROM_MAX_DVEN_V_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_12s24M := ifelse(PROM_MAX_DVEN_P_OP_24M > 0, PROM_MAX_DVEN_P_OP_12M/PROM_MAX_DVEN_P_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_12s24M := ifelse(PROM_MAX_DVEN_OTROS_OP_24M > 0, PROM_MAX_DVEN_OTROS_OP_12M/PROM_MAX_DVEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_12s24M := ifelse(PROM_MAX_DVEN_SBS_OP_24M > 0, PROM_MAX_DVEN_SBS_OP_12M/PROM_MAX_DVEN_SBS_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_12s24M := ifelse(PROM_MAX_DVEN_SC_OP_24M > 0, PROM_MAX_DVEN_SC_OP_12M/PROM_MAX_DVEN_SC_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_12s24M := ifelse(PROM_MAX_DVEN_SICOM_OP_24M > 0, PROM_MAX_DVEN_SICOM_OP_12M/PROM_MAX_DVEN_SICOM_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_12s24M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_24M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_12M/PROM_MAX_DVEN_OTROS_SIS_OP_24M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_12s24M := ifelse(PROM_XVEN_SBS_OP_24M > 0, PROM_XVEN_SBS_OP_12M/PROM_XVEN_SBS_OP_24M, 0), ]
  data[, r_PROM_NDI_SBS_OP_12s24M := ifelse(PROM_NDI_SBS_OP_24M > 0, PROM_NDI_SBS_OP_12M/PROM_NDI_SBS_OP_24M, 0), ]
  data[, r_PROM_VEN_SBS_OP_12s24M := ifelse(PROM_VEN_SBS_OP_24M > 0, PROM_VEN_SBS_OP_12M/PROM_VEN_SBS_OP_24M, 0), ]
  data[, r_PROM_DEM_SBS_OP_12s24M := ifelse(PROM_DEM_SBS_OP_24M > 0, PROM_DEM_SBS_OP_12M/PROM_DEM_SBS_OP_24M, 0), ]
  data[, r_PROM_CAS_SBS_OP_12s24M := ifelse(PROM_CAS_SBS_OP_24M > 0, PROM_CAS_SBS_OP_12M/PROM_CAS_SBS_OP_24M, 0), ]
  data[, r_PROM_XVEN_SC_OP_12s24M := ifelse(PROM_XVEN_SC_OP_24M > 0, PROM_XVEN_SC_OP_12M/PROM_XVEN_SC_OP_24M, 0), ]
  data[, r_PROM_NDI_SC_OP_12s24M := ifelse(PROM_NDI_SC_OP_24M > 0, PROM_NDI_SC_OP_12M/PROM_NDI_SC_OP_24M, 0), ]
  data[, r_PROM_VEN_SC_OP_12s24M := ifelse(PROM_VEN_SC_OP_24M > 0, PROM_VEN_SC_OP_12M/PROM_VEN_SC_OP_24M, 0), ]
  data[, r_PROM_DEM_SC_OP_12s24M := ifelse(PROM_DEM_SC_OP_24M > 0, PROM_DEM_SC_OP_12M/PROM_DEM_SC_OP_24M, 0), ]
  data[, r_PROM_CAS_SC_OP_12s24M := ifelse(PROM_CAS_SC_OP_24M > 0, PROM_CAS_SC_OP_12M/PROM_CAS_SC_OP_24M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_12s24M := ifelse(PROM_XVEN_SICOM_OP_24M > 0, PROM_XVEN_SICOM_OP_12M/PROM_XVEN_SICOM_OP_24M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_12s24M := ifelse(PROM_NDI_SICOM_OP_24M > 0, PROM_NDI_SICOM_OP_12M/PROM_NDI_SICOM_OP_24M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_12s24M := ifelse(PROM_VEN_SICOM_OP_24M > 0, PROM_VEN_SICOM_OP_12M/PROM_VEN_SICOM_OP_24M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_12s24M := ifelse(PROM_DEM_SICOM_OP_24M > 0, PROM_DEM_SICOM_OP_12M/PROM_DEM_SICOM_OP_24M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_12s24M := ifelse(PROM_CAS_SICOM_OP_24M > 0, PROM_CAS_SICOM_OP_12M/PROM_CAS_SICOM_OP_24M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_12s24M := ifelse(PROM_XVEN_OTROS_OP_24M > 0, PROM_XVEN_OTROS_OP_12M/PROM_XVEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_12s24M := ifelse(PROM_NDI_OTROS_OP_24M > 0, PROM_NDI_OTROS_OP_12M/PROM_NDI_OTROS_OP_24M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_12s24M := ifelse(PROM_VEN_OTROS_OP_24M > 0, PROM_VEN_OTROS_OP_12M/PROM_VEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_12s24M := ifelse(PROM_DEM_OTROS_OP_24M > 0, PROM_DEM_OTROS_OP_12M/PROM_DEM_OTROS_OP_24M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_12s24M := ifelse(PROM_CAS_OTROS_OP_24M > 0, PROM_CAS_OTROS_OP_12M/PROM_CAS_OTROS_OP_24M, 0), ]
  
  #data[, r_NOPE_REFIN_OP_12s36M := ifelse(NOPE_REFIN_OP_36M > 0, NOPE_REFIN_OP_12M/NOPE_REFIN_OP_36M, 0), ]
  data[, r_NOPE_XVEN_OP_12s36M := ifelse(NOPE_XVEN_OP_36M > 0, NOPE_XVEN_OP_12M/NOPE_XVEN_OP_36M, 0), ]
  data[, r_NOPE_VENC_OP_12s36M := ifelse(NOPE_VENC_OP_36M > 0, NOPE_VENC_OP_12M/NOPE_VENC_OP_36M, 0), ]
  data[, r_NOPE_NDI_OP_12s36M := ifelse(NOPE_NDI_OP_36M > 0, NOPE_NDI_OP_12M/NOPE_NDI_OP_36M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_12s36M := ifelse(NOPE_VENC_1A30_OP_36M > 0, NOPE_VENC_1A30_OP_12M/NOPE_VENC_1A30_OP_36M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_12s36M := ifelse(NOPE_VENC_31A90_OP_36M > 0, NOPE_VENC_31A90_OP_12M/NOPE_VENC_31A90_OP_36M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_12s36M := ifelse(NOPE_VENC_91A180_OP_36M > 0, NOPE_VENC_91A180_OP_12M/NOPE_VENC_91A180_OP_36M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_12s36M := ifelse(NOPE_VENC_181A360_OP_36M > 0, NOPE_VENC_181A360_OP_12M/NOPE_VENC_181A360_OP_36M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_12s36M := ifelse(NOPE_VENC_MAYOR360_OP_36M > 0, NOPE_VENC_MAYOR360_OP_12M/NOPE_VENC_MAYOR360_OP_36M, 0), ]
  data[, r_NOPE_DEMANDA_OP_12s36M := ifelse(NOPE_DEMANDA_OP_36M > 0, NOPE_DEMANDA_OP_12M/NOPE_DEMANDA_OP_36M, 0), ]
  data[, r_NOPE_CASTIGO_OP_12s36M := ifelse(NOPE_CASTIGO_OP_36M > 0, NOPE_CASTIGO_OP_12M/NOPE_CASTIGO_OP_36M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_12s36M := ifelse(NOPE_APERT_SBS_OP_36M > 0, NOPE_APERT_SBS_OP_12M/NOPE_APERT_SBS_OP_36M, 0), ]
  data[, r_NOPE_APERT_SC_OP_12s36M := ifelse(NOPE_APERT_SC_OP_36M > 0, NOPE_APERT_SC_OP_12M/NOPE_APERT_SC_OP_36M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_12s36M := ifelse(NOPE_APERT_SICOM_OP_36M > 0, NOPE_APERT_SICOM_OP_12M/NOPE_APERT_SICOM_OP_36M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_12s36M := ifelse(NOPE_APERT_OTROS_OP_36M > 0, NOPE_APERT_OTROS_OP_12M/NOPE_APERT_OTROS_OP_36M, 0), ]
  data[, r_MVALVEN_SBS_OP_12s36M := ifelse(MVALVEN_SBS_OP_36M > 0, MVALVEN_SBS_OP_12M/MVALVEN_SBS_OP_36M, 0), ]
  data[, r_MVALVEN_SC_OP_12s36M := ifelse(MVALVEN_SC_OP_36M > 0, MVALVEN_SC_OP_12M/MVALVEN_SC_OP_36M, 0), ]
  data[, r_MVALVEN_SICOM_OP_12s36M := ifelse(MVALVEN_SICOM_OP_36M > 0, MVALVEN_SICOM_OP_12M/MVALVEN_SICOM_OP_36M, 0), ]
  data[, r_MVALVEN_OTROS_OP_12s36M := ifelse(MVALVEN_OTROS_OP_36M > 0, MVALVEN_OTROS_OP_12M/MVALVEN_OTROS_OP_36M, 0), ]
  #data[, r_DEUDA_TOTAL_SBS_OP_12s36M := ifelse(DEUDA_TOTAL_SBS_OP_36M > 0, DEUDA_TOTAL_SBS_OP_12M/DEUDA_TOTAL_SBS_OP_36M, 0), ]
  #data[, r_DEUDA_TOTAL_SC_OP_12s36M := ifelse(DEUDA_TOTAL_SC_OP_36M > 0, DEUDA_TOTAL_SC_OP_12M/DEUDA_TOTAL_SC_OP_36M, 0), ]
  #data[, r_DEUDA_TOTAL_SICOM_OP_12s36M := ifelse(DEUDA_TOTAL_SICOM_OP_36M > 0, DEUDA_TOTAL_SICOM_OP_12M/DEUDA_TOTAL_SICOM_OP_36M, 0), ]
  #data[, r_DEUDA_TOTAL_OTROS_OP_12s36M := ifelse(DEUDA_TOTAL_OTROS_OP_36M > 0, DEUDA_TOTAL_OTROS_OP_12M/DEUDA_TOTAL_OTROS_OP_36M, 0), ]
  data[, r_NENT_VEN_SBS_OP_12s36M := ifelse(NENT_VEN_SBS_OP_36M > 0, NENT_VEN_SBS_OP_12M/NENT_VEN_SBS_OP_36M, 0), ]
  data[, r_NENT_VEN_SC_OP_12s36M := ifelse(NENT_VEN_SC_OP_36M > 0, NENT_VEN_SC_OP_12M/NENT_VEN_SC_OP_36M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_12s36M := ifelse(NENT_VEN_SICOM_OP_36M > 0, NENT_VEN_SICOM_OP_12M/NENT_VEN_SICOM_OP_36M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_12s36M := ifelse(NENT_VEN_OTROS_OP_36M > 0, NENT_VEN_OTROS_OP_12M/NENT_VEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_12s36M := ifelse(PROM_MAX_DVEN_N_OP_36M > 0, PROM_MAX_DVEN_N_OP_12M/PROM_MAX_DVEN_N_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_12s36M := ifelse(PROM_MAX_DVEN_M_OP_36M > 0, PROM_MAX_DVEN_M_OP_12M/PROM_MAX_DVEN_M_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_12s36M := ifelse(PROM_MAX_DVEN_C_OP_36M > 0, PROM_MAX_DVEN_C_OP_12M/PROM_MAX_DVEN_C_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_12s36M := ifelse(PROM_MAX_DVEN_V_OP_36M > 0, PROM_MAX_DVEN_V_OP_12M/PROM_MAX_DVEN_V_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_12s36M := ifelse(PROM_MAX_DVEN_P_OP_36M > 0, PROM_MAX_DVEN_P_OP_12M/PROM_MAX_DVEN_P_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_12s36M := ifelse(PROM_MAX_DVEN_OTROS_OP_36M > 0, PROM_MAX_DVEN_OTROS_OP_12M/PROM_MAX_DVEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_12s36M := ifelse(PROM_MAX_DVEN_SBS_OP_36M > 0, PROM_MAX_DVEN_SBS_OP_12M/PROM_MAX_DVEN_SBS_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_12s36M := ifelse(PROM_MAX_DVEN_SC_OP_36M > 0, PROM_MAX_DVEN_SC_OP_12M/PROM_MAX_DVEN_SC_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_12s36M := ifelse(PROM_MAX_DVEN_SICOM_OP_36M > 0, PROM_MAX_DVEN_SICOM_OP_12M/PROM_MAX_DVEN_SICOM_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_12s36M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_36M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_12M/PROM_MAX_DVEN_OTROS_SIS_OP_36M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_12s36M := ifelse(PROM_XVEN_SBS_OP_36M > 0, PROM_XVEN_SBS_OP_12M/PROM_XVEN_SBS_OP_36M, 0), ]
  data[, r_PROM_NDI_SBS_OP_12s36M := ifelse(PROM_NDI_SBS_OP_36M > 0, PROM_NDI_SBS_OP_12M/PROM_NDI_SBS_OP_36M, 0), ]
  data[, r_PROM_VEN_SBS_OP_12s36M := ifelse(PROM_VEN_SBS_OP_36M > 0, PROM_VEN_SBS_OP_12M/PROM_VEN_SBS_OP_36M, 0), ]
  data[, r_PROM_DEM_SBS_OP_12s36M := ifelse(PROM_DEM_SBS_OP_36M > 0, PROM_DEM_SBS_OP_12M/PROM_DEM_SBS_OP_36M, 0), ]
  data[, r_PROM_CAS_SBS_OP_12s36M := ifelse(PROM_CAS_SBS_OP_36M > 0, PROM_CAS_SBS_OP_12M/PROM_CAS_SBS_OP_36M, 0), ]
  data[, r_PROM_XVEN_SC_OP_12s36M := ifelse(PROM_XVEN_SC_OP_36M > 0, PROM_XVEN_SC_OP_12M/PROM_XVEN_SC_OP_36M, 0), ]
  data[, r_PROM_NDI_SC_OP_12s36M := ifelse(PROM_NDI_SC_OP_36M > 0, PROM_NDI_SC_OP_12M/PROM_NDI_SC_OP_36M, 0), ]
  data[, r_PROM_VEN_SC_OP_12s36M := ifelse(PROM_VEN_SC_OP_36M > 0, PROM_VEN_SC_OP_12M/PROM_VEN_SC_OP_36M, 0), ]
  data[, r_PROM_DEM_SC_OP_12s36M := ifelse(PROM_DEM_SC_OP_36M > 0, PROM_DEM_SC_OP_12M/PROM_DEM_SC_OP_36M, 0), ]
  data[, r_PROM_CAS_SC_OP_12s36M := ifelse(PROM_CAS_SC_OP_36M > 0, PROM_CAS_SC_OP_12M/PROM_CAS_SC_OP_36M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_12s36M := ifelse(PROM_XVEN_SICOM_OP_36M > 0, PROM_XVEN_SICOM_OP_12M/PROM_XVEN_SICOM_OP_36M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_12s36M := ifelse(PROM_NDI_SICOM_OP_36M > 0, PROM_NDI_SICOM_OP_12M/PROM_NDI_SICOM_OP_36M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_12s36M := ifelse(PROM_VEN_SICOM_OP_36M > 0, PROM_VEN_SICOM_OP_12M/PROM_VEN_SICOM_OP_36M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_12s36M := ifelse(PROM_DEM_SICOM_OP_36M > 0, PROM_DEM_SICOM_OP_12M/PROM_DEM_SICOM_OP_36M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_12s36M := ifelse(PROM_CAS_SICOM_OP_36M > 0, PROM_CAS_SICOM_OP_12M/PROM_CAS_SICOM_OP_36M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_12s36M := ifelse(PROM_XVEN_OTROS_OP_36M > 0, PROM_XVEN_OTROS_OP_12M/PROM_XVEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_12s36M := ifelse(PROM_NDI_OTROS_OP_36M > 0, PROM_NDI_OTROS_OP_12M/PROM_NDI_OTROS_OP_36M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_12s36M := ifelse(PROM_VEN_OTROS_OP_36M > 0, PROM_VEN_OTROS_OP_12M/PROM_VEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_12s36M := ifelse(PROM_DEM_OTROS_OP_36M > 0, PROM_DEM_OTROS_OP_12M/PROM_DEM_OTROS_OP_36M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_12s36M := ifelse(PROM_CAS_OTROS_OP_36M > 0, PROM_CAS_OTROS_OP_12M/PROM_CAS_OTROS_OP_36M, 0), ]
  
  #data[, r_NOPE_REFIN_OP_24s36M := ifelse(NOPE_REFIN_OP_36M > 0, NOPE_REFIN_OP_24M/NOPE_REFIN_OP_36M, 0), ]
  data[, r_NOPE_XVEN_OP_24s36M := ifelse(NOPE_XVEN_OP_36M > 0, NOPE_XVEN_OP_24M/NOPE_XVEN_OP_36M, 0), ]
  data[, r_NOPE_VENC_OP_24s36M := ifelse(NOPE_VENC_OP_36M > 0, NOPE_VENC_OP_24M/NOPE_VENC_OP_36M, 0), ]
  data[, r_NOPE_NDI_OP_24s36M := ifelse(NOPE_NDI_OP_36M > 0, NOPE_NDI_OP_24M/NOPE_NDI_OP_36M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_24s36M := ifelse(NOPE_VENC_1A30_OP_36M > 0, NOPE_VENC_1A30_OP_24M/NOPE_VENC_1A30_OP_36M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_24s36M := ifelse(NOPE_VENC_31A90_OP_36M > 0, NOPE_VENC_31A90_OP_24M/NOPE_VENC_31A90_OP_36M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_24s36M := ifelse(NOPE_VENC_91A180_OP_36M > 0, NOPE_VENC_91A180_OP_24M/NOPE_VENC_91A180_OP_36M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_24s36M := ifelse(NOPE_VENC_181A360_OP_36M > 0, NOPE_VENC_181A360_OP_24M/NOPE_VENC_181A360_OP_36M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_24s36M := ifelse(NOPE_VENC_MAYOR360_OP_36M > 0, NOPE_VENC_MAYOR360_OP_24M/NOPE_VENC_MAYOR360_OP_36M, 0), ]
  data[, r_NOPE_DEMANDA_OP_24s36M := ifelse(NOPE_DEMANDA_OP_36M > 0, NOPE_DEMANDA_OP_24M/NOPE_DEMANDA_OP_36M, 0), ]
  data[, r_NOPE_CASTIGO_OP_24s36M := ifelse(NOPE_CASTIGO_OP_36M > 0, NOPE_CASTIGO_OP_24M/NOPE_CASTIGO_OP_36M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_24s36M := ifelse(NOPE_APERT_SBS_OP_36M > 0, NOPE_APERT_SBS_OP_24M/NOPE_APERT_SBS_OP_36M, 0), ]
  data[, r_NOPE_APERT_SC_OP_24s36M := ifelse(NOPE_APERT_SC_OP_36M > 0, NOPE_APERT_SC_OP_24M/NOPE_APERT_SC_OP_36M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_24s36M := ifelse(NOPE_APERT_SICOM_OP_36M > 0, NOPE_APERT_SICOM_OP_24M/NOPE_APERT_SICOM_OP_36M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_24s36M := ifelse(NOPE_APERT_OTROS_OP_36M > 0, NOPE_APERT_OTROS_OP_24M/NOPE_APERT_OTROS_OP_36M, 0), ]
  data[, r_MVALVEN_SBS_OP_24s36M := ifelse(MVALVEN_SBS_OP_36M > 0, MVALVEN_SBS_OP_24M/MVALVEN_SBS_OP_36M, 0), ]
  data[, r_MVALVEN_SC_OP_24s36M := ifelse(MVALVEN_SC_OP_36M > 0, MVALVEN_SC_OP_24M/MVALVEN_SC_OP_36M, 0), ]
  data[, r_MVALVEN_SICOM_OP_24s36M := ifelse(MVALVEN_SICOM_OP_36M > 0, MVALVEN_SICOM_OP_24M/MVALVEN_SICOM_OP_36M, 0), ]
  data[, r_MVALVEN_OTROS_OP_24s36M := ifelse(MVALVEN_OTROS_OP_36M > 0, MVALVEN_OTROS_OP_24M/MVALVEN_OTROS_OP_36M, 0), ]
  #data[, r_DEUDA_TOTAL_SBS_OP_24s36M := ifelse(DEUDA_TOTAL_SBS_OP_36M > 0, DEUDA_TOTAL_SBS_OP_24M/DEUDA_TOTAL_SBS_OP_36M, 0), ]
  #data[, r_DEUDA_TOTAL_SC_OP_24s36M := ifelse(DEUDA_TOTAL_SC_OP_36M > 0, DEUDA_TOTAL_SC_OP_24M/DEUDA_TOTAL_SC_OP_36M, 0), ]
  #data[, r_DEUDA_TOTAL_SICOM_OP_24s36M := ifelse(DEUDA_TOTAL_SICOM_OP_36M > 0, DEUDA_TOTAL_SICOM_OP_24M/DEUDA_TOTAL_SICOM_OP_36M, 0), ]
  #data[, r_DEUDA_TOTAL_OTROS_OP_24s36M := ifelse(DEUDA_TOTAL_OTROS_OP_36M > 0, DEUDA_TOTAL_OTROS_OP_24M/DEUDA_TOTAL_OTROS_OP_36M, 0), ]
  data[, r_NENT_VEN_SBS_OP_24s36M := ifelse(NENT_VEN_SBS_OP_36M > 0, NENT_VEN_SBS_OP_24M/NENT_VEN_SBS_OP_36M, 0), ]
  data[, r_NENT_VEN_SC_OP_24s36M := ifelse(NENT_VEN_SC_OP_36M > 0, NENT_VEN_SC_OP_24M/NENT_VEN_SC_OP_36M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_24s36M := ifelse(NENT_VEN_SICOM_OP_36M > 0, NENT_VEN_SICOM_OP_24M/NENT_VEN_SICOM_OP_36M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_24s36M := ifelse(NENT_VEN_OTROS_OP_36M > 0, NENT_VEN_OTROS_OP_24M/NENT_VEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_24s36M := ifelse(PROM_MAX_DVEN_N_OP_36M > 0, PROM_MAX_DVEN_N_OP_24M/PROM_MAX_DVEN_N_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_24s36M := ifelse(PROM_MAX_DVEN_M_OP_36M > 0, PROM_MAX_DVEN_M_OP_24M/PROM_MAX_DVEN_M_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_24s36M := ifelse(PROM_MAX_DVEN_C_OP_36M > 0, PROM_MAX_DVEN_C_OP_24M/PROM_MAX_DVEN_C_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_24s36M := ifelse(PROM_MAX_DVEN_V_OP_36M > 0, PROM_MAX_DVEN_V_OP_24M/PROM_MAX_DVEN_V_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_24s36M := ifelse(PROM_MAX_DVEN_P_OP_36M > 0, PROM_MAX_DVEN_P_OP_24M/PROM_MAX_DVEN_P_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_24s36M := ifelse(PROM_MAX_DVEN_OTROS_OP_36M > 0, PROM_MAX_DVEN_OTROS_OP_24M/PROM_MAX_DVEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_24s36M := ifelse(PROM_MAX_DVEN_SBS_OP_36M > 0, PROM_MAX_DVEN_SBS_OP_24M/PROM_MAX_DVEN_SBS_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_24s36M := ifelse(PROM_MAX_DVEN_SC_OP_36M > 0, PROM_MAX_DVEN_SC_OP_24M/PROM_MAX_DVEN_SC_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_24s36M := ifelse(PROM_MAX_DVEN_SICOM_OP_36M > 0, PROM_MAX_DVEN_SICOM_OP_24M/PROM_MAX_DVEN_SICOM_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_24s36M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_36M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_24M/PROM_MAX_DVEN_OTROS_SIS_OP_36M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_24s36M := ifelse(PROM_XVEN_SBS_OP_36M > 0, PROM_XVEN_SBS_OP_24M/PROM_XVEN_SBS_OP_36M, 0), ]
  data[, r_PROM_NDI_SBS_OP_24s36M := ifelse(PROM_NDI_SBS_OP_36M > 0, PROM_NDI_SBS_OP_24M/PROM_NDI_SBS_OP_36M, 0), ]
  data[, r_PROM_VEN_SBS_OP_24s36M := ifelse(PROM_VEN_SBS_OP_36M > 0, PROM_VEN_SBS_OP_24M/PROM_VEN_SBS_OP_36M, 0), ]
  data[, r_PROM_DEM_SBS_OP_24s36M := ifelse(PROM_DEM_SBS_OP_36M > 0, PROM_DEM_SBS_OP_24M/PROM_DEM_SBS_OP_36M, 0), ]
  data[, r_PROM_CAS_SBS_OP_24s36M := ifelse(PROM_CAS_SBS_OP_36M > 0, PROM_CAS_SBS_OP_24M/PROM_CAS_SBS_OP_36M, 0), ]
  data[, r_PROM_XVEN_SC_OP_24s36M := ifelse(PROM_XVEN_SC_OP_36M > 0, PROM_XVEN_SC_OP_24M/PROM_XVEN_SC_OP_36M, 0), ]
  data[, r_PROM_NDI_SC_OP_24s36M := ifelse(PROM_NDI_SC_OP_36M > 0, PROM_NDI_SC_OP_24M/PROM_NDI_SC_OP_36M, 0), ]
  data[, r_PROM_VEN_SC_OP_24s36M := ifelse(PROM_VEN_SC_OP_36M > 0, PROM_VEN_SC_OP_24M/PROM_VEN_SC_OP_36M, 0), ]
  data[, r_PROM_DEM_SC_OP_24s36M := ifelse(PROM_DEM_SC_OP_36M > 0, PROM_DEM_SC_OP_24M/PROM_DEM_SC_OP_36M, 0), ]
  data[, r_PROM_CAS_SC_OP_24s36M := ifelse(PROM_CAS_SC_OP_36M > 0, PROM_CAS_SC_OP_24M/PROM_CAS_SC_OP_36M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_24s36M := ifelse(PROM_XVEN_SICOM_OP_36M > 0, PROM_XVEN_SICOM_OP_24M/PROM_XVEN_SICOM_OP_36M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_24s36M := ifelse(PROM_NDI_SICOM_OP_36M > 0, PROM_NDI_SICOM_OP_24M/PROM_NDI_SICOM_OP_36M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_24s36M := ifelse(PROM_VEN_SICOM_OP_36M > 0, PROM_VEN_SICOM_OP_24M/PROM_VEN_SICOM_OP_36M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_24s36M := ifelse(PROM_DEM_SICOM_OP_36M > 0, PROM_DEM_SICOM_OP_24M/PROM_DEM_SICOM_OP_36M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_24s36M := ifelse(PROM_CAS_SICOM_OP_36M > 0, PROM_CAS_SICOM_OP_24M/PROM_CAS_SICOM_OP_36M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_24s36M := ifelse(PROM_XVEN_OTROS_OP_36M > 0, PROM_XVEN_OTROS_OP_24M/PROM_XVEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_24s36M := ifelse(PROM_NDI_OTROS_OP_36M > 0, PROM_NDI_OTROS_OP_24M/PROM_NDI_OTROS_OP_36M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_24s36M := ifelse(PROM_VEN_OTROS_OP_36M > 0, PROM_VEN_OTROS_OP_24M/PROM_VEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_24s36M := ifelse(PROM_DEM_OTROS_OP_36M > 0, PROM_DEM_OTROS_OP_24M/PROM_DEM_OTROS_OP_36M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_24s36M := ifelse(PROM_CAS_OTROS_OP_36M > 0, PROM_CAS_OTROS_OP_24M/PROM_CAS_OTROS_OP_36M, 0), ]
  
  data[, r_MVAL_DEMANDA_SBS_OP_6s12M := ifelse(MVAL_DEMANDA_SBS_OP_12M > 0, MVAL_DEMANDA_SBS_OP_6M/MVAL_DEMANDA_SBS_OP_12M, 0), ]
  data[, r_MVAL_DEMANDA_SC_OP_6s12M := ifelse(MVAL_DEMANDA_SC_OP_12M > 0, MVAL_DEMANDA_SC_OP_6M/MVAL_DEMANDA_SC_OP_12M, 0), ]
  data[, r_MVAL_DEMANDA_SICOM_OP_6s12M := ifelse(MVAL_DEMANDA_SICOM_OP_12M > 0, MVAL_DEMANDA_SICOM_OP_6M/MVAL_DEMANDA_SICOM_OP_12M, 0), ]
  data[, r_MVAL_DEMANDA_OTROS_OP_6s12M := ifelse(MVAL_DEMANDA_OTROS_OP_12M > 0, MVAL_DEMANDA_OTROS_OP_6M/MVAL_DEMANDA_OTROS_OP_12M, 0), ]
  data[, r_MVAL_CASTIGO_SBS_OP_6s12M := ifelse(MVAL_CASTIGO_SBS_OP_12M > 0, MVAL_CASTIGO_SBS_OP_6M/MVAL_CASTIGO_SBS_OP_12M, 0), ]
  data[, r_MVAL_CASTIGO_SC_OP_6s12M := ifelse(MVAL_CASTIGO_SC_OP_12M > 0, MVAL_CASTIGO_SC_OP_6M/MVAL_CASTIGO_SC_OP_12M, 0), ]
  data[, r_MVAL_CASTIGO_SICOM_OP_6s12M := ifelse(MVAL_CASTIGO_SICOM_OP_12M > 0, MVAL_CASTIGO_SICOM_OP_6M/MVAL_CASTIGO_SICOM_OP_12M, 0), ]
  data[, r_MVAL_CASTIGO_OTROS_OP_6s12M := ifelse(MVAL_CASTIGO_OTROS_OP_12M > 0, MVAL_CASTIGO_OTROS_OP_6M/MVAL_CASTIGO_OTROS_OP_12M, 0), ]
  
  data[, r_MVAL_DEMANDA_SBS_OP_6s24M := ifelse(MVAL_DEMANDA_SBS_OP_24M > 0, MVAL_DEMANDA_SBS_OP_6M/MVAL_DEMANDA_SBS_OP_24M, 0), ]
  data[, r_MVAL_DEMANDA_SC_OP_6s24M := ifelse(MVAL_DEMANDA_SC_OP_24M > 0, MVAL_DEMANDA_SC_OP_6M/MVAL_DEMANDA_SC_OP_24M, 0), ]
  data[, r_MVAL_DEMANDA_SICOM_OP_6s24M := ifelse(MVAL_DEMANDA_SICOM_OP_24M > 0, MVAL_DEMANDA_SICOM_OP_6M/MVAL_DEMANDA_SICOM_OP_24M, 0), ]
  data[, r_MVAL_DEMANDA_OTROS_OP_6s24M := ifelse(MVAL_DEMANDA_OTROS_OP_24M > 0, MVAL_DEMANDA_OTROS_OP_6M/MVAL_DEMANDA_OTROS_OP_24M, 0), ]
  data[, r_MVAL_CASTIGO_SBS_OP_6s24M := ifelse(MVAL_CASTIGO_SBS_OP_24M > 0, MVAL_CASTIGO_SBS_OP_6M/MVAL_CASTIGO_SBS_OP_24M, 0), ]
  data[, r_MVAL_CASTIGO_SC_OP_6s24M := ifelse(MVAL_CASTIGO_SC_OP_24M > 0, MVAL_CASTIGO_SC_OP_6M/MVAL_CASTIGO_SC_OP_24M, 0), ]
  data[, r_MVAL_CASTIGO_SICOM_OP_6s24M := ifelse(MVAL_CASTIGO_SICOM_OP_24M > 0, MVAL_CASTIGO_SICOM_OP_6M/MVAL_CASTIGO_SICOM_OP_24M, 0), ]
  data[, r_MVAL_CASTIGO_OTROS_OP_6s24M := ifelse(MVAL_CASTIGO_OTROS_OP_24M > 0, MVAL_CASTIGO_OTROS_OP_6M/MVAL_CASTIGO_OTROS_OP_24M, 0), ]
  
  data[, r_MVAL_DEMANDA_SBS_OP_12s24M := ifelse(MVAL_DEMANDA_SBS_OP_24M > 0, MVAL_DEMANDA_SBS_OP_12M/MVAL_DEMANDA_SBS_OP_24M, 0), ]
  data[, r_MVAL_DEMANDA_SC_OP_12s24M := ifelse(MVAL_DEMANDA_SC_OP_24M > 0, MVAL_DEMANDA_SC_OP_12M/MVAL_DEMANDA_SC_OP_24M, 0), ]
  data[, r_MVAL_DEMANDA_SICOM_OP_12s24M := ifelse(MVAL_DEMANDA_SICOM_OP_24M > 0, MVAL_DEMANDA_SICOM_OP_12M/MVAL_DEMANDA_SICOM_OP_24M, 0), ]
  data[, r_MVAL_DEMANDA_OTROS_OP_12s24M := ifelse(MVAL_DEMANDA_OTROS_OP_24M > 0, MVAL_DEMANDA_OTROS_OP_12M/MVAL_DEMANDA_OTROS_OP_24M, 0), ]
  data[, r_MVAL_CASTIGO_SBS_OP_12s24M := ifelse(MVAL_CASTIGO_SBS_OP_24M > 0, MVAL_CASTIGO_SBS_OP_12M/MVAL_CASTIGO_SBS_OP_24M, 0), ]
  data[, r_MVAL_CASTIGO_SC_OP_12s24M := ifelse(MVAL_CASTIGO_SC_OP_24M > 0, MVAL_CASTIGO_SC_OP_12M/MVAL_CASTIGO_SC_OP_24M, 0), ]
  data[, r_MVAL_CASTIGO_SICOM_OP_12s24M := ifelse(MVAL_CASTIGO_SICOM_OP_24M > 0, MVAL_CASTIGO_SICOM_OP_12M/MVAL_CASTIGO_SICOM_OP_24M, 0), ]
  data[, r_MVAL_CASTIGO_OTROS_OP_12s24M := ifelse(MVAL_CASTIGO_OTROS_OP_24M > 0, MVAL_CASTIGO_OTROS_OP_12M/MVAL_CASTIGO_OTROS_OP_24M, 0), ]
  
  data[, r_MVAL_DEMANDA_SBS_OP_24s36M := ifelse(MVAL_DEMANDA_SBS_OP_36M > 0, MVAL_DEMANDA_SBS_OP_24M/MVAL_DEMANDA_SBS_OP_36M, 0), ]
  data[, r_MVAL_DEMANDA_SC_OP_24s36M := ifelse(MVAL_DEMANDA_SC_OP_36M > 0, MVAL_DEMANDA_SC_OP_24M/MVAL_DEMANDA_SC_OP_36M, 0), ]
  data[, r_MVAL_DEMANDA_SICOM_OP_24s36M := ifelse(MVAL_DEMANDA_SICOM_OP_36M > 0, MVAL_DEMANDA_SICOM_OP_24M/MVAL_DEMANDA_SICOM_OP_36M, 0), ]
  data[, r_MVAL_DEMANDA_OTROS_OP_24s36M := ifelse(MVAL_DEMANDA_OTROS_OP_36M > 0, MVAL_DEMANDA_OTROS_OP_24M/MVAL_DEMANDA_OTROS_OP_36M, 0), ]
  data[, r_MVAL_CASTIGO_SBS_OP_24s36M := ifelse(MVAL_CASTIGO_SBS_OP_36M > 0, MVAL_CASTIGO_SBS_OP_24M/MVAL_CASTIGO_SBS_OP_36M, 0), ]
  data[, r_MVAL_CASTIGO_SC_OP_24s36M := ifelse(MVAL_CASTIGO_SC_OP_36M > 0, MVAL_CASTIGO_SC_OP_24M/MVAL_CASTIGO_SC_OP_36M, 0), ]
  data[, r_MVAL_CASTIGO_SICOM_OP_24s36M := ifelse(MVAL_CASTIGO_SICOM_OP_36M > 0, MVAL_CASTIGO_SICOM_OP_24M/MVAL_CASTIGO_SICOM_OP_36M, 0), ]
  data[, r_MVAL_CASTIGO_OTROS_OP_24s36M := ifelse(MVAL_CASTIGO_OTROS_OP_36M > 0, MVAL_CASTIGO_OTROS_OP_24M/MVAL_CASTIGO_OTROS_OP_36M, 0), ]
  
  data[, r_NOPE_VENC_31AMAS_OP_3s6M := ifelse(NOPE_VENC_31AMAS_OP_6M > 0, NOPE_VENC_31AMAS_OP_3M/NOPE_VENC_31AMAS_OP_6M, 0), ]
  data[, r_NOPE_DEMANDAyCAST_OP_3s6M := ifelse(NOPE_DEMANDAyCAST_OP_6M > 0, NOPE_DEMANDAyCAST_OP_3M/NOPE_DEMANDAyCAST_OP_6M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_OP_3s6M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_6M > 0, PROM_DEUDA_TOTAL_SBS_OP_3M/PROM_DEUDA_TOTAL_SBS_OP_6M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SBS_OP_3s6M := ifelse(PROM_DEMANDAyCAST_SBS_OP_6M > 0, PROM_DEMANDAyCAST_SBS_OP_3M/PROM_DEMANDAyCAST_SBS_OP_6M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SC_OP_3s6M := ifelse(PROM_DEUDA_TOTAL_SC_OP_6M > 0, PROM_DEUDA_TOTAL_SC_OP_3M/PROM_DEUDA_TOTAL_SC_OP_6M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SC_OP_3s6M := ifelse(PROM_DEMANDAyCAST_SC_OP_6M > 0, PROM_DEMANDAyCAST_SC_OP_3M/PROM_DEMANDAyCAST_SC_OP_6M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SICOM_OP_3s6M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_6M > 0, PROM_DEUDA_TOTAL_SICOM_OP_3M/PROM_DEUDA_TOTAL_SICOM_OP_6M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SICOM_OP_3s6M := ifelse(PROM_DEMANDAyCAST_SICOM_OP_6M > 0, PROM_DEMANDAyCAST_SICOM_OP_3M/PROM_DEMANDAyCAST_SICOM_OP_6M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_OTROS_OP_3s6M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_6M > 0, PROM_DEUDA_TOTAL_OTROS_OP_3M/PROM_DEUDA_TOTAL_OTROS_OP_6M, 0), ]
  data[, r_PROM_DEMANDAyCAST_OTROS_OP_3s6M := ifelse(PROM_DEMANDAyCAST_OTROS_OP_6M > 0, PROM_DEMANDAyCAST_OTROS_OP_3M/PROM_DEMANDAyCAST_OTROS_OP_6M, 0), ]
  
  data[, r_NOPE_VENC_31AMAS_OP_3s12M := ifelse(NOPE_VENC_31AMAS_OP_12M > 0, NOPE_VENC_31AMAS_OP_3M/NOPE_VENC_31AMAS_OP_12M, 0), ]
  data[, r_NOPE_DEMANDAyCAST_OP_3s12M := ifelse(NOPE_DEMANDAyCAST_OP_12M > 0, NOPE_DEMANDAyCAST_OP_3M/NOPE_DEMANDAyCAST_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_OP_3s12M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_12M > 0, PROM_DEUDA_TOTAL_SBS_OP_3M/PROM_DEUDA_TOTAL_SBS_OP_12M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SBS_OP_3s12M := ifelse(PROM_DEMANDAyCAST_SBS_OP_12M > 0, PROM_DEMANDAyCAST_SBS_OP_3M/PROM_DEMANDAyCAST_SBS_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SC_OP_3s12M := ifelse(PROM_DEUDA_TOTAL_SC_OP_12M > 0, PROM_DEUDA_TOTAL_SC_OP_3M/PROM_DEUDA_TOTAL_SC_OP_12M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SC_OP_3s12M := ifelse(PROM_DEMANDAyCAST_SC_OP_12M > 0, PROM_DEMANDAyCAST_SC_OP_3M/PROM_DEMANDAyCAST_SC_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SICOM_OP_3s12M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_12M > 0, PROM_DEUDA_TOTAL_SICOM_OP_3M/PROM_DEUDA_TOTAL_SICOM_OP_12M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SICOM_OP_3s12M := ifelse(PROM_DEMANDAyCAST_SICOM_OP_12M > 0, PROM_DEMANDAyCAST_SICOM_OP_3M/PROM_DEMANDAyCAST_SICOM_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_OTROS_OP_3s12M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_12M > 0, PROM_DEUDA_TOTAL_OTROS_OP_3M/PROM_DEUDA_TOTAL_OTROS_OP_12M, 0), ]
  data[, r_PROM_DEMANDAyCAST_OTROS_OP_3s12M := ifelse(PROM_DEMANDAyCAST_OTROS_OP_12M > 0, PROM_DEMANDAyCAST_OTROS_OP_3M/PROM_DEMANDAyCAST_OTROS_OP_12M, 0), ]
  
  data[, r_NOPE_VENC_31AMAS_OP_6s12M := ifelse(NOPE_VENC_31AMAS_OP_12M > 0, NOPE_VENC_31AMAS_OP_6M/NOPE_VENC_31AMAS_OP_12M, 0), ]
  data[, r_NOPE_DEMANDAyCAST_OP_6s12M := ifelse(NOPE_DEMANDAyCAST_OP_12M > 0, NOPE_DEMANDAyCAST_OP_6M/NOPE_DEMANDAyCAST_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_OP_6s12M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_12M > 0, PROM_DEUDA_TOTAL_SBS_OP_6M/PROM_DEUDA_TOTAL_SBS_OP_12M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SBS_OP_6s12M := ifelse(PROM_DEMANDAyCAST_SBS_OP_12M > 0, PROM_DEMANDAyCAST_SBS_OP_6M/PROM_DEMANDAyCAST_SBS_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SC_OP_6s12M := ifelse(PROM_DEUDA_TOTAL_SC_OP_12M > 0, PROM_DEUDA_TOTAL_SC_OP_6M/PROM_DEUDA_TOTAL_SC_OP_12M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SC_OP_6s12M := ifelse(PROM_DEMANDAyCAST_SC_OP_12M > 0, PROM_DEMANDAyCAST_SC_OP_6M/PROM_DEMANDAyCAST_SC_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SICOM_OP_6s12M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_12M > 0, PROM_DEUDA_TOTAL_SICOM_OP_6M/PROM_DEUDA_TOTAL_SICOM_OP_12M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SICOM_OP_6s12M := ifelse(PROM_DEMANDAyCAST_SICOM_OP_12M > 0, PROM_DEMANDAyCAST_SICOM_OP_6M/PROM_DEMANDAyCAST_SICOM_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_OTROS_OP_6s12M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_12M > 0, PROM_DEUDA_TOTAL_OTROS_OP_6M/PROM_DEUDA_TOTAL_OTROS_OP_12M, 0), ]
  data[, r_PROM_DEMANDAyCAST_OTROS_OP_6s12M := ifelse(PROM_DEMANDAyCAST_OTROS_OP_12M > 0, PROM_DEMANDAyCAST_OTROS_OP_6M/PROM_DEMANDAyCAST_OTROS_OP_12M, 0), ]
  
  data[, r_NOPE_VENC_31AMAS_OP_6s24M := ifelse(NOPE_VENC_31AMAS_OP_24M > 0, NOPE_VENC_31AMAS_OP_6M/NOPE_VENC_31AMAS_OP_24M, 0), ]
  data[, r_NOPE_DEMANDAyCAST_OP_6s24M := ifelse(NOPE_DEMANDAyCAST_OP_24M > 0, NOPE_DEMANDAyCAST_OP_6M/NOPE_DEMANDAyCAST_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_OP_6s24M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_24M > 0, PROM_DEUDA_TOTAL_SBS_OP_6M/PROM_DEUDA_TOTAL_SBS_OP_24M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SBS_OP_6s24M := ifelse(PROM_DEMANDAyCAST_SBS_OP_24M > 0, PROM_DEMANDAyCAST_SBS_OP_6M/PROM_DEMANDAyCAST_SBS_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SC_OP_6s24M := ifelse(PROM_DEUDA_TOTAL_SC_OP_24M > 0, PROM_DEUDA_TOTAL_SC_OP_6M/PROM_DEUDA_TOTAL_SC_OP_24M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SC_OP_6s24M := ifelse(PROM_DEMANDAyCAST_SC_OP_24M > 0, PROM_DEMANDAyCAST_SC_OP_6M/PROM_DEMANDAyCAST_SC_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SICOM_OP_6s24M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_24M > 0, PROM_DEUDA_TOTAL_SICOM_OP_6M/PROM_DEUDA_TOTAL_SICOM_OP_24M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SICOM_OP_6s24M := ifelse(PROM_DEMANDAyCAST_SICOM_OP_24M > 0, PROM_DEMANDAyCAST_SICOM_OP_6M/PROM_DEMANDAyCAST_SICOM_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_OTROS_OP_6s24M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_24M > 0, PROM_DEUDA_TOTAL_OTROS_OP_6M/PROM_DEUDA_TOTAL_OTROS_OP_24M, 0), ]
  data[, r_PROM_DEMANDAyCAST_OTROS_OP_6s24M := ifelse(PROM_DEMANDAyCAST_OTROS_OP_24M > 0, PROM_DEMANDAyCAST_OTROS_OP_6M/PROM_DEMANDAyCAST_OTROS_OP_24M, 0), ]
  
  data[, r_NOPE_VENC_31AMAS_OP_12s24M := ifelse(NOPE_VENC_31AMAS_OP_24M > 0, NOPE_VENC_31AMAS_OP_12M/NOPE_VENC_31AMAS_OP_24M, 0), ]
  data[, r_NOPE_DEMANDAyCAST_OP_12s24M := ifelse(NOPE_DEMANDAyCAST_OP_24M > 0, NOPE_DEMANDAyCAST_OP_12M/NOPE_DEMANDAyCAST_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_OP_12s24M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_24M > 0, PROM_DEUDA_TOTAL_SBS_OP_12M/PROM_DEUDA_TOTAL_SBS_OP_24M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SBS_OP_12s24M := ifelse(PROM_DEMANDAyCAST_SBS_OP_24M > 0, PROM_DEMANDAyCAST_SBS_OP_12M/PROM_DEMANDAyCAST_SBS_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SC_OP_12s24M := ifelse(PROM_DEUDA_TOTAL_SC_OP_24M > 0, PROM_DEUDA_TOTAL_SC_OP_12M/PROM_DEUDA_TOTAL_SC_OP_24M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SC_OP_12s24M := ifelse(PROM_DEMANDAyCAST_SC_OP_24M > 0, PROM_DEMANDAyCAST_SC_OP_12M/PROM_DEMANDAyCAST_SC_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SICOM_OP_12s24M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_24M > 0, PROM_DEUDA_TOTAL_SICOM_OP_12M/PROM_DEUDA_TOTAL_SICOM_OP_24M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SICOM_OP_12s24M := ifelse(PROM_DEMANDAyCAST_SICOM_OP_24M > 0, PROM_DEMANDAyCAST_SICOM_OP_12M/PROM_DEMANDAyCAST_SICOM_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_OTROS_OP_12s24M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_24M > 0, PROM_DEUDA_TOTAL_OTROS_OP_12M/PROM_DEUDA_TOTAL_OTROS_OP_24M, 0), ]
  data[, r_PROM_DEMANDAyCAST_OTROS_OP_12s24M := ifelse(PROM_DEMANDAyCAST_OTROS_OP_24M > 0, PROM_DEMANDAyCAST_OTROS_OP_12M/PROM_DEMANDAyCAST_OTROS_OP_24M, 0), ]
  
  data[, r_NOPE_VENC_31AMAS_OP_24s36M := ifelse(NOPE_VENC_31AMAS_OP_36M > 0, NOPE_VENC_31AMAS_OP_24M/NOPE_VENC_31AMAS_OP_36M, 0), ]
  data[, r_NOPE_DEMANDAyCAST_OP_24s36M := ifelse(NOPE_DEMANDAyCAST_OP_36M > 0, NOPE_DEMANDAyCAST_OP_24M/NOPE_DEMANDAyCAST_OP_36M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_OP_24s36M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_36M > 0, PROM_DEUDA_TOTAL_SBS_OP_24M/PROM_DEUDA_TOTAL_SBS_OP_36M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SBS_OP_24s36M := ifelse(PROM_DEMANDAyCAST_SBS_OP_36M > 0, PROM_DEMANDAyCAST_SBS_OP_24M/PROM_DEMANDAyCAST_SBS_OP_36M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SC_OP_24s36M := ifelse(PROM_DEUDA_TOTAL_SC_OP_36M > 0, PROM_DEUDA_TOTAL_SC_OP_24M/PROM_DEUDA_TOTAL_SC_OP_36M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SC_OP_24s36M := ifelse(PROM_DEMANDAyCAST_SC_OP_36M > 0, PROM_DEMANDAyCAST_SC_OP_24M/PROM_DEMANDAyCAST_SC_OP_36M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SICOM_OP_24s36M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_36M > 0, PROM_DEUDA_TOTAL_SICOM_OP_24M/PROM_DEUDA_TOTAL_SICOM_OP_36M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SICOM_OP_24s36M := ifelse(PROM_DEMANDAyCAST_SICOM_OP_36M > 0, PROM_DEMANDAyCAST_SICOM_OP_24M/PROM_DEMANDAyCAST_SICOM_OP_36M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_OTROS_OP_24s36M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_36M > 0, PROM_DEUDA_TOTAL_OTROS_OP_24M/PROM_DEUDA_TOTAL_OTROS_OP_36M, 0), ]
  data[, r_PROM_DEMANDAyCAST_OTROS_OP_24s36M := ifelse(PROM_DEMANDAyCAST_OTROS_OP_36M > 0, PROM_DEMANDAyCAST_OTROS_OP_24M/PROM_DEMANDAyCAST_OTROS_OP_36M, 0), ]
  
  data[, r_NTC_REFIN_TC_3s6M := ifelse(NTC_REFIN_TC_6M > 0, NTC_REFIN_TC_3M/NTC_REFIN_TC_6M, 0), ]
  data[, r_NTC_VENC_TC_3s6M := ifelse(NTC_VENC_TC_6M > 0, NTC_VENC_TC_3M/NTC_VENC_TC_6M, 0), ]
  data[, r_NTC_VENC_1A30_TC_3s6M := ifelse(NTC_VENC_1A30_TC_6M > 0, NTC_VENC_1A30_TC_3M/NTC_VENC_1A30_TC_6M, 0), ]
  data[, r_NTC_VENC_31AMAS_TC_3s6M := ifelse(NTC_VENC_31AMAS_TC_6M > 0, NTC_VENC_31AMAS_TC_3M/NTC_VENC_31AMAS_TC_6M, 0), ]
  data[, r_NTC_DEMANDAyCAST_TC_3s6M := ifelse(NTC_DEMANDAyCAST_TC_6M > 0, NTC_DEMANDAyCAST_TC_3M/NTC_DEMANDAyCAST_TC_6M, 0), ]
  data[, r_NTC_APERT_SBS_TC_3s6M := ifelse(NTC_APERT_SBS_TC_6M > 0, NTC_APERT_SBS_TC_3M/NTC_APERT_SBS_TC_6M, 0), ]
  data[, r_MVALVEN_SBS_TC_3s6M := ifelse(MVALVEN_SBS_TC_6M > 0, MVALVEN_SBS_TC_3M/MVALVEN_SBS_TC_6M, 0), ]
  #data[, r_MVAL_DEMANDA_SBS_TC_3s6M := ifelse(MVAL_DEMANDA_SBS_TC_6M > 0, MVAL_DEMANDA_SBS_TC_3M/MVAL_DEMANDA_SBS_TC_6M, 0), ]
  #data[, r_MVAL_CASTIGO_SBS_TC_3s6M := ifelse(MVAL_CASTIGO_SBS_TC_6M > 0, MVAL_CASTIGO_SBS_TC_3M/MVAL_CASTIGO_SBS_TC_6M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_TC_3s6M := ifelse(DEUDA_TOTAL_SBS_TC_6M > 0, DEUDA_TOTAL_SBS_TC_3M/DEUDA_TOTAL_SBS_TC_6M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_TC_3s6M := ifelse(PROM_MAX_DVEN_SBS_TC_6M > 0, PROM_MAX_DVEN_SBS_TC_3M/PROM_MAX_DVEN_SBS_TC_6M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_TC_3s6M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_6M > 0, PROM_DEUDA_TOTAL_SBS_TC_3M/PROM_DEUDA_TOTAL_SBS_TC_6M, 0), ]
  data[, r_PROM_DEMANDAyCAST_SBS_TC_3s6M := ifelse(PROM_DEMANDAyCAST_SBS_TC_6M > 0, PROM_DEMANDAyCAST_SBS_TC_3M/PROM_DEMANDAyCAST_SBS_TC_6M, 0), ]
  data[, r_PROM_VEN_SBS_TC_3s6M := ifelse(PROM_VEN_SBS_TC_6M > 0, PROM_VEN_SBS_TC_3M/PROM_VEN_SBS_TC_6M, 0), ]
  
  data[, r_NTC_REFIN_TC_6s12M := ifelse(NTC_REFIN_TC_12M > 0, NTC_REFIN_TC_6M/NTC_REFIN_TC_12M, 0), ]
  data[, r_NTC_VENC_TC_6s12M := ifelse(NTC_VENC_TC_12M > 0, NTC_VENC_TC_6M/NTC_VENC_TC_12M, 0), ]
  data[, r_NTC_VENC_1A30_TC_6s12M := ifelse(NTC_VENC_1A30_TC_12M > 0, NTC_VENC_1A30_TC_6M/NTC_VENC_1A30_TC_12M, 0), ]
  data[, r_NTC_VENC_31AMAS_TC_6s12M := ifelse(NTC_VENC_31AMAS_TC_12M > 0, NTC_VENC_31AMAS_TC_6M/NTC_VENC_31AMAS_TC_12M, 0), ]
  data[, r_NTC_DEMANDAyCAST_TC_6s12M := ifelse(NTC_DEMANDAyCAST_TC_12M > 0, NTC_DEMANDAyCAST_TC_6M/NTC_DEMANDAyCAST_TC_12M, 0), ]
  data[, r_NTC_APERT_SBS_TC_6s12M := ifelse(NTC_APERT_SBS_TC_12M > 0, NTC_APERT_SBS_TC_6M/NTC_APERT_SBS_TC_12M, 0), ]
  data[, r_MVALVEN_SBS_TC_6s12M := ifelse(MVALVEN_SBS_TC_12M > 0, MVALVEN_SBS_TC_6M/MVALVEN_SBS_TC_12M, 0), ]
  data[, r_MVAL_DEMANDA_SBS_TC_6s12M := ifelse(MVAL_DEMANDA_SBS_TC_12M>0,MVAL_DEMANDA_SBS_TC_6M/MVAL_DEMANDA_SBS_TC_12M,0)]


info<-data

#IMPORTANTISIMOPARA CLEAN AND DIRTY SCE
temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_castigo_demanda_SCE<-grep(paste0("(MVAL_CASTIGO_SCE|MVAL_DEMANDA_SCE)._*",i,"$"), columnas, value = TRUE)
  info[,paste0("MVAL_DEMANDA_CASTIGO_SCE_",i)  := do.call(pmax, .SD), .SDcols = columnas_castigo_demanda_SCE]
}
print(colnames(info),max=100000)
dim(info)

#IMPORTANTISIMOPARA CLEAN AND DIRTY SF
temporalidad<-c("3M","6M","12M","24M","36M")
for(i in temporalidad){
  columnas_castigo_demanda_SF<-grep(paste0("(MVAL_CASTIGO_SF|MVAL_DEMANDA_SF)._*",i,"$"), columnas, value = TRUE)
  info[,paste0("MVAL_DEMANDA_CASTIGO_SF_",i)  := do.call(pmax, .SD), .SDcols = columnas_castigo_demanda_SF]
}
print(colnames(info),max=100000)
dim(info)
save(list = c("info"), file = "InfoConsolidad.RData", envir = .GlobalEnv)


#Tener encuenta cuando es max cuando son targfetas de credito y operaciones


