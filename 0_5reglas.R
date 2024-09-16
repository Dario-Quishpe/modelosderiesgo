# Modelo Experto
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

info[, MAX_DVEN_SCE_12M_c := ifelse(MAX_DVEN_SCE_12M > 360, 360, MAX_DVEN_SCE_12M)]
info[, NOPE_APERT_SCE_24M_c := ifelse(NOPE_APERT_SCE_24M > 9, 9, NOPE_APERT_SCE_24M)]


# Ejecución de la fórmula sobre toda la base
info[, Y := 0.0021110*MAX_DVEN_SCE_12M_c -0.0504459*numMesesSinVenDesdeUltVenD334 + 0.2377995*NOPE_APERT_SCE_24M_c]
info[, SCORE := ceiling(1000/(1 + exp(Y)))]
info[, SEGMENTO := ifelse(SCORE <= 125, "5. C",
                          ifelse(SCORE <= 349, "4. B",
                                 ifelse(SCORE <= 707, "3. A",
                                        ifelse(SCORE <= 829, "2. AA", "1. AAA"))))]



info[, SEGMENTO_ALINEADO := ifelse(SCORE_ALINEADO <= 65, "5. C",
                                   ifelse(SCORE_ALINEADO<= 201, "4. B",
                                          ifelse(SCORE_ALINEADO<= 690, "3. A",
                                                 ifelse(SCORE_ALINEADO <= 893, "2. AA", "1. AAA"))))]

summary(info$score419)
info[, SEGMENTO_GEN := ifelse(score419 <= 80, "5. C",
                              ifelse(score419 <= 644, "4. B",
                                     ifelse(score419 <= 904, "3. A",
                                            ifelse(score419 <= 953, "2. AA", "1. AAA"))))]

# Matriz dual de riesgo
info[SCORE_ALINEADO > 0][,table(SEGMENTO_ALINEADO, SEGMENTO_GEN)]
info[SCORE_ALINEADO > 0 & VarDep %in% c(1,3)][,table(SEGMENTO_ALINEADO, SEGMENTO_GEN)]

# Nueva variable dependiente para la fijación de reglas
info[, VarDep_Reglas := ifelse(VarDep %in% c(1,3), 1, 0)]
info[,table(SEGMENTO_ALINEADO, VarDep_Reglas)]
prop.table(info[,table(SEGMENTO_ALINEADO, VarDep_Reglas)],1)
info[, d_NumOpsVencidas := ifelse(numOpsVencidas101 == 0, 0, 1)]



#ARBOLITOS


arbol<-info[SEGMENTO_ALINEADO=="5. C" & VarDep_Reglas %in% c(0,1)][,.(VarDep_Reglas,
                                                                 antiguedadOpBanCoo388,antiguedadOpTcBanCoo390,
                                                                 DEUDA_TOTAL_SC_OP_24M,r_PROM_XVEN_SC_OP_6s24M,
                                                                 MAX_DIA_MOROSIDAD,r_DEUDA_TOTAL_SC_OP_12s24M,
                                                                 ANTIG_DOMICILIARIA,ANTIGUEDAD_OP_SC,
                                                                 numMesesInfoCredBanCoopD36M421,
                                                                 r_PROM_XVEN_SC_OP_6s24M,
                                                                 r_PROM_XVEN_SC_OP_24s36M,
                                                                 NOPE_APERT_SF_OP_24M,salTCDia031,
                                                                 numOpsAperturadas106

                                                                 
                                                                
                                                                 

)]
write.table(arbol,"Score.txt",sep="\t",row.names = FALSE)

dim(arbol)
# Reglas
info[, SEGMENTO_NEW := SEGMENTO_ALINEADO]
info[, SEGMENTO_NEW := ifelse(SEGMENTO_ALINEADO == "1. AAA" & (salTCDia031>0),"2. AA", SEGMENTO_NEW)]

#info[, SEGMENTO_NEW := ifelse(SEGMENTO_ALINEADO == "1. AAA" &(maxCCa36M108>0),"2. AA", SEGMENTO_NEW)]#se arregla bonito jeje
#info[, SEGMENTO_NEW := ifelse(SEGMENTO_ALINEADO == "1. AAA" &(numTCVencidas3M127>0),"2. AA", SEGMENTO_NEW)]#se arregla bonito jeje

info[, SEGMENTO_NEW := ifelse(SEGMENTO_ALINEADO == "1. AAA" &(numOpsAperturadas106>0),"2. AA", SEGMENTO_NEW)]#se arregla bonito jeje

info[, SEGMENTO_NEW := ifelse(SEGMENTO_ALINEADO == "1. AAA" &(r_DEUDA_TOTAL_SC_OP_12s24M>0.634),"2. AA", SEGMENTO_NEW)]#se arregla bonito jeje
info[, SEGMENTO_NEW := ifelse(SEGMENTO_ALINEADO == "1. AAA" &(antiguedadOpBanCoo388<51),"2. AA", SEGMENTO_NEW)]#se arregla bonito jeje

#info[, SEGMENTO_NEW := ifelse(SEGMENTO_ALINEADO == "1. AAA" &(antiguedadOpBanCoo388<51),"2. AA", SEGMENTO_NEW)]#se arregla bonito jeje


info[, SEGMENTO_NEW := ifelse(SEGMENTO_ALINEADO == "1. AAA" &(ANTIGUEDAD_OP_SC<27.9),"2. AA", SEGMENTO_NEW)]#se arregla bonito jeje

info[, SEGMENTO_NEW := ifelse(SEGMENTO_ALINEADO == "1. AAA" &(numMesesInfoCredBanCoopD36M421<30),"2. AA", SEGMENTO_NEW)]#se arregla bonito jeje



info[SCORE_ALINEADO > 0][,table(SEGMENTO_NEW, SEGMENTO_GEN)]
info[SCORE_ALINEADO > 0 & VarDep %in% c(1,3)][,table(SEGMENTO_NEW, SEGMENTO_GEN)]


prop.table(info[,table(SEGMENTO_NEW, VarDep_Reglas)],1)

info[,table(SEGMENTO_ALINEADO, VarDep)]
info[,table(SEGMENTO_NEW, SEGMENTO_ALINEADO)]


info[SEGMENTO == "5. C"][,table(d_NumOpsVencidas, VarDep_Reglas)]
prop.table(info[SEGMENTO == "5. C"][,table(d_NumOpsVencidas, VarDep_Reglas)],1)
prop.table(info[,table(d_NumOpsVencidas, VarDep_Reglas)],1)


clean<-info_clean[ModVal==1 & VarDep%in%c(0,1,2,3,4)][,.(VarDep,SCORE_RF_clean,SCORE_RF_dirty,SCORE_ALINEADO,SEGMENTO_NEW,SEGMENTO_ALINEADO)]
dirty<-info_dirty[ModVal==1 & VarDep%in%c(0,1,2,3,4)][,.(VarDep,SCORE_RF_clean,SCORE_RF_dirty,SCORE_ALINEADO,SEGMENTO_NEW,SEGMENTO_ALINEADO)]

#info_clean[ModVal==1 & VarDep%in%c(0,1,2,3,4)][,table(SEGMENTO_ALINEADO, VarDep)]
info_alineacion_segmentacion_new<-rbind(clean,dirty)
info_alineacion_segmentacion_new[,table(SEGMENTO_ALINEADO, VarDep)]
dim(info_alineacion_segmentacion_new)
info_alineacion_segmentacion_new[,table(SEGMENTO_NEW, VarDep)]

dim(info_alineacion_segmentacion_new)
#Tablita 5 rangos
res_total <- info_alineacion_segmentacion_new[, .(VarDep,SEGMENTO_NEW, SCORE_ALINEADO)]

res_total <- info_alineacion_segmentacion_new[, .(VarDep,SEGMENTO_ALINEADO, SCORE_ALINEADO)]

res_total <- data.table(Var = res_total$VarDep, Score = res_total$SCORE_ALINEADO)
res_total$Rango <- rango_score(res_total$Score)
res_total[, list(Min = min(Score), Max = max(Score)), by = Rango][order(Rango)]
res_total[, table(Rango,Var)]



res_total <- data.table(Var = res_total$SEGMENTO_ALINEADO, Score = res_total$SCORE_ALINEADO)
res_total$Rango <- rango_score(res_total$Score)
res_total[, list(Min = min(Score), Max = max(Score)), by = Rango][order(Rango)]
res_total[, table(Rango, Var)]


rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 6),0), labels = seq(1,5))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}
