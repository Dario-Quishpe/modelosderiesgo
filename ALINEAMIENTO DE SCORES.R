# Tablas performance - Muestra Validación
res_clean <- info_clean[ VarDep %in% c(0, 1,2,3,4) & ModVal == 1][, .(SCORE_RF_clean, VarDep)]
res_clean$Rango <- rango_score(res_clean$SCORE_RF_clean)
res_clean[, list(Min = min(SCORE_RF_clean), Max = max(SCORE_RF_clean)), by = Rango][order(Rango)]
res_clean[, table(Rango, VarDep)]
dim(clean)

res_dirty<-val_glm[][,.(SCORE_RF_dirty=Score,VarDep,Rango)]
val_glm
#res_dirty <- info_dirty[ VarDep %in% c(0, 1) & ModVal == 1][, .(Score_Dirty, VarDep)]
#res_dirty$Rango <- rango_score(res_dirty$Score_Dirty)
res_dirty[, list(Min = min(SCORE_RF_dirty), Max = max(SCORE_RF_dirty)), by = Rango][order(Rango)]
res_dirty[, table(Rango, VarDep)]
dirty[,table(VarDep)]

# Sub poblaciones para alineación

clean<-info_clean[ModVal==0 & VarDep%in%c(0,1,2,3,4)][,.(VarDep,Score_Clean=SCORE_RF_clean,Score_Dirty=SCORE_RF_dirty)]
dirty<-info_dirty[ModVal==0 & VarDep%in%c(0,1,2,3,4)][,.(VarDep,Score_Clean=SCORE_RF_clean,Score_Dirty=SCORE_RF_dirty)]

# Estimación Probabilidades para la base de validación 
val <- info_dirty[ModVal == 1 & VarDep %in% c(0,1,2,3,4)]
res_val <- data.table(Var=val$VarDep, Score=val$SCORE_RF_dirty)
res_val$Rango <- rango_score(res_val$Score)
res_val[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_val[,table(Rango, Var)]

# Estimación Probabilidades para la base de modelamiento
mod <- info_dirty[ModVal == 0 & VarDep %in% c(0,1,2,3,4)]
res_mod <- data.table(Var=mod$VarDep, Score=mod$SCORE_RF_dirty)
res_mod$Rango <- rango_score(res_mod$Score)
res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
res_mod[,table(Rango, Var)]


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


dim(clean)
dim(dirty)
#clean <- val_glm1[VarDep%in%c(0,1,2,3,4)][,.(VarDep,Score_Clean=SCORE_RF_clean,Score_Dirty=Score)]
#dirty <- val_glm[VarDep%in%c(0,1,2,3,4)][,.(VarDep,Score_Clean=SCORE_RF_clean,Score_Dirty=Score)]


#clean <- mod_glm1[VarDep%in%c(0,1,2,3,4)][,.(VarDep,Score_Clean=SCORE_RF_clean,Score_Dirty=Score)]
#dirty <- mod_glm[VarDep%in%c(0,1,2,3,4)][,.(VarDep,Score_Clean=SCORE_RF_clean,Score_Dirty=Score)]

##############################################################################
### La Alineación se realiza tomando como pivote uno de los segmentos
##############################################################################
#library(minpack.lm)

# Score Clean como pivote
# Score Clean como pivote
TPa <- tabla.perfali(y = clean$VarDep, score = clean$Score_Clean,n.rangos = 40)
# Elegir Score (De, Hasta o Punto Medio)
TPa$Score <- ( TPa$Hasta+TPa$De)/2
tab <- TPa

# Transform exp curve
curve.ini <- nlsLM(Score ~ a * exp(b * PD), start = list(a = 500, b = -1.5), data = tab)
plot(Score ~ PD, data = tab)
lines(seq(0, 1, 0.01), col = "red", predict(curve.ini, newdata = data.frame(PD = seq(0, 1, 0.01))))
summary(curve.ini)

# Score Dirty
TPa <- tabla.perfali(y = dirty$VarDep, score = dirty$Score_Dirty, n.rangos = 50)
# Elegir Score (De, Hasta o Punto Medio)
TPa$Score <- ( TPa$De+TPa$Hasta)/2
tab <- TPa

# Transform exp curve
curve.exp <- nlsLM(Score ~ a * exp(b * PD), start = list(a = 500, b = -1.5), data = tab)
plot(Score ~ PD, data = tab)
lines(seq(0, 1, 0.01), col = "red", predict(curve.exp, newdata = data.frame(PD = seq(0, 1, 0.01))))
curve.expbase <- curve.exp
summary(curve.expbase)
# Proceso de alineación
# 1) Estimar S0 = f(PD0) en Score base
fr <- function(pd) {
  a <-790.5509#1073.71031#1055.5227#1078.2338
  b <- -2.5754  #-2.7269
  score <- a * exp(b * pd)
  score <- round(score, 0)
  score[score <= 0] <- 0
  score[score > 999] <- 999
  return(score)
}

# 2) Estimación de los PDs: S2 = g(PD2) en Score alinear y despejar PD2. PD = log(Score/a)/b si g = exp
# Dirty
gc <- function(score) {
  a <-1038.43653#1279.2203#-1626.7281
  b <- -2.38493 #-4.23#-4.7281 
  #a <-1616.1481 
  #b <- -4.7102 
  pd <- log(score / a) / b
  return(pd)
}

rm(list = c("clean", "dirty","info_alineacion"))

# 3 Reemplazar PD1 de 2 en 1 para encontrar score alineado
clean<-cbind(clean,SEGMENTACION_SF=0)

dirty<-cbind(dirty,SEGMENTACION_SF=1)

info_alineacion<-rbind(clean,dirty)
info_alineacion[, SCORE_ALINEADO := ifelse(SEGMENTACION_SF == 0, Score_Clean, fr(gc(Score_Dirty)))]

info[,SCORE_ALINEADO := ifelse(SEGMENTACION_SF == 0, SCORE_RF_clean, fr(gc(SCORE_RF_dirty)))]

info_clean[,SCORE_ALINEADO :=  SCORE_RF_clean]
info_dirty[,SCORE_ALINEADO :=  fr(gc(SCORE_RF_dirty))]

clean<-info_clean[ModVal==1 & VarDep%in%c(0,1,2,3,4)][,.(VarDep,SCORE_RF_clean,SCORE_RF_dirty,SCORE_ALINEADO,SEGMENTO_NEW)]
dirty<-info_dirty[ModVal==1 & VarDep%in%c(0,1,2,3,4)][,.(VarDep,SCORE_RF_clean,SCORE_RF_dirty,SCORE_ALINEADO,SEGMENTO_NEW)]
info_alineacion<-rbind(clean,dirty)
#info_alineacion_segmentacion_new<-rbind(clean,dirty)
res_total <- info_alineacion[, .(VarDep, SCORE_ALINEADO)]
#res_total <- info_alineacion[, .(VarDep, SCORE_ALINEADO)]

res_total[,.N,by=VarDep][order(VarDep)]
clean[,.N,VarDep][order(VarDep)]
dirty[,.N,VarDep][order(VarDep)]
info[][,table(VarDep,SEGMENTO_ALINEADO)]

res_gb <- info[VarDep %in% c(0, 1) & ModVal == 1][, .(VarDep, SCORE_ALINEADO)]
res_total <- info_alineacion[, .(VarDep, SCORE_ALINEADO)]

res_total <- info[, .(SEGMENTO_NEW, SCORE_ALINEADO)]


# Estimación Probabilidades para la base de bueno y malos
res_gb <- data.table(Var = res_gb$VarDep, Score = res_gb$SCORE_ALINEADO)
res_gb$Rango <- rango_score(res_gb$Score)
res_gb[, list(Min = min(Score), Max = max(Score)), by = Rango][order(Rango)]
res_gb[, table(Rango, Var)]



res_total <- data.table(Var = res_total$VarDep, Score = res_total$SCORE_ALINEADO)
res_total$Rango <- rango_score(res_total$Score)
res_total[, list(Min = min(Score), Max = max(Score)), by = Rango][order(Rango)]
res_total[, table(Rango, Var)]


res_total <- data.table(Var = res_total$SEGMENTO_NEW, Score = res_total$SCORE_ALINEADO)
res_total$Rango <- rango_score(res_total$Score)
res_total[, list(Min = min(Score), Max = max(Score)), by = Rango][order(Rango)]
res_total[, table(Var,Rango)]


rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}

# PD punto a punto
crear.codifali <- function(x, y) {
  tab <- prop.table(table(x, y), 1)
  tab <- data.frame(names(tab[, 2]), as.numeric(tab[, 2]), as.numeric(table(x)))
  colnames(tab) <- c("nomb", "porcm", "tot")
  tab$Porctot <- round(tab$tot / sum(tab$tot), 4)
  rownames(tab) <- NULL
  tab$cod <- tab$porcm
  tab <- tab[, c(1, 2)]
  tab[, 1] <- as.numeric(as.character(tab[, 1]))
  colnames(tab) <- c("Score", "PD")
  return(tab)
}

# Tabla de performance
tabla.perfali <- function(y, score, n.rangos = 10) {
  prb <- seq(from = 0, to = 1, length.out = n.rangos + 1)
  decil <- quantile(score, probs = prb)
  decil <- unique(decil)
  decil <- unname(decil)
  if (length(decil) == 2) decil <- c(0, decil)
  f.decil <- cut(score, decil)
  tp <- table(f.decil, y)
  
  # Cambio orden deciles 10 a 1
  tp[] <- tp[(length(decil) - 1):1, ]
  
  if (ncol(tp) >= 4) {
    colaux <- rowSums(x = as.data.frame.matrix(tp[, 4:ncol(tp)]), na.rm = TRUE)
    tp <- cbind(tp, colaux)
  } else {
    colaux <- matrix(data = 0, nrow = nrow(tp), ncol = 4 - ncol(tp))
    tp <- cbind(tp, colaux)
  }
  
  nclie <- rowSums(x = tp[, 1:4], na.rm = TRUE)
  pclie <- round(nclie / sum(nclie), 4)
  acumclie <- round(cumsum(pclie), 4)
  nexit <- tp[, 2]
  nfrac <- tp[, 1]
  pfrac <- round(nfrac / sum(nfrac), 4)
  acumfrac <- round(cumsum(pfrac), 4)
  
  # Exit rate, elimino rechazados
  rexitmod <- nexit / (nclie - tp[, 4])
  nexitmod <- round(rexitmod * tp[, 4] + nexit)
  pexitmod <- round(nexitmod / sum(nexitmod), 4)
  acumexitmod <- round(cumsum(pexitmod), 4)
  rexit <- round(nexitmod / nclie, 4)
  acumrexit <- round(cumsum(nexitmod) / cumsum(nclie), 4)
  
  decil0 <- c(1, (decil[-c(1, length(decil))]) + 1)[(length(decil) - 1):1]
  decilf <- c(decil[-c(1, length(decil))], 999)[(length(decil) - 1):1]
  
  tp <- data.frame(
    (length(decil) - 1):1, decil0, decilf, nclie, pclie, acumclie, nfrac, pfrac, acumfrac, 
    nexitmod, pexitmod, acumexitmod, rexit, acumrexit
  )
  
  colnames(tp) <- c(
    "Decil", "De", "Hasta", "NClie", "%Clie", "%CumClie", "NFrac", "%Frac", "%CumFrac", 
    "NExit", "%Exit", "%CumExit", "%Exit(decil)", "%CumExit(decil)"
  )
  
  rownames(tp) <- NULL
  tp <- tp[, c("De", "Hasta", "NClie", "NExit")]
  tp$De <- round(tp$De, 0)
  tp$Hasta <- round(tp$Hasta, 0)
  tp$PD <- tp$NExit / tp$NClie
  return(tp)
}
