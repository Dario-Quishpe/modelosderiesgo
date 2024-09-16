# Regresión Logística

dir.p <- getwd()
dir.b <- paste0(dir.p, "/BDD")
dir.s <- paste0(dir.p, "/Scripts")
list.files()
load("InfoModelamiento.RData")
library(twosamples)
library(tidyverse)

#setwd(dir.b)
#list.files()
#load("InfoModelamiento.RData")
dim(info)

# Comparativa de funciones de densidad
info[, INGRESOS_mod := ifelse(INGRESOS > 10000, 10000, INGRESOS)]
info %>% dplyr::filter(ModVal == 0 & VarDep %in% c(0,1)) %>% 
      select(INGRESOS_mod, VarDep) %>% 
      mutate(Sujetos = factor(VarDep, labels = c("Buenos", "Malos"))) %>% 
      ggplot(aes(x = INGRESOS_mod, fill = Sujetos)) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = c("gray60", "orangered2"))


# Comparativa de funciones de distribución (cambiar aqui res_mod)
buenos <- ecdf(info %>% dplyr::filter(ModVal == 0 & VarDep == 0) %>% pull(INGRESOS_mod))
malos <- ecdf(info %>% dplyr::filter(ModVal == 0 & VarDep == 1) %>% pull(INGRESOS_mod))

grid_var <- unique(info %>% dplyr::filter(ModVal == 0) %>% pull(INGRESOS_mod))
prob_acumulada_ecdf_b <- buenos(v = grid_var)
prob_acumulada_ecdf_m <- malos(v = grid_var)

df_ecdf <- data.frame(var = grid_var, buenos = prob_acumulada_ecdf_b, malos = prob_acumulada_ecdf_m) %>%
      pivot_longer(cols = c(buenos, malos), names_to = "Marca", values_to = "ecdf")

grafico_ecdf <- ggplot(data = df_ecdf, aes(x = var, y = ecdf, color = Marca)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("gray60", "orangered1")) +
      labs(color = "Marca", y = "Probabilidad acumulada", x = "NOPE_VENC_SCE_24M") +
      theme_bw() +
      theme(legend.position = "bottom", plot.title = element_text(size = 12))

grafico_ecdf

abs_dif <-  abs(prob_acumulada_ecdf_b - prob_acumulada_ecdf_m)
distancia_ks <- max(abs_dif)
paste("Distancia Kolmogorov–Smirnov:", distancia_ks)
indice_ks <- which.max(abs_dif)
grafico_ecdf + 
      geom_segment(aes(x = grid_var[indice_ks], xend = grid_var[indice_ks],
                       y = prob_acumulada_ecdf_b[indice_ks], yend = prob_acumulada_ecdf_m[indice_ks]),
                   arrow = arrow(ends = "both", length = unit(0.2,"cm")), color = "black")

#INDENTIFICAR LAS VARIABLES NUMERICAS 
variable<-data.table(
  Variable=colnames(info),
  Tipo=unname(unlist(sapply(info,function(x){class(x)[1]})))
)

var<-variable[Tipo  %in% c("numeric","integer") ][["Variable"]]
var_num <- variable[Tipo %in% c("numeric", "integer")][["Variable"]]
mod_num <- info[VarDep %in% c(0,1) & ModVal == 0][,var_num, with = FALSE]

# Valor de información
res <- info %>% mutate(TIPO_VIVIENDA = ifelse(TIPO_VIVIENDA %in% c("F", "A", "ARRENDADA"), "ARRENDADA",
                                               ifelse(TIPO_VIVIENDA %in% c("P", "PRESTADA"), "PRESTADA",
                                                      ifelse(TIPO_VIVIENDA %in% c("VIVE CON FAMILIARES"), "FAMILIARES", "PROPIA")))) %>% 
      dplyr::filter(VarDep %in% c(0,1)) %>% 
      mutate(Marca = ifelse(VarDep == 0, "Buenos", "Malos")) %>% 
      select(TIPO_VIVIENDA, Marca) %>% 
      group_by(TIPO_VIVIENDA, Marca) %>% 
      dplyr::summarise(Conteo = n()) 

library(plyr)
ce <- ddply(res, "TIPO_VIVIENDA", transform, Porcentaje = Conteo / sum(Conteo) * 100)
ggplot(ce, aes(x=TIPO_VIVIENDA, y=Porcentaje, fill=Marca)) + geom_bar(stat="identity") + 
      scale_fill_manual(values=c('gray60', 'orangered1'))

options(scipen=9999)
# Medición del poder predictivo de las variables
#k<-0
#ksresults<-numeric(1000)
#adresults<-numeric(1000)
#for(i in var){
#  k<-k+1
#  aux<-info[, c("ModVal","VarDep",i), with=FALSE]
#  res <- aux[VarDep%in%c(0,1)&ModVal==0]
#  res[is.na(res)]<-0
#  buenos<-res[VarDep%in%c(0)][,c(i),with = FALSE]
#  malos<-res[VarDep%in%c(1)][,c(i),with = FALSE]
#  ksresults[i]<-ks.test(as.numeric(buenos[[1]]),as.numeric(malos[[1]]))$statistic
#  adresults[i]<-ad_stat(as.numeric(buenos[[1]]), as.numeric(malos[[1]]))
#  
#}
options(scipen=99999)
#CALCULAR VALORES PARA TABLA UNIVARIANTE : 
Matriz_univariados <- sapply(var, function(columna_name) {
  columna <- info[[columna_name]]
  perdidos <- sum(is.na(columna)) 
  total <- length(columna)
  nulos <- sum(columna == 0, na.rm = TRUE) 
  min_value <- min(columna, na.rm = TRUE) 
  max_value <- max(columna, na.rm = TRUE) 
  median_value <- median(columna, na.rm = TRUE)
  mean_value <- mean(columna, na.rm = TRUE) 
  sd_value <- sd(columna, na.rm = TRUE) 
  
  # Percentiles específicos
  percentiles <- quantile(columna, probs = c(0.01, 0.02, 0.05, 0.10, 0.25,0.50, 0.75, 0.90, 0.95, 0.98, 0.99), na.rm = TRUE)
  
  # Combina todas las métricas en un vector
  resp <- c(columna_name,
    perdidos,
    perdidos / total , # % perdidos
    nulos,
    nulos / total , # % nulos
    min_value,
    percentiles,
    max_value,
    mean_value,
    sd_value
  )
  
  return(resp)
})
dim(Matriz_univariados)
Matriz_univariados <- t(Matriz_univariados)
dim(Matriz_univariados)
view(Matriz_univariados)
colnames(Matriz_univariados) <- c("Variables",
  "Perdidos", "% Perdidos", "Nulos", "% Nulos", "Mínimo", 
  "P1", "P2", "P5", "P10", "P25", "Mediana", "P75", "P90", "P95", "P98", "P99", 
  "Máximo", "Media", "Desviación Estándar"
)
if (!is.data.table(Matriz_univariados)) {
  Matriz_univariados <- as.data.table(Matriz_univariados)
}
convertir_coma <- function(x) {
  if (is.numeric(x)) {
    return(gsub("\\.", ",", as.character(x)))
  }
  return(x)
}


# Aplicar la función a todo el data.table
Matriz_univariados<- Matriz_univariados[, lapply(.SD, convertir_coma)]
view(Matriz_univariados)
write.table(Matriz_univariados,"Tabla_UNIVARIADOS.csv",sep = "\t",row.names = FALSE,fileEncoding = "UTF-8")
write.table(Matriz_univariados,"Tabla_Univariados.txt",sep="\t",row.names = FALSE,fileEncoding = "UTF-8")
write.table(Matriz_univariados,"Tabla_Univariados.xls",sep="\t",row.names = FALSE,fileEncoding = "UTF-8")
#CALCULAR KS Y AD
Trabajo_results <- sapply(var, function(x) {
  # Calcular la proporción de NA en la columna
  na_proportion <- sum(is.na(info[[x]])) / nrow(info)
  
  # Si más del 75% de los valores son NA, omitir los cálculos
  if (na_proportion > 0.75) {
    return(list(Variable = x, KS = NA, AD = NA, NA_Proportion = na_proportion))
  } else {
    # Reemplazar NA con 0 en las columnas 'malo' y 'bueno'
    malo <- info |> filter(VarDep == 1) |> pull(x) |> replace_na(0)
    bueno <- info |> filter(VarDep == 0) |> pull(x) |> replace_na(0)
    
    # Realizar los tests KS y AD
    ks_test <- ks.test(bueno, malo)
    adtest <- ad_stat(bueno, malo)
    
    return(list(Variable = x, KS = ks_test$statistic, AD = adtest, NA_Proportion = na_proportion))
  }
})

# Convertir los resultados en un data frame para facilitar la visualización
Trabajo_Grupal <- as.data.frame(do.call(rbind, Trabajo_results))
print(dim(Trabajo_Grupal))


write.table(Trabajo_Grupal,"Tabla_KS_AD.xls")
#CLEAN


#CALCULAR KS Y AD
Trabajo_results <- sapply(var, function(x) {
  # Calcular la proporción de NA en la columna
  na_proportion <- sum(is.na(info_clean[[x]])) / nrow(info_clean)
  
  # Si más del 75% de los valores son NA, omitir los cálculos
  if (na_proportion > 0.5) {
    return(list(Variable = x, KS = NA, AD = NA, NA_Proportion = na_proportion))
  } else {
    # Reemplazar NA con 0 en las columnas 'malo' y 'bueno'
    malo <- info_clean |> filter(VarDep == 1) |> pull(x) |> replace_na(0)
    bueno <- info_clean |> filter(VarDep == 0) |> pull(x) |> replace_na(0)
    
    # Realizar los tests KS y AD
    ks_test <- ks.test(bueno, malo)
    adtest <- ad_stat(bueno, malo)
    
    return(list(Variable = x, KS = ks_test$statistic, AD = adtest, NA_Proportion = na_proportion))
  }
})

# Convertir los resultados en un data frame para facilitar la visualización
Trabajo_Grupal <- as.data.frame(do.call(rbind, Trabajo_results))
print(dim(Trabajo_Grupal))



write.table(Trabajo_Grupal,"Tabla_KS_AD_Clean.xls")


#DIRTY


#CALCULAR KS Y AD
Trabajo_results <- sapply(var, function(x) {
  # Calcular la proporción de NA en la columna
  na_proportion <- sum(is.na(info_dirty[[x]])) / nrow(info_dirty)
  
  # Si más del 75% de los valores son NA, omitir los cálculos
  if (na_proportion > 0.75) {
    return(list(Variable = x, KS = NA, AD = NA, NA_Proportion = na_proportion))
  } else {
    # Reemplazar NA con 0 en las columnas 'malo' y 'bueno'
    malo <- info_dirty |> filter(VarDep == 1) |> pull(x) |> replace_na(0)
    bueno <- info_dirty |> filter(VarDep == 0) |> pull(x) |> replace_na(0)
    
    # Realizar los tests KS y AD
    ks_test <- ks.test(bueno, malo)
    adtest <- ad_stat(bueno, malo)
    
    return(list(Variable = x, KS = ks_test$statistic, AD = adtest, NA_Proportion = na_proportion))
  }
})

# Convertir los resultados en un data frame para facilitar la visualización
Trabajo_Grupal <- as.data.frame(do.call(rbind, Trabajo_results))
print(dim(Trabajo_Grupal))
write.table(Trabajo_Grupal,"Tabla_KS_AD_Dirty.xls")




#PARA LOS CLEAN DEL PRIMER RANGO JEJEJE

#CALCULAR KS Y ADSEGMENTO_ALINEADO=="5. C"
Trabajo_results <- sapply(var, function(x) {
  # Calcular la proporción de NA en la columna
  na_proportion <- sum(is.na(info[SEGMENTO_NEW=="5. C"][[x]])) / nrow(info[SEGMENTO_NEW=="5. C"])
  
  # Si más del 75% de los valores son NA, omitir los cálculos
  if (na_proportion > 0.5) {
    return(list(Variable = x, KS = NA, AD = NA))
  } else {
    # Reemplazar NA con 0 en las columnas 'malo' y 'bueno'
    malo <- info[SEGMENTO_NEW=="5. C"] |> filter(VarDep_Reglas == 1) |> pull(x) |> replace_na(0)
    bueno <- info[SEGMENTO_NEW=="5. C"] |> filter(VarDep_Reglas == 0) |> pull(x) |> replace_na(0)
    
    # Realizar los tests KS y AD
    ks_test <- ks.test(bueno, malo)
    #adtest <- ad_stat(bueno, malo)
    
    return(data.frame(Variable = x, KS = ks_test$statistic))
  }
})

# Convertir los resultados en un data frame para facilitar la visualización
#Trabajo_results
print(dim(Trabajo_results))



write.table(t(Trabajo_results),"Tabla_KS_AD_C_varDEP.xls")


