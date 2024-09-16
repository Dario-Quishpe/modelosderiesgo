#DATOS PARA RESUMEN DEBER#1
res <- c("prbb_peorNivelRiesgoValorOpBanCooComD415",
         "r_PROM_MAX_DVEN_M_OP_3s6M",
         "prbm_maxMorosidadCoo133",
         "numAcreedoresOpBanCooComD414_MOD",
         "prbm_MVALVEN_SCE_3M",
         "r_salTotOp040smaxMontoOp096",
         "prbb_SALDO_PROMEDIO_AHORRO",
         "r_PROM_MAX_DVEN_SC_OP_3s6M",
         "prbm_PROM_NDI_SCE_3M")
Matriz_univariados <- sapply(res, function(columna_name) {
  columna <- mod[[columna_name]]
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
Matriz_univariados<-t(Matriz_univariados)
view(Matriz_univariados)
colnames(Matriz_univariados) <- c("Variables",
                                  "Perdidos", "% Perdidos", "Nulos", "% Nulos", "Mínimo", 
                                  "P1", "P2", "P5", "P10", "P25", "Mediana", "P75", "P90", "P95", "P98", "P99", 
                                  "Máximo", "Media", "Desviación Estándar"
)
write.table(Matriz_univariados,"Tabla_Univariados_Modelo.xls",sep="\t",row.names = FALSE,fileEncoding = "UTF-8")


#GRaficas para el deber 
for(i in c(1:length(res))){
  # Comparativa de funciones de distribución (cambiar aqui res_mod)
  buenos <- ecdf(mod %>% dplyr::filter(ModVal == 0 & VarDep == 0) %>% pull(res[i]))
  malos <- ecdf(mod %>% dplyr::filter(ModVal == 0 & VarDep == 1) %>% pull(res[i]))
  
  grid_var <- unique(mod %>% dplyr::filter(ModVal == 0) %>% pull(res[i]))
  prob_acumulada_ecdf_b <- buenos(v = grid_var)
  prob_acumulada_ecdf_m <- malos(v = grid_var)
  
  df_ecdf <- data.frame(var = grid_var, buenos = prob_acumulada_ecdf_b, malos = prob_acumulada_ecdf_m) %>%
    pivot_longer(cols = c(buenos, malos), names_to = "Marca", values_to = "ecdf")
  
  grafico_ecdf <- ggplot(data = df_ecdf, aes(x = var, y = ecdf, color = Marca)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("gray60", "orangered1")) +
    labs(color = "Marca", y = "Probabilidad acumulada", x = res[i]) +
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
  
}

