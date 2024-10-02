
# PRUEBA TECNICA PRONACA ------------------------------------------------
# DARIO QUISHPE , 17/09/2024 

## PREGUNTA 1  -------------------------------------------------------------
# CARGA DE DATOS y EXPLORACION DE DATOS
install.packages(c("readxl","ranger","h2o","data.table","xlsx")) 
suppressMessages(library(readxl))
suppressMessages(library(ranger))
suppressMessages(library(h2o))
suppressMessages(library(data.table))
suppressMessages(library(ROSE))
suppressMessages(library(xlsx))

dir<-getwd()
dir.carpeta<-paste0(dir,"/Bases ejercicio 1 y 2")
list.files(path = dir.carpeta)
#Base Usd y Kilogramos.xlsx
Base_U_K<-read_excel(path = paste0(dir.carpeta,"/Base Usd y Kilogramos.xlsx"))
Base_U_K<-as.data.table(Base_U_K)
colSums(is.na(Base_U_K))#existen valores faltantes
Base_U_K[is.na(KG)]
Base_U_K[is.na(KG), `:=`(KG = 0, USD = 0)] #reemplazamos con 0's
Base_U_K <- Base_U_K[FechaFechaAño != "FechaFechaAño"] #eliminando resgitros erroneos
Base_U_K[, FechaFechaDia := as.Date(FechaFechaDia, format = "%d/%m/%Y")]
str(Base_U_K)
#"Base03092024.xlsx"
Base03092024<-read_excel(path = paste0(dir.carpeta,"/Base03092024.xlsx"))
Base03092024<-as.data.table(Base03092024)
colSums(is.na(Base03092024))
Base03092024[is.na(FechaFechaAño),]# existen dos columnas vacias
Base03092024<-na.omit(Base03092024)
Base03092024 <- Base03092024[FechaFechaAño != "FechaFechaAño"] #eliminando resgitros erroneos
Base03092024[, FechaFechaDia := as.Date(FechaFechaDia, format = "%d/%m/%Y")]
str(Base03092024)
#"RuteroPrueba.xlsx"
RuteroPrueba<-read_excel(path = paste0(dir.carpeta,"/RuteroPrueba.xlsx"))
RuteroPrueba<-as.data.table(RuteroPrueba)
colSums(is.na(RuteroPrueba))
str(RuteroPrueba)

## OBTENIENDO INFORMACION DE LA DATA HISTORICA -----------------------------

Resumen_Base03092024_1 <- Base03092024[, .N, by = .(FechaFechaDia, Cliente,`ItemNegocioLinea Negocio`)][
                                    , .SD[which.max(N)], by = .(FechaFechaDia, Cliente)]

Resumen_Base03092024_2 <- Base03092024[, .N, by = .(FechaFechaDia, Cliente,ItemNegocioFamilia)][
  , .SD[which.max(N)], by = .(FechaFechaDia, Cliente)]

Resumen_Base03092024_3 <- Base03092024[, .N, by = .(FechaFechaDia, Cliente,`ClienteRegional GeograficaRegion`)][
  , .SD[which.max(N)], by = .(FechaFechaDia, Cliente)]

Resumen_Base03092024_4 <- Base03092024[, .N, by = .(FechaFechaDia, Cliente,`Tipo PlataformaTipo PlataformaTipoPlataforma`)][
  , .SD[which.max(N)], by = .(FechaFechaDia, Cliente)]

Resumen_Base03092024_5 <- Base03092024[, .N, by = .(FechaFechaDia, Cliente, ItemNegocioFamilia)][
  order(-FechaFechaDia), .SD[1], by = .(Cliente)]

Resumen_Base03092024_6 <- Base03092024[, .N, by = .(FechaFechaDia, Cliente, `ItemItem PadreCódigo Padre`)][
  order(-FechaFechaDia), .SD[1], by = .(Cliente)]

Resumen_Base03092024_7 <- Base03092024[, .N, by = .(FechaFechaDia, Cliente,`ItemItem PadreCódigo Padre`)][
  , .SD[which.max(N)], by = .(FechaFechaDia, Cliente)]


setnames(Resumen_Base03092024_1, c("ItemNegocioLinea Negocio","N"), 
                                c("ItemNegocioLineaNegocio_mas_frec","Frec_ItemNegocioLineaNegocio_mas_frec"))

setnames(Resumen_Base03092024_2, c("ItemNegocioFamilia","N"), 
         c("ItemNegocioFamilia_mas_frec","Frec_ItemNegocioFamilia_mas_frec"))

setnames(Resumen_Base03092024_3, c("ClienteRegional GeograficaRegion","N"), 
         c("ClienteRegionalGeograficaRegion_mas_frec","Frec_ClienteRegionalGeograficaRegion_mas_frec"))

setnames(Resumen_Base03092024_4, c("Tipo PlataformaTipo PlataformaTipoPlataforma","N"), 
         c("TipoPlataformaTipoPlataformaTipoPlataforma_mas_frec","Frec_TipoPlataformaTipoPlataformaTipoPlataforma_mas_frec"))

setnames(Resumen_Base03092024_5, c("ItemNegocioFamilia","N"), 
         c("ItemNegocioFamilia_mas_actual","Frec_ItemNegocioFamilia_mas_actual"))

setnames(Resumen_Base03092024_6, c("ItemItem PadreCódigo Padre","N"), 
         c("ItemItem_PadreCódigo_Padre_mas_actual","Frec_ItemItem_PadreCódigo_Padre_mas_actual"))

setnames(Resumen_Base03092024_7, c("ItemItem PadreCódigo Padre","N"), 
         c("ItemItem_PadreCódigo_Padre_mas_frec","Frec_ItemItem_PadreCódigo_Padre_mas_frec"))

Resumen_Base03092024 <- merge(
  Resumen_Base03092024_1, 
  Resumen_Base03092024_2,
  by = c("FechaFechaDia", "Cliente"), 
  all = FALSE 
)
Resumen_Base03092024 <- merge(
  Resumen_Base03092024, 
  Resumen_Base03092024_3,
  by = c("FechaFechaDia", "Cliente"), 
  all = FALSE 
)

Resumen_Base03092024 <- merge(
  Resumen_Base03092024, 
  Resumen_Base03092024_4,
  by = c("FechaFechaDia", "Cliente"), 
  all = FALSE 
)
Resumen_Base03092024 <- merge(
  Resumen_Base03092024, 
  Resumen_Base03092024_5,
  by = c("FechaFechaDia", "Cliente"), 
  all = FALSE 
)

Resumen_Base03092024 <- merge(
  Resumen_Base03092024, 
  Resumen_Base03092024_6,
  by = c("FechaFechaDia", "Cliente"), 
  all = FALSE 
)

Resumen_Base03092024 <- merge(
  Resumen_Base03092024, 
  Resumen_Base03092024_7,
  by = c("FechaFechaDia", "Cliente"), 
  all = FALSE 
)

str(Resumen_Base03092024)
dim(Resumen_Base03092024)

#CONSOLIDANDO LA BASE GENERAL
data_consolidada<-merge(
  Base_U_K,
  Resumen_Base03092024,#Añadimos la informacion encontrada anteriormente
  by = c("Cliente"),
  all.x = TRUE
)
#data_consolidada[,.N,by=ItemNegocioLineaNegocio_mas_frec]

data_consolidada<-merge(
  data_consolidada,
  RuteroPrueba,#Añadimos la informacion adicional sobre la frecuencia rutero
  by="Cliente",
  all.x=TRUE
)
dim(data_consolidada)
## ANALSIS DE LA VARIABLE DEPENDIENTE = ItemNegocioFamilia --------------
data_consolidada[,.N,by=ItemNegocioFamilia_mas_actual][order(N,decreasing = TRUE)]
save(data_consolidada, file = "data_consolidada.Rdata")
data_remuestreo<-data_consolidada[,.(Cliente,FechaFechaDia.x,FechaFechaTrimestre,FechaFechaMes,
                                     `ItemNegocioLineaNegocio_mas_frec`,`TipoPlataformaTipoPlataformaTipoPlataforma_mas_frec`,
                                     `ClienteRegionalGeograficaRegion_mas_frec`,
                                      ClienteCanalCanal,ClienteCanalSubcanal,KG,USD,
                                     ItemNegocioFamilia_mas_frec,ItemNegocioFamilia_mas_actual,
                                     ItemItem_PadreCódigo_Padre_mas_actual,ItemItem_PadreCódigo_Padre_mas_frec,`Frecuencia rutero`)]

data_consolidada[Cliente==225642]
class(data_remuestreo)
# notemos que hay bastante desvalance entre las categorias, por lo cual se procede a 
# utilizar tecnicas para balancear la base.

## Remuestreo -----------------
#set.seed(1723951065)
#partial_balance_dt <- function(dt, target_col = "ItemNegocioFamilia_mas_frec", balance_factor = 2, min_size = 100) {
#  # Verificar si la columna de la var dependiente existe
#  if (!(target_col %in% names(dt))) {
#    stop(paste("La columna", target_col, "no existe en el data.table"))
#  }
#  
#  # Asegurarse de que la vardep sea un factor
#  dt[, (target_col) := as.factor(get(target_col))]
#  
#  # Remover NA 
#  dt <- dt[!is.na(get(target_col))]
#  
#  # Calcular el tamaño de cada categoria
#  class_sizes <- dt[, .(N = .N), by = target_col]
#  if (nrow(class_sizes) == 0) {
#    stop("No hay datos válidos después de remover NA")
#  }
#  setorder(class_sizes, N)  # Ordenar 
#  
#  # Calcular el nuevo tamaño para cada clase categoria
#  class_sizes[, new_size := pmax(
#    min_size,  # Asegurar un mínimo de observaciones
#    pmin(
#      N * balance_factor, 
#      class_sizes$N[.N]  # No exceder el tamaño de la categ más grande
#    )
#  )]
#  
#  print("Tamaños de clase calculados:")
#  print(class_sizes)
#  
#  # Balancear cada categoria
#  balanced_dt <- dt[, {
#    current_size <- .N
#    target_size <- class_sizes[get(target_col) == .BY[[1]], new_size]
#    
#    # Imprimir información de depuración
#    cat("Procesando categoría:", .BY[[1]], "\n")
#    cat("Tamaño actual:", current_size, "\n")
#    cat("Tamaño objetivo:", target_size, "\n")
#    
#    if (length(target_size) == 0) {
#      warning(paste("No se encontró tamaño objetivo para la categoría", .BY[[1]]))
#      return(.SD)
#    }
#    
#    if (current_size < target_size) {
#      indices <- sample(current_size, target_size, replace = TRUE)
#      .SD[indices]
#    } else if (current_size > target_size) {
#      .SD[sample(.N, target_size)]
#    } else {
#      .SD
#    }
#  }, by = target_col]
#  
#  return(balanced_dt)
#}



library(data.table)

set.seed(1723951065)

balance_all_categories <- function(dt, target_col = "ItemItem_PadreCódigo_Padre_mas_actual", min_size = 100) {
  # Verificar si la columna de la var dependiente existe
  if (!(target_col %in% names(dt))) {
    stop(paste("La columna", target_col, "no existe en el data.table"))
  }
  
  # Asegurarse de que la vardep sea un factor
  dt[, (target_col) := as.factor(get(target_col))]
  
  # Remover NA 
  dt <- dt[!is.na(get(target_col))]
  
  # Calcular el tamaño de cada categoria
  class_sizes <- dt[, .(N = .N), by = target_col]
  if (nrow(class_sizes) == 0) {
    stop("No hay datos válidos después de remover NA")
  }
  
  # Determinar el tamaño objetivo para todas las categorías
  target_size <- max(min_size, max(class_sizes$N)/2)
  
  print(paste("Tamaño objetivo para todas las categorías:", target_size))
  
  # Balancear cada categoria
  balanced_dt <- dt[, {
    current_size <- .N
    
    # Imprimir información de depuración
    cat("Procesando categoría:", .BY[[1]], "\n")
    cat("Tamaño actual:", current_size, "\n")
    cat("Tamaño objetivo:", target_size, "\n")
    
    if (current_size < target_size) {
      indices <- sample(current_size, target_size, replace = TRUE)
      .SD[indices]
    } else if (current_size > target_size) {
      .SD[sample(.N, target_size)]
    } else {
      .SD
    }
  }, by = target_col]
  
  return(balanced_dt)
}

# Función para analizar la distribución 
analyze_distribution <- function(dt, target_col = "ItemNegocioFamilia_mas_frec") {
  dist <- dt[, .N, by = target_col]
  setorder(dist, -N)
  print(dist)
  
  cat("\nResumen de la distribución:\n")
  print(summary(dist$N))
  
  cat("\nProporciones relativas:\n")
  print(dist[, .(Category = get(target_col), Proportion = N / sum(N))])
}


cat("Distribución original:\n")
analyze_distribution(data_remuestreo)

# Balancear  data
data_modelamiento <- balance_all_categories(data_remuestreo, min_size = 100)
#data_modelamiento<-data_remuestreo
#REsultado
save(data_modelamiento , file = "data_modelamiento.Rdata")
data_modelamiento[,.N,by=ItemNegocioFamilia_mas_frec][order(N,decreasing = TRUE)]
data_modelamiento[,.N,by=ItemNegocioFamilia_mas_actual][order(N,decreasing = TRUE)]

#PARTICION= MODELAMIENTO 60% , EVALUACION40%
marca<-sample(1:nrow(data_modelamiento),size=floor(0.4*nrow(data_modelamiento)),replace = FALSE)
data_modelamiento[,ModVal:= 1:nrow(data_modelamiento)]
data_modelamiento[,ModVal:=ifelse(ModVal %in% marca,1,0)]
data_modelamiento[,.N,by=ModVal]
data_modelamiento[,table(ItemItem_PadreCódigo_Padre_mas_actual,ModVal)]
dim(data_modelamiento)

##AJUSTANDO LOS MODELOS--------------------------------------------------------------

library(h2o)

h2o.init(ip = "localhost", nthreads = -1, max_mem_size = "6G")
mod <- data_modelamiento[ModVal == 0]
# Import a sample binary outcome train/test set into H2O
vars <- c("Cliente", "FechaFechaDia.x", "FechaFechaTrimestre", "FechaFechaMes",
          "ItemNegocioLineaNegocio_mas_frec", "TipoPlataformaTipoPlataformaTipoPlataforma_mas_frec",
          "ClienteRegionalGeograficaRegion_mas_frec", "ClienteCanalCanal", "ClienteCanalSubcanal",
          "KG", "USD", "ItemNegocioFamilia_mas_frec","ItemNegocioFamilia_mas_actual","ItemItem_PadreCódigo_Padre_mas_actual",
          "ItemItem_PadreCódigo_Padre_mas_frec","Frecuencia rutero"
)

mod_em <- as.h2o(x = setDT(mod)[, vars, with=FALSE])

# Identify predictors and response
y_em <- "ItemItem_PadreCódigo_Padre_mas_actual"; x_em <- setdiff(names(mod_em), y_em)

# For binary classification, response should be a factor
mod_em[,y_em] <- as.factor(mod_em[,y_em])

# Number of CV folds (to generate level-one data for stacking)
nfolds <- 4

my_rf <- h2o.randomForest(x = x_em,
                          y = y_em,
                          model_id = "RF",
                          training_frame = mod_em,
                          ntrees = 200,
                          min_rows = 7210,
                          mtries = 3,
                          nfolds = nfolds,
                          fold_assignment = "Stratified",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1723951065)



mod <- data_modelamiento[ModVal == 0 ]
val <- data_modelamiento[ModVal == 1 ]


mod_em <- as.h2o(x = setDT(mod)[, vars, with=FALSE])
val_em <- as.h2o(x = setDT(val)[, vars, with=FALSE])
mod_em[,y_em] <- as.factor(mod_em[,y_em])
val_em[,y_em] <- as.factor(val_em[,y_em])

res_fun <- function(valida, resultado){
  res <- data.frame(VarDep=valida$ItemNegocioFamilia_mas_frec,
                    Score=1000-round(1000*as.data.frame(resultado)[,3],0), 
                    Rango=rango_score(1000-round(1000*as.data.frame(resultado)[,3],0)))
  return(res)
}


rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}
# Random Forest(Observando el comportamiento del modelo)
mod_rf <- setDT(res_fun(mod, h2o.predict(my_rf, newdata = mod_em)))
val_rf <- setDT(res_fun(val, h2o.predict(my_rf, newdata = val_em)))
mod_rf[,table(Rango, VarDep)]
val_rf[,table(Rango, VarDep)]

# Obteniendo las predicciones Requeridas
#data_evaluacion<-data_consolidada[]
data_consolidada_miercoles <- data_consolidada[grepl("Mié", `Frecuencia rutero`)]
info_em <- as.h2o(x = setDT(data_consolidada_miercoles)[ ,vars, with=FALSE])
#info_em[] <- as.factor(data_modelamiento[,ItemNegocioFamilia_mas_frec])
predicciones <- h2o.predict(my_rf, newdata = info_em)
predicciones<-as.data.table(as.data.frame(predicciones)) 
dim(predicciones)
nombre_columnas<-colnames(predicciones)
predicciones[, max_value := do.call(pmax, .SD), .SDcols = nombre_columnas[2:23]]
data_consolidada_miercoles[, prediccion := predicciones$predict]
exp<-data_consolidada_miercoles
exp<-cbind(exp,predicciones)
head(exp)
exp1<-exp[, .(prob_final = mean(probabilidad_prediccion)), by = .(Cliente,prediccion)]
exp2<-exp1[ , .SD[which.max(prob_final)], by = Cliente]

data_consolidada_miercoles[,probabilidad_prediccion:=predicciones$max_value]
data_resumida<-data_consolidada_miercoles[, .(prob_final = mean(probabilidad_prediccion)), by = .(Cliente,prediccion)]
data_resumida[,.N,by = prediccion]
resultado_final <- data_resumida[ , .SD[which.max(prob_final)], by = Cliente]
resultado_final[,.N,by=prediccion]

resultado_final[Cliente==132525]

#data_consolidada[, probabilidad_prediccion := predicciones[2]]

#Habiendo obtenido las predicciones se procede a filtrar los resultados que se esperan para el dia miercoles 
#filtrados_miercoles <- data_consolidada[grepl("Mié", `Frecuencia rutero`)]

#colocando linea de negocio a el item predicho 
Linea_de_negocio<-Base03092024[!duplicated(`ItemItem PadreCódigo Padre`),.(`ItemNegocioLinea Negocio`,`ItemItem PadreCódigo Padre`)]
setnames(Linea_de_negocio,"ItemItem PadreCódigo Padre","prediccion")
data_final<-merge(data_resumida,
                  Linea_de_negocio,
                  by="prediccion",
                  all.x = TRUE)

aux<-data_final
aux1<-aux[, .(prob_final = mean(prob_final)), by = .(Cliente,prediccion)]
data_final<-merge(aux1,
                  Linea_de_negocio,
                  by="prediccion",
                  all.x = TRUE)


archivo_excel<-data_final[,.(Cliente,`ItemNegocioLinea Negocio`,prediccion,prob_final)]
write.xlsx(archivo_excel, file = "Pregunta1.xlsx", sheetName = "PREDICCION", row.names = TRUE)

#"Ventas históricas ejercicio 2.xlsx"
Ventas_históricas<-read_excel(path = paste0(dir.carpeta,"/Base Usd y Kilogramos.xlsx"))
