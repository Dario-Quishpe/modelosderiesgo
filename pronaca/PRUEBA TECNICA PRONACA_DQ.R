#Modelo RFM 

install.packages(c("readxl","ranger","h2o","data.table","xlsx")) 
suppressMessages(library(readxl))
suppressMessages(library(ranger))
suppressMessages(library(h2o))
suppressMessages(library(data.table))
suppressMessages(library(ROSE))
suppressMessages(library(xlsx))
options(scipen=999)
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
Base_U_K[, USD:= as.numeric(USD)]
suma_por_cliente_USD <- Base_U_K[, .(suma_USD = sum(USD)), by = Cliente]
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
data_consolidada<-merge(
  Base03092024,
  RuteroPrueba,#Añadimos la informacion adicional sobre la frecuencia rutero
  by="Cliente",
  all.x=TRUE
)


data_consolidada<-merge(
  data_consolidada,
  suma_por_cliente_USD,#Añadimos la informacion adicional sobre la frecuencia rutero
  by="Cliente",
  all.x=TRUE
)



fecha_objetivo <- as.Date("2024-09-04")

rfm_data <- data_consolidada[, .(
  Recencia = as.numeric(difftime(fecha_objetivo, max(FechaFechaDia), units = "days")),
  Frecuencia = .N,  # Número de registros por cliente-producto
  Monto = sum(suma_USD)  # Total en MONTO (o usar KG si quieres otra métrica)
), by = .(Cliente, `ItemItem PadreCódigo Padre`)]

rfm_data[, Recencia_norm := (Recencia - min(Recencia)) / (max(Recencia) - min(Recencia))]
rfm_data[, Frecuencia_norm := (Frecuencia - min(Frecuencia)) / (max(Frecuencia) - min(Frecuencia))]
rfm_data[, Monto_norm := (Monto - min(Monto)) / (max(Monto) - min(Monto))]

set.seed(123)  # Para reproducibilidad

# Seleccionar solo las columnas normalizadas para K-means
rfm_matrix <- as.matrix(rfm_data[, .(Recencia_norm, Frecuencia_norm, Monto_norm)])

# Aplicar K-means con k = 4
kmeans_result <- kmeans(rfm_matrix, centers = 4, nstart = 25)

# Agregar los clusters resultantes a la tabla original
rfm_data[, Cluster := kmeans_result$cluster]
rfm_data[,.N,by = Cluster]

rfm_data<-merge(
  rfm_data,
  RuteroPrueba,
  by="Cliente",
  all.x=TRUE
  
)
setnames(rfm_data, c("ItemItem PadreCódigo Padre"), 
         c("Producto"))

setnames(rfm_data, c("Frecuencia rutero"), 
         c("Frecuencia_rutero"))

set.seed(123)  # Para reproducibilidad
train_indices <- sample(1:nrow(rfm_data), size = 0.5 * nrow(rfm_data))  # 50% para entrenamiento
train_data <- rfm_data[train_indices]
test_data <- rfm_data[-train_indices]

rfm_data[, Producto := as.factor(Producto)]
formula_rf<-"Producto ~ Cliente + Recencia + Frecuencia + Monto + Frecuencia_rutero + Cluster"

modelo_rf <- ranger(formula = as.formula(formula_rf),
                    data = train_data,num.trees = 200, mtry = 3, importance = "impurity", write.forest = T, probability = T, 
                    min.node.size = floor(0.03*nrow(train_data)), seed = 1723951065)  

train_data[,.N,by=Producto][order(N,decreasing = TRUE)]
test_data[,.N,by=Producto][order(N,decreasing = TRUE)]

# Hacer predicciones

predicciones <- predict(modelo_rf, data = rfm_data)
#probabilidades de predicción
probabilidades <- predicciones$predictions[, 1]  # Probabilidad de clase 1 (Comprado)

clases_predichas <- predict(modelo_rf, rfm_data)  # Por defecto, devuelve las clases predichas

# Agregar las probabilidades y predicciones a rfm_data
rfm_data[, Probabilidad := probabilidades]
rfm_data[, Prediccion := clases_predichas]
# Añadir las probabilidades 
rfm_data[Cliente=="100898"][order(Probabilidad)]

test_data[, Probabilidad := probabilidades]
test_data[, Probabilidad := ifelse(Probabilidad>0.5,1,0)]

test_data[,.N,by = `ItemItem PadreCódigo Padre`]
test_data[,.N,by = Probabilidad]

miercoles_df<-test_data[grepl("Mié", Frecuencia_rutero)]
predicciones_base_final <- predict(modelo_rf, data = miercoles_df)
probabilidades_base_final<-predicciones_base_final$predictions[, "1"]
miercoles_df[,Probabilidad:=probabilidades_base_final]
miercoles_df[,Probabilidad:=ifelse(Probabilidad>0.5,1,0)]
miercoles_df[,.N,by = Comprado]
miercoles_df[,.N,by = Probabilidad]

# Ver los resultados
head(miercoles_df)

# tabla de confusión
confusion_matrix <- table(Predicho = predicciones$predictions[, "1"], Real = test_data$Comprado)

# precisión
precision <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Precisión:", round(precision, 4)))


#importancia de las variables

imp <- data.table(Variable=names(ranger::importance(modelo_rf)), Valor=unname(ranger::importance(modelo_rf)))[order(Valor)]
imp[order(Valor,decreasing = TRUE)]

#Aniadiendo linea de negocio.
Linea_de_negocio<-Base03092024[!duplicated(`ItemItem PadreCódigo Padre`),.(`ItemNegocioLinea Negocio`,`ItemItem PadreCódigo Padre`)]

miercoles_df<-merge(
  miercoles_df,
  Linea_de_negocio,
  by="ItemItem PadreCódigo Padre",
  all.x = TRUE
)

archivo_excel<-miercoles_df[Probabilidad==1,.(Cliente,`ItemNegocioLinea Negocio`,`ItemItem PadreCódigo Padre`,Probabilidad)]
write.xlsx(archivo_excel, file = "Pregunta1.xlsx", sheetName = "PREDICCION", row.names = TRUE)
