#### DARIO_QUISHPE_2024

#DATA.TABLE
options(scipen=999)
library(data.table)
library(readxl)
library(writexl)
library(lubridate)
library(tidyverse)
library(factoextra)
library(cowplot)
library(ggpubr)
library(cluster)
library(FactoMineR)
dir<-getwd()
dir.carpeta<-paste0(dir,"/Bases ejercicio 1 y 2")
list.files(path = dir.carpeta)
Base_U_K<-read_excel(path = paste0(dir.carpeta,"/Base Usd y Kilogramos.xlsx"))
str(Base_U_K)
colSums(is.na(Base_U_K))
Base_U_K<-as.data.table(Base_U_K)
Base_U_K[is.na(KG)]
#Base_U_K$FechaFechaDia
Base_U_K<-Base_U_K[FechaFechaAño!="FechaFechaAño"]
colSums(is.na(Base_U_K))
Base_U_K[is.na(KG),`:=`(KG=0,USD=0)]
colSums(Base_U_K[,.(KG,USD)])
Base_U_K[,FechaFechaDia:=as.Date(FechaFechaDia, format = "%d/%m/%Y")]

Base03092024<-read_excel(path = paste0(dir.carpeta,"/Base03092024.xlsx"))
Base03092024<-as.data.table(Base03092024)
str(Base03092024)
colSums(is.na(Base03092024))
Base03092024[is.na(FechaFechaAño)]
Base03092024<-na.omit(Base03092024,cols = "FechaFechaAño")
Base03092024$FechaFechaDia
Base03092024[,FechaFechaDia:=as.Date(FechaFechaDia, format = "%d/%m/%Y")]


colSums(is.na(Base03092024))
RuteroPrueba<-read_excel(path=paste0(dir.carpeta,"/RuteroPrueba.xlsx"))
str(RuteroPrueba)
colSums(is.na(Base03092024))


#RESUMIENTO LA INFORMACION DE LA DATA 
str(Base_U_K)
resumen_Base_U_K<-Base_U_K[, .(suma_usd = sum(USD)), by = .(Cliente)]

Base03092024<-merge(
  Base03092024,
  resumen_Base_U_K,
  by="Cliente",
  all.x = TRUE
)
Base03092024<-merge(
  
  Base03092024,
  RuteroPrueba,
  by="Cliente",
  all.x = TRUE,
)

Base03092024[,marca_miercoles:=ifelse(grepl("Mié",`Frecuencia rutero`)==FALSE,0,1)]

Base03092024<-na.omit(Base03092024,cols = "suma_usd")
Base03092024[,Dia:=weekdays(FechaFechaDia)]
Base03092024[,.N,by = Dia][order(N)]
Base03092024[,Dia:=ifelse(Dia=="miércoles","Mié",Dia)]
#MODELAMIENTO 
fecha_objetivo<-as.Date("04/09/2024",format="%d/%m/%Y")


rfm_data <- Base03092024[marca_miercoles==1, .(
  Recencia = as.numeric(difftime(fecha_objetivo, max(FechaFechaDia), units = "days")),
  Frecuencia = .N,  # Número de registros por cliente-producto
  Monto = sum(suma_usd),  # Total en MONTO (o usar KG si quieres otra métrica)
  Frec_compra_porcentaje = ifelse(.N==1,0,.N / pmax(as.numeric(difftime(max(fecha_objetivo), min(FechaFechaDia), units = "days")), 1)),
  Dia_compra_mas_Frecuente = names(which.max(table(Dia)))
), by = .(Cliente, `ItemItem PadreCódigo Padre`)]



rfm_data<-merge(
  
  rfm_data,
  RuteroPrueba,
  by="Cliente",
  all.x = TRUE,
)

rfm_data[,.N,by = `ItemItem PadreCódigo Padre`]
rfm_data[,marca_miercoles:=ifelse(grepl("Mié",`Frecuencia rutero`)==FALSE,0,1)]
#ajustando el modelo
BASE<-rfm_data[marca_miercoles==1]

#APLICANDO K MEANS
#ESTANDARIZACION 

columnas_a_estandarizar <- c("Recencia", "Frecuencia", "Frec_compra_porcentaje","Monto")

# Aplicar scale() a las columnas seleccionadas y guardarlas con el sufijo '_estandarizada'
BASE[, (paste0(columnas_a_estandarizar, "_estandarizada")) := lapply(.SD, scale), .SDcols = columnas_a_estandarizar]

set.seed(123)  # Para reproducibilidad

# Seleccionar solo las columnas normalizadas para K-means
BASE_matrix <- as.matrix(BASE[, .(Recencia_estandarizada, 
                                      Frecuencia_estandarizada, 
                                      Frec_compra_porcentaje_estandarizada,
                                      Monto_estandarizada)])
rownames(BASE_matrix) <- paste0(BASE$Cliente,BASE$`ItemItem PadreCódigo Padre`)
# Aplicar K-means con k = 4
kmeans_result <- kmeans(BASE_matrix, centers = 5, nstart = 25)
BASE[, Cluster := kmeans_result$cluster]

#GRAFICANDO LAS RESPUESTAS

ggpairs(BASE, 
        columns = c("Cluster", "Recencia", "Frecuencia", "Monto"),
        mapping = aes(color = Cluster), 
        upper = list(continuous = "points"),
        diag = list(continuous = wrap("barDiag", binwidth = 10)),  # Ajusta binwidth a 10
        lower = list(continuous = "points")) 



#graficamos los cluster en funcion 

(g1=ggplot(BASE[,.(Cliente,Cluster,Recencia,Frecuencia)], aes(x = Recencia, y = Frecuencia)) +
    geom_point(aes(color=as.factor(Cluster)), size=5)+
    geom_text(aes(label = " "), size = 2) +
    theme_bw() +
    theme(legend.position = "none")+
    labs(title = "Kmenas con k=6") 
)

#graficamos sus 2 primeras componentes

fviz_cluster(kmeans_result, data = BASE_matrix)+
  theme_minimal()

#rownames(inseguridad)=ciudades

fviz_cluster(kmeans_result, BASE_matrix, show.clust.cent = T,
             ellipse.type = "euclid", star.plot = T, repel = T) +
  labs(title = "Resultados clustering K-means") +
  theme_bw()


# Aplicar PCA
pca_result <- PCA(BASE_matrix)
plot(pca_result,choix="ind")
plot(pca_result,choix="var")
pca_result$var$contrib
# Ver resumen del PCA (explorar la varianza explicada por las componentes)
summary(pca_result)


#Adicionamos la etiqueta de las ciudades

fviz_cluster(kmcluster, inseguridad, show.clust.cent = T,
             ellipse.type = "euclid", star.plot = T, repel = T) +
  labs(title = "Resultados clustering K-means") +
  theme_bw()


###GENERANDO ACP

cluster_result<-HCPC(pca_result,nb.clust = 5,graph = FALSE)

cluster_result$desc.var$quanti

quantile(rfm_data$Recencia, probs = seq(0,1,by=0.01),na.rm = TRUE)
quantile(rfm_data$Frecuencia, probs = seq(0,1,by=0.01),na.rm = TRUE)
quantile(rfm_data$Frec_compra_porcentaje, probs = seq(0,1,by=0.01),na.rm = TRUE)




#NUMERO OPTIMO DE CLUSTERS
matriz_dist<-get_dist(BASE_matrix, method = "euclidean")

fviz_nbclust(BASE_matrix, FUNcluster = kmeans, 
             method = "wss", k.max = 15, 
             diss = matriz_dist, nstart = 50)



#CONTINUACION

BASE[, Cluster_kmeans := kmeans_result$cluster]
BASE[, Cluster_jeraraquico := cluster_result$data.clust$clust]


#PUNTAJES RFM
quantile(BASE$Recencia, probs = seq(0,1,by=0.01),na.rm = TRUE)
quantile(BASE$Recencia, probs = seq(0,1,by=0.2),na.rm = TRUE)
quantile(BASE$Frecuencia, probs = seq(0,1,by=0.01),na.rm = TRUE)
quantile(BASE$Frecuencia, probs = seq(0,1,by=0.2),na.rm = TRUE)#47
quantile(BASE$Monto, probs = seq(0,1,by=0.01),na.rm = TRUE)
quantile(BASE$Monto, probs = seq(0,1,by=0.2),na.rm = TRUE)

BASE[,`:=`(P_R=ifelse(Recencia<13,"5",
                      ifelse(Recencia<42,"4",
                             ifelse(Recencia<83,"3",
                                    ifelse(Recencia<146,"2","1")))
                      ))]

BASE[,`:=`(P_F=ifelse(Frecuencia<2,"1",
                      ifelse(Frecuencia<4,"2",
                             ifelse(Frecuencia<10,"3",
                                    ifelse(Frecuencia<20,"4","5")))
))]

BASE[,`:=`(P_M=ifelse(Monto<2.26098,"1",
                      ifelse(Monto<5.45395,"2",
                             ifelse(Monto<12.50598,"3",
                                    ifelse(Monto<37.46681,"4","5")))
))]

BASE[,Score_RFM:=as.numeric(paste0(P_R,P_F,P_M))]
BASE[,match_dia_prob:=ifelse(Dia_compra_mas_Frecuente=="jueves",1.5,1)]
BASE[,Probabilidad:=(1 - exp(-Frec_compra_porcentaje *1/Recencia))*match_dia_prob]
#BASE[,.N,by = Score_RFM][order(N,decreasing = TRUE)]
BASE[Cliente==289644][order(Score_RFM,decreasing = T)]
BASE[Cliente==289644,.N,by=Dia_compra_mas_Frecuente]




#matriz de correlaciones

library(ggplot2)
library(reshape2)
library(corrplot)
# Crear el heatmap con pheatmap
BASE_matrix_cor <- cor(BASE_matrix, use = "complete.obs")
corrplot(BASE_matrix_cor)
base_test<-cor.mtest(BASE_matrix,conf.level=0.95)

corrplot(BASE_matrix_cor,p.mat = base_test$p, sig.level = 0.05,  method = "color",order = "hclust",  tl.srt = 45,
         tl.cex = 0.4)
recencia_cor<-BASE_matrix_cor[,"Recencia_estandarizada"]
# Ordenar las correlaciones por valor absoluto y seleccionar los índices de las dos mayores
top_2_indices <- order(abs(recencia_cor), decreasing = TRUE)[1:3]

# Mostrar los nombres de las variables con las dos mayores correlaciones
rownames(BASE_matrix_cor)[top_2_indices]
matriz_importante<-BASE_matrix[,top_2_indices]
matrizcor_importante_test<-cor.mtest(matriz_importante,conf.level=0.95)
matrizcor_importante<-cor(matriz_importante)
corrplot(matrizcor_importante, method = "number",order = "hclust",  tl.srt = 45,
         tl.cex = 0.4)


corrplot(M, method = 'number') # colorful number

#Scatter Caballos de fuerza y tiempo de cuarto de milla
ggplot(mtcars,aes( x=hp, y=qsec))+
  geom_point()

#Otras opciones
corrplot(M, method = 'square') # colorful number
corrplot(M, method = 'shade') # Sombrear los negativos
corrplot(M, method = 'pie') # Torta


# Cuadrado de color, y ordenamiento alfabético
corrplot(M, method = 'color', order = 'alphabet')


#Ordenamiento!
corrplot(M, order = 'AOE') #'AOE' is for the angular order of the eigenvectors
corrplot(M, order = 'FPC') # 'FPC' for the first principal component order.
corrplot(M, order = 'hclust') # for hierarchical clustering order




# Hacer gráfico de la mitad !
corrplot(M, method = 'ellipse', order = 'AOE', type = 'upper')
corrplot(M, method = 'ellipse', order = 'AOE', type = 'lower')


# Mitad elipse mitad número
corrplot.mixed(M, upper = 'ellipse', order = 'FPC')
