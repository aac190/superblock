install.packages("corrplot")
install.packages("factoextra")
install.packages("plotrix")
install.packages("scatterplot3d")
install.packages("geojsonio")
install.packages("NbClust")

library(corrplot)
library(ggplot2) 
library(factoextra)
library(cluster)
library(plotrix)
library(sp)
library(rgdal)
library(tmap)
library(scatterplot3d)
library(jsonlite)
library(geojsonio)
library(NbClust)

#1.Read From a CSV
node_data_original <- read.csv(file = "Data/Node-Superblock-count.csv",head = TRUE,sep = ";")
#names of the columns
names(node_data_original)
#subset data to work with
node_data_work <- node_data_original[,c("Residentialcount","Businesscount","Culturalcount","Restaurantcount","Schoolcount","Retirementcount")]

#2.Statistic summary (example)
hist(node_data_original$Residential.count, xlab = "num. sq housing", main = NULL)

#3.Standarize values Z-Score
value <- colnames(node_data_work)
#creates a new data frame 
stand_data <- node_data_work 
#loops columns from position 1 : the last column. // Important to return as vector! 
for(i in 1: ncol (node_data_work)){stand_data[, value[i]] <- as.vector(scale(node_data_work[, value[i]]))}
#change stand data column names 
colnames(stand_data) <- paste(colnames(stand_data), "st", sep = "_")

#4.Correlation between variables
stand_data.cor = cor(stand_data, method = "pearson")
stand_data.cor
corrplot(stand_data.cor, method = "number", tl.col="black")

#5.K-means clustering
#Finding right amount of clusters
NbClust(data = stand_data, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = "kmeans", index = "all", alphaBeale = 0.1)
#Final number of clusters
num_clusters = 5
Km <- kmeans(stand_data, num_clusters, nstart = 25, iter.max = 1000)
#Cluster membership for each case 
KmClusters <- as.matrix(Km$cluster) 
KmClusters <- as.data.frame(KmClusters)
#Change name column
names(KmClusters) <- c("Cluster")
#Cluster centres 
KmCenters <- as.matrix(Km$centers) 
KmCenters <- as.data.frame(KmCenters)
#Frequency of cases in each cluster 
table(KmClusters)

#6.Visualize Clusters
# Cluster Plot against 1st 2 principal components 
clusplot(stand_data,Km$cluster, color = TRUE, shade = FALSE, labels = 4, lines = 0, plotchar = FALSE)
scatterplot3d(stand_data[,2:4], angle = 60)
# plots cases in the kmeans against 1st 2 principal components 
fviz_cluster(Km, data = stand_data, geom = "point", ellipse = F, pointsize = 0.5, ggtheme = theme_classic())
# creates a object of six zeros
KmCenters[num_clusters+1,]<- c(0)
# creates a radial plot for the two columns (example)
radial.plot(KmCenters[c(1,num_clusters+1),], main = "cluster1", labels = colnames(KmCenters), boxed.radial = FALSE, show.radial.grid = TRUE, line.col = c("blue", "red"), radlab = TRUE, rp.type = "p", show.grid.labels = 3)

#7.Mapping the preview of the clusters
#Extract join the cluster labels to the first column of the pop_data file (OA codes) 
Classification <- as.data.frame(cbind(as.character(node_data_original[,1]), KmClusters[,1])) 
#Rename the column headers 
names(Classification) <- c("C_Nus", "Classification")
#Load Shapefile and preview results
Node_shp<- readOGR(dsn="Data", layer="Node-count")
Node.Class<- merge(Node_shp, Classification, by.x = "C_Nus", by.y = "C_Nus")
tm_shape(Node.Class) + tm_symbols(col = "Classification") + tm_layout(frame = FALSE)

#8. Output CSV with all the info and GEOjson for map making
Node_cluster <- as.data.frame(cbind(node_data_original[,], KmClusters,stand_data[,]))
write.csv(Node_cluster, "Output/Node_cluster.csv")
Node_shp_cluster<- merge(Node_shp, Node_cluster, by.x = "C_Nus", by.y = "C_Nus")#add to the point shape the csv info
geojson_write(Node_shp_cluster, file = "Output/Node_cluster.geojson")