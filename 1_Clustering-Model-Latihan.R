# Panggil library yang diperlukan
library(tidyverse)
library(cluster)
library(factoextra)
library(skimr)
library(gridExtra)

# load dataset
df <- agriculture

# cek dimensi
dim(df)

# cek kelas data
class(df)

# buat ringkasan data
skim(df)

# berdasarkan hasil yang diperoleh tidak terdapat missing value pada data

# normalisasi data
# dfnorm<- scale(df)

# cek data hasil normalisasi
# skim(data.frame(dfnorm))

# Menhitung Ditance Matrix menggunakan fungsi get_distance()
distance <- get_dist(df, method = "euclidean" )

# Memvisualisasikan Distance Matrix menggunakan fungsi fviz_dist()
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Membuat Model K-Means Klustering dengan Jumlah K/Centers =2, nstart = 6, dengan nama K2
k2 <- kmeans(df, centers = 2, nstart = 10)
str(k2)

# Print Hasil Kluster
k2

# visualisasi kluster menggunakan fungsi fviz_cluster()
fviz_cluster(k2, data = df, ggtheme=theme_classic())

#Visualisasi ScatterPlots menggunakan ggplot2
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         country = row.names(agriculture)) %>%
  ggplot(aes(x, y, color = factor(cluster), label = country)) +
  geom_text()+
  theme_classic()

#Membuat Untuk kluster dengan masing-masing K =3.4.5
k3 <- kmeans(df, centers = 3, nstart = 10)
k4 <- kmeans(df, centers = 4, nstart = 10)
k5 <- kmeans(df, centers = 5, nstart = 10)

# Membuat Komparasi Plot
p1 <- fviz_cluster(k2, geom = "point", data = df,ggtheme=theme_classic()) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = df,ggtheme=theme_classic()) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = df,ggtheme=theme_classic()) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = df,ggtheme=theme_classic()) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

# penentuan optimal kluster (Elbow method)

set.seed(123)

#Visualisasi Elbow Method menggunakan fungsi fviz_nbclust()
fviz_nbclust(df, kmeans, method = "wss")

# penentuan jumlah kluster optimal dengan Silhoutte
#Visualisasi Average Silhoutte menggunakan fugsi fviz_nbclust()
fviz_nbclust(df, kmeans, method = "silhouette")

# berdasarkan kedua metode diketahui jumlah kluster optimal adalah k=2

# Compute k-means clustering with k = 2
final <- kmeans(df, centers = 2, nstart = 10)

# Print Hasil Kluster
final

# visualisasi kluster menggunakan fungsi fviz_cluster()
fviz_cluster(final, data = df, ggtheme=theme_classic())

# menghitung rata-rata pusat kluster
agriculture %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

