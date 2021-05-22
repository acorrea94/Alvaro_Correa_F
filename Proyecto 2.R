#Para el desarrollo del proyecto se utilizaran las siguiente libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(janitor)
library(dplyr)
library(dbscan)
library(ggplot2)
library(NbClust)
library(tidyr)
##En primera instancia se realizara una limpieza de datos

summary(beats)
names(beats)
beats2<-select(beats, artist_name,album_release_year,danceability,energy,key,loudness,
               mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,
               duration_ms,track_name)
table(beats$key_mode)
summary(beats2)

## Se proceden a seleccionar las canciones que fueron lanzadas durante los 80s (desde 1980 a 1989)
## con un factor de danceability mayor a 0.6
beats3<-filter(beats2,album_release_year>1980 & album_release_year<1990
               &danceability > 0.6)

#Evaluamos que la data no tenga valores NA
beats3[beats3 == ""] <- NA
summarise_all(beats3, funs(sum(is.na(.))))

#Para realizar algorimtos de clustering es necesario que los valores esten
#normalizados, para que todos las variables tengas el mismo peso en el calculo
#de distancias

str(beats3)
names(beats3)
beatschar<-c("artist_name","album_release_year","track_name")
beatsnum<-c("danceability","energy","energy","key","loudness","mode",
            "speechiness","acousticness","instrumentalness","liveness",
            "valence","tempo","duration_ms")

beats_char <- beats3 %>% 
  select(beatschar)
beats_num <- beats3 %>% 
  select(beatsnum)
str(beats_num)
beats_sca <- sapply(beats_num, scale)


#calcular K (numero de clusters optimos)
fviz_nbclust(beats_sca, kmeans, method = "silhouette")
#segun el metodo de silhouette el mejor valor de K es 2

##k <- kmeans(beats_sca, centers = 2, nstart = 25)
k <- kmeans(beats_sca, centers = 2)
k
str(k)


fviz_cluster(k, data = beats_sca)
fviz_cluster(k, data = beats_sca, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k, data = beats_sca, ellipse.type = "norm")
fviz_cluster(k, data = beats_sca, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())


## Con esto podemos observar que efectivamente existe 2 clusters, se procede a elegir una cancion en especifico
#esta cancion es 


table(k$cluster)
beats_clus=cbind(clus=k$cluster,beats3)
table(beats_clus$clus)

names(beats_clus)
beats_clus$clus <- as.character(beats_clus$clus)

cluscancion=beats_clus$clus[beats_clus$track_name == "El Baile De Los Que Sobran"]
playlist=filter(beats_clus,clus==cluscancion)
sum(playlist$duration_ms)/2.7777777777777776*10^-6


## Se calcula que existen m as de 180 minutos de reproducción en canciones por lo
## que es posible crear una lista de 3 horas

