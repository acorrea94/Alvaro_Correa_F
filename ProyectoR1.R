library(dplyr)
library(tidyverse)
library(utf8)
library(ggplot2)
library(quanteda)
library("quanteda.textstats")


setwd("C:/Users/Alvaro/Desktop/xdd/Mineria de Datos TICS 411/sanguchez.csv", sep=";")
sanguchez <- read.csv("C:/Users/Alvaro/Desktop/xdd/Mineria de Datos TICS 411/sanguchez.csv", sep=";")

View(sanguchez)



##En primer lugar se eliminan las filas que poseen valores nulos, ya que no nos entregan informacion util
##con esto se reduce de 410 sandwiches a 402 (8 filas de datos vacio eliminados)

sanguche<-na.omit(sanguchez)

##se procede a seleccionar solo los atributos importantes (Dirección,Ingredientes,nota,Precio)
## así se reducen datos (atributos) no relevantes dentro de la data

sandwich<-sanguche %>% select(Direccion,Precio,Ingredientes,nota)

## se procede a obtener información a partir de los datos que poseemos y separamos las columnas para
##poder llamarlas y trabajar con ellas mas facilmente

attach(sandwich)
str(sandwich)


##Luego como observamos, el promedio de las notas de nuestros datos es 3.5 y como trabajamos con notas en
##valores enteros entonces trabajaremos con una nota igual a 4, entonces para elegir la mejor receta de 
##sandwich debemos fijarnos unicamente en sandwiches que posean una nota mayor al promedio, 
##por lo tanto se filtrara y se obtendran unicamente los sandwichs que tengan una nota igual a 4

mejores<-filter(sandwich,nota==4)

##Con esta limpieza podemos reducir desde 402 filas de datos a un total de 88.
##Luego hay que diferenciar ¿Cuantos de estos sandwiches con buenas notas son del extranjero?
##¿Será necesario incluir sandwiches del extranjero cuando quizas en nuestro país no existen
## dichos ingredientes? Para esto se filtra a través del Precio (USD,AUD,NZD) para observar que es lo
##que ocurre con nuestros sandwiches

dolares<- mejores %>% filter(str_detect(Precio, 'USD'))                    
oceania1<- mejores %>% filter(str_detect(Precio, 'AUD|NZD'))

## De aqui se concluye que de los 88 sandwiches que tenemos con nota máxima, solo 3 son de Estados Unidos
## y 5 pertenecen a Oceania (Australia y Nueva Zelanda) , por lo que luego de observar la data disponible
## se determina que no son influyentes en el resultado del análisis.


##Luego se procede a transformar el atributo de Precio de variable string (caracteres) a integer (enteros)
##se aplica la funcion gsub junto a as.numeric para transformar de str a int


mejores$Precio <- gsub('[$ .]','',mejores$Precio)
mejores$Precio <- as.numeric(mejores$Precio)
mejores<-na.omit(mejores)

## Hay que notar que en este proceso los Precios que contenian "USD","NZD",AUD","soles" se transforman en celdas
##vacias (NA) y por lo que luego se eliminaran las celdas vacias y entonces serán eliminadas como se establecio
##anteriormente. Quedando asi los 79 mejores sandwiches. 




## Luego de esto se procede a extraer los ingredientes de nuestros 80 mejores sandwiches para poder analizar
##de mejor manera el cómo obtener la mejor receta posible. Pero antes hay que separar los ingredientes y
## transformarlos a minusculas para evitar que por ejemplo "Queso" y "queso" se consideren como ingredientes
##distintos , si no que sean iguales



ingre<-unlist(strsplit(mejores$Ingredientes, split=','))
ingre<-unlist(strsplit(ingre, split=" "))
lowe<-str_to_lower(ingre)
lowe<-gsub('[.0 1 2 3 4 5 6 7 8 9 ( ) ]','',lowe)
unico_ingr<-unique(lowe)


##Luego de esto se procede a eliminar conectores como "las","los","al","con","de","%","-","sin","no", entre otros


unico_ingr<-na.omit(unico_ingr)

ingredientes_unicos<-data.frame(unico_ingr)
View(ingredientes_unicos)

## Se procede a realizar una matriz de comparación para establecer cuantas veces
##se repite cada ingrediente en el total de nuestros 80 mejores sandwiches, para asi quizas
##lograr entender cual podrìa ser nuestra receta perfecta
#Para esto se obtienen los ingredientes de cada sandwich y se compara con el total de ingredientes

best<-str_to_lower(mejores$Ingredientes)
best<-gsub('[.0123456789(),?]','',best)

hamburguesa<-best

resultado <- dfm(hamburguesa, remove = c(stopwords("es")))

textstat_frequency(resultado, n=10)

##Con esto observamos que entre los 10 ingredientes mas usados en el top 79, se 
##encuentran queso,mayonesa,cebolla,palta,lechuga,hamburguesa,rucula

receta_perfecta<-data.frame(textstat_frequency(resultado, n=10))

receta_perfecta<-receta_perfecta$feature


View(data.frame(receta_perfecta))     
