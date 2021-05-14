# **Proyecto 1**

### Librerias Utilizadas y Cargar base de datos
Se proceden a utilizar las siguientes librerias: \
 `library(dplyr)`\
`library(tidyverse)`\
`library(utf8)`\
`library(ggplot2)`\
`library(quanteda)`\
`library("quanteda.textstats")` \
Posteriormente se carga la base de datos "sanguchez" \
`setwd("C:/Users/Alvaro/Desktop/xdd/Mineria de Datos TICS 411/sanguchez.csv", sep=";")`\
`sanguchez <- read.csv("C:/Users/Alvaro/Desktop/xdd/Mineria de Datos TICS 411/sanguchez.csv", sep=";")`

### Primer Filtro
En primer lugar se eliminan las filas que poseen valores nulos, ya que no nos entregan informacion util
con esto se reduce de 410 sandwiches a 402 (8 filas de datos vacio eliminados) \
`sanguche<-na.omit(sanguchez)`

