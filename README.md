

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Modelo 1

# Librerias
```{r}
library(dplyr)
library(tidyverse)
library(outliers)
library(pROC)
library(xlsx)
```

Comenzamos llamando la data


```{r}
setwd("d:/Users/franco/Desktop/tarea3")

data = readRDS("endurance.rds")
head(data)
```

analisis de la data

```{r}
summary(data)
unique(data$type)
unique(data$has_heartrate)
unique(data$device_name)
```

podemos ver que la data contiene variables numericas y characteres.

##Modificar data.
LLevamos a numeric las variables que pueden ser numericas

```{r, warning = FALSE}
data$max_speed = as.numeric(data$max_speed)
data$records = as.numeric(data$records)
data$elev_low = as.numeric(data$elev_low)
data$elev_high = as.numeric(data$elev_high)
data$average_speed = as.numeric(data$average_speed)
data$has_heartrate = ifelse(data$has_heartrate == FALSE, 0, 1)
```
# Work directory
setwd("C:\Users\Alvaro\Desktop\xdd\Mineria de Datos TICS 411\Proyecto 3")
getwd()

# Librerias
#install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(outliers)
library(pROC)
library(xlsx)

# Data
data = readRDS("endurance.rds")

#Analisis Data de las variables
summary(data)
unique(data$type)
unique(data$has_heartrate)
unique(data$device_name)

# Modificar data.
# LLevamos a numeric las variables que pueden ser numericas

data$max_speed = as.numeric(data$max_speed)
data$records = as.numeric(data$records)
data$elev_low = as.numeric(data$elev_low)
data$elev_high = as.numeric(data$elev_high)
data$average_speed = as.numeric(data$average_speed)
data$has_heartrate = ifelse(data$has_heartrate == FALSE, 0, 1)

# Filtramos data que queremos estudiar es decir si hace deporte caminando o en bicicleta
# Si anda en bicileta le damos el valor de uno, sino 0
# Respondiendo al problema original filtramos solo los registros de participantes que caminan
# o andan en bicicleta

data <- data %>% filter(type == c('Ride', 'Walk'))
unique(data$type)
#Verficamos que el filtro esta correcto.

# Como queremos hacer un modelo logistico y la variable respuesta son dos valores (Wlak y Ride) 
# entonces hacemos la siguiente transformacion.

data$type = ifelse(data$type == 'Ride', 1, 0)


# Outliers
# Con metodo estadistico Grubbs. Si el p-value es menor a 0.01 entonces exite un alto 99.99% 
# de confianza que el elemento es un outlier

# Forma visual de Outliers con boxplot
boxplot(data$calories, horizontal = FALSE)

#Veo el dato erroneo
data %>% filter(calories > 300000) 

#Conclsion, lo elimino porque no tiene sentido que alguien consuma 300000 calorias
data <- data %>% filter(calories < 300000) 

#Veamos que pasa con el outlier de calories
boxplot(data$calories, horizontal = FALSE)

# Metodo estadistico de Grubbs

calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative

# Lo sacamos
data <- data %>% filter(calories != 43095)

#Volvemos a realizar el test estadistico

calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative

# Vemos que 32600.6 es un outlier, lo sacamos
data <- data %>% filter(calories != 32600.6)

#Volvemos a realizar el test estadistico

calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative

# Vemos que 32600.6 es un outlier, lo sacamos
data <- data %>% filter(calories != 25456.1)

#Volvemos a realizar el test estadistico

calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative

# Vemos que 32600.6 es un outlier, lo sacamos
data <- data %>% filter(calories != 21048.5)

#Volvemos a realizar el test estadistico

calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative

# Vemos que 32600.6 es un outlier, lo sacamos
data <- data %>% filter(calories != 14541)

#Volvemos a realizar el test estadistico

calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative

# Vemos que 32600.6 es un outlier, lo sacamos
data <- data %>% filter(calories != 14392.5)

#Volvemos a realizar el test estadistico

calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative

# Vemos que 32600.6 es un outlier, lo sacamos
data <- data %>% filter(calories != 12250)

#Volvemos a realizar el test estadistico

calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative

# Vemos que 32600.6 es un outlier, lo sacamos
data <- data %>% filter(calories != 10975)

#Volvemos a realizar el test estadistico

calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative

# Vemos que 32600.6 es un outlier, lo sacamos
data <- data %>% filter(calories != 10791.8)

# Como p-value es menor a 0.01 entonces con un 99.99% de confianza 43095 es un outlier
distance_outlier = grubbs.test(data[['distance']], two.sided = FALSE)
distance_outlier$p.value
distance_outlier$alternative

# Como p-value es menor a 0.01 entonces con un 99.99% de confianza 1478170 es un outlier

elev_low_outlier = grubbs.test(data[['elev_low']], two.sided = FALSE)

elev_low_outlier$p.value
elev_low_outlier$alternative

# Como p-value es menor a 0.01 entonces con un 99.99% de confianza 10490.2 es un outlier

elev_high_outlier = grubbs.test(data[['elev_high']], two.sided = FALSE)

elev_high_outlier$p.value
elev_high_outlier$alternative

# Como p-value es menor a 0.01 entonces con un 99.99% de confianza 12605.8 es un outlier


max_speed_outlier = grubbs.test(data[['max_speed']], two.sided = FALSE)

max_speed_outlier$p.value
max_speed_outlier$alternative

# Como p-value es menor a 0.01 entonces con un 99.99% de confianza 152.9 es un outlier

average_speed_outlier = grubbs.test(data[['average_speed']], two.sided = FALSE)

average_speed_outlier$p.value
average_speed_outlier$alternative

# Como p-value es menor a 0.01 entonces con un 99.99% de confianza 2296.088 es un outlier


#Quitamos los outliers identificados

data <- data %>% filter(calories != 43095)
data <- data %>% filter(distance != 1478170)
data <- data %>% filter(elev_low != 10490.2)
data <- data %>% filter(elev_high != 12605.8)
data <- data %>% filter(average_speed != 2296.088)


# Seleccion de variables para creación del modelo.
# Vemos si exise alguna correlacion alta entre type y las demas variables.
# Si existe una correlacion alta entre variables que no sean type entonces
# quedan afuera o sino sería información ya existente.

cor(data[c('type','athlete', 'calories', 'distance', 'elev_low', 'records', 
           'elev_high', 'moving_time', 'elapsed_time',  'average_speed',
           'has_heartrate', 'max_speed')])

# Las 3 correlaciones mas alta de la variable type son:
# max_speed, has_heartrate, average_speed

# Tiene sentido que las variables en funcion de la velocidad este en el modelo porque 
# se espera que alguien que anda en bicicleta vaya mas rapido que alguien que camina.

# Planteamiento modelo
# Como la variable es binaria es decir toma 1 si es Ride o 0 si es Walk entonces 
# planteamos un modelo de regresion logpistica.

modelo1 <- glm( type ~ max_speed + has_heartrate + average_speed , data = data, family = 'binomial')
summary(modelo1)

# Vemos que la Mediana de los residuos es cercana a cero. OK
# El modelo nos dice que todas las variables escogidas son significativas estadisticamente.
# PORQUE vemos que el p-value es cercano a cero, es decir a un 99.99% de confianza
# las variables max_speed, has_heartache y average_speed son explicativas de la 
# variale type (anda en bici o camina)

# Vamos a predecir los valores de 'type' segun el modelo.
# A pesar que type deberia ser 1 o 0 se trate el concepto como probabilidad

probabilidad <- predict(modelo1,type=c("response"))

data$probabilidad <- probabilidad

# Analisis de ajuste del modelo con la curva Roc.

curva_roc <- roc(type ~ probabilidad, data = data)
plot(curva_roc)   
auc(curva_roc)

## PENDIENTE DAR EXPLICACION DE LA CURVA ROC

# Vemos que el valor del AUC es de 93.7%, lo que significa que el modelo tiene un
# gran poder de clasificacion

modelo2 <- glm( type ~ calories + distance + elev_low + records + elev_high + 
moving_time + elapsed_time + average_speed + has_heartrate + max_speed, data = data, family = 'binomial')
summary(modelo2)

# Vemos que calories, distance, elev_high, moving_time y elapsed_time son no significativas

# Predicción del modelo.

probabilidad2 <- predict(modelo2,type=c("response"))

data$probabilidad2 <- probabilidad2

# Analisis de ajuste del modelo2 con la curva Roc.

curva_roc2 <- roc(type ~ probabilidad2, data = data)
plot(curva_roc2)   
auc(curva_roc2)

# El modelo solo mejoró un 1% agregando todas las demas variables.
# Esto nos lleva a pensar que la sencillez del modelo 1 es mejor para predecir
# si alguien camina o anda en bicicleta.

## NO ES TAN EFICIENTE AGREGAR 7 VARIABLES PARA QUE EL MODELO MEJORE UN 1%


########### MODELO NAIVE BAYES


#Librerias
library(dplyr)
library(class)
library(e1071)
library(pROC)


# Data
#setwd("d:/Users/cnunez/Desktop/clases_particulares/franco-r/tarea3")

data_modelo_knn = readRDS("endurance.rds")

# Entrenamiento y Testeo
# Dividimos la data para entrenar y testear en proporcion 80%, 20% con semilla 123
# Mulear porque 80, 20.

set.seed(123)
sample <- sample(1:nrow(data_modelo_knn), .8*167615)

# Modifico data

data_modelo_knn$max_speed = as.numeric(data_modelo_knn$max_speed)
data_modelo_knn$records = as.numeric(data_modelo_knn$records)
data_modelo_knn$elev_low = as.numeric(data_modelo_knn$elev_low)
data_modelo_knn$elev_high = as.numeric(data_modelo_knn$elev_high)
data_modelo_knn$average_speed = as.numeric(data_modelo_knn$average_speed)
data_modelo_knn$has_heartrate = ifelse(data_modelo_knn$has_heartrate == FALSE, 0, 1)
data_modelo_knn$type = ifelse(data_modelo_knn$type == "Ride", 1, 0)

# Genero la data de entrenamieto y de testeo

trainData <- data_modelo_knn[sample,]
testData <- data_modelo_knn[-sample,]

# Modelo Naive Bayes

modelo_3 <- naiveBayes(type ~ max_speed + has_heartrate + average_speed, data = trainData)
pred <- predict(modelo_3, testData, type ="raw")

pred
# En pred podemos ver la probabilidad de que un atleta camine o ande en bicicleta

modelo_3

# Ajuste del modelo en funcion de la data testeo.

testData$prob <- pred[,2]

curva_roc <- roc(type ~ prob, data = testData)
plot(curva_roc)

auc(curva_roc)

# El modelo se ajusta un 89.91%

# POr lo tanto, el modelo de naive bayes es mejor porque tiene un auc mayor
# es decir, se ajusta mejor a la data.
# 


