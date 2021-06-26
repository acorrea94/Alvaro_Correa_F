
# Proyecto 3
##Alvaro Correa , Franco Constanzo
## Librerias
```{r}
library(dplyr)
library(tidyverse)
library(outliers)
library(pROC)
library(xlsx)
library(dplyr)
library(class)
library(e1071)


```

## Comenzamos llamando la data

```{r}
setwd("")

data = readRDS("endurance.rds")
head(data)
```

## Analisis de la data


```{r}
summary(data)
unique(data$type)
unique(data$has_heartrate)
unique(data$device_name)
```

Podemos ver que la data contiene variables numericas y caracteres. Se remueven instancias repetidas en la data.

## Modificando la data.

LLevamos de character a numeric las variables que pueden ser numericas

```{r, warning = FALSE}
data$max_speed = as.numeric(data$max_speed)
data$records = as.numeric(data$records)
data$elev_low = as.numeric(data$elev_low)
data$elev_high = as.numeric(data$elev_high)
data$average_speed = as.numeric(data$average_speed)
data$has_heartrate = ifelse(data$has_heartrate == FALSE, 0, 1)
```

## Filtrando data

Filtramos data que queremos estudiar es decir si hace deporte caminando o en bicicleta.
Si anda en bicileta le damos el valor de 1, sino 0.

```{r}
data <- data %>% filter(type == c('Ride', 'Walk'))
unique(data$type)
```


Respondiendo al problema original filtramos solo los registros de participantes que caminan o andan en bicicleta. Verficamos que el filtro esta correcto (sin instancias repetidas)

Como queremos hacer un modelo logistico y la variable respuesta son dos valores (Walk y Ride) entonces hacemos la siguiente transformacion.

```{r}
data$type = ifelse(data$type == 'Ride', 1, 0)
```

# Outliers
Ahora para poder eliminar los Outliers, utilizaremos el modelo estadistico de Grubbs. A su vez estaremos testeando y comparando tambien de forma visual. Si el p-value es menor a 0.01 entonces exite un alto porcentaje de confianza que el elemento sea un outlaiers, en este caso con un 99.99% de confianza

Forma visual de Outliers con boxplot

```{r}
boxplot(data$calories, horizontal = FALSE)
```

## De forma visual observamos el dato atipico.

```{r}
data %>% filter(calories > 300000)
```

Lo eliminamos, porque consideramos un error de digitalizarlo, ya que no tiene sentido que una persona consuma mas de 30 mil calor?as.

```{r}
data <- data %>% filter(calories < 300000)
```


Observamos los mas outlaiers en la data de calor?as. Por lo que entendemos que se deben eliminar mas datos at?picos

```{r}
boxplot(data$calories, horizontal = FALSE)
```

Dicho lo anterior, vamos a usar el m?todo de Grubbs y lo compararemos mas adelante con el m?todo de Naives Bayes, para poder entender y concluir que m?todo es mejor.

# Metodo estadistico de Grubbs


## Realizamos el test estad?stico de grubbs


```{r}
calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative
```

Entendemos que existe un dato err?neo con 43095 calor?as, por tanto lo sacamos de la data.
 
```{r}
data <- data %>% filter(calories != 43095)
```

Volvemos a realizar el test estadistico

```{r}
calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative
```

## Asi mismo realizamos el mismo procedimiento con los siguientes outlaiers

Vemos que 32600.6 es un outlier, lo sacamos

```{r}
data <- data %>% filter(calories != 32600.6)
```



Volvemos a realizar el test estadistico

```{r}
calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative
```


Vemos que 25456.1 es un outlier, lo sacamos

```{r}
data <- data %>% filter(calories != 25456.1)
```

Volvemos a realizar el test estadistico

```{r}
calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative
```

Vemos que 21048.5 es un outlier, lo sacamos
 
```{r}
data <- data %>% filter(calories != 21048.5)
```
 
Volvemos a realizar el test estadistico

```{r}
calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative
```


Vemos que 14541 es un outlier, lo sacamos

```{r}
data <- data %>% filter(calories != 14541)
```

Volvemos a realizar el test estadistico

```{r}
calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative
```

Vemos que 14392.5 es un outlier, lo sacamos

```{r}
data <- data %>% filter(calories != 14392.5)
```

Volvemos a realizar el test estadistico

```{r}
calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative
```

Vemos que 12250 es un outlier, lo sacamos

```{r}
data <- data %>% filter(calories != 12250)
```

Volvemos a realizar el test estadistico

```{r}
calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative
```

 Vemos que 10975 es un outlier, lo sacamos
 
```{r}
data <- data %>% filter(calories != 10975)
```
 

Volvemos a realizar el test estadistico

```{r}
calories_outlier = grubbs.test(data[['calories']], two.sided = FALSE)
calories_outlier$p.value
calories_outlier$alternative
```


Vemos que 10791.8 es un outlier, lo sacamos

```{r}
data <- data %>% filter(calories != 10791.8)
```



Como p-value es menor a 0.01 entonces con un 99.99% de confianza 43095 es un outlier

```{r}
distance_outlier = grubbs.test(data[['distance']], two.sided = FALSE)
distance_outlier$p.value
distance_outlier$alternative
```

Como p-value es menor a 0.01 entonces con un 99.99% de confianza 1478170 es un outlier

```{r}
elev_low_outlier = grubbs.test(data[['elev_low']], two.sided = FALSE)

elev_low_outlier$p.value
elev_low_outlier$alternative
```


Como p-value es menor a 0.01 entonces con un 99.99% de confianza 10490.2 es un outlier

```{r}
elev_high_outlier = grubbs.test(data[['elev_high']], two.sided = FALSE)

elev_high_outlier$p.value
elev_high_outlier$alternative
```


Como p-value es menor a 0.01 entonces con un 99.99% de confianza 12605.8 es un outlier

```{r}
max_speed_outlier = grubbs.test(data[['max_speed']], two.sided = FALSE)

max_speed_outlier$p.value
max_speed_outlier$alternative
```


Como p-value es menor a 0.01 entonces con un 99.99% de confianza 152.9 es un outlier


```{r}
average_speed_outlier = grubbs.test(data[['average_speed']], two.sided = FALSE)

average_speed_outlier$p.value
average_speed_outlier$alternative
```



Como p-value es menor a 0.01 entonces con un 99.99% de confianza 2296.088 es un outlier


Quitamos los outliers identificados

```{r}
data <- data %>% filter(calories != 43095)
data <- data %>% filter(distance != 1478170)
data <- data %>% filter(elev_low != 10490.2)
data <- data %>% filter(elev_high != 12605.8)
data <- data %>% filter(average_speed != 2296.088)
```


# Modelo 1 :

## Seleccion de variables para creaci?n del modelo.

En este paso, seleccionaremos las variables que son necesarias para usarlas para el  modelo que usaremos. Para la selecci?n veremos entre las variables cuales tienen la mayor correlaci?n entre ellas con respecto a la variable type. Si existe una correlacion alta entre variables que no sean type entonces quedan afuera.



```{r}

cor <-cor(data[c('type','athlete', 'calories', 'distance', 'elev_low', 'records', 
           'elev_high', 'moving_time', 'elapsed_time',  'average_speed',
           'has_heartrate', 'max_speed')])
cor
```

 Aqu? observamos cuales son las 3 correlaciones mas alta de la variable type, estas son:
 max_speed, has_heartrate, average_speed

Tiene sentido que las variables en funcion de la velocidad este en el modelo porque 
se espera que alguien que anda en bicicleta vaya mas rapido que alguien que camina.

## Planteamiento modelo

Como la variable es binaria es decir toma 1 si es Ride o 0 si es Walk entonces 
planteamos un modelo de regresion logpistica.

```{r}
modelo1 <- glm( type ~ max_speed + has_heartrate + average_speed , data = data, family = 'binomial')
summary(modelo1)
modelo1
```


Vemos que la Mediana de los residuos es cercana a cero.

El modelo nos dice que todas las variables escogidas son significativas estadisticamente porque vemos que el p-value es cercano a cero, es decir a un 99.99% de confianza

Las variables max_speed, has_heartache y average_speed son explicativas de la 
variale type (andar en bici o camina)

## Vamos a predecir los valores de 'type' segun el modelo.

A pesar que type deberia ser 1 o 0 se trate el concepto como probabilidad

```{r}
probabilidad <- predict(modelo1,type=c("response"))

data$probabilidad <- probabilidad
```


## Analisis de ajuste del modelo con la curva Roc.

Observamos la curva Roc
```{r}
curva_roc <- roc(type ~ probabilidad, data = data)
plot(curva_roc)   

```

Observamos el auc
```{r}
auc(curva_roc)
```


Vemos que el valor del AUC es de 93.7%, lo que significa que el modelo tiene un
gran poder de clasificacion


# Modelo 2 (modelo 1 solo incluyendo mas variables) :

```{r}
modelo2 <- glm( type ~ calories + distance + elev_low + records + elev_high + 
moving_time + elapsed_time + average_speed + has_heartrate + max_speed, data = data, family = 'binomial')
summary(modelo2)
```


Vemos que calories, distance, elev_high, moving_time y elapsed_time son no significativas

## Predicción del modelo.

```{r}
probabilidad2 <- predict(modelo2,type=c("response"))

data$probabilidad2 <- probabilidad2
```


## Analisis de ajuste del modelo2 con la curva Roc.

Observamos la curva roc2
```{r}
curva_roc2 <- roc(type ~ probabilidad2, data = data)
plot(curva_roc2)   

```

Observamos el auc
```{r}
auc(curva_roc2)
```


El modelo solo mejoró un 1% agregando todas las demas variables.Esto nos lleva a pensar que la sencillez del modelo 1 es mejor para predecir si alguien camina o anda en bicicleta.

 NO ES TAN EFICIENTE AGREGAR 7 VARIABLES PARA QUE EL MODELO MEJORE UN 1%


# Modelo 3 : Naive Bayes

Entrenamiento y Testeo
Dividimos la data para entrenar y testear en proporcion 80%, 20% con semilla 123 Mulear porque 80, 20.

```{r}
set.seed(123)
sample <- sample(1:nrow(data_modelo_knn), .8*167615)
```


## Modificamos la data, para tener las variables numericas y ahorrarnos futuros problemas

```{r}
data_modelo_knn$max_speed = as.numeric(data_modelo_knn$max_speed)
data_modelo_knn$records = as.numeric(data_modelo_knn$records)
data_modelo_knn$elev_low = as.numeric(data_modelo_knn$elev_low)
data_modelo_knn$elev_high = as.numeric(data_modelo_knn$elev_high)
data_modelo_knn$average_speed = as.numeric(data_modelo_knn$average_speed)
data_modelo_knn$has_heartrate = ifelse(data_modelo_knn$has_heartrate == FALSE, 0, 1)
data_modelo_knn$type = ifelse(data_modelo_knn$type == "Ride", 1, 0)

```


# Genero la data de entrenamieto y de testeo
Generamos data de entrenamiento y data de testeo, con objetivo usar distintos datas en distintos procesos. Es decir, generaremos con un 80% de la data el modelo y con el resto testearemos el modelo concluido.

```{r}
trainData <- data_modelo_knn[sample,]
testData <- data_modelo_knn[-sample,]

```


## Modelo Naive Bayes

```{r}
modelo_3 <- naiveBayes(type ~ max_speed + has_heartrate + average_speed, data = trainData)
pred <- predict(modelo_3, testData, type ="raw")

pred
```


En pred podemos ver la probabilidad de que un atleta camine o ande en bicicleta


```{r}
modelo_3
```

## Ajuste del modelo en funcion de la data testeo.

```{r}

testData$prob <- pred[,2]

```

Observamos la curva roc
```{r}
curva_roc <- roc(type ~ prob, data = testData)
plot(curva_roc)
```


Observamos el auc
```{r}
auc(curva_roc)
```

# El modelo de naive bayes se ajusta un 94,41%, como podemos observar en lo anterior.


Por lo tanto, observamos que en el modelo inicial, el modelo de Grubbs obtenemos que se ajusta con un 93% aproximado, a diferencia que en el ?ltimo modelo desarrollado, el modelo de Naive Bayes se ajusta en un 94,41%. Esto explica, que  el modelo de Naive Bayes tiene un mejor porcentaje, lo que concluye que es un modelo mas acertado.
