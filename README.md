

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
