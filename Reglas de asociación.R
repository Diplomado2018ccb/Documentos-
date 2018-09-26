##### Reglas de Asociación  #####


## Cargar librerias
library(readr)
#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(knitr)


#### Este es un ejemplo de manual, solo para ver como funciona el algoritmo 


### Los datos se encuentran en el siguiente link

url.data    <- "https://www.dropbox.com/s/tub6vmcfb319uo3/groceries_4.csv?dl=1"
groceries    <- read.csv(url.data)


## Se explora la base de datos
groceries%>%
  str()

groceries%>%
  summary()

cbind(table(groceries$Item))
dim(table(groceries$Item))

groceries%>%
  ggplot(aes(.$Item, fill=.$Item))+
  geom_bar()+
  guides(fill=FALSE)+
  geom_text(stat = "count", aes(y = ..count.., label = ..count..))+
  theme(axis.text.x=element_text(angle=90, hjust=1))


## Se va a visualizar los datos con mayor conteo
groceries$Item%>%
  table()%>%
  cbind()

groceries$Item<-as.factor(groceries$Item)
t<-groceries$Item%>%
  table()%>%
  cbind()
colnames(t)<-'freq'  

tt<-data.frame(t)
summary(tt)

## Se filtra por 100 unidades

gr<-tt%>%
  filter(.$freq>=100)

dim(gr)[1]


groceries%>%
  group_by(.$Item)%>%
  filter(n()>100)%>%
  ggplot(aes(.$Item, fill=.$Item))+
  geom_bar(aes(y=..count..))+
  geom_text(stat = "count", aes(y = ..count.., label = ..count..))+
  guides(fill=FALSE)+
  theme(axis.text.x=element_text(angle=90, hjust=1))
  

### Se transforma la base de datos en transacciones

trx       <- groceries 
trx       <- split(trx$Item,trx$Id_Factura)
trx       <- as(trx,"transactions")

colnames(trx)%>%
  cbind()

inspect(trx[1:10,])



### Tamaño de las transacciones

hist(size(trx), breaks = 0:10, xaxt="n", 
     main = "Number of Items per basket", xlab = "#Items")
axis(1, at=seq(0,10,by=1), cex.axis=0.8)
mtext(paste("Total:", length(trx), "baskets,", sum(size(trx)), "items"))

n<-trx%>%
  size()
n%>%
  summary()

n%>%
  quantile(probs=seq(0,1,0.25))



data.frame(n) %>%
  ggplot(aes(x = n)) +
  geom_histogram() +
  labs(title = "Distribución del tamaño de las transacciones",
       x = "Tamaño") +
  theme_classic()


frecuencia_items <- itemFrequency(x = trx, type = "relative")
frecuencia_items %>% sort(decreasing = TRUE) %>% head(3)


## Se crea la regla

reglas <- apriori(trx, parameter=list(support=0.01, confidence = 0.01))

print(reglas)
inspect(reglas)
reglas <-sort(reglas, by="confidence", decreasing=TRUE) 
inspect(head(reglas,10))

### Revaluando la regla
soporte <- 100 / dim(trx)[1]
regla <- apriori(data = trx,
                    parameter = list(support = soporte,
                                     minlen = 1,
                                     confidence=0.9,
                                     maxlen = 10,
                                     target = "frequent itemset"))
#support: soporte mínimo que debe tener un itemset para ser considerado frecuente. Por defecto es 0.1.
# minlen: número mínimo de items que debe tener un itemset para ser incluido en los resultados. Por defecto 1.
# maxlen: número máximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10.
# target: tipo de resultados que debe de generar el algoritmo, pueden ser “frequent itemsets”, “maximally frequent itemsets”, “closed frequent itemsets”, “rules” o “hyperedgesets”.
# confidence: confianza mínima que debe de tener una regla para ser incluida en los resultados. Por defecto 0.8.
# maxtime: tiempo máximo que puede estar el algoritmo buscando subsets. Por defecto 5 segundos.


inspect(reglas)
reglas <-sort(reglas, by="confidence", decreasing=TRUE) 
inspect(head(reglas,10))

reglas%>%
  summary()

plot(reglas)

top_10_itemsets <- sort(regla, by = "support", decreasing = TRUE)[1:10]
top_10_itemsets%>%
  inspect()

as(top_10_itemsets, Class = "data.frame") %>%
  ggplot(aes(x = reorder(items, support), y = support)) +
  geom_col() +
  coord_flip() +
  labs(title = "Itemsets más frecuentes", x = "itemsets") +
  theme_bw()



inspect(sort(itemsets[size(itemsets) > 1], decreasing = TRUE)[1:10])



## Evaluar la regla

soporte <- 10 / dim(trx)[1]
reglas <- apriori(data = trx,
                  parameter = list(support = soporte,
                                   confidence = 0.40,
                                   target = "rules"))
reglas%>%
  summary()


inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))




# Entre más lejos este el lift de 1 se manifiesta que el resultado no es aleatorio

#Coverage: es el soporte de la parte izquierda de la regla (antecedente). Se interpreta como la frecuencia con la que el antecedente aparece en el conjunto de transacciones.
#Fisher exact test: devuelve el p-value asociado a la probabilidad de observar la regla solo por azar
metricas <- interestMeasure(reglas, measure = c("coverage", "fishersExactTest"),
                            transactions = trx)

metricas

quality(reglas) <- cbind(quality(reglas), metricas)
# inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))
df_reglas <- as(reglas, Class = "data.frame") 
df_reglas %>% as.tibble() %>% arrange(desc(confidence)) %>% head()


as(reglas, "data.frame") %>%
  arrange(desc(confidence)) %>%
  head(1) %>%
  pull(rules)

par(mfrow=c(1,1))


itemFrequencyPlot(trx,topN=5,type="absolute")
plot(head(reglas,5), method="graph", control=list(type="items"))









