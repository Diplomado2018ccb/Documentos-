###### Análisis Exploratorio de Datos #####
### BBDD: Celebrity Deaths ######
#### Fuente : Kaggle ######
### Se intenta identificar clasificar las causas de las muertes de los famosos y hacer 
#### Inferencia sobre el conocimiento que se pueden extraer de estos datos #####


### Librerias ####
library(dplyr)  # Para manuipular datos, sirve para trabajar con objetos similares en marco de datos
library(ggplot2)  # Es una forma de gráficar a través de parámetros establecidos y en dimensiones
library(readr) # Lee bases de datos CVS y TXT
library(ggvis) # Paquete de visualización
library(sqldf) # SQL
library(reshape)
library(extrafont)  # Exporta la fuente del script
library(wesanderson) # paleta de colores
library(knitr)
library(tab)
### Cargar Base de datos

celebridades <- read.csv("~/Desktop/Script_Uniempresarial/Bases_de_datos/celebrity_deaths_4.csv",na.strings = c("", "NA"), stringsAsFactors = F)



### Examinar base de datos

## Se busca la estructura de la BBDD

# Se visualiza la BBDD de forma compacta e imprime todas las columnas que caben dentro de la pantalla
tbl_df(celebridades)

celebrity_deaths_4%>%
  glimpse()

# Algunas medidas de resumen 
summary(celebrity_deaths_4)

## Imputación de valores perdidos
celebrity_deaths_4$fame_score[is.na(celebrity_deaths_4$fame_score)]<-
  median(celebrity_deaths_4$fame_score,na.rm = TRUE)
summary(celebrity_deaths_4$fame_score)

## Se crea  la columna de causa del fallecimiento on SQL
celebrity_deaths_4<- sqldf("SELECT *,
CASE 
                           WHEN cause_of_death LIKE '%cancer%' OR 'leukemia' THEN 'Cancer'
                           WHEN cause_of_death LIKE 'natural' OR '%sleep%' OR '%health%' THEN 'Causas Naturales' 
                           WHEN cause_of_death LIKE '%murder%' OR 'shot' THEN 'Muerte'
                           WHEN cause_of_death LIKE '%Alzheimer%' OR '%Parkinson%' THEN 'Alzheimer o Parkinson'
                           WHEN cause_of_death LIKE '%heart%' OR '%stroke%' OR '%cardiac%' THEN 'Corazón'
                           WHEN cause_of_death LIKE '%suicide%' THEN 'Suicidio'
                           WHEN cause_of_death LIKE '%pneumonia%' OR '%respiratory%' THEN 'Respiratorias'
                           WHEN cause_of_death LIKE '%crash%' OR '%accident%' OR '%fall%' OR '%collision%' OR '%car%' THEN 'Accident'
                           WHEN cause_of_death IS NULL THEN NULL
                           ELSE 'Otras' END AS cause_group
                           FROM celebrity_deaths_4")

celebrity_deaths_4$cause_group <- factor(celebrity_deaths_4$cause_group)
table(celebrity_deaths_4$cause_group)%>% cbind()
## Se visualiza la base de datos
celebrity_deaths_4%>%
  head(5)%>%
  tbl_df()


## Se tabula las causas de la muerte
celebrity_deaths_4$cause_group%>%
  table()%>%
  cbind()

## Se crea una tabla de proporciones
Prop<-round(prop.table(table(celebrity_deaths_4$cause_group))*100,2)%>%
  cbind()

Prop
Proporcionalidad<- sprintf("%.1f%%",prop.table(table(celebrity_deaths_4$cause_group))*100, 3)
Proporcionalidad

cbind(Prop,Proporcionalidad)



celebrity_deaths_4$death_year <- factor(celebrity_deaths_4$death_year)


#### Se genera el conteo del año de la muerte
count(celebrity_deaths_4,death_year)

cbind(table(celebrity_deaths_4$death_year))

a<-cbind(table(celebrity_deaths_4$death_year))
b<-round(prop.table(a)*100,2)
colnames(b)<-'Proporción'

b



#### Visualización
text_theme <- theme(text = element_text(size = 10, 
                                        family = "Verdana", 
                                        face = "italic"),
                    plot.title = element_text(hjust = 0.5))


color_theme <- scale_fill_brewer(palette = "RdYlBu", 
                                 na.value = "grey90")


celebrity_deaths_4%>%
  ggplot(aes(.$death_year))+
  geom_bar(aes(fill=.$cause_group))+
  text_theme+
  color_theme+
  labs(title='Causas de Muerte por año')+
  xlab('Año')+
  ylab('Conteo')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


### Se debe recordar
cbind(table(celebrity_deaths_4$death_year))
a<-cbind(table(celebrity_deaths_4$death_year))
b<-cbind(prop.table(table(celebrity_deaths_4$death_year))*100)
table_de_conteo_prp<-cbind(a,b)
table_de_conteo_prp

sum(table_de_conteo_prp[,2]) ## Suma proporcional
sum(table_de_conteo_prp[,1]) ## Suma total 



### Segunda visualización
celebrity_deaths_4%>%
  ggplot(aes(.$fame_score))+
  geom_histogram(fill="skyblue",col="black")+
  geom_vline(aes(xintercept = median(celebrity_deaths_4$fame_score)), 
             col = "grey",
             size = 1)+
  labs(title='Distribución del indicador de Fama', x='Fama',y='Conteo')+ ## Sesgo hacia la derecha
  coord_cartesian(xlim = c(0, 150)) +
  text_theme



## Se encuentran tres categorias 
# Localment famoso, debajo de la mediana
# Famoso : Sobre la mediana y superior
# Mundialmente famoso: dos veces la media y superior


celebrity_deaths_4$Grupo_de_fama<-factor(findInterval(celebrity_deaths_4$fame_score,c(0,4,8)))
levels(celebrity_deaths_4$Grupo_de_fama)<-c('Localmente Famoso','Famoso','Mundialmente Famoso')

celebrity_deaths_4%>%
  head(2)



### Otro factor a evaluar

celebrity_deaths_4$age%>%
  summary()

celebrity_deaths_4%>%
  ggplot(aes(.$age, fill=.$cause_group))+
  geom_bar(aes(y=..prop..))+
  theme(legend.position = 'bottom')+
  labs(title='Proporción de la edad según\n la causa de muerte', x='edad',
       y='Prop')+
  theme(plot.title = element_text(hjust=0.5, colour ='red'))+
  guides(fill=guide_legend(title = 'Causas de\n muerte'))


## Crear intervalos 
## Edades que importan

celebrity_deaths_4$age[celebrity_deaths_4$age>100 |celebrity_deaths_4$age<20]<-'NA'
celebrity_deaths_4$grupos_edad<-factor(findInterval(celebrity_deaths_4$age,c(20,40,60,80,100)))
levels(celebrity_deaths_4$grupos_edad)<-c('20-39','40-59','60-79','80-100', '101-')

#Limpiando los valores no conocidos o 'NA'

celebrity_deaths_4%>%
  filter(!is.na(.$grupos_edad))%>%
  ggplot(aes(x=reorder(grupos_edad,grupos_edad, 
                       function(x)-length(x))))+
  geom_bar(aes(fill=Grupo_de_fama), position = 'dodge')+
  labs(title='Grupo de famosos según la categoría', x='Rango edad', y="conteo de muertes")+
  theme(plot.title = element_text(hjust = 0.5,colour = 'red'))+
  theme_classic()+
  theme(legend.position = 'bottom')+
  guides(fill=guide_legend(title = 'Grupo de\n fama'))


Mayores<-celebrity_deaths_4%>%
  filter(age>= 90)


###  Comparando a los famosos con el resto del grupo según la edad 

celebrity_deaths_4%>%
  filter(.$Grupo_de_fama=="Famoso" & .$grupos_edad==c('60-79','50-59'))%>%
  ggplot(aes(.$death_year))+
  geom_bar(aes(fill=cause_group),stat = 'count')+
  labs(title='Muerte de famosos desde los 40 años', x='grupo de edad',
       y='Conteo de muertes')+
  theme_bw()+
  theme(legend.position = 'bottom')

ggplot(celebrity_deaths_4 %>% filter(Grupo_de_fama == "Famoso" & grupos_edad == c('60-79','40-59')), 
       aes(x = death_year)) +
  geom_bar(aes(fill = cause_group), 
           stat = "count") +
  text_theme +
  color_theme +
  labs(title = "Causas de muerte de famosos desde los 40 años", 
       x = "Grupo de edad", 
       y = "Número de muertes")+
  theme(legend.position = 'bottom')


## causas de muerte a menores de 40
table(celebrity_deaths_4$grupos_edad)
ggplot(celebrity_deaths_4 %>% filter(Grupo_de_fama == "Famoso" & grupos_edad == c('20-39')), 
       aes(x = death_year)) +
  geom_bar(aes(fill = cause_group), 
           stat = "count") +
  text_theme +
  color_theme +
  labs(title = "Causas de muerte de famosos menores de 40 años", 
       x = "Grupo de edad", 
       y = "Número de muertes")+
  theme(legend.position = 'bottom')

## Causa de la muerte entre los datos faltantes
ggplot_missing <- function(x) {
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle = 45, vjust = 0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
ggplot_missing(celebrity_deaths_4 %>% 
                 select(-cause_group, -fame_score,-age))
names(celebrity_deaths_4)

## Cúal es la causa mayor de muertes?

ggplot(celebrity_deaths_4, aes(x = reorder(cause_group, 
                                 cause_group, 
                                 function(x) length(x)))) +
  geom_bar(aes(fill = cause_group), 
           stat = "count") +
  text_theme +
  color_theme +
  labs(title = "Cuáles son las mayores causas de muerte? ", 
       x = "Causas de muerte", 
       y = "Número de muertes") +
  coord_flip() +
  guides(fill = FALSE)+
  theme_classic()



celebrity_deaths_4%>%
  head(5)%>%
  kable()


