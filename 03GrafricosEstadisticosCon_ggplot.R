########################################################
# Visualización de datos con R y Shiny                 #
# Argenis Alain Gustavo Padilla Córdoba                #
# Tema 3: Gráfricos Estadisticos con ggplot            #
# Elaboración propia con datos de ENIGH 2020           #
########################################################



# librerias a utilizar ----

library(tidyverse)
library(haven)
library(foreign)
library(readxl)
library(extrafont)
library(scales)
library(data.table)
library(devtools)
library(modeest)
library(ggplot2)
library(GGally) 
# Importar datos -----

## Datos para gráfico de barras ----

ENIGH <- read.csv("Bases/ENIGH/ENIGH_AAGPC.csv")

iris <- iris
##Histograma base   deaucero con la regla de Sturges ----


hist(ENIGH$edad_jefe)


#Histograma Base ----

ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_histogram()

##Histograma configurado con regla de Sturges y tema ----

ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_histogram(breaks = pretty(range(ENIGH$edad_jefe),
                                n=nclass.Sturges(ENIGH$edad_jefe),
                                min.n= 1),
                 fill="steelblue",col="black")+
  scale_x_continuous(breaks = pretty(range(ENIGH$edad_jefe),
                                     n=nclass.Sturges(ENIGH$edad_jefe),
                                     min.n= 1))+
  scale_y_continuous(breaks = seq(0,12000,1000))+
 
  labs(title = "Histograma de la edad del Jefe de familia",
       subtitle = "Intervalos Construidos por la regla de STURGES ",
       caption = "Elaboracion propia con datos del ENIGH 2020",
       x="Intervalos de Edad",y="Frecuencia")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#e0eee0"), 
        legend.background = element_rect(fill ="#e0eee0"),
        panel.grid.major = element_line(color="grey"),
        panel.grid.minor = element_blank(),
        plot.title = ggtext::element_markdown(hjust=0.5,
                                              size=18,
                                              colour="black"),
        plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                 size=16,
                                                 colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size=14, colour="black"),
        axis.ticks = element_blank(),
        axis.title = element_text(size=14, colour="black"),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="blac k"))

##Histograma configurado con regla de Sturges ,tema  agregando media mediana y moda  ----



estadisticos_educajefe <- data.frame(
  "Estadisticos" = c("Media", "Mediana", "Moda"),
  "Resultados"= c(mean(ENIGH$edad_jefe),
                  median(ENIGH$edad_jefe),
                  mfv1(ENIGH$edad_jefe))
)


### Histograma solamente la mediana


ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_histogram(breaks = pretty(range(ENIGH$edad_jefe),
                                 n=nclass.Sturges(ENIGH$edad_jefe),
                                 min.n= 1),
                 fill="steelblue",col="black")+
  scale_x_continuous(breaks = pretty(range(ENIGH$edad_jefe),
                                     n=nclass.Sturges(ENIGH$edad_jefe),
                                     min.n= 1))+
  scale_y_continuous(breaks = seq(0,12000,1000))+
  geom_vline(xintercept = mean(ENIGH$edad_jefe),
             linetype="dashed",
             colour= "red")+
  labs(title = "Histograma de la edad del Jefe de familia",
       subtitle = "Intervalos Construidos por la regla de STURGES ",
       caption = "Elaboracion propia con datos del ENIGH 2020",
       x="Intervalos de Edad",y="Frecuencia")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#e0eee0"), 
        legend.background = element_rect(fill ="#e0eee0"),
        panel.grid.major = element_line(color="grey"),
        panel.grid.minor = element_blank(),
        plot.title = ggtext::element_markdown(hjust=0.5,
                                              size=18,
                                              colour="black"),
        plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                 size=16,
                                                 colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size=14, colour="black"),
        axis.ticks = element_blank(),
        axis.title = element_text(size=14, colour="black"),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="blac k"))





ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_histogram(breaks = pretty(range(ENIGH$edad_jefe),
                                 n=nclass.Sturges(ENIGH$edad_jefe),
                                 min.n= 1),
                 fill="steelblue",col="black")+
  scale_x_continuous(breaks = pretty(range(ENIGH$edad_jefe),
                                     n=nclass.Sturges(ENIGH$edad_jefe),
                                     min.n= 1))+
  scale_y_continuous(breaks = seq(0,12000,1000))+
  geom_vline(data = estadisticos_educajefe,
             aes(xintercept=Resultados,
                 linetype=Estadisticos,
                 color=Estadisticos))+
  scale_color_manual(values = c("red","green","brown"))+
  labs(title = "Histograma de la edad del Jefe de familia",
       subtitle = "Intervalos Construidos por la regla de STURGES ",
       caption = "Elaboracion propia con datos del ENIGH 2020",
       x="Intervalos de Edad",y="Frecuencia")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#e0eee0"), 
        legend.background = element_rect(fill ="#e0eee0"),
        panel.grid.major = element_line(color="grey"),
        panel.grid.minor = element_blank(),
        plot.title = ggtext::element_markdown(hjust=0.5,
                                              size=18,
                                              colour="black"),
        plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                 size=16,
                                                 colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size=14, colour="black"),
        axis.ticks = element_blank(),
        axis.title = element_text(size=14, colour="black"),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="blac k"),
        legend.key = element_rect(fill = "#e0eee0"),
        axis.title.x = element_text(size = 16))



#Grafico de densidad ------
##Densidad Base----
##la dendidad es la probabilidad retomada por frecuencia

ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_density()

##Densidad Completa----


ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_density(color="red",
               fill="steelblue",
               alpha=0.25)+
  scale_x_continuous(breaks = pretty(range(ENIGH$edad_jefe),
                                     n=nclass.Sturges(ENIGH$edad_jefe),
                                     min.n= 1))+
  scale_y_continuous(breaks = seq(0,0.03,0.001))+
 
  scale_color_manual(values = c("red","green","brown"))+
  labs(title = "Densidad de la edad del Jefe de familia",
       subtitle = "Intervalos Construidos por la regla de STURGES ",
       caption = "Elaboracion propia con datos del ENIGH 2020",
       x="Intervalos de Edad",y="Densidad/Probabilidad")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#e0eee0"), 
        legend.background = element_rect(fill ="#e0eee0"),
        panel.grid.major = element_line(color="grey"),
        panel.grid.minor = element_blank(),
        plot.title = ggtext::element_markdown(hjust=0.5,
                                              size=18,
                                              colour="black"),
        plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                 size=16,
                                                 colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size=14, colour="black"),
        axis.ticks = element_blank(),
        axis.title = element_text(size=14, colour="black"),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="blac k"),
        legend.key = element_rect(fill = "#e0eee0"),
        axis.title.x = element_text(size = 16))

#Histograma con Gráfico de densidad -----

##Grafico Base ----

ENIGH %>%
  ggplot(aes(x=edad_jefe))+
  geom_histogram(aes(y=..density..))+
  geom_density()

ENIGH %>%
  ggplot(aes(x=edad_jefe))+
  geom_histogram(aes(y= after_stat(density)))+
  geom_density()



##Ajuste del grafico ----
###Retomando el Data.frame de Estadisticos_educajefe
estadisticos_educajefe <- estadisticos_educajefe %>% 
  mutate(Etiquetas = paste(Estadisticos, ":","", round(Resultados,2)))


ENIGH %>%
  ggplot(aes(x=edad_jefe))+
  geom_histogram(aes(y= after_stat(density)),
                 breaks= pretty(range(ENIGH$edad_jefe),
                                n = nclass.Sturges(ENIGH$edad_jefe),
                                min.n = 1),
                 color="black", fill="steelblue")+
                 
               
  geom_density(color="darkred",
               # fill="darkgreen",Se omite el rellenos de densidad por que no es estetico
               alpha=0.25,
               linetype="solid",
               size=1)+ #dashed es punteado , solid lineas solidas+
  
  geom_vline(data = estadisticos_educajefe,
             aes(xintercept= Resultados,
                 linetype=Etiquetas,
                 color=Etiquetas))  +
  scale_x_continuous(breaks= pretty(range(ENIGH$edad_jefe),
                                    n = nclass.Sturges(ENIGH$edad_jefe),
                                    min.n = 1)) +
  scale_color_manual(values = c("chocolate","darkorchid4","gold4"))+
  labs(title = "Histograma y densidad  de la edad del Jefe de familia",
       subtitle = "Intervalos Construidos por la regla de STURGES ",
       caption = "Elaboracion propia con datos del ENIGH 2020",
       x="Intervalos de Edad",y="Densidad / Probabilidad",
       color="" , linetype="")+
  theme(legend.position = "bottom",
        panel.background = element_blank(), 
        plot.background = element_rect(fill = "#e0eee0"), 
        legend.background = element_rect(fill ="#e0eee0"),
        panel.grid.major = element_line(color="grey"),
        panel.grid.minor = element_blank(),
        plot.title = ggtext::element_markdown(hjust=0.5,
                                              size=18,
                                              colour="black"),
        plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                 size=16,
                                                 colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size=14, colour="black"),
        axis.ticks = element_blank(),
        axis.title = element_text(size=14, colour="black"),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="blac k"),
        legend.key = element_rect(fill = "#e0eee0"),
        axis.title.x = element_text(size = 16))

 

 


# grafico de caja y bigotes ----
## grafico base----
ENIGH %>% 
  ggplot(aes(x="", y=edad_jefe))+
  stat_boxplot(geom = "errorbar",
               width=0.15,
               color="black")+
  geom_boxplot(fill="steelblue",
               alpha=0.5,
               color="black",
               outlier.colour = "red")

##Grafico final----

ENIGH %>% 
  ggplot(aes(x="", y=edad_jefe))+
  stat_boxplot(geom = "errorbar",
               width=0.15, #tamaño bigotes
               color="black")+
  geom_boxplot(fill="steelblue",
               alpha=0.5,
               color="black",
               outlier.colour = "red",
               width= 0.5)+ #tamaño de la caja
  stat_summary(fun = mean , geom = "point",
               shape=20 , size=3, color="red",fill="red")+#para ver media
  scale_y_continuous(breaks=seq(min(ENIGH$edad_jefe),
                         max(ENIGH$edad_jefe),
                         5))+
  labs(title = "Diagrama de cajas y bigotes  de la edad del Jefe de familia",
       subtitle = "",
       caption = "Elaboracion propia con datos del ENIGH 2020",
       x="",
       y="Edad del jefe de familia",
       color="" , linetype="")+
  theme(legend.position = "bottom",
        panel.background = element_blank(), 
        plot.background = element_rect(fill = "#e0eee0"), 
        legend.background = element_rect(fill ="#e0eee0"),
        panel.grid.major = element_line(color="grey"),
        panel.grid.minor = element_blank(),
        plot.title = ggtext::element_markdown(hjust=0.5,
                                              size=18,
                                              colour="black"),
        plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                 size=16,
                                                 colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size=14, colour="black"),
        axis.ticks = element_blank(),
        axis.title = element_text(size=14, colour="black"),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="blac k"),
        legend.key = element_rect(fill = "#e0eee0"),
        axis.title.x = element_text(size = 16))


## Grafica de cajas con factores----

#1 Bajo
#2 Medio Bajo
#3 Medio Alto
#4 Alto

Valores <- c ("Bajo", "Medio Bajo", "Medio Alto", "Alto")
est_socio <- c(1,2,3,4)
for(i in 1:4){
  ENIGH$est_socio[ENIGH$est_socio==est_socio[i]] <- Valores[i]
}

unique(ENIGH$est_socio)

ENIGH$est_socio <- factor(ENIGH$est_socio,
                          levels = c ("Bajo", "Medio Bajo", "Medio Alto", "Alto"))





ENIGH %>% 
  ggplot(aes(x=est_socio, y=edad_jefe))+
  stat_boxplot(geom = "errorbar",
               width=0.15, #tamaño bigotes
               color="black")+
  geom_boxplot(fill="steelblue",
               alpha=0.5,
               color="black",
               outlier.colour = "red",
               width= 0.5)+ #tamaño de la caja
  stat_summary(fun = mean , geom = "point",
               shape=20 , size=3, color="red",fill="red")+#para ver media
  scale_y_continuous(breaks=seq(min(ENIGH$edad_jefe),
                                max(ENIGH$edad_jefe),
                                5))+
  labs(title = "Diagrama de cajas y bigotes  de la edad del Jefe de familia",
       subtitle = "Dividido por factor de nivel socioeconomico",
       caption = "Elaboracion propia con datos del ENIGH 2020",
       x="",
       y="Edad del jefe de familia",
       color="" , linetype="")+
  theme(legend.position = "bottom",
        panel.background = element_blank(), 
        plot.background = element_rect(fill = "#e0eee0"), 
        legend.background = element_rect(fill ="#e0eee0"),
        panel.grid.major = element_line(color="grey"),
        panel.grid.minor = element_blank(),
        plot.title = ggtext::element_markdown(hjust=0.5,
                                              size=18,
                                              colour="black"),
        plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                 size=16,
                                                 colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size=14, colour="black"),
        axis.ticks = element_blank(),
        axis.title = element_text(size=14, colour="black"),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="blac k"),
        legend.key = element_rect(fill = "#e0eee0"),
        axis.title.x = element_text(size = 16))


#Grafico de Violin----
##Grafico Base----

ENIGH %>% 
  ggplot(aes(x="",y=edad_jefe))+
  geom_violin()


ENIGH %>% 
  ggplot(aes(x=edad_jefe,y=""))+
  geom_violin()

##Grafico Final ----
ENIGH %>% 
ggplot(aes(x=est_socio, y=edad_jefe))+
  geom_violin(fill="steelblue",
               alpha=0.5,
               color="black",
               
               width= 0.5)+ #tamaño de la caja
  stat_summary(fun = mean , geom = "point",
               shape=20 , size=3, color="red",fill="red")+#para ver media
  scale_y_continuous(breaks=seq(min(ENIGH$edad_jefe),
                                max(ENIGH$edad_jefe),
                                5))+
  labs(title = "Diagrama de Violin  de la edad del Jefe de familia",
       subtitle = "Dividido por factor de nivel socioeconomico",
       caption = "Elaboracion propia con datos del ENIGH 2020",
       x="",
       y="Edad del jefe de familia",
       color="" , linetype="")+
  theme(legend.position = "bottom",
        panel.background = element_blank(), 
        plot.background = element_rect(fill = "#e0eee0"), 
        legend.background = element_rect(fill ="#e0eee0"),
        panel.grid.major = element_line(color="grey"),
        panel.grid.minor = element_blank(),
        plot.title = ggtext::element_markdown(hjust=0.5,
                                              size=18,
                                              colour="black"),
        plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                 size=16,
                                                 colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size=14, colour="black"),
        axis.ticks = element_blank(),
        axis.title = element_text(size=14, colour="black"),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="blac k"),
        legend.key = element_rect(fill = "#e0eee0"),
        axis.title.x = element_text(size = 16))



#Graficos de correlacion ----


GC <- iris[,1:4] %>%  

  rename(`longitud de tallo` = Sepal.Length,
          `Ancho de Tallo` = Sepal.Width,
          `Longitud del petalo` = Petal.Length,
          `Ancho del Petalo` = Petal.Width) %>% 
  ggpairs(columns= 1:4,
          aes(color = Species))+
  labs(title = "Gráfico de correlacion entre las variables de IRIS",
      subtitle= "Para todas las especies",
      caption = "Elaboracion propia")+
  
  theme(legend.position = "bottom",
        panel.background = element_blank(), 
        plot.background = element_rect(fill = "#e0eee0"), 
        legend.background = element_rect(fill ="#e0eee0"),
        panel.grid.major = element_line(color="grey"),
        panel.grid.minor = element_blank(),
        plot.title = ggtext::element_markdown(hjust=0.5,
                                              size=18,
                                              colour="black"),
        plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                 size=16,
                                                 colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size=14, colour="black"),
        axis.ticks = element_blank(),
        axis.title = element_text(size=14, colour="black"),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="blac k"),
        legend.key = element_rect(fill = "#e0eee0"),
        axis.title.x = element_text(size = 16),
        strip.text=element_text(size=14,color="white"),
        strip.background= element_rect(fill="black"))

GC



iris


