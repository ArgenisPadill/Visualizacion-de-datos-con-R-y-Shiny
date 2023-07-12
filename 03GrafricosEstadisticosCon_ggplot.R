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
 
# Importar datos -----

## Datos para gráfico de barras ----

ENIGH <- read.csv("Bases/ENIGH/ENIGH_AAGPC.csv")

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

