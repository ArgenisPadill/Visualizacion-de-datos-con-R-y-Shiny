## Censo de población 2020 ----


Censo2020 <- read.csv(file = "Bases/CENSO2020/Censo_VS.csv")

### Construcción de base para piramide poblacional ----

## División de edades

Censo2020 <- Censo2020 %>%
  mutate(Edad4=case_when(EDAD %in% c(0:4)~"De 0 a 4 años",
                         EDAD %in% c(5:9)~"De 5 a 9 años",
                         EDAD %in% c(10:14)~"De 10 a 14 años",
                         EDAD %in% c(15:19)~"De 15 a 19 años",
                         EDAD %in% c(20:24)~"De 20 a 24 años",
                         EDAD %in% c(25:29)~"De 25 a 29 años",
                         EDAD %in% c(30:34)~"De 30 a 34 años",
                         EDAD %in% c(35:39)~"De 35 a 39 años",
                         EDAD %in% c(40:44)~"De 40 a 44 años",
                         EDAD %in% c(45:49)~"De 45 a 49 años",
                         EDAD %in% c(50:54)~"De 50 a 54 años",
                         EDAD %in% c(55:59)~"De 55 a 59 años",
                         EDAD %in% c(60:64)~"De 60 a 64 años",
                         EDAD %in% c(65:69)~"De 65 a 69 años",
                         EDAD %in% c(70:74)~"De 70 a 74 años",
                         EDAD %in% c(75:79)~"De 75 a 79 años",
                         EDAD %in% c(80:84)~"De 80 a 84 años",
                         EDAD %in% c(85:130)~"Mas de 84 años",
                         EDAD ==999~"No especificado"))

Censo2020$Edad4<-factor(x=Censo2020$Edad4,
                        levels = c("De 0 a 4 años",
                                   "De 5 a 9 años",
                                   "De 10 a 14 años",
                                   "De 15 a 19 años",
                                   "De 20 a 24 años",
                                   "De 25 a 29 años",
                                   "De 30 a 34 años",
                                   "De 35 a 39 años",
                                   "De 40 a 44 años",
                                   "De 45 a 49 años",
                                   "De 50 a 54 años",
                                   "De 55 a 59 años",
                                   "De 60 a 64 años",
                                   "De 65 a 69 años",
                                   "De 70 a 74 años",
                                   "De 75 a 79 años",
                                   "De 80 a 84 años",
                                   "Mas de 84 años",
                                   "No especificado"))

Censo2020 <- Censo2020 %>%
  mutate(Sexo=case_when(SEXO==1~"Hombre",
                        SEXO==3~"Mujer"))

## Total por edades y sexo

Total_edades_sexo<-Censo2020 %>%
  filter(EDAD %in% c(0:130)) %>%
  group_by(Edad4, Sexo) %>%
  summarise(Total=sum(FACTOR)) %>%
  ungroup() %>%
  group_by(Edad4) %>%
  mutate(FR=Total/sum(Total),
         Porcentaje=paste(round(FR*100,2),
                          "%", sep=""))

Total_edades_sexo2<-Censo2020 %>%
  filter(EDAD %in% c(0:130)) %>%
  group_by(Edad4, Sexo) %>%
  summarise(Total=sum(FACTOR)) %>%
  ungroup() %>%
  mutate(FR=Total/sum(Total),
         Porcentaje=paste(round(FR*100,2),
                          "%", sep=""))

### Exportar datos ----

list_of_datasets <- list("ptc_edad" = Total_edades_sexo,
                         "ptc_total" = Total_edades_sexo2)


openxlsx::write.xlsx(list_of_datasets,
                     file = "Bases/CENSO2020/Datos_piramide_pob.xlsx")