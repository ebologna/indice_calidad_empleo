library(eph)
library(foreign)
library(questionr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(kableExtra)
library(ggcorrplot)
library(rstatix)
library(quantreg)

datos <- get_microdata(year = 2005:2021, 
                       trimester = 2,
                       type = 'individual')


# Choose data frame: lo hago de a un año, veamos si sale
# de un saque, para comparar los histogramas, no solo la serie de medias

base<-datos[datos$ANO4==2016,]


# Label variables & levels  


base <- rename(base, año = ANO4)
base$sexo = ifelse(base$CH04==1, "Varón", "Mujer")

base$REGION_rot<-as.factor(base$REGION)
levels(base$REGION_rot)<-c("Gran Buenos Aires", "NOA",
                           "NEA", "Cuyo", "Pampeana", "Patagónica")

base$CH06[base$CH06==-1]<-0
base$grupos_edades<-cut(base$CH06, c(0,24,54,max(base$CH06)))

base$grupos_edades_2<-quant.cut(base$CH06, 3)

base$NIVEL_ED[base$NIVEL_ED==9]<-NA
base$NIVEL_ED[base$NIVEL_ED==7]<-0
base$educacion<-as.factor(base$NIVEL_ED)
levels(base$educacion)<-c("nunca asistió", "primario incompleto", "primario completo",
                          "secundario incompleto", "secundario completo", "superior incompleto", 
                          "superior completo")

base$PP04A[base$PP04A==9]<-NA
base$PP04A[base$PP04A==0]<-NA
base$sector<-as.factor(base$PP04A)
levels(base$sector)<-c("estatal", "privado", "otro")



# Keep working people  


ocupades<-subset(base, base$ESTADO==1)
table(ocupades$CAT_OCUP)



# Security = stability + health protection  

## employees  

### Stability   

# combines "term" or "permanent"  (PP07C) with duration of "term" (PP07D).    
# just for employees

asalariades<-subset(ocupades, ocupades$CAT_OCUP==3 &
                      ocupades$PP04B1!=1)
table(asalariades$PP07C)
asalariades$estabilidad<-ifelse(asalariades$PP07C==2, 6, ifelse(
  asalariades$PP07C==1 & asalariades$PP07D==5, 5, ifelse(
    asalariades$PP07C==1 & asalariades$PP07D==4, 4, ifelse(
      asalariades$PP07C==1 & asalariades$PP07D== 3, 3, ifelse(
        asalariades$PP07C==1 & asalariades$PP07D== 2, 2, ifelse(
          asalariades$PP07C==1 & asalariades$PP07D== 1, 1, NA
        )
      )
    )
  )
))
table(asalariades$estabilidad)


### Health protection  

table(asalariades$PP07G4)

asalariades$obra_social <-1
asalariades$obra_social[asalariades$PP07G4==0]<-NA
asalariades$obra_social[asalariades$PP07G4==9]<-NA
asalariades$obra_social[asalariades$PP07G4==2]<-0

table(asalariades$PP07G4, asalariades$obra_social)



### Combinación  
#Para que tengan igual peso se estandarizan al intervalo [0 - 0.50] por medio de $0.50*\frac{x-min}{max-min}$ cada una  


asalariades$estabilidad_st<-.5*(asalariades$estabilidad-1)/5
summary(asalariades$estabilidad_st)

asalariades$obra_social_st<-.5*asalariades$obra_social
summary(asalariades$obra_social_st)


## Seguridad  
#Combinación aditiva de las dos  

asalariades$seguridad<-asalariades$estabilidad_st+
  asalariades$obra_social_st
summary(asalariades$seguridad)



## Cuenta propia  

### Estabilidad  
#Combina capital (maquinaria PP05C_1, local PP05C_2, vehículo PP05C_3) y clientes (uno solo o varios PP05F)  
#Se hace para el subconjunto de cuenta propia  


cuentapropia<-subset(ocupades,
                     (ocupades$CAT_OCUP==2 | ocupades$CAT_OCUP ==1) &
                       ocupades$PP04B1!=1)
table(cuentapropia$PP05C_1)
table(cuentapropia$PP05C_2)
table(cuentapropia$PP05C_3)
table(cuentapropia$PP05F)

cuentapropia$maquinas<-0
cuentapropia$maquinas[cuentapropia$PP05C_1==0]<-NA
cuentapropia$maquinas[cuentapropia$PP05C_1==9]<-NA
cuentapropia$maquinas[cuentapropia$PP05C_1==1]<-2
cuentapropia$maquinas[cuentapropia$PP05C_1==2]<-1
table(cuentapropia$maquinas, cuentapropia$PP05C_1)

cuentapropia$local<-0
cuentapropia$local[cuentapropia$PP05C_2==0]<-NA
cuentapropia$local[cuentapropia$PP05C_2==9]<-NA
cuentapropia$local[cuentapropia$PP05C_2==1]<-2
cuentapropia$local[cuentapropia$PP05C_2==2]<-1
table(cuentapropia$local, cuentapropia$PP05C_2)

cuentapropia$vehiculo<-0
cuentapropia$vehiculo[cuentapropia$PP05C_3==0]<-NA
cuentapropia$vehiculo[cuentapropia$PP05C_3==9]<-NA
cuentapropia$vehiculo[cuentapropia$PP05C_3==1]<-2
cuentapropia$vehiculo[cuentapropia$PP05C_3==2]<-1
table(cuentapropia$vehiculo, cuentapropia$PP05C_3)

cuentapropia$clientes<-0
cuentapropia$clientes[cuentapropia$PP05F==0]<-NA
cuentapropia$clientes[cuentapropia$PP05F==9]<-NA
cuentapropia$clientes[cuentapropia$PP05F==6]<-0
cuentapropia$clientes[cuentapropia$PP05F==7]<-3

table(cuentapropia$clientes, cuentapropia$PP05F)

cuentapropia$estabilidad<-cuentapropia$maquinas+cuentapropia$local+
  cuentapropia$vehiculo+cuentapropia$clientes

table(cuentapropia$estabilidad)



### Obra social (del cuestionario hogar)

table(cuentapropia$CH08)
cuentapropia$obra_social<-1
cuentapropia$obra_social[cuentapropia$CH08==4]<-0
cuentapropia$obra_social[cuentapropia$CH08==9]<-NA

table(cuentapropia$CH08, cuentapropia$obra_social)



### Combinación  
#Para que tengan igual peso se estandarizan al intevalo [0 - 0.50] por medio de $0.50*\frac{x-min}{max-min}$ cada una y luego se suman  


cuentapropia$estabilidad_st<-.5*(cuentapropia$estabilidad)/9
summary(cuentapropia$estabilidad_st)

cuentapropia$obra_social_st<-.5*cuentapropia$obra_social
summary(cuentapropia$obra_social_st)



## Seguridad  
#Combiación aditiva de las dos  


cuentapropia$seguridad<-cuentapropia$estabilidad_st+
  cuentapropia$obra_social_st
summary(cuentapropia$seguridad)


## Servicio doméstico  


servicio_domestico<-subset(ocupades, ocupades$PP04B1==1)


### Estabilidad  
#Combina vacaciones, aguinaldo, dias por enfermedad y obra social


table(servicio_domestico$PP07G1)
table(servicio_domestico$PP07G2)
table(servicio_domestico$PP07G3)
table(servicio_domestico$PP07G4)

servicio_domestico$PP07G1[servicio_domestico$PP07G1==2]<-0
servicio_domestico$PP07G1[servicio_domestico$PP07G1==9]<-NA

servicio_domestico$PP07G2[servicio_domestico$PP07G2==2]<-0
servicio_domestico$PP07G2[servicio_domestico$PP07G2==9]<-NA

servicio_domestico$PP07G3[servicio_domestico$PP07G3==2]<-0
servicio_domestico$PP07G3[servicio_domestico$PP07G3==9]<-NA

servicio_domestico$PP07G4[servicio_domestico$PP07G4==2]<-0
servicio_domestico$PP07G4[servicio_domestico$PP07G4==9]<-NA

servicio_domestico$seguridad<-(servicio_domestico$PP07G1+
                                 servicio_domestico$PP07G2+
                                 servicio_domestico$PP07G3+
                                 servicio_domestico$PP07G4)/4
summary(servicio_domestico$seguridad)


### Se eliminan las variables que no están en las tres bases y se unen nuevamente  


asalariades<-subset(asalariades, select = -c(estabilidad, obra_social,
                                             estabilidad_st,obra_social_st))
cuentapropia<-
  subset(cuentapropia, select=-c(local, clientes, vehiculo, maquinas,
                                 estabilidad, obra_social, estabilidad_st,
                                 obra_social_st))

ocupades<-data.frame(rbind(asalariades, cuentapropia, servicio_domestico))
summary(ocupades$seguridad)
ggplot(ocupades)+geom_histogram(aes(seguridad), bins = 25)

summary(ocupades$seguridad)




# Consistencia  
#Es la relación entre la calificación de la tarea y la educación del trabajador  


class(ocupades$PP04D_COD)
ocupades$calif.ocup= substr(ocupades$PP04D_COD, 5,5)

ocupades$calif.ocup[ocupades$calif.ocup>4]<-NA
table(ocupades$calif.ocup)




#Se considera consistente (1) si educación y calificación están ambas por encima o ambas por debajo de sus medianas e inconsistente (0) en caso contrario  


ocupades$educa_num<-as.numeric(ocupades$educacion)
ocupades$calif_num<-as.numeric(ocupades$calif.ocup)

ocupades$consistencia<-as.factor(
  ifelse(
    (ocupades$educa_num >= median(ocupades$educa_num, na.rm = TRUE)) &
      (ocupades$calif_num >= median(ocupades$calif_num, na.rm = TRUE)) |
      (ocupades$educa_num < median(ocupades$educa_num, na.rm = TRUE)) &
      (ocupades$calif_num < median(ocupades$calif_num, na.rm = TRUE)),1,0))








ggplot(subset(ocupades, ocupades$calif.ocup>=1 & ocupades$calif.ocup<5))+
  geom_point(aes(NIVEL_ED, calif.ocup, col=consistencia), alpha=.1, size=3)





# Ingresos  

#Se construye el ingreso laboral, combinando ingresos salariales con ingresos de cuentapropistas con o sin socios (en EPH son tres variables)    


ocupades$ingreso_laboral<-ifelse(
  (ocupades$CAT_OCUP==1 | ocupades$CAT_OCUP ==2) &
    ocupades$PP06A==1,ocupades$PP06D, ifelse(
      (ocupades$CAT_OCUP==1 | ocupades$CAT_OCUP ==2) &
        ocupades$PP06A==2,ocupades$PP06C, ifelse(
          ocupades$CAT_OCUP==3, ocupades$PP08D1, 0
        )
    )
)
summary(ocupades$ingreso_laboral)


#Se retienen los casos con ingreso no nulo y horas semanales trabajadas no nulas y menores a 168  


ocupades<-subset(ocupades, ocupades$ingreso_laboral>0 &
                   ocupades$PP3E_TOT>0 & ocupades$PP3E_TOT < 168
)


#raw-hourly-income  
#It´s the quotient Definido como el cociente entre el ingreso mensual y cuatro veces las hoas trabajadas en la semana de referencia

ocupades$ingreso_hora_bruto<-ocupades$ingreso_laboral/(4*ocupades$PP3E_TOT)
summary(ocupades$ingreso_hora_bruto)

#Those over $P_{99}$ are dropped  


ocupades<-subset(ocupades, ocupades$ingreso_hora_bruto<
                   quantile(ocupades$ingreso_hora_bruto,.99))



#Se toma el logaritmo de los ingresos-hora y se estandariza (puntaje $z$) respecto de las medias y desviaciones (ponderadas) de cada aglomerado  


aglos<-as.numeric(as.character(levels(as.factor(ocupades$AGLOMERADO))))

for (i in aglos) {ocupades$ingreso_hora_z=(
  log(ocupades$ingreso_hora_bruto)-
    wtd.mean(
      log(ocupades[ocupades$AGLOMERADO==i,]$ingreso_hora_bruto),
      weights = ocupades[ocupades$AGLOMERADO==i,]$PONDERA))/sqrt(wtd.var(
        log(ocupades[ocupades$AGLOMERADO==i,]$ingreso_hora_bruto),
        weights=ocupades[ocupades$AGLOMERADO==i,]$PONDERA))
}

summary(ocupades$ingreso_hora_z)



ggplot(ocupades)+geom_histogram(aes(ingreso_hora_z))



#Y se estandariza al [0 1]  


ocupades$ingreso_hora<-(ocupades$ingreso_hora_z-min(ocupades$ingreso_hora_z))/
  (max(ocupades$ingreso_hora_z)-min(ocupades$ingreso_hora_z))

ggplot(ocupades)+geom_histogram(aes(ingreso_hora))+
  theme_tufte()




## Components´ correlation  

#Spearman´s $\rho$  
  
  
ocupades$consistencia<-as.numeric(as.character(ocupades$consistencia))

matriz_correl<-rcorr(as.matrix(data.frame(ocupades$seguridad,
                                          ocupades$consistencia,
                                          ocupades$ingreso_hora)), type =
                       "spearman")
r<-round(matriz_correl[[1]],3)

r

ggcorrplot(r, hc.order = TRUE, type = "lower",
           lab = TRUE)

cor.test(ocupades$seguridad, ocupades$ingreso_hora, type = "spearman")
cor.test(ocupades$seguridad, ocupades$consistencia, type = "spearman")
cor.test(ocupades$consistencia, ocupades$ingreso_hora, type = "spearman")



#p-values  


p<-round(matriz_correl[[3]],4)
p




# Combinación de los indicadores  



ocupades$consistencia<-as.numeric(as.character(ocupades$consistencia))

ocupades$calidad<-100*(ocupades$seguridad+ocupades$consistencia+4*ocupades$ingreso_hora)/6

min(ocupades$calidad, na.rm = T)
max(ocupades$calidad, na.rm = T)
wtd.quantile(ocupades$calidad, weights = ocupades$PONDERA,
             probs = c(.25,.5, .75))





ggplot(ocupades)+geom_histogram(aes(calidad), fill="green")+
  theme_tufte()




# Comparaciones simples  

## Por sector  



por_sector<-ocupades %>%
  group_by(sector) %>%
  summarise(media_calidad = round(wtd.mean(calidad, weights = PONDERA),3),
            mediana = round(median(calidad, na.rm = TRUE),3),
            casos=n())

por_sector<-ocupades %>%
  group_by(sector) %>%
  summarise(p25 = round(wtd.quantile(calidad, weights = PONDERA, .25),2),
            p50 = round(wtd.quantile(calidad, weights = PONDERA, .5), 2),
            p75 = round(wtd.quantile(calidad, weights = PONDERA, .75),2),
            casos=n())





ggplot(ocupades)+geom_violin(aes(sector, calidad, fill=sector))+
  scale_fill_brewer(palette = 5)+
  theme_tufte()+theme(legend.position = "none")




kruskal.test(ocupades$calidad, ocupades$sector)




pairwise.wilcox.test(ocupades$calidad, ocupades$sector)



## Por sexos  


por_sexo<-ocupades %>%
  group_by(sexo) %>%
  summarise(p25 = round(wtd.quantile(calidad, weights = PONDERA, .25),2),
            p50 = round(wtd.quantile(calidad, weights = PONDERA, .5), 2),
            p75 = round(wtd.quantile(calidad, weights = PONDERA, .75),2),
            casos=n())



wilcox.test(calidad~sexo, data=ocupades, conf.int=T)





ggplot(ocupades)+geom_violin(aes(sexo, calidad, fill=sexo))+
  scale_fill_brewer(palette = 5)+
  theme_tufte()+theme(legend.position = "none")




## Por regiones  


por_region<-ocupades %>%
  group_by(REGION_rot) %>%
  summarise(p25 = round(wtd.quantile(calidad, weights = PONDERA, .25),2),
            p50 = round(wtd.quantile(calidad, weights = PONDERA, .5), 2),
            p75 = round(wtd.quantile(calidad, weights = PONDERA, .75),2),
            casos=n())



pairwise.wilcox.test(ocupades$calidad, ocupades$REGION_rot)




ggplot(ocupades)+geom_violin(aes(REGION_rot, calidad, fill=REGION_rot))+
  scale_fill_brewer(palette = 5)+xlab("Región")+
  theme_tufte()+theme(legend.position = "none")





### Se fusionan los sectores y las regiones que no muestran diferencias significativas  

ocupades$sector_fusionado<-ocupades$sector
levels(ocupades$sector_fusionado)[
  levels(ocupades$sector)=="otro"] <-"estatal_y_otro"

levels(ocupades$sector_fusionado)[
  levels(ocupades$sector)=="estatal"] <-"estatal_y_otro"

table(ocupades$sector, ocupades$sector_fusionado)


ocupades$region_fusionadas<-ocupades$REGION_rot

levels(ocupades$region_fusionadas)[
  levels(ocupades$REGION_rot)=="Pampeana"] <-
  "Pampeana y Gran Buenos Aires"
levels(ocupades$region_fusionadas)[
  levels(ocupades$REGION_rot)=="Gran Buenos Aires"] <-
  "Pampeana y Gran Buenos Aires"
levels(ocupades$region_fusionadas)[
  levels(ocupades$REGION_rot)=="NOA"] <-
  "NOA NEA Cuyo"
levels(ocupades$region_fusionadas)[
  levels(ocupades$REGION_rot)=="NEA"] <-
  "NOA NEA Cuyo"
levels(ocupades$region_fusionadas)[
  levels(ocupades$region_fusionadas)=="Cuyo"] <-
  "NOA NEA Cuyo"
levels(ocupades$region_fusionadas)[
  levels(ocupades$region_fusionadas)=="Patagónica"] <-
  "Patagónica"




# Regresión  


rq_P10 <- rq(calidad ~ sexo+sector_fusionado+
               region_fusionadas,
             tau = .1, data = ocupades)


rq_P50 <- rq(calidad ~ sexo+sector_fusionado+
               region_fusionadas,
             tau = .5, data = ocupades)


rq_P90 <- rq(calidad ~ sexo+sector_fusionado+
               region_fusionadas,
             tau = .9, data = ocupades)


round(summary(rq_P10)[[3]], 3)
round(summary(rq_P50)[[3]],3)
round(summary(rq_P90)[[3]],3)




# Gaps  

## Distribución de varones y mujeres por sectores  


100*round(addmargins(prop.table(table(ocupades$sexo,
                                            ocupades$sector_fusionado),2),1),3)

100*round(addmargins(prop.table(table(ocupades$region_fusionadas,
                                            ocupades$sector_fusionado),1),2),3)





## General

por_sexo
por_sexo[3][[1]][2]
gap_sexo<-100*(por_sexo[3][[1]][1]-por_sexo[3][[1]][2])/por_sexo[3][[1]][1]
gap_sexo




## Por sectores  

gap_sexo_sector<-ocupades %>%
  group_by(sector, sexo) %>%
  summarise(mediana=wtd.quantile(calidad, weights = PONDERA, .50), casos=n())

gap_sexo_sector <- as.data.frame(gap_sexo_sector)

gap_sexo_estatal<-
  100*(gap_sexo_sector[1,3]-gap_sexo_sector[2,3])/
  gap_sexo_sector[1,3]
gap_sexo_privado<-100*(gap_sexo_sector[3,3]-gap_sexo_sector[4,3])/
  gap_sexo_sector[3,3]
gap_sexo_otro<-100*(gap_sexo_sector[5,3]-gap_sexo_sector[6,3])/
  gap_sexo_sector[5,3]

# es la media ponderada de los gaps estatal y otro
gap_sexo_estatal_y_otro<-(gap_sexo_estatal*(gap_sexo_sector[1,4]+
                                              gap_sexo_sector[2,4])+gap_sexo_otro*(gap_sexo_sector[5,4]+
                                                                                     gap_sexo_sector[6,4]))/(gap_sexo_sector[1,4]+gap_sexo_sector[2,4]+
                                                                                                               gap_sexo_sector[5,4]+gap_sexo_sector[6,4])

sector<-c("estatal y otro", "privado")
gap_sexo_sector<-c(gap_sexo_estatal_y_otro, gap_sexo_privado)
a<-data.frame(sector, gap_sexo_sector)
a



## Por región  

gap_sexo_region <- ocupades %>%
  group_by(REGION_rot, sexo) %>%
  summarise(mediana=wtd.quantile(calidad, weights = PONDERA, .50), casos=n())

gap_sexo_region<-as.data.frame(gap_sexo_region)

gap_sexo_GBA<-100*(gap_sexo_region[1,3]-gap_sexo_region[2,3])/
  gap_sexo_region[1,3]

gap_sexo_NOA<-100*(gap_sexo_region[3,3]-gap_sexo_region[4,3])/
  gap_sexo_region[3,3]

gap_sexo_NEA<-100*(gap_sexo_region[5,3]-gap_sexo_region[6,3])/
  gap_sexo_region[5,3]

gap_sexo_Cuyo<-100*(gap_sexo_region[7,3]-gap_sexo_region[8,3])/
  gap_sexo_region[7,3]
gap_sexo_Pampeana<-100*(gap_sexo_region[9,3]-gap_sexo_region[10,3])/
  gap_sexo_region[9,3]

gap_sexo_Patagonica<-100*(gap_sexo_region[11,3]-gap_sexo_region[12,3])/
  gap_sexo_region[11,3]

region<-c("GBA", "NOA", "NEA", "Cuyo", "Pampeana", "Patagónica")
gap_sexo_region_1<-c(gap_sexo_GBA, gap_sexo_NOA, gap_sexo_NEA, gap_sexo_Cuyo,
                     gap_sexo_Pampeana, gap_sexo_Patagonica)

region<-c("GBA", "NOA", "NEA", "Cuyo", "Pampeana", "Patagónica")
gap_sexo_region_2<-c(gap_sexo_GBA, gap_sexo_NOA, gap_sexo_NEA, gap_sexo_Cuyo,
                     gap_sexo_Pampeana, gap_sexo_Patagonica)

gap_sexo_region_tabla<-data.frame(region, gap_sexo_region_2)
gap_sexo_region_tabla

# agrupo regiones con diferencia no significativa
gap_sexo_NOA_NEA_Cuyo<-(gap_sexo_NOA*(gap_sexo_region[3,4]+
                                        gap_sexo_region[4,4]) +gap_sexo_NEA*(gap_sexo_region[5,4]+gap_sexo_region[6,4]) +gap_sexo_Cuyo*(
                                          gap_sexo_region[7,4]+gap_sexo_region[8,4]
                                        ))/(
                                          gap_sexo_region[3,4]+
                                            gap_sexo_region[4,4]+
                                            gap_sexo_region[5,4]+
                                            gap_sexo_region[6,4]+
                                            gap_sexo_region[7,4]+
                                            gap_sexo_region[8,4]
                                        )

gap_sexo_pampeana_y_buenos_aires<-
  (gap_sexo_GBA*(gap_sexo_region[1,4]+gap_sexo_region[2,4])+
     gap_sexo_Pampeana*(gap_sexo_region[9,4]+gap_sexo_region[10,4]))/
  (gap_sexo_region[1,4]+gap_sexo_region[2,4]+
     gap_sexo_region[9,4]+gap_sexo_region[10,4])

gap_sexo_region_fusionadas<-c(gap_sexo_NOA_NEA_Cuyo,
                              gap_sexo_pampeana_y_buenos_aires,
                              gap_sexo_Patagonica)

b<-data.frame(region, gap_sexo_region)
b


# Las tres variables y su efecto en el índice de calidad  


ocupades %>%
  group_by(sexo, REGION_rot, sector_fusionado) %>%
  summarise(mediana=median(calidad, na.rm=TRUE), casos=n())



# Para lectura longitudinal  


# brecha
gap_sexo

# sexo por sectores
u<-questionr::wtd.table(ocupades$sexo,
                        ocupades$sector_fusionado, weights = ocupades$PONDERA)
100*round(addmargins(prop.table(u,1),2),3)


# tasa de actividad
base$active<-1
base$active[base$ESTADO==0]<-0
base$active[base$ESTADO==3]<-0
base$active[base$ESTADO==4]<-0

table(base$ESTADO, base$active)

t.test(base$active, alternative="two.sided")
t.test(subset(base, base$CH04==1)$active, alternative="two.sided")
t.test(subset(base, base$CH04==2)$active, alternative="two.sided")




## Bimodalidad  

ggplot(ocupades)+geom_histogram(aes(calidad))+facet_grid(.~sexo)
ggplot(ocupades)+geom_histogram(aes(calidad))+facet_grid(.~sector_fusionado)
ggplot(ocupades)+geom_histogram(aes(calidad))+facet_grid(.~region_fusionadas)



# Occupational hierarchy by sex  


ocupades$jerarquia.ocup= substr(ocupades$PP04D_COD, 3,3)
ocupades$jerarquia.ocup[ocupades$jerarquia.ocup>3]<-NA


addmargins(questionr::wtd.table(ocupades$jerarquia.ocup, ocupades$sexo, weights = 
                                  ocupades$PONDERA),1)

100*round(addmargins(
  prop.table(
    questionr::wtd.table(
      ocupades$jerarquia.ocup, ocupades$sexo, weights = 
        ocupades$PONDERA),2),1), 4)



