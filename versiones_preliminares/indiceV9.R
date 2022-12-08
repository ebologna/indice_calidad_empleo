library(foreign)
library(questionr)
library(dplyr)
library(xlsx)
library(ggplot2)
library(ggthemes)
library(matrixStats)
library(BBmisc)
# lectura bases
eph_1_05<-read.dbf("bases_EPH/Ind_t105.DBF")
eph_1_10<-read.dbf("bases_EPH/Ind_t110.dbf")
eph_1_15<-read.dbf("bases_EPH/Ind_t115.dbf")
eph_1_20<-read.table("bases_EPH/usu_individual_T120.txt", 
                     header = TRUE, sep= ";", dec = ",")

# se elige con que base trabajar
base<-eph_1_20
# regiones rotuladas
base$REGION<-as.factor(base$REGION)
levels(base$REGION)<-c("Gran Buenos Aires", "NOA",
                       "NEA", "Cuyo", "Pampeana", "Patagónica")

# sexos rotulados
base$CH04<-as.factor(base$CH04)
levels(base$CH04)<-c("varones", "mujeres")
# Grupos de edades

base$CH06[base$CH06==-1]<-0
base$grupos_edades<-cut(base$CH06, c(0,24,39, 54,99))


# Niveles educación
base$NIVEL_ED[base$NIVEL_ED==9]<-NA
base$NIVEL_ED[base$NIVEL_ED==7]<-0
base$NIVEL_ED<-as.factor(base$NIVEL_ED)
levels(base$NIVEL_ED)<-c("nunca asistió", "primario incompleto", "primario completo",
                         "secundario incompleto", "secundario completo",
                         "superior incompleto", "superior completo")
# Tasa de actividad para personas de 10 y más años
# de estado, ocupados+desocupados/total
mayores_9<-subset(base, base$CH06>9)
mayores_9$ESTADO[mayores_9$ESTADO==0]<-NA

tasa_actividad <- (((addmargins(wtd.table(mayores_9$ESTADO, weights = mayores_9$PONDERA)))[1]+
                      (addmargins(wtd.table(mayores_9$ESTADO, weights = mayores_9$PONDERA)))[2])/
                     (addmargins(wtd.table(mayores_9$ESTADO, weights = mayores_9$PONDERA)))[4])
tasa_actividad<-as.numeric(tasa_actividad)

actividad_sexo<-mayores_9 %>%
  group_by(CH04) %>% 
  summarise(valor=(((addmargins(wtd.table(ESTADO, weights = PONDERA)))[1]+
                      (addmargins(wtd.table(ESTADO, weights = PONDERA)))[2])/
                     (addmargins(wtd.table(ESTADO, weights = PONDERA)))[4]))

gap_actividad_sexo<-100*round((actividad_sexo[1,2]-actividad_sexo[2,2])/actividad_sexo[1,2],4)

# prueba de significación (dicotomizo ESTADO en activos inactivos)
mayores_9$active<-0
mayores_9$active[mayores_9$ESTADO==1 | mayores_9$ESTADO==2]<-1
t.test(mayores_9$active~mayores_9$CH04)


actividad_region<-mayores_9 %>%
  group_by(REGION) %>% 
  summarise(valor=(((addmargins(wtd.table(ESTADO, weights = PONDERA)))[1]+
                      (addmargins(wtd.table(ESTADO, weights = PONDERA)))[2])/
                     (addmargins(wtd.table(ESTADO, weights = PONDERA)))[4]))

gap_actividad_region<-100*round((max(actividad_region[,2])-
                                   min(actividad_region[,2]))/max(actividad_region[,2]),4)

u <- aov(mayores_9$active~mayores_9$REGION)
w<-TukeyHSD(x=u, 'mayores_9$REGION', conf.level=0.95)
attributes(w)

actividad_edad<-mayores_9 %>%
  group_by(grupos_edades) %>% 
  summarise(valor=(((addmargins(wtd.table(ESTADO, weights = PONDERA)))[1]+
                      (addmargins(wtd.table(ESTADO, weights = PONDERA)))[2])/
                     (addmargins(wtd.table(ESTADO, weights = PONDERA)))[4]))

gap_actividad_edad<-100*round((max(actividad_edad[,2])-
                                 min(actividad_edad[,2]))/max(actividad_edad[,2]),4)

u <- aov(mayores_9$active~mayores_9$grupos_edades)
TukeyHSD(x=u, 'mayores_9$grupos_edades', conf.level=0.95)

actividad_educacion<-mayores_9 %>%
  group_by(NIVEL_ED) %>% 
  summarise(valor=(((addmargins(wtd.table(ESTADO, weights = PONDERA)))[1]+
                      (addmargins(wtd.table(ESTADO, weights = PONDERA)))[2])/
                     (addmargins(wtd.table(ESTADO, weights = PONDERA)))[4]))

gap_actividad_educacion<-100*round((max(actividad_educacion[,2])-
                                      min(actividad_educacion[,2]))/max(actividad_educacion[,2]),4)


u <- aov(mayores_9$active~mayores_9$NIVEL_ED)
TukeyHSD(x=u, 'mayores_9$NIVEL_ED', conf.level=0.95)

# desocupación: solo actives

actives<-subset(base, base$ESTADO==1 | base$ESTADO==2)

tasa_desocupacion <- addmargins(wtd.table(actives$ESTADO, weights = actives$PONDERA))[2]/
  addmargins(wtd.table(actives$ESTADO, weights = actives$PONDERA))[3]

tasa_desocupacion<-as.numeric(tasa_desocupacion)


desocupacion_sexo<-actives %>%
  group_by(CH04) %>% 
  summarise(valor=(addmargins(wtd.table(ESTADO, weights = PONDERA))[2]/
                     addmargins(wtd.table(ESTADO, weights = PONDERA))[3]))

gap_desocupacion_sexo<-100*round((desocupacion_sexo[1,2]-desocupacion_sexo[2,2])/
                                   desocupacion_sexo[1,2],4)

desocupacion_region<-actives %>%
  group_by(REGION) %>% 
  summarise(valor=(addmargins(wtd.table(ESTADO, weights = PONDERA))[2]/
                     addmargins(wtd.table(ESTADO, weights = PONDERA))[3]))

gap_desocupacion_region<-100*round((max(desocupacion_region[,2])-min(desocupacion_region[,2]))/
                                     max(desocupacion_region[,2]),4)


desocupacion_edad<-actives %>%
  group_by(grupos_edades) %>% 
  summarise(valor=(addmargins(wtd.table(ESTADO, weights = PONDERA))[2]/
                     addmargins(wtd.table(ESTADO, weights = PONDERA))[3]))

gap_desocupacion_edad<-100*round((max(desocupacion_edad[,2])-min(desocupacion_edad[,2]))/
                                   max(desocupacion_edad[,2]),4)


desocupacion_educacion<-actives %>%
  group_by(NIVEL_ED) %>% 
  summarise(valor=(addmargins(wtd.table(ESTADO, weights = PONDERA))[2]/
                     addmargins(wtd.table(ESTADO, weights = PONDERA))[3]))

gap_desocupacion_educacion<-100*round((max(desocupacion_educacion[,2])-min(desocupacion_educacion[,2]))/
                                        max(desocupacion_educacion[,2]),4)


# proporcion independientes (solo ocupades, cat_ocup=2)
ocupades<-subset(base, base$ESTADO==1)
addmargins(table(ocupades$CAT_OCUP))
addmargins(wtd.table(ocupades$CAT_OCUP, weights = ocupades$PONDERA))
prop_independientes<-(addmargins(wtd.table(ocupades$CAT_OCUP, weights = ocupades$PONDERA)))[2]/
  (addmargins(wtd.table(ocupades$CAT_OCUP, weights = ocupades$PONDERA)))[5]

prop_independientes<-as.numeric(prop_independientes)

prop_independientes_sexo<-ocupades %>%
  group_by(CH04) %>% 
  summarise(valor=(addmargins(wtd.table(CAT_OCUP, weights = PONDERA)))[2]/
              (addmargins(wtd.table(CAT_OCUP, weights = PONDERA)))[5])

gap_prop_independientes_sexo<-100*round((prop_independientes_sexo[1,2]-prop_independientes_sexo[2,2])/
                                          prop_independientes_sexo[1,2],4)

prop_independientes_region<-ocupades %>%
  group_by(REGION) %>% 
  summarise(valor=(addmargins(wtd.table(CAT_OCUP, weights = PONDERA))[2]/
                     addmargins(wtd.table(CAT_OCUP, weights = PONDERA))[5]))

gap_prop_independientes_region<-100*round((max(prop_independientes_region[,2])-
                                             min(prop_independientes_region[,2]))/
                                            max(prop_independientes_region[,2]),4)


prop_independientes_edad<-ocupades %>%
  group_by(grupos_edades) %>% 
  summarise(valor=(addmargins(wtd.table(CAT_OCUP, weights = PONDERA))[2]/
                     addmargins(wtd.table(CAT_OCUP, weights = PONDERA))[5]))

gap_prop_independientes_edad<-100*round((max(prop_independientes_edad[,2])-
                                           min(prop_independientes_edad[,2]))/
                                          max(prop_independientes_edad[,2]),4)


prop_independientes_educacion<-ocupades %>%
  group_by(NIVEL_ED) %>% 
  summarise(valor=(addmargins(wtd.table(CAT_OCUP, weights = PONDERA))[2]/
                     addmargins(wtd.table(CAT_OCUP, weights = PONDERA))[5]))

gap_prop_independientes_educacion<-100*round((max(prop_independientes_educacion[,2])-
                                                min(prop_independientes_educacion[,2]))/
                                               max(prop_independientes_educacion[,2]),4)

## 1. Seguridad: solo asalariades
## 1.1 Permanencia: PP07C
## 1.2 obra social +aguinaldo + vacaciones + dias por enfermedad + descuento jubilatorio
## es la media de la combinación aditiva

asalariades<-subset(base, base$CAT_OCUP==3)

table(asalariades$PP07C)
asalariades$PP07C[asalariades$PP07C==0]<-NA
asalariades$PP07C[asalariades$PP07C==9]<-NA
asalariades$estabilidad<-asalariades$PP07C-1
table(asalariades$estabilidad)


table(asalariades$PP07G1)
asalariades$PP07G1[asalariades$PP07G1==0]<-NA
asalariades$PP07G1[asalariades$PP07G1==9]<-NA
asalariades$vacaciones <-asalariades$PP07G1
asalariades$vacaciones[asalariades$vacaciones==2]<-0
table(asalariades$vacaciones)


table(asalariades$PP07G2)
asalariades$PP07G2[asalariades$PP07G2==0]<-NA
asalariades$PP07G2[asalariades$PP07G2==9]<-NA
asalariades$aguinaldo<-asalariades$PP07G2
asalariades$aguinaldo[asalariades$aguinaldo==2]<-0
table(asalariades$aguinaldo)


table(asalariades$PP07G3)
asalariades$PP07G3[asalariades$PP07G3==0]<-NA
asalariades$PP07G3[asalariades$PP07G3==9]<-NA
asalariades$dias_enfermedad<-asalariades$PP07G3
asalariades$dias_enfermedad[asalariades$dias_enfermedad==2]<-0
table(asalariades$dias_enfermedad)

table(asalariades$PP07G4)
asalariades$PP07G4[asalariades$PP07G4==0]<-NA
asalariades$PP07G4[asalariades$PP07G4==9]<-NA
asalariades$obra_social<-asalariades$PP07G4
asalariades$obra_social[asalariades$obra_social==2]<-0
table(asalariades$obra_social)

table(asalariades$PP07H)
asalariades$PP07H[asalariades$PP07H==0]<-NA
asalariades$PP07H[asalariades$PP07H==9]<-NA
asalariades$aporte_jubilatorio<-asalariades$PP07H
asalariades$aporte_jubilatorio[asalariades$aporte_jubilatorio==2]<-0
table(asalariades$aporte_jubilatorio)


asalariades$seguridad<-(asalariades$estabilidad + asalariades$vacaciones+
                          asalariades$aguinaldo + asalariades$dias_enfermedad+
                          asalariades$obra_social+
                          asalariades$aporte_jubilatorio)/6



seguridad<-round(wtd.mean(asalariades$seguridad, weights = asalariades$PONDERA),3)
sd_seguridad<-weightedSd(asalariades$seguridad, weights = asalariades$PONDERA,
                         na.rm=TRUE)


seguridad_sexo<-asalariades %>%
  group_by(CH04) %>% 
  summarise(valor=wtd.mean(seguridad, weights = PONDERA))

gap_seguridad_sexo<-100*round((seguridad_sexo[1,2]-seguridad_sexo[2,2])/
                                seguridad_sexo[1,2],4)


seguridad_region<-asalariades %>%
  group_by(REGION) %>% 
  summarise(valor=wtd.mean(seguridad, weights = PONDERA))

min(seguridad_region)[,2]
gap_seguridad_region<-100*round((max(seguridad_region[,2])-min(seguridad_region[,2]))/
                                  max(seguridad_region[,2]),4)


seguridad_edad<-asalariades %>%
  group_by(grupos_edades) %>% 
  summarise(valor=wtd.mean(seguridad, weights = PONDERA))

gap_seguridad_edad<-100*round((max(seguridad_edad[,2])-min(seguridad_edad[,2]))/
                                max(seguridad_edad[,2]),4)

seguridad_educacion<-asalariades %>%
  group_by(NIVEL_ED) %>% 
  summarise(valor=wtd.mean(seguridad, weights = PONDERA))

gap_seguridad_educacion<-100*round((max(seguridad_educacion[,2])-min(seguridad_educacion[,2]))/
                                     max(seguridad_educacion[,2]),4)


## 3. ingresos -hora solo ocupades

# ingresos, se quita el 1% superior
summary(ocupades$PP08D1)
ocupades$PP08D1[ocupades$PP08D1==-9]<-NA
ocupades$PP08D1[ocupades$PP08D1==0]<-NA
ocupades$PP08D1[ocupades$PP08D1>quantile(
  ocupades$PP08D1,.99, na.rm = TRUE)]<-NA
ocupades$ingresos<-ocupades$PP08D1
summary(ocupades$ingresos)

ingresos<-wtd.mean(ocupades$ingresos, weights = ocupades$PONDERA)


ingreso_sexo<-ocupades %>%
  group_by(CH04) %>% 
  summarise(valor=wtd.mean(ingresos, weights = PONDERA))

gap_ingreso_sexo<-100*round((ingreso_sexo[1,2]-ingreso_sexo[2,2])/
                              ingreso_sexo[1,2],4)


ingreso_region<-ocupades %>%
  group_by(REGION) %>% 
  summarise(valor=wtd.mean(ingresos, weights = PONDERA))

gap_ingreso_region<-100*round((max(ingreso_region[,2])-min(ingreso_region[,2]))/
                                max(ingreso_region[,2]),4)


ingreso_edad<-ocupades %>%
  group_by(grupos_edades) %>% 
  summarise(valor=wtd.mean(ingresos, weights = PONDERA))

gap_ingreso_edad<-100*round((max(ingreso_edad[,2])-min(ingreso_edad[,2]))/
                              max(ingreso_edad[,2]),4)


ingreso_educacion<-ocupades %>%
  group_by(NIVEL_ED) %>% 
  summarise(valor=wtd.mean(ingresos, weights = PONDERA))

gap_ingreso_educacion<-100*round((max(ingreso_educacion[,2])-min(ingreso_educacion[,2]))/
                                   max(ingreso_educacion[,2]),4)

# horas

ocupades$horas_semana<-ocupades$PP3E_TOT
ocupades$horas_semana[ocupades$horas_semana==0]<-NA
ocupades$horas_semana[ocupades$horas_semana>84]<-NA
summary(ocupades$horas_semana)

# ingreso-hora
ocupades$ingreso_hora<-ocupades$ingresos/(ocupades$horas_semana*4)
summary(ocupades$ingreso_hora)


ingreso_hora<-wtd.mean(ocupades$ingreso_hora, weights = ocupades$PONDERA)

sd_ingreso_hora<-sqrt(weightedVar(ocupades$ingreso_hora, ocupades$PONDERA, na.rm = TRUE))



ingreso_hora_sexo<-ocupades %>%
  group_by(CH04) %>% 
  summarise(valor=wtd.mean(ingreso_hora, weights = PONDERA))

gap_ingreso_hora_sexo<-100*round((ingreso_hora_sexo[1,2]-ingreso_hora_sexo[2,2])/
                                   ingreso_hora_sexo[1,2],4)


ingreso_hora_region<-ocupades %>%
  group_by(REGION) %>% 
  summarise(valor=wtd.mean(ingreso_hora, weights = PONDERA))

gap_ingreso_hora_region<-100*round((max(ingreso_hora_region[,2])-min(ingreso_hora_region[,2]))/
                                     max(ingreso_hora_region[,2]),4)


ingreso_hora_edad<-ocupades %>%
  group_by(grupos_edades) %>% 
  summarise(valor=wtd.mean(ingreso_hora, weights = PONDERA))

gap_ingreso_hora_edad<-100*round((max(ingreso_hora_edad[,2])-min(ingreso_hora_edad[,2]))/
                                   max(ingreso_hora_edad[,2]),4)


ingreso_hora_educacion<-ocupades %>%
  group_by(NIVEL_ED) %>% 
  summarise(valor=wtd.mean(ingreso_hora, weights = PONDERA))

gap_ingreso_hora_educacion<-100*round((max(ingreso_hora_educacion[,2])-min(ingreso_hora_educacion[,2]))/
                                        max(ingreso_hora_educacion[,2]),4)


# Desigualdad en ITF

summary(base$IPCF)
base$IPCF[base$IPCF==0]<-NA
# quito uno por mil sperior (más de cinco desviaciones)
base$IPCF[base$IPCF>quantile(
  base$IPCF,.999, na.rm = TRUE)]<-NA


ingresos_bajos<-subset(base, base$IPCF<=quantile(base$IPCF,.50, na.rm = TRUE))
ingresos_altos<-subset(base, base$IPCF>=quantile(base$IPCF,.90, na.rm = TRUE))
p90_p50_total<-wtd.mean(ingresos_altos$IPCF, weights = ingresos_altos$PONDERA)/
  wtd.mean(ingresos_bajos$IPCF, weights = ingresos_bajos$PONDERA)
# Piketty dice que en Europa es 8 a 1, muy cercano a esto
# hay que poder abrir esto para los grupos
# voy a reemplazar todos loa desigualdad ingreso hora por p90_p50

# por sexos
ingresos_bajos_sexo<-base %>%  group_by(CH04) %>% 
  subset(IPCF<=quantile(IPCF,.50, na.rm = TRUE))%>%
    summarise(media_ingresos_bajos=wtd.mean(IPCF, weights = PONDERA))

ingresos_altos_sexo<-base %>%  group_by(CH04) %>% 
  subset(IPCF>=quantile(IPCF,.90, na.rm = TRUE))%>%
  summarise(media_ingresos_altos=wtd.mean(IPCF, weights = PONDERA))

p90_p50_sexos<-merge(ingresos_bajos_sexo, ingresos_altos_sexo)
p90_p50_sexos$diferencia<-p90_p50_sexos$media_ingresos_altos/p90_p50_sexos$media_ingresos_bajos

# por región
ingresos_bajos_region<-base %>%  group_by(REGION) %>% 
  subset(IPCF<=quantile(IPCF,.50, na.rm = TRUE))%>%
  summarise(media_ingresos_bajos=wtd.mean(IPCF, weights = PONDERA))

ingresos_altos_region<-base %>%  group_by(REGION) %>% 
  subset(IPCF>=quantile(IPCF,.90, na.rm = TRUE))%>%
  summarise(media_ingresos_altos=wtd.mean(IPCF, weights = PONDERA))

p90_p50_region<-merge(ingresos_bajos_region, ingresos_altos_region)
p90_p50_region$diferencia<-p90_p50_region$media_ingresos_altos/p90_p50_region$media_ingresos_bajos

# por edades

ingresos_bajos_edad<-base %>%  group_by(grupos_edades) %>% 
  subset(IPCF<=quantile(IPCF,.50, na.rm = TRUE))%>%
  summarise(media_ingresos_bajos=wtd.mean(IPCF, weights = PONDERA))

ingresos_altos_edad<-base %>%  group_by(grupos_edades) %>% 
  subset(IPCF>=quantile(IPCF,.90, na.rm = TRUE))%>%
  summarise(media_ingresos_altos=wtd.mean(IPCF, weights = PONDERA))

p90_p50_edad<-merge(ingresos_bajos_edad, ingresos_altos_edad)
p90_p50_edad$diferencia<-p90_p50_edad$media_ingresos_altos/p90_p50_edad$media_ingresos_bajos

# por educacion
ingresos_bajos_educacion<-base %>%  group_by(NIVEL_ED) %>% 
  subset(IPCF<=quantile(IPCF,.50, na.rm = TRUE))%>%
  summarise(media_ingresos_bajos=wtd.mean(IPCF, weights = PONDERA))

ingresos_altos_educacion<-base %>%  group_by(NIVEL_ED) %>% 
  subset(IPCF>=quantile(IPCF,.90, na.rm = TRUE))%>%
  summarise(media_ingresos_altos=wtd.mean(IPCF, weights = PONDERA))

p90_p50_educacion<-merge(ingresos_bajos_educacion, ingresos_altos_educacion)
p90_p50_educacion$diferencia<-p90_p50_educacion$media_ingresos_altos/p90_p50_educacion$media_ingresos_bajos

# Comparaciones simples (sin intersecciones)

sexo<-c("varones", "mujeres")
region<-c("Gran Buenos Aires", "NOA",
          "NEA", "Cuyo", "Pampeana", "Patagónica")
edad<-c("menos de 25", "25 -39", "40 - 54", "55 y más")
educacion<-c("nunca asistió", "primario incompleto", "primario completo",
             "secundario incompleto", "secundario completo",
             "superior incompleto", "superior completo")

grupos<-c("general", sexo, region, edad, educacion)

valores_actividad<-c(tasa_actividad[[1]], actividad_sexo[[2]],
                     actividad_region[[2]], actividad_edad[[2]],
                     actividad_educacion[[2]])
tabla_actividad<-data.frame(grupos, valores_actividad)

valores_desocupacion<-c(tasa_desocupacion[[1]], desocupacion_sexo[[2]],
                        desocupacion_region[[2]], desocupacion_edad[[2]],
                        desocupacion_educacion[[2]])
tabla_desocupacion<-data.frame(grupos, valores_desocupacion)

valores_prop_independientes<- c(prop_independientes[[1]], prop_independientes_sexo[[2]],
                                prop_independientes_region[[2]], prop_independientes_edad[[2]],
                                prop_independientes_educacion[[2]])
tabla_prop_independientes<-data.frame(grupos, valores_prop_independientes)

valores_seguridad<-c(seguridad[[1]], seguridad_sexo[[2]], seguridad_region[[2]],
                     seguridad_edad[[2]], seguridad_educacion[[2]])
tabla_seguridad<-data.frame(grupos, valores_seguridad)

valores_ingreso<-c(ingresos[[1]], ingreso_sexo[[2]], ingreso_region[[2]],
                   ingreso_edad[[2]], ingreso_educacion[[2]])
tabla_ingreso<-data.frame(grupos, valores_ingreso)

valores_ingreso_hora<-c(ingreso_hora[[1]], ingreso_hora_sexo[[2]], ingreso_hora_region[[2]],
                        ingreso_hora_edad[[2]], ingreso_hora_educacion[[2]])
tabla_ingreso_hora<-data.frame(grupos, valores_ingreso_hora)

valores_desig_ingreso_hora<-c(desig_ingreso_hora[[1]], desig_ingreso_hora_sexo[[2]],
                              desig_ingreso_hora_region[[2]],
                              desig_ingreso_hora_edad[[2]],
                              desig_ingreso_hora_educacion[[2]])
tabla_desig_ingreso_hora<-data.frame(grupos, valores_desig_ingreso_hora)

valores_diferencia_p90_p50<-c(p90_p50_sexos$diferencia, p90_p50_region$diferencia,
                              p90_p50_edad$diferencia, p90_p50_educacion$diferencia)

compara_grupos<-data.frame(grupos, valores_actividad, valores_desocupacion,
                           valores_prop_independientes, valores_seguridad, valores_ingreso,
                           valores_ingreso_hora, valores_desig_ingreso_hora)

# con p90_p50 en ves de p90/p10
compara_grupos_1<-data.frame(grupos, valores_actividad, valores_desocupacion,
                           valores_prop_independientes, valores_seguridad, valores_ingreso,
                           valores_ingreso_hora, valores_diferencia_p90_p50)
# normalizo a 0 - 100 para sintetizar

compara_grupos_normalized<-
  data.frame(compara_grupos, normalize(compara_grupos[,-1],
                                       method = "range",
                                                 range = c(0,100)))
compara_grupos_1_normalized<-
  data.frame(compara_grupos_1, normalize(compara_grupos_1[,-1],
                                       method = "range",
                                       range = c(0,100)))

# otro indice más
compara_grupos_normalized$LaUnIn_5<-(compara_grupos_normalized$valores_actividad.1+ (100-compara_grupos_normalized$valores_desocupacion.1)+ 
                                     (100-compara_grupos_normalized$valores_prop_independientes.1)+
                                       compara_grupos_normalized$valores_seguridad.1+
                                       compara_grupos_normalized$valores_ingreso.1+
                                       compara_grupos_normalized$valores_ingreso_hora.1+
                                     (100-compara_grupos_normalized$valores_desig_ingreso_hora.1))/7

# y uno más, usando P90_p50
compara_grupos_1_normalized$LaUnIn_5<-(compara_grupos_1_normalized$valores_actividad.1+
                                         (100-compara_grupos_1_normalized$valores_desocupacion.1)+ 
                                       (100-compara_grupos_1_normalized$valores_prop_independientes.1)+
                                       compara_grupos_1_normalized$valores_seguridad.1+
                                       compara_grupos_1_normalized$valores_ingreso.1+
                                       compara_grupos_1_normalized$valores_ingreso_hora.1+
                                       (100-compara_grupos_1_normalized$valores_diferencia_p90_p50))/7

# lo mando a excel

write.xlsx(compara_grupos_normalized, "sintesis.xlsx", sheetName = "prueba_todes")


compara_grupos_1_normalized$fronteras<-c("media nacional", rep("sexo",2), rep("region", 6), rep("edad",4), rep("educacion",7) )
compara_grupos_1_normalized$grupos <- factor(compara_grupos_normalized$grupos,
                                           levels = compara_grupos_normalized$grupos)
ggplot(compara_grupos_1_normalized[-1,] )+
  geom_bar(aes(grupos, LaUnIn_5, fill=fronteras), stat = "identity")+
  geom_hline(yintercept = compara_grupos_normalized[1,16], col="red")+coord_flip()+
  theme_tufte()

# gaps del índice
gaps_LaUnIn_5<-compara_grupos_normalized %>%
  group_by(fronteras) %>% 
  summarise(gap=(max(LaUnIn_5)-min(LaUnIn_5))/max(LaUnIn_5))

## lo disponible hasta acá
# compara_grupos (sintesis.xlsx): valores de los indicadores para cada categoría (1+2+6+3+7 categorías)
# u (todes_todes.xlsx): las intersecciones (2*6*3*7 categorías)
# gaps (gaps.xlsx): 
u$sexo_cod<-1
u$sexo_cod[u$sexo=="mujeres"]<-2
table(u$sexo, u$sexo_cod)

u$region_cod<-1
u$region_cod[u$región=="NOA"]<-2
u$region_cod[u$región=="NEA"]<-3
u$region_cod[u$región=="Cuyo"]<-4
u$region_cod[u$región=="Pampeana"]<-5
u$region_cod[u$región=="Patagónica"]<-6
table(u$región, u$region_cod)

u$edad_cod<-1
u$edad_cod[u$`grupo de edades`=="(24,54]"]<-2
u$edad_cod[u$`grupo de edades`=="(54,99]"]<-3
table(u$`grupo de edades`, u$edad_cod)

u$educacion_cod<-1
u$educacion_cod[u$educación=="primario incompleto"]<-2
u$educacion_cod[u$educación=="primario completo"]<-3
u$educacion_cod[u$educación=="secundario incompleto"]<-4
u$educacion_cod[u$educación=="secundario completo"]<-5
u$educacion_cod[u$educación=="superior incompleto"]<-6
u$educacion_cod[u$educación=="superior completo"]<-7
table(u$educación, u$educacion_cod)


u$codigo_grupo<-u$sexo_cod*1000+u$region_cod*100+u$edad_cod*10+u$educacion_cod
u_con_codigos_de_grupo<-u[, -c(1:4, 12:15)] 

# Clustering
u_con_codigos_de_grupo <- na.omit(u_con_codigos_de_grupo) # vuelas filas con NA
u_con_codigos_de_grupo_st <- scale(u_con_codigos_de_grupo[,-8]) # estandarización

wss <- (nrow(u_con_codigos_de_grupo_st[,-8])-1)*sum(apply(u_con_codigos_de_grupo_st[,-8],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(u_con_codigos_de_grupo_st[,-8],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Número de clusters",
     ylab="Suma de cuadrados dentro")

fit <- kmeans(u_con_codigos_de_grupo_st[,-8], 4) # decido 4
# medias de cada cluster
aggregate(u_con_codigos_de_grupo_st[,-8],by=list(fit$cluster),FUN=mean)
# append cluster assignment
con_clusters <- data.frame(u_con_codigos_de_grupo_st, fit$cluster)

# Gráficos

ggplot(actividad_sexo)+geom_bar(aes(CH04, valor), fill="green", stat="identity")+xlab ("sexo")+
  ylab("tasa de actividad")+coord_flip()+theme_tufte()
ggplot(actividad_region)+geom_bar(aes(REGION, valor), fill="green",stat="identity")+xlab("regiones")+
  ylab("tasa de actividad")+coord_flip()+theme_tufte()
ggplot(actividad_edad)+geom_bar(aes(grupos_edades, valor), fill="green",stat="identity")+xlab("grupos de edades")+
  ylab("tasa de actividad")+coord_flip()+theme_tufte()
ggplot(actividad_educacion)+geom_bar(aes(NIVEL_ED, valor), fill="green",stat="identity")+xlab("nivel de educación")+
  ylab("tasa de actividad")+coord_flip()+theme_tufte()


ggplot(desocupacion_sexo)+geom_bar(aes(CH04, valor), fill="green",stat="identity")+xlab ("sexo")+
  ylab("tasa de desocupacion")+coord_flip()+theme_tufte()
ggplot(desocupacion_region)+geom_bar(aes(REGION, valor), fill="green",stat="identity")+xlab("regiones")+
  ylab("tasa de desocupacion")+coord_flip()+theme_tufte()
ggplot(desocupacion_edad)+geom_bar(aes(grupos_edades, valor), fill="green",stat="identity")+xlab("grupos de edades")+
  ylab("tasa de desocupacion")+coord_flip()+theme_tufte()
ggplot(desocupacion_educacion)+geom_bar(aes(NIVEL_ED, valor), fill="green",stat="identity")+xlab("nivel de educación")+
  ylab("tasa de desocupacion")+coord_flip()+theme_tufte()


ggplot(prop_independientes_sexo)+geom_bar(aes(CH04, valor), fill="green",stat="identity")+xlab ("sexo")+
  ylab("proporción de cuentapropistas")+coord_flip()+theme_tufte()
ggplot(prop_independientes_region)+geom_bar(aes(REGION, valor), fill="green",stat="identity")+xlab("regiones")+
  ylab("proporción de cuentapropistas")+coord_flip()+theme_tufte()
ggplot(prop_independientes_edad)+geom_bar(aes(grupos_edades, valor), fill="green",stat="identity")+xlab("grupos de edades")+
  ylab("proporción de cuentapropistas")+coord_flip()+theme_tufte()
ggplot(prop_independientes_educacion)+geom_bar(aes(NIVEL_ED, valor), fill="green",stat="identity")+xlab("nivel de educación")+
  ylab("proporción de cuentapropistas")+coord_flip()+theme_tufte()

ggplot(seguridad_sexo)+geom_bar(aes(CH04, valor), fill="green",stat="identity")+xlab ("sexo")+
  ylab("índice de seguridad laboral")+coord_flip()+theme_tufte()
ggplot(seguridad_region)+geom_bar(aes(REGION, valor), fill="green",stat="identity")+xlab("regiones")+
  ylab("índice de seguridad laboral")+coord_flip()+theme_tufte()
ggplot(seguridad_edad)+geom_bar(aes(grupos_edades, valor), fill="green",stat="identity")+xlab("grupos de edades")+
  ylab("índice de seguridad laboral")+coord_flip()+theme_tufte()
ggplot(seguridad_educacion)+geom_bar(aes(NIVEL_ED, valor), fill="green",stat="identity")+xlab("nivel de educación")+
  ylab("índice de seguridad laboral")+coord_flip()+theme_tufte()

ggplot(ingreso_sexo)+geom_bar(aes(CH04, valor), fill="green",stat="identity")+xlab ("sexo")+
  ylab("media de ingresos laborales")+coord_flip()+theme_tufte()
ggplot(ingreso_region)+geom_bar(aes(REGION, valor), fill="green",stat="identity")+xlab("regiones")+
  ylab("media de ingresos laborales")+coord_flip()+theme_tufte()
ggplot(ingreso_edad)+geom_bar(aes(grupos_edades, valor), fill="green",stat="identity")+xlab("grupos de edades")+
  ylab("media de ingresos laborales")+coord_flip()+theme_tufte()
ggplot(ingreso_educacion)+geom_bar(aes(NIVEL_ED, valor), fill="green",stat="identity")+xlab("nivel de educación")+
  ylab("media de ingresos laborales")+coord_flip()+theme_tufte()

ggplot(ingreso_hora_sexo)+geom_bar(aes(CH04, valor), fill="green",stat="identity")+xlab ("sexo")+
  ylab("media de ingresos laborales por hora")+coord_flip()+theme_tufte()
ggplot(ingreso_hora_region)+geom_bar(aes(REGION, valor), fill="green",stat="identity")+xlab("regiones")+
  ylab("media de ingresos laborales por hora")+coord_flip()+theme_tufte()
ggplot(ingreso_hora_edad)+geom_bar(aes(grupos_edades, valor), fill="green",stat="identity")+xlab("grupos de edades")+
  ylab("media de ingresos laborales por hora")+coord_flip()+theme_tufte()
ggplot(ingreso_hora_educacion)+geom_bar(aes(NIVEL_ED, valor), fill="green",stat="identity")+xlab("nivel de educación")+
  ylab("media de ingresos laborales por hora")+coord_flip()+theme_tufte()

# Para después, por año: cambiar la eph que se llama "base" y calcular todo cada vez
tabla_x<-data.frame(indicadores, indicadores_2005,
                    indicadores_2010, indicadores_2015, indicadores_2020)
tabla_x<-data.frame(tabla_x[,1], round(tabla_x[,2:5],2))
write.csv(tabla_x, "tabla_x.csv", row.names = FALSE)
