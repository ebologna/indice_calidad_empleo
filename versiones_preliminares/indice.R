library(foreign)
library(questionr)
# lectura bases
eph_1_05<-read.dbf("bases_EPH/Ind_t105.DBF")
eph_1_15<-read.dbf("bases_EPH/Ind_t115.DBF")
eph_1_20<-read.table("bases_EPH/usu_individual_T120.txt", 
                     header = TRUE, sep= ";")

# se elige con que base trabajar
base<-eph_1_20
# regiones rotuladas
base$REGION<-as.factor(base$REGION)
levels(base$REGION)<-c("Gran Buenos Aires", "NOA",
                             "NEA", "Cuyo", "Pampeana", "Patagónica")

# Grupos de edades

base$CH06[base$CH06==-1]<-0
base$grupos_edades<-cut(base$CH06, c(0,24,54,99))
table(base$grupos_edades)

# Niveles educación
table(base$NIVEL_ED)
base$NIVEL_ED[base$NIVEL_ED==9]<-NA
base$NIVEL_ED[base$NIVEL_ED==7]<-0

###Hay que hacer todo ed nuevo con wtd.table y wtd.mean###

# Tasa de actividad
# de estado, ocupados+desocupados/total
mayores_9<-subset(base, base$CH06>9)
ESTADO[ESTADO==0]<-NA


actividad <-function(x) {((addmargins(table(x)))[1]+
                        (addmargins(table(x)))[2])/
                        (addmargins(table(x)))[4]
                      }

actividad(ESTADO)

actividad_general<-actividad(ESTADO)

actividad_sexo<-tapply(ESTADO, CH04, actividad)
gap_actividad_sexo<-100*round((actividad_sexo[1]-actividad_sexo[2])/actividad_sexo[1],4)

actividad_region<-tapply(ESTADO, REGION, actividad)
gap_actividad_region<-100*round((max(actividad_region)-min(actividad_region))/
                                  max(actividad_region),4)

actividad_edad<-tapply(ESTADO, grupos_edades, actividad)
gap_actividad_edad<-100*round((max(actividad_edad)-min(actividad_edad))/
                                max(actividad_edad),4)


actividad_educacion<-tapply(ESTADO, NIVEL_ED, actividad)
gap_actividad_educacion<-100*round((max(actividad_educacion)-min(actividad_educacion))/
                                     max(actividad_educacion),4)

## con wtd.table

library(dplyr)

ingreso_por_sexos<-mayores_9 %>%
  group_by(CH04) %>% 
  summarise(media_ingreso = weighted.mean(PP08D1, PONDERA, na.rm = TRUE))

actividad_sexo<-mayores_9 %>%
  group_by(CH04) %>% 
  summarise(tasa_actividad = (((addmargins(wtd.table(ESTADO, weights = PONDERA)))[1]+
                                 (addmargins(wtd.table(ESTADO, weights = PONDERA)))[2])/
                                (addmargins(wtd.table(ESTADO, weights = PONDERA)))[4]))


actividad_sexo


# desocupación

table(ESTADO)
desocupacion<-function(x){100*round((table(x))[2]/
    ((table(x))[1]+(table(x))[2]),4)}

desocupacion(ESTADO)
desocupacion_sexo<-tapply(ESTADO, CH04, desocupacion)
gap_desocupacion_sexo<-(desocupacion_sexo[1]-desocupacion_sexo[2])/desocupacion_sexo[1]

wtd.table(ESTADO, weights = PONDERA)

# proporcion independientes (cat_ocup=2)
table(CAT_OCUP)
CAT_OCUP[CAT_OCUP==0]<-NA
CAT_OCUP[CAT_OCUP==9]<-NA

independientes<-function(x){100*round(
  (addmargins(table(x)))[2]/(addmargins(table(x)))[5],4)}
independientes(CAT_OCUP)
independientes_sexo<-tapply(CAT_OCUP, CH04, independientes)
gap_independientes_sexo<-(independientes_sexo[1]-independientes_sexo[2])/
  independientes_sexo[1]




## 1. Seguridad
## 1.1 Permanencia: PP07C
## 1.2 obra social +aguinaldo + vacaciones + dias por enfermedad + descuento jubilatorio

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


summary(asalariades$seguridad)

seguridad_sexo<-tapply(asalariades$seguridad, asalariades$CH04, mean, na.rm=TRUE)
gap_seguridad_sexo<-100*round((seguridad_sexo[1]-seguridad_sexo[2])/seguridad_sexo[1],4)

seguridad_region<-tapply(asalariades$seguridad,
                         asalariades$REGION, mean, na.rm=TRUE)

gap_seguridad_region<-100*round((max(seguridad_region)-min(seguridad_region))/
                                            max(seguridad_region),4)

## 3. ingresos -hora
## y ajustar a la media de la región

ocupades<-subset(base, base$ESTADO==1)
# ingresos, se quita el 1% superior
summary(ocupades$PP08D1)
ocupades$PP08D1[ocupades$PP08D1==-9]<-NA
ocupades$PP08D1[ocupades$PP08D1==0]<-NA
ocupades$PP08D1[ocupades$PP08D1>quantile(
  ocupades$PP08D1,.99, na.rm = TRUE)]<-NA
ocupades$ingresos<-ocupades$PP08D1
summary(ocupades$ingresos)

ingreso_sexo<-tapply(ocupades$ingresos, ocupades$CH04, mean, na.rm=TRUE)
gap_ingreso_sexo<-100*round((ingreso_sexo[1]-ingreso_sexo[2])/ingreso_sexo[1],4)

ingreso_region<-tapply(ocupades$ingresos, ocupades$REGION, mean, na.rm=TRUE)

gap_ingreso_region<-100*round((max(ingreso_region)-
  min(ingreso_region))/max(ingreso_region),4)



# horas

ocupades$horas_semana<-ocupades$PP3E_TOT
ocupades$horas_semana[ocupades$horas_semana==0]<-NA
ocupades$horas_semana[ocupades$horas_semana>84]<-NA
summary(ocupades$horas_semana)

# ingreso-hora
ocupades$ingreso_hora<-ocupades$ingresos/(ocupades$horas_semana*4)
summary(ocupades$ingreso_hora)


ingreso_hora_region<-
  tapply(ocupades$ingreso_hora, ocupades$REGION, mean, na.rm=TRUE)

gap_ingreso_hora_region<-100*round((max(ingreso_region)-
                                           min(ingreso_region))/max(ingreso_region),4)

ingreso_hora_sexo<-
  tapply(ocupades$ingreso_hora, ocupades$CH04, median, na.rm=TRUE)
gap_ingreso_hora_sexo<-100*round(
  (ingreso_hora_sexo[1]-ingreso_hora_sexo[2])/ingreso_hora_sexo[1],4)

gap_ingreso_sexo[[1]]

rm(columna_actividad)
porcentaje_actives<-100*round(c(actividad_general, actividad_sexo, actividad_edad, 
                     actividad_educacion, actividad_region),4)

u<-data.frame(porcentaje_actives, 
              row.names =c("general", "varones", "mujeres", "menores de 25",
                           "25-54", "55 y más", "sin asistencia escolar",
                           "primeria incompleta", "primaria completa", 
                           "secundaria incompleta", "secundaria completa",
                           "superior incompleta", "superior completa", 
                           "GBA", "NOA", "NEA", "Cuyo", "Pampeana", "Patagónica"))
u
write.csv(u, "actividad.csv")
