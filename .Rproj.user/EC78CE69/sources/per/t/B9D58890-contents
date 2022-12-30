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

base <- get_microdata(year = 2005:2022, 
                       trimester = 2,
                       type = 'individual')


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

base$caso<-1:length(base$CODUSU)
save(base, file = "base.rda")

############################ desde acá

load("base.rda")
                      ############INDICADORES GLOBALES#############
# tasas por año
tasas_de_actividad_aglo<-base %>% 
  group_by(AGLOMERADO) %>% 
  summarise(caso=caso, actividad_aglo=100*round((questionr::wtd.table(
    ESTADO,weights = PONDERA)[2]+questionr::wtd.table(
      ESTADO,weights = PONDERA)[3])/(addmargins(questionr::wtd.table(
        ESTADO,weights = PONDERA))[6]-questionr::wtd.table(
          ESTADO,weights = PONDERA)[1]),3))


tasas_de_actividad_año<-base %>% 
  group_by(año) %>% 
  summarise(caso=caso, actividad_año=100*round((questionr::wtd.table(
    ESTADO,weights = PONDERA)[2]+questionr::wtd.table(
      ESTADO,weights = PONDERA)[3])/(addmargins(questionr::wtd.table(
        ESTADO,weights = PONDERA))[6]-questionr::wtd.table(
          ESTADO,weights = PONDERA)[1]),3))


tasas_de_desocupacion_aglo<-base %>% 
  group_by(AGLOMERADO) %>% 
summarise(caso=caso, desocupacion_aglo=100*round(questionr::wtd.table(
  ESTADO,weights = PONDERA)[3]/(questionr::wtd.table(
    ESTADO,weights = PONDERA)[2]+questionr::wtd.table(
      ESTADO,weights = PONDERA)[3]),3))

tasas_de_desocupacion_año<-base %>% 
  group_by(año) %>% 
  summarise(caso=caso, desocupacion_año=100*round(questionr::wtd.table(
    ESTADO,weights = PONDERA)[3]/(questionr::wtd.table(
      ESTADO,weights = PONDERA)[2]+questionr::wtd.table(
        ESTADO,weights = PONDERA)[3]),3))

#############
base<-left_join(base, tasas_de_actividad_aglo)
base<-left_join(base, tasas_de_actividad_año)
base<-left_join(base, tasas_de_desocupacion_aglo)
base<-left_join(base, tasas_de_desocupacion_año)

rm(tasas_de_actividad_aglo, tasas_de_actividad_año,
   tasas_de_desocupacion_aglo, tasas_de_desocupacion_año)

# Keep working people
ocupades<-subset(base, base$ESTADO==1)


# Security = stability + health protection  

## employees  

### Stability   

# combines "term" or "permanent"  (PP07C) with duration of "term" (PP07D).    
# just for employees

asalariades<-subset(ocupades, ocupades$CAT_OCUP==3 &
                      ocupades$PP04B1!=1) # empleade no casa de familia
# puntaje desde temporario hasta permanente, según tiempo de finalización
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


### Health protection  

# obra social

asalariades$obra_social <-1
asalariades$obra_social[asalariades$PP07G4==0]<-NA
asalariades$obra_social[asalariades$PP07G4==9]<-NA
asalariades$obra_social[asalariades$PP07G4==2]<-0


### Combinación  
#Para que tengan igual peso se estandarizan al intervalo [0 - 0.50] por medio de $0.50*\frac{x-min}{max-min}$ cada una  


asalariades$estabilidad_st<-.5*(asalariades$estabilidad-1)/5

asalariades$obra_social_st<-.5*asalariades$obra_social

## Seguridad  
#Combinación aditiva de las dos  

asalariades$seguridad<-asalariades$estabilidad_st+
  asalariades$obra_social_st

## Cuenta propia  

### Estabilidad  
#Combina capital (maquinaria PP05C_1, local PP05C_2, vehículo PP05C_3) y clientes (uno solo o varios PP05F)  
#Se hace para el subconjunto de cuenta propia  


cuentapropia<-subset(ocupades,
                     (ocupades$CAT_OCUP==2 | ocupades$CAT_OCUP ==1) &
                       ocupades$PP04B1!=1)

cuentapropia$maquinas<-0
cuentapropia$maquinas[cuentapropia$PP05C_1==0]<-NA
cuentapropia$maquinas[cuentapropia$PP05C_1==9]<-NA
cuentapropia$maquinas[cuentapropia$PP05C_1==1]<-2
cuentapropia$maquinas[cuentapropia$PP05C_1==2]<-1

cuentapropia$local<-0
cuentapropia$local[cuentapropia$PP05C_2==0]<-NA
cuentapropia$local[cuentapropia$PP05C_2==9]<-NA
cuentapropia$local[cuentapropia$PP05C_2==1]<-2
cuentapropia$local[cuentapropia$PP05C_2==2]<-1

cuentapropia$vehiculo<-0
cuentapropia$vehiculo[cuentapropia$PP05C_3==0]<-NA
cuentapropia$vehiculo[cuentapropia$PP05C_3==9]<-NA
cuentapropia$vehiculo[cuentapropia$PP05C_3==1]<-2
cuentapropia$vehiculo[cuentapropia$PP05C_3==2]<-1

cuentapropia$clientes<-0
cuentapropia$clientes[cuentapropia$PP05F==0]<-NA
cuentapropia$clientes[cuentapropia$PP05F==9]<-NA
cuentapropia$clientes[cuentapropia$PP05F==6]<-0
cuentapropia$clientes[cuentapropia$PP05F==7]<-2

# estabilidad: suma
cuentapropia$estabilidad<-cuentapropia$maquinas+cuentapropia$local+
  cuentapropia$vehiculo+cuentapropia$clientes



### Obra social (del cuestionario hogar)

cuentapropia$obra_social<-1
cuentapropia$obra_social[cuentapropia$CH08==4]<-0
cuentapropia$obra_social[cuentapropia$CH08==9]<-NA

### Combinación  
#Para que tengan igual peso se estandarizan al intevalo [0 - 0.50] por medio de $0.50*\frac{x-min}{max-min}$ cada una y luego se suman  


cuentapropia$estabilidad_st<-.5*(cuentapropia$estabilidad)/8

cuentapropia$obra_social_st<-.5*cuentapropia$obra_social

## Seguridad  
#Combiación aditiva de las dos  


cuentapropia$seguridad<-cuentapropia$estabilidad_st+
  cuentapropia$obra_social_st


## Servicio doméstico  


servicio_domestico<-subset(ocupades, ocupades$PP04B1==1)


### Estabilidad  
#Combina vacaciones, aguinaldo, dias por enfermedad y obra social


# vacaciones,  aguinaldo, licencia enfermedad, obra social

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

### Se eliminan las variables que no están en las tres bases y se unen nuevamente  


asalariades<-subset(asalariades, select = -c(estabilidad, obra_social,
                                             estabilidad_st,obra_social_st))
cuentapropia<-
  subset(cuentapropia, select=-c(local, clientes, vehiculo, maquinas,
                                 estabilidad, obra_social, estabilidad_st,
                                 obra_social_st))

ocupades<-data.frame(rbind(asalariades, cuentapropia, servicio_domestico))

estabilidad_por_sexo_y_año<-ocupades %>% 
  group_by(sexo, año) %>% 
  summarise(seguridad=mean(seguridad, na.rm = T))


# Consistencia  
#Es la relación entre la calificación de la tarea y la educación del trabajador  

ocupades$calif.ocup= substr(ocupades$PP04D_COD, 5,5)

ocupades$calif.ocup[ocupades$calif.ocup>4]<-NA


#Se considera consistente (1) si educación y calificación están ambas por encima o ambas por debajo de sus medianas e inconsistente (0) en caso contrario  
############CONSISTENCIA E INGRESOS SON RELATIVOS AL AÑO Y AL AGLOMERADO
############HAY QUE CALCULARLAS ANTES
ocupades$educa_num<-as.numeric(ocupades$educacion)
ocupades$calif_num<-as.numeric(ocupades$calif.ocup)

medianas_calificacion_ocupacion<-ocupades %>% 
  group_by(año, AGLOMERADO) %>% 
  summarise(caso=caso, mediana_calificacion= wtd.quantile(calif_num, weights=PONDERA,
                                                          probs=.5),
            mediana_educacion=wtd.quantile(educa_num, weights=PONDERA,
                                           probs=.5))

ocupades<-left_join(ocupades, medianas_calificacion_ocupacion)

ocupades$consistencia<-as.factor(
  ifelse(
    (ocupades$educa_num >= ocupades$mediana_educacion) &
      (ocupades$calif_num >= ocupades$mediana_calificacion) |
      (ocupades$educa_num < ocupades$mediana_educacion) &
      (ocupades$calif_num < ocupades$mediana_calificacion),1,0))

ocupades$consistencia<-as.numeric(as.character(ocupades$consistencia))

# Ingresos  

#Se construye el ingreso laboral, combinando ingresos salariales con
#ingresos de cuentapropistas con o sin socios (en EPH son tres variables)    

ocupades$ingreso_laboral<-ifelse(
  (ocupades$CAT_OCUP==1 | ocupades$CAT_OCUP ==2) &
    ocupades$PP06A==1,ocupades$PP06D, ifelse(
      (ocupades$CAT_OCUP==1 | ocupades$CAT_OCUP ==2) &
 +       ocupades$PP06A==2,ocupades$PP06C, ifelse(
          ocupades$CAT_OCUP==3, ocupades$PP08D1, 0
        )
    )
)

#Se retienen los casos con ingreso no nulo y horas semanales trabajadas
# no nulas y menores a 168  7/7 24/24

ocupades<-subset(ocupades, ocupades$ingreso_laboral>0 &
                   ocupades$PP3E_TOT>0 & ocupades$PP3E_TOT < 168
)


#raw-hourly-income  
#It´s the quotient of monthly income & 4* weekly hours

ocupades$ingreso_hora_bruto<-ocupades$ingreso_laboral/(4*ocupades$PP3E_TOT)


#Se toma el logaritmo de los ingresos-hora y se estandariza (puntaje $z$)
# respecto de las medias y desviaciones (ponderadas) de cada aglomerado 
# en cada año

ingreso_hora_z<-ocupades %>% 
  group_by(año, AGLOMERADO) %>% 
  summarise(caso=caso, ingreso_hora_z=(
    log(ingreso_hora_bruto)-
      wtd.mean(
        log(ingreso_hora_bruto),
        weights = log(PONDERA)))/sqrt(wtd.var(
          log(ingreso_hora_bruto),weights=log(PONDERA))))



ocupades<-left_join(ocupades, ingreso_hora_z)


#Y se estandariza al [0 1]  

ocupades$ingreso_hora<-(ocupades$ingreso_hora_z-min(ocupades$ingreso_hora_z))/
  (max(ocupades$ingreso_hora_z)-min(ocupades$ingreso_hora_z))

# se normaliza demasiado por los logaritmos

# opción B

media_ingreso_hora_aglo_año<-ocupades %>% 
  group_by(año, AGLOMERADO) %>% 
  summarise(caso=caso, media_ingreso_hora_aglo_año=wtd.mean(
    ingreso_hora_bruto, weights=PONDERA))


ocupades<-left_join(ocupades, media_ingreso_hora_aglo_año)

ocupades$ingreso_alto<-ifelse(ocupades$ingreso_hora_bruto>
                                ocupades$media_ingreso_hora_aglo_año, 1,0)


rm(base, estabilidad_por_sexo_y_año, ingreso_hora_z, media_ingreso_hora_aglo_año,
   medianas_calificacion_ocupacion, servicio_domestico, cuentapropia, asalariades)
# Combinación de los componentes

# opción A
ocupades$calidad<-
  100*(ocupades$seguridad+ocupades$consistencia+ocupades$ingreso_hora)/3


calidad_por_año<-ocupades %>% 
  group_by(año) %>% 
  summarise(media_calidad_año=wtd.mean(calidad, weights=PONDERA))

calidad_por_año_aglo<-ocupades %>% 
  group_by(año, AGLOMERADO) %>% 
  summarise(media_calidad_año_aglo=wtd.mean(calidad, weights=PONDERA))


# Opción B
ocupades$calidad_B<-
  100*(ocupades$seguridad+ocupades$consistencia+ocupades$ingreso_alto)/3


calidadB_por_año<-ocupades %>% 
  group_by(año) %>% 
  summarise(media_calidadB_año=wtd.mean(calidad_B, weights=PONDERA))

calidadB_por_año_aglo<-ocupades %>% 
  group_by(año, AGLOMERADO) %>% 
  summarise(media_calidadB_año_aglo=wtd.mean(calidad_B, weights=PONDERA))


ocupades<-left_join(ocupades, calidad_por_año)
ocupades<-left_join(ocupades, calidad_por_año_aglo)
ocupades<-left_join(ocupades, calidadB_por_año)
ocupades<-left_join(ocupades, calidadB_por_año_aglo)

rm(calidad_por_año, calidad_por_año_aglo,calidadB_por_año,
   calidadB_por_año_aglo)

# A ver
library(gridExtra)
## Tendencia
calidad<-ggplot(ocupades)+geom_point(aes(año, media_calidad_año))+
  xlab("Año")+ylab("Calidad ocupación")+theme_tufte()
calidad_B<-ggplot(ocupades)+geom_point(aes(año, media_calidadB_año))+
  xlab("Año")+ylab("Calidad ocupación (opción B)")+theme_tufte()
actividad<-ggplot(ocupades)+geom_point(aes(año, actividad_año))+
  xlab("Año")+ylab("Tasa de actividad (%)")+theme_tufte()
desocupacion<-ggplot(ocupades)+geom_point(aes(año, desocupacion_año))+
  xlab("Año")+ylab("Tasa de desocupación (%)")+theme_tufte()


grid.arrange(calidad, calidad_B, actividad, desocupacion)

## Por sector

por_sector<-ocupades %>%
  group_by(año, sector) %>%
  summarise(media_calidad = round(wtd.mean(calidad, weights = PONDERA),3),
            mediana = round(median(calidad, na.rm = TRUE),3),
            casos=n())
gg_sector<-ggplot(por_sector[is.na(por_sector$sector)==F,],
       aes(año, media_calidad, col=sector))+
  geom_point()+ geom_smooth(se=F)+
  xlab("Año")+ylab("Calidad de la ocupación")+theme_tufte()

por_sector_B<-ocupades %>%
  group_by(año, sector) %>%
  summarise(media_calidad_B = round(wtd.mean(calidad_B, weights = PONDERA),3),
            mediana = round(median(calidad_B, na.rm = TRUE),3),
            casos=n())
gg_sector_B<-ggplot(por_sector_B[is.na(por_sector_B$sector)==F,],
       aes(año, media_calidad_B, col=sector))+
  geom_point()+ geom_smooth(se=F)+
  xlab("Año")+ylab("Calidad de la ocupación (opción B)")+theme_tufte()


## Por sexos  
por_sexos<-ocupades %>%
  group_by(año, sexo) %>%
  summarise(media_calidad = round(wtd.mean(calidad, weights = PONDERA),3),
            mediana = round(median(calidad, na.rm = TRUE),3),
            casos=n())
gg_sexos<-ggplot(por_sexos,aes(año, media_calidad, col=sexo))+
  geom_point()+ geom_smooth(se=F)+
  xlab("Año")+ylab("Calidad de la ocupación")+theme_tufte()

por_sexos_B<-ocupades %>%
  group_by(año, sexo) %>%
  summarise(media_calidad = round(wtd.mean(calidad_B, weights = PONDERA),3),
            mediana = round(median(calidad_B, na.rm = TRUE),3),
            casos=n())
gg_sexos_B<-ggplot(por_sexos,aes(año, media_calidad, col=sexo))+
  geom_point()+ geom_smooth(se=F)+
  xlab("Año")+ylab("Calidad de la ocupación (opción B)")+theme_tufte()

grid.arrange(gg_sector, gg_sector_B, gg_sexos, gg_sexos_B)

## Por regiones  
por_region<-ocupades %>%
  group_by(año, REGION_rot) %>%
  summarise(media_calidad = round(wtd.mean(calidad, weights = PONDERA),3),
            mediana = round(median(calidad, na.rm = TRUE),3),
            casos=n())
gg_region<-ggplot(por_region,aes(año, media_calidad, col=REGION_rot))+
  geom_point()+ geom_smooth(se=F)+
  xlab("Año")+ylab("Calidad de la ocupación")+theme_tufte()

por_region_B<-ocupades %>%
  group_by(año, REGION_rot) %>%
  summarise(media_calidad = round(wtd.mean(calidad_B, weights = PONDERA),3),
            mediana = round(median(calidad_B, na.rm = TRUE),3),
            casos=n())
gg_region_B<-ggplot(por_region_B,aes(año, media_calidad, col=REGION_rot))+
  geom_point()+ geom_smooth(se=F)+
  xlab("Año")+ylab("Calidad de la ocupación (opción B)")+theme_tufte()

grid.arrange(gg_region, gg_region_B)
