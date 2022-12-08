library(foreign)
library(questionr)
library(dplyr)
library(xlsx)
library(ggplot2)
library(ggthemes)
library(matrixStats)

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



summary(base$grupos_edades)
## 3. ingresos -hora solo ocupades

# ingreso per capita familiar, se quita el 1% superior
summary(base$IPCF)
base$IPCF[base$IPCF==0]<-NA
base$IPCF[base$IPCF>quantile(
  base$IPCF,.99, na.rm = TRUE)]<-NA

ingresos_bajos<-subset(base, base$IPCF<=quantile(base$IPCF,.10, na.rm = TRUE))
ingresos_altos<-subset(base, base$IPCF>=quantile(base$IPCF,.90, na.rm = TRUE))
p90_p10_total<-wtd.mean(ingresos_altos$IPCF, weights = ingresos_altos$PONDERA)/
  wtd.mean(ingresos_bajos$IPCF, weights = ingresos_bajos$PONDERA)

ingresos_bajos_mujeres<-subset(ingresos_bajos, ingresos_bajos$CH04=="mujeres")
ingresos_altos_mujeres<-subset(ingresos_altos, ingresos_altos$CH04=="mujeres")
p90_p10_mujeres<-wtd.mean(ingresos_altos_mujeres$IPCF, weights = ingresos_altos_mujeres$PONDERA)/
  wtd.mean(ingresos_bajos_mujeres$IPCF, weights = ingresos_bajos_mujeres$PONDERA)


ingresos_bajos_varones<-subset(ingresos_bajos, ingresos_bajos$CH04=="varones")
ingresos_altos_varones<-subset(ingresos_altos, ingresos_altos$CH04=="varones")
p90_p10_varones<-wtd.mean(ingresos_altos_varones$IPCF, weights = ingresos_altos_varones$PONDERA)/
  wtd.mean(ingresos_bajos_varones$IPCF, weights = ingresos_bajos_varones$PONDERA)



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


# Desigualdad en ingreso hora
ocupades$ingreso_hora<-ocupades$ingresos/(ocupades$horas_semana*4)
summary(ocupades$ingreso_hora)

desig_ingreso_hora<-quantile(ocupades$ingreso_hora,.9, na.rm = TRUE)/
  quantile(ocupades$ingreso_hora,.1, na.rm = TRUE)

por_sexos<-ocupades%>% group_by(CH04)
por_sexos_desig_ingreso_hora<-summarise(quantile(ingreso_hora,.9, na.rm = TRUE)/
                                          quantile(ingreso_hora,.1, na.rm = TRUE))

desig_ingreso_hora_sexo<-ocupades %>%  group_by(CH04) %>% 
  summarise(valor = quantile(ingreso_hora,.9, na.rm = TRUE)/
              quantile(ingreso_hora,.1, na.rm = TRUE))



gap_desig_ingreso_hora_sexo<-100*round((desig_ingreso_hora_sexo[1,2]-desig_ingreso_hora_sexo[2,2])/
                                         desig_ingreso_hora_sexo[1,2],4)


desig_ingreso_hora_region<-ocupades %>%  group_by(REGION) %>% 
  summarise(valor= quantile(ingreso_hora,.9, na.rm = TRUE)/
              quantile(ingreso_hora,.1, na.rm = TRUE))
desig_ingreso_hora_region

gap_desig_ingreso_hora_region<-100*round((max(desig_ingreso_hora_region[,2])-min(desig_ingreso_hora_region[,2]))/
                                           max(desig_ingreso_hora_region[,2]),4)


desig_ingreso_hora_edad<-ocupades %>% group_by(grupos_edades) %>% 
  summarise(valor=quantile(ingreso_hora,.9, na.rm = TRUE)/
              quantile(ingreso_hora,.1, na.rm = TRUE))

gap_desig_ingreso_hora_edad<-100*round((max(desig_ingreso_hora_edad[,2])-min(desig_ingreso_hora_edad[,2]))/
                                         max(desig_ingreso_hora_edad[,2]),4)


desig_ingreso_hora_educacion<-ocupades %>% group_by(NIVEL_ED) %>% 
  summarise(valor=quantile(ingreso_hora,.9, na.rm = TRUE)/
              quantile(ingreso_hora,.1, na.rm = TRUE))
desig_ingreso_hora_educacion
gap_desig_ingreso_hora_educacion<-100*round((max(desig_ingreso_hora_educacion[,2])-min(desig_ingreso_hora_educacion[,2]))/
                                              max(desig_ingreso_hora_educacion[,2]),4)


# para la tabla de gaps
fronteras<-c("sexo", "región", "grupos de edades", "educación")
tasa_de_actividad<-c(gap_actividad_sexo[1,1],
                     gap_actividad_region, gap_actividad_edad,
                     gap_actividad_educacion)
tasa_de_desocupacion<-c(gap_desocupacion_sexo[1,1],
                        gap_desocupacion_region, gap_desocupacion_edad,
                        gap_desocupacion_educacion)
seguridad_laboral<-c(gap_seguridad_sexo[1,1],
                     gap_seguridad_region, gap_seguridad_edad,
                     gap_seguridad_educacion)
proporcion_de_cuentapropistas<-c(gap_prop_independientes_sexo[1,1],
                                 gap_prop_independientes_region, gap_prop_independientes_edad,
                                 gap_prop_independientes_educacion)
ingresos_mes<-c(gap_ingreso_sexo[1,1],
                gap_ingreso_region, gap_ingreso_edad,
                gap_ingreso_educacion)
ingresos_por_hora<-c(gap_ingreso_hora_sexo[1,1],
                     gap_ingreso_hora_region,gap_ingreso_hora_edad,
                     gap_ingreso_hora_educacion)
desig_ingresos_por_hora<-c(gap_desig_ingreso_hora_sexo[1,1],
                           gap_desig_ingreso_hora_region,gap_desig_ingreso_hora_edad,
                           gap_desig_ingreso_hora_educacion)

gaps<-data.frame(cbind(fronteras, tasa_de_actividad, tasa_de_desocupacion,
                       proporcion_de_cuentapropistas, seguridad_laboral,
                       ingresos_mes, ingresos_por_hora, desig_ingresos_por_hora))

gaps$tasa_de_actividad<-as.numeric(gaps$tasa_de_actividad)
gaps$tasa_de_desocupacion<-as.numeric(gaps$tasa_de_desocupacion)
gaps$proporcion_de_cuentapropistas<-as.numeric(gaps$proporcion_de_cuentapropistas)
gaps$seguridad_laboral<-as.numeric(gaps$seguridad_laboral)
gaps$ingresos_mes<-as.numeric(gaps$ingresos_mes)
gaps$ingresos_por_hora<-as.numeric(gaps$ingresos_por_hora)
gaps$desig_ingresos_por_hora<-as.numeric(gaps$desig_ingresos_por_hora)

# se escala con media ceo y varianza uno
gaps_standarized<-data.frame(fronteras, scale(gaps[,-1]))


# se calcula el Labor Unequality Index (LaUnIn)
gaps_standarized$LaUnIn<-(gaps_standarized$tasa_de_actividad -
                            gaps_standarized$tasa_de_desocupacion - gaps_standarized$proporcion_de_cuentapropistas +
                            gaps_standarized$seguridad_laboral + gaps_standarized$ingresos_mes +
                            gaps_standarized$ingresos_por_hora - gaps_standarized$desig_ingresos_por_hora)/7


write.xlsx(gaps, "gaps.xlsx", sheetName = "gaps")
write.xlsx(gaps_standarized, "gaps.xlsx", sheetName = "estandarizadas", append = TRUE)



#############TODES JUNTES ###############
actividad_todes<-mayores_9 %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=(((addmargins(wtd.table(ESTADO, weights = PONDERA)))[1]+
                      (addmargins(wtd.table(ESTADO, weights = PONDERA)))[2])/
                     (addmargins(wtd.table(ESTADO, weights = PONDERA)))[4]))
names(actividad_todes)<-c("sexo", "región", "grupo de edades", "educación",
                          "tasa de actividad")

# contar los casos que hay en cada combinación y quedarse solo con las categorías que tienen
# al menos n
casos_actividad<-mayores_9 %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(casos=length(ESTADO))

# Acá está el problema
fsffs<-subset(mayores_9, mayores_9$CH04 == "varones" & 
                mayores_9$REGION == "Gran Buenos Aires" &
                mayores_9$grupos_edades == "(24,39]" &
                mayores_9$NIVEL_ED == "nunca asistió")
wtd.table(fsffs$ESTADO, weights = fsffs$PONDERA)

desocupacion_todes<-actives %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=(addmargins(wtd.table(ESTADO, weights = PONDERA))[2]/
                     addmargins(wtd.table(ESTADO, weights = PONDERA))[3]))
names(desocupacion_todes)<-c("sexo", "región", "grupo de edades", "educación",
                             "tasa de desocupación")

prop_independientes_todes<-ocupades %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=(addmargins(wtd.table(CAT_OCUP, weights = PONDERA)))[2]/
              (addmargins(wtd.table(CAT_OCUP, weights = PONDERA)))[5])
names(prop_independientes_todes)<-c("sexo", "región", "grupo de edades", "educación",
                                    "proporción de cuenta propia")

seguridad_todes<-asalariades %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=wtd.mean(seguridad, weights = PONDERA))
names(seguridad_todes)<-c("sexo", "región", "grupo de edades", "educación",
                          "índice de seguridad laboral")

ingreso_todes<-ocupades %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=wtd.mean(ingresos, weights = PONDERA))
names(ingreso_todes)<-c("sexo", "región", "grupo de edades", "educación",
                        "media de ingresos laborales")

ingreso_hora_todes<-ocupades %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=wtd.mean(ingreso_hora, weights = PONDERA))
names(ingreso_hora_todes)<-c("sexo", "región", "grupo de edades", "educación",
                             "media de ingresos por hora")

desig_ingreso_hora_todes<-ocupades %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=quantile(ingreso_hora,.9, na.rm = TRUE)/
              quantile(ingreso_hora,.1, na.rm = TRUE))

names(desig_ingreso_hora_todes)<-c("sexo", "región", "grupo de edades", "educación",
                                   "P90/P10")


u<-merge(actividad_todes, desocupacion_todes, all.x = TRUE)
u<-merge(desocupacion_todes,prop_independientes_todes, all.x = TRUE)
u<-merge(u,seguridad_todes, all.x = TRUE)
u<-merge(u, ingreso_todes, all.x = TRUE)
u<-merge(u,ingreso_hora_todes, all.x = TRUE)
u<-merge(u,desig_ingreso_hora_todes, all.x = TRUE)

# les puedo asignar rangos (con el signo que corresponda)
# o estandarizar a la media y desviación del valor nacional
# luego se hace el índice sumando y restando valires z
# que quiere decir z alto:
# bajo desempleo, alta seguridad, alto ingreso hora, poca desigualdad

# asignacion de rangos
a<-c(5,NA, 3,8,4,9,9, NA)
b<-rank(a, na.last = "keep")
b
c<-rev(rank(a, na.last = "keep"))

# valores nacionales
tasa_desocupacion
seguridad
ingreso_hora
desig_ingreso_hora

# estandarizacion
u$desocupacion_st<-(u$`tasa de desocupación`-tasa_desocupacion)/
  sqrt(tasa_desocupacion*(1-tasa_desocupacion))

u$ingreso_hora_st<-(u$`media de ingresos por hora` -ingreso_hora)/
  sd_ingreso_hora

u$seguridad_st<-(u$`índice de seguridad laboral`-seguridad)/
  sd_seguridad

# Falta la desigualdad p90/p10, pero tengo el rumbo perdido
# 

write.xlsx(u, "todes_todes.xlsx")

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

compara_grupos<-data.frame(grupos, valores_actividad, valores_desocupacion,
                           valores_prop_independientes, valores_seguridad, valores_ingreso,
                           valores_ingreso_hora, valores_desig_ingreso_hora)


# lo mando a excel
write.xlsx(compara_grupos, "sintesis.xlsx", sheetName = "prueba_todes")

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
##################################################
#############TODES JUNTES ###############
actividad_todes<-grupo_25_64 %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=(((addmargins(wtd.table(ESTADO, weights = PONDERA)))[1]+
                      (addmargins(wtd.table(ESTADO, weights = PONDERA)))[2])/
                     (addmargins(wtd.table(ESTADO, weights = PONDERA)))[4]))
names(actividad_todes)<-c("sexo", "región", "grupo de edades", "educación",
                          "tasa de actividad")

# contar los casos que hay en cada combinación y quedarse solo con las categorías que tienen
# al menos n
casos_actividad<-mayores_9 %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(casos=length(ESTADO))

# Acá está el problema
fsffs<-subset(mayores_9, mayores_9$CH04 == "varones" & 
                mayores_9$REGION == "Gran Buenos Aires" &
                mayores_9$grupos_edades == "(24,39]" &
                mayores_9$NIVEL_ED == "nunca asistió")
wtd.table(fsffs$ESTADO, weights = fsffs$PONDERA)

desocupacion_todes<-actives %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=(addmargins(wtd.table(ESTADO, weights = PONDERA))[2]/
                     addmargins(wtd.table(ESTADO, weights = PONDERA))[3]))
names(desocupacion_todes)<-c("sexo", "región", "grupo de edades", "educación",
                             "tasa de desocupación")

prop_independientes_todes<-ocupades %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=(addmargins(wtd.table(CAT_OCUP, weights = PONDERA)))[2]/
              (addmargins(wtd.table(CAT_OCUP, weights = PONDERA)))[5])
names(prop_independientes_todes)<-c("sexo", "región", "grupo de edades", "educación",
                                    "proporción de cuenta propia")

seguridad_todes<-asalariades %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=wtd.mean(seguridad, weights = PONDERA))
names(seguridad_todes)<-c("sexo", "región", "grupo de edades", "educación",
                          "índice de seguridad laboral")

ingreso_todes<-ocupades %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=wtd.mean(ingresos, weights = PONDERA))
names(ingreso_todes)<-c("sexo", "región", "grupo de edades", "educación",
                        "media de ingresos laborales")

ingreso_hora_todes<-ocupades %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=wtd.mean(ingreso_hora, weights = PONDERA))
names(ingreso_hora_todes)<-c("sexo", "región", "grupo de edades", "educación",
                             "media de ingresos por hora")

desig_ingreso_hora_todes<-ocupades %>%
  group_by(CH04, REGION, grupos_edades, NIVEL_ED) %>% 
  summarise(valor=quantile(ingreso_hora,.9, na.rm = TRUE)/
              quantile(ingreso_hora,.1, na.rm = TRUE))

names(desig_ingreso_hora_todes)<-c("sexo", "región", "grupo de edades", "educación",
                                   "P90/P10")


u<-merge(actividad_todes, desocupacion_todes, all.x = TRUE)
u<-merge(desocupacion_todes,prop_independientes_todes, all.x = TRUE)
u<-merge(u,seguridad_todes, all.x = TRUE)
u<-merge(u, ingreso_todes, all.x = TRUE)
u<-merge(u,ingreso_hora_todes, all.x = TRUE)
u<-merge(u,desig_ingreso_hora_todes, all.x = TRUE)

# les puedo asignar rangos (con el signo que corresponda)
# o estandarizar a la media y desviación del valor nacional
# luego se hace el índice sumando y restando valires z
# que quiere decir z alto:
# bajo desempleo, alta seguridad, alto ingreso hora, poca desigualdad

# asignacion de rangos
a<-c(5,NA, 3,8,4,9,9, NA)
b<-rank(a, na.last = "keep")
b
c<-rev(rank(a, na.last = "keep"))
###########################

#################
desig_ingreso_hora_region<-ocupades %>%  group_by(REGION) %>% 
  summarise(valor= quantile(ingreso_hora,.9, na.rm = TRUE)/
              quantile(ingreso_hora,.1, na.rm = TRUE))
desig_ingreso_hora_region

gap_desig_ingreso_hora_region<-100*round((max(desig_ingreso_hora_region[,2])-min(desig_ingreso_hora_region[,2]))/
                                           max(desig_ingreso_hora_region[,2]),4)


desig_ingreso_hora_edad<-ocupades %>% group_by(grupos_edades) %>% 
  summarise(valor=quantile(ingreso_hora,.9, na.rm = TRUE)/
              quantile(ingreso_hora,.1, na.rm = TRUE))

gap_desig_ingreso_hora_edad<-100*round((max(desig_ingreso_hora_edad[,2])-min(desig_ingreso_hora_edad[,2]))/
                                         max(desig_ingreso_hora_edad[,2]),4)


desig_ingreso_hora_educacion<-ocupades %>% group_by(NIVEL_ED) %>% 
  summarise(valor=quantile(ingreso_hora,.9, na.rm = TRUE)/
              quantile(ingreso_hora,.1, na.rm = TRUE))
desig_ingreso_hora_educacion
gap_desig_ingreso_hora_educacion<-100*round((max(desig_ingreso_hora_educacion[,2])-min(desig_ingreso_hora_educacion[,2]))/
                                              max(desig_ingreso_hora_educacion[,2]),4)


# para la tabla de gaps
fronteras<-c("sexo", "región", "grupos de edades", "educación")
tasa_de_actividad<-c(gap_actividad_sexo[1,1],
                     gap_actividad_region, gap_actividad_edad,
                     gap_actividad_educacion)
tasa_de_desocupacion<-c(gap_desocupacion_sexo[1,1],
                        gap_desocupacion_region, gap_desocupacion_edad,
                        gap_desocupacion_educacion)
seguridad_laboral<-c(gap_seguridad_sexo[1,1],
                     gap_seguridad_region, gap_seguridad_edad,
                     gap_seguridad_educacion)
proporcion_de_cuentapropistas<-c(gap_prop_independientes_sexo[1,1],
                                 gap_prop_independientes_region, gap_prop_independientes_edad,
                                 gap_prop_independientes_educacion)
ingresos_mes<-c(gap_ingreso_sexo[1,1],
                gap_ingreso_region, gap_ingreso_edad,
                gap_ingreso_educacion)
ingresos_por_hora<-c(gap_ingreso_hora_sexo[1,1],
                     gap_ingreso_hora_region,gap_ingreso_hora_edad,
                     gap_ingreso_hora_educacion)
desig_ingresos_por_hora<-c(gap_desig_ingreso_hora_sexo[1,1],
                           gap_desig_ingreso_hora_region,gap_desig_ingreso_hora_edad,
                           gap_desig_ingreso_hora_educacion)

gaps<-data.frame(cbind(fronteras, tasa_de_actividad, tasa_de_desocupacion,
                       proporcion_de_cuentapropistas, seguridad_laboral,
                       ingresos_mes, ingresos_por_hora, desig_ingresos_por_hora))

gaps$tasa_de_actividad<-as.numeric(gaps$tasa_de_actividad)
gaps$tasa_de_desocupacion<-as.numeric(gaps$tasa_de_desocupacion)
gaps$proporcion_de_cuentapropistas<-as.numeric(gaps$proporcion_de_cuentapropistas)
gaps$seguridad_laboral<-as.numeric(gaps$seguridad_laboral)
gaps$ingresos_mes<-as.numeric(gaps$ingresos_mes)
gaps$ingresos_por_hora<-as.numeric(gaps$ingresos_por_hora)
gaps$desig_ingresos_por_hora<-as.numeric(gaps$desig_ingresos_por_hora)

# se escala con media ceo y varianza uno
gaps_standarized<-data.frame(fronteras, scale(gaps[,-1]))

# se escala al 0 - 100
gaps_normalized<-data.frame(fronteras, normalize(gaps[,-1], method = "range",
                                                 range = c(0,100)))

# se calcula el Labor Unequality Index (LaUnIn)
gaps_standarized$LaUnIn<-(gaps_standarized$tasa_de_actividad -
                            gaps_standarized$tasa_de_desocupacion - gaps_standarized$proporcion_de_cuentapropistas +
                            gaps_standarized$seguridad_laboral + gaps_standarized$ingresos_mes +
                            gaps_standarized$ingresos_por_hora - gaps_standarized$desig_ingresos_por_hora)/7

# se calcula el Labor Unequality Index_1 (LaUnIn_1) con normalización
gaps_normalized$LaUnIn_1<-(gaps_normalized$tasa_de_actividad -
                             gaps_normalized$tasa_de_desocupacion - gaps_normalized$proporcion_de_cuentapropistas +
                             gaps_normalized$seguridad_laboral + gaps_normalized$ingresos_mes +
                             gaps_normalized$ingresos_por_hora - gaps_normalized$desig_ingresos_por_hora)/7

write.xlsx(gaps, "gaps.xlsx", sheetName = "gaps")
write.xlsx(gaps_standarized, "gaps.xlsx", sheetName = "estandarizadas", append = TRUE)
write.xlsx(gaps_normalized, "gaps.xlsx", sheetName = "normalizadas", append = TRUE)

##### hay que tomar menos indicadores: desocpacion, seguridad, ingresos-hora,
##desig_ingresohora. Se llama LaUnIn_3

gaps_normalized$LaUnIn_2<-((100-gaps_normalized$tasa_de_desocupacion) +
                             gaps_normalized$seguridad_laboral +
                             gaps_normalized$ingresos_por_hora +
                             (100-gaps_normalized$desig_ingresos_por_hora))/7


# valores nacionales
tasa_desocupacion
seguridad
ingreso_hora
desig_ingreso_hora

# estandarizacion
u$desocupacion_st<-(u$`tasa de desocupación`-tasa_desocupacion)/
  sqrt(tasa_desocupacion*(1-tasa_desocupacion))

u$ingreso_hora_st<-(u$`media de ingresos por hora` -ingreso_hora)/
  sd_ingreso_hora

u$seguridad_st<-(u$`índice de seguridad laboral`-seguridad)/
  sd_seguridad

# Falta la desigualdad p90/p10, pero tengo el rumbo perdido
# 

write.xlsx(u, "todes_todes.xlsx")

