library(eph)
library(dplyr)
library(caret)
datos <- get_microdata(year = 2005:2021, 
                       trimester = 2,
                       type = 'individual',
                       vars = c('ANO4','TRIMESTRE' ,'CH04', 'ESTADO','CAT_OCUP','PP07G1','PP07G2',
                                'PP07G3','PP07G4','PP07H','PP08D1','P21','PP3E_TOT',
                                'PP03C','PP03D','REGION','NIVEL_ED','CH06','PP04D_COD','DECOCUR',
                                'PP04A','PP03G','AGLOMERADO'))
datos <- rename(datos, año = ANO4)
datos$sexo = ifelse(datos$CH04==1, "Varón", "Mujer")

datos$calificacion = substr(datos$PP04D_COD, 5, 5)

datos$calificacion_rec<-as.factor(datos$calificacion)
levels(datos$calificacion_rec)<-c(NA, "profesional", "técnico", "operativo",
                                  "no calificado", NA, NA,NA,  NA)

table(datos$calificacion, datos$calificacion_rec)
datos$CAT_OCUP_rec<-as.factor(datos$CAT_OCUP)

levels(datos$CAT_OCUP_rec) = c(NA, "Patrón","Cuenta propia","Obrero o empleado",
                                                  "Trabajador familiar sin remuneración", NA)



datos$vacaciones <- ifelse(datos$PP07G1==1, 1,
                          ifelse(datos$PP07G1==2, 0, NA))
datos$aguinaldo <- ifelse(datos$PP07G2==1, 1,
                          ifelse(datos$PP07G2==2, 0, NA))
datos$dias_enferm <- ifelse(datos$PP07G3==1, 1,
                            ifelse(datos$PP07G3==2, 0, NA))
datos$obra_social <- ifelse(datos$PP07G4==1, 1,
                            ifelse(datos$PP07G4==2, 0, NA))
datos$jubilacion <- ifelse(datos$PP07H==1, 1,
                           ifelse(datos$PP07H==2, 0, NA))


u<-datos %>% 
  group_by(año) %>% 
  summarise(categoria=100*round(prop.table(table(CAT_OCUP_rec)),3),
            calif=100*round(prop.table(table(calificacion_rec)),3),
            vaca=mean(vacaciones, na.rm=T),
            agui=mean(aguinaldo, na.rm=T),
            dias=mean(dias_enferm, na.rm=T),
            obra=mean(obra_social, na.rm=T),
            jubi=mean(jubilacion, na.rm=T),
            segu=mean(seguri, na.rm=T)
)

datos$seguri<-datos$vacaciones+datos$aguinaldo+datos$dias_enferm+
  datos$obra_social+datos$jubilacion
summary(datos$vacaciones)

para_cor_suguridad<-datos[,c("vacaciones", "aguinaldo", "dias_enferm", "obra_social",
                    "jubilacion", "seguri")]

cor(para_cor_suguridad, method = "pearson", use = "complete.obs")
# muy correlacionadas, con uno bastaría

u<-datos %>% 
  group_by(año) %>% 
  summarise(categoria=100*round(prop.table(table(CAT_OCUP_rec)),3),
            calif=100*round(prop.table(table(calificacion_rec)),3),
            vaca=mean(vacaciones, na.rm=T),
            agui=mean(aguinaldo, na.rm=T),
            dias=mean(dias_enferm, na.rm=T),
            obra=mean(obra_social, na.rm=T),
            jubi=mean(jubilacion, na.rm=T),
            segu=mean(seguri, na.rm=T)
  )

summary(datos[datos$año==2020, ]$P21)
datos_con_ingreso<-subset(datos, datos$P21>0)
quantile(datos_con_ingreso$P21, .99, na.rm = T)

w<- datos_con_ingreso %>% 
  group_by(año, REGION) %>% 
  summarise(p25=quantile(P21,.25, na.rm=T),
            max=max(P21))
# le tengo que eliminar a cada región lo que suere al p99