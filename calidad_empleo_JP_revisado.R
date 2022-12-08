rm(list=ls())
library(eph)
library(dplyr)
library(caret)
datos <- get_microdata(year = 2005:2021, 
                       trimester = 2,
                       type = 'individual',
                       vars = c('ANO4', 'CH04', 'ESTADO','CAT_OCUP','PP07G1','PP07G2',
                                'PP07G3','PP07G4','PP07H','PP08D1','P21','PP3E_TOT',
                                'PP03C','PP03D','REGION','NIVEL_ED','CH06','PP04D_COD','DECOCUR',
                                'PP04A'))
datos <- rename(datos, año = ANO4)
datos$sexo = ifelse(datos$CH04==1, "Varón", "Mujer")
# generomos las variables necesarias para la construcción del índice
# para los cuentapropistas, vamos a considerar sólo los que tienen calificación
# Operativa o No calificada, según el 5to dígito del CNO.
datos$calificacion = substr(datos$PP04D_COD, 5, 5)


datos$cuentapropista = ifelse(datos$CAT_OCUP==2 &
                                (datos$calificacion=="3" |
                                   datos$calificacion==4), 1, 0)

###NO SIGO ESTO:
datos$cuentapropista = ifelse(datos$ESTADO==1, datos$cuentapropista, NA)


# variables para generar el índice de "seguridad" en el empleo (formalidad)
datos$vacaciones = ifelse(datos$PP07G1==1, 1, 0)
datos$aguinaldo = ifelse(datos$PP07G2==1, 1, 0)
datos$obra_social = ifelse(datos$PP07G4==1, 1, 0)
datos$dias_enferm = ifelse(datos$PP07G3==1, 1, 0)
datos$jubilacion = ifelse(datos$PP07H==1, 1, 0)
attach(datos)
datos$seguridad = (vacaciones + aguinaldo +
                     obra_social + dias_enferm + jubilacion)/5
# ingreso por hora
datos$ingreso_hora = ifelse(datos$P21<=0 |
                              datos$PP3E_TOT==0, NA,
                            datos$P21/(
                              ((as.numeric(strftime(paste0(datos[1,]$año,
                                                           "-12-31"),
                                                    format = "%V")))/12)*
                                datos$PP3E_TOT))


# todavía contiene los ceros
summary(datos[datos$año=="2021",]$ingreso_hora)

# cantidad de ocupaciones
datos$cantidad_ocupaciones = ifelse(datos$PP03C==1, 1, datos$PP03D)
datos$cantidad_ocupaciones = ifelse(datos$cantidad_ocupaciones==0, NA,
                                    datos$cantidad_ocupaciones)
# horas trabajadas
datos$PP3E_TOT = ifelse(datos$PP3E_TOT==0 |
                          datos$PP3E_TOT > 100, NA, datos$PP3E_TOT)
# decil de ingreso de la ocupación principal
datos = datos %>% 
  group_by(año) %>% 
  filter(P21 > 10) %>% 
  mutate(decil = ntile(P21, 10))

table(is.na(datos$decil))
# empleo público o privado
datos$publico = ifelse(datos$PP04A == 1, 1, 0)
datos$publico = ifelse(is.na(datos$PP04A), NA, datos$publico)
# para segmentar por nivel educativo
datos$educacion <- case_when(datos$NIVEL_ED == 1 ~ "Primaria o -",
                             datos$NIVEL_ED == 2 ~ "Primaria o -",
                             datos$NIVEL_ED == 7 ~ "Primaria o -",
                             datos$NIVEL_ED == 3 ~ "Secundaria incompleta",
                             datos$NIVEL_ED == 4 ~ "Secundaria completa",
                             datos$NIVEL_ED == 5 ~ "Universitaria incompleta",
                             datos$NIVEL_ED == 6 ~ "Universitaria completa")
# para segmentar por región
datos$region <- case_when(datos$REGION == 1  ~ "GBA",
                          datos$REGION == 40 ~ "NOA",
                          datos$REGION == 41 ~ "NEA",
                          datos$REGION == 42 ~ "Cuyo",
                          datos$REGION == 43 ~ "Pampeana",
                          datos$REGION == 44 ~ "Patagonia")

# armamos una funcioncita para escalar por rango, ya que lo vamos a usar de manera intensiva
esc <- function(x){(x-min(x, na.rm = T))/
    (max(x, na.rm = T)-min(x, na.rm = T))}

# agregamos las variables a nivel micro

datos2 = datos %>%
  group_by(año) %>% 
  mutate(seguridad = esc(seguridad),
         horas_trabajadas = esc(PP3E_TOT),
         ingreso_hs = esc(ingreso_hora),
         ocupaciones = esc(cantidad_ocupaciones),
         cuentaprop = esc(cuentapropista),
         decil = esc(decil),
         publico = esc(publico))

# Contrucción de índice con PCA

indice = datos2[,c(
  "seguridad","horas_trabajadas","ingreso_hs",
  "ocupaciones","cuentaprop",
                   "decil","publico")]
# escalo las variables
pre_proceso <- preProcess(indice, method = c("center", "scale")) 
indice <- predict(pre_proceso, indice) 
summary(indice) 
# elimino NA
indice = na.omit(indice)
# calculo las CP
library(factoextra)
set.seed (7) 
res.pca <- prcomp(indice, scale = TRUE) 
# extraigo los vectores propios
eig.val <- get_eigenvalue(res.pca)
eig.val <- round(eig.val, digits = 2)
eig.val = subset(eig.val, eig.val$cumulative.variance.percent < 90)
ncomp = dim(eig.val)[1]
# reescalo los valores propios
eig.val$varianza_reescalada = eig.val$variance.percent/
  eig.val[nrow(eig.val), c("cumulative.variance.percent")]
# hago la rotación ortogonal
loadings_rotada <- varimax(res.pca$rotation[,1:ncomp])
matrix_varimax = as.matrix(loadings_rotada$loadings) 
# hago el producto matricial entre la matriz rotada y vector de valores propios
ponderador = matrix_varimax %*% eig.val$varianza_reescalada
# Calculo el índice
indice =  as.data.frame(as.matrix(indice) %*% ponderador)
summary(indice)
# esto lo hago porque me parece que los signos del PCA están al reves. Le da valores negativos a cosas con efecto "positivo".
indice = esc(1-indice)
summary(indice)
# se distribuye bien el índice
hist(indice$V1)

# pego el índice a la base de datos original
datos3 = datos2[,c("seguridad","horas_trabajadas","ingreso_hs","ocupaciones","cuentaprop",
                   "decil","publico","año","sexo","region","educacion")]
summary(datos3)
datos3 = na.omit(datos3)
datos3$indice_acp = indice$V1
summary(datos3)

# Por sexo

tabla = datos3 %>% 
  group_by(año, sexo) %>% 
  summarise(indice = mean(indice_acp, na.rm = T),
            inf = indice - (1.96 * sqrt((indice * (1 - indice)) / n())),
            sup = indice + (1.96 * sqrt((indice * (1 - indice)) / n())))
tabla

ggplot(tabla, aes(x = año, y = indice, colour = sexo, group = sexo)) + 
  geom_line() +
  geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.2, linetype = 3, col = "transparent") +
  theme_minimal() +
  labs(title = "Evolución del índice xxxx",
       caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")

ggplot(tabla, aes(x = año, y = indice, colour = sexo, group = sexo)) + 
  geom_point() +
  geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.2, linetype = 3, col = "transparent") +
  theme_minimal() +
  labs(title = "Evolución del índice xxxx",
       caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")

# Por region

tabla = datos3 %>% 
  group_by(año, region) %>% 
  summarise(indice = mean(indice_acp, na.rm = T),
            inf = indice - (1.96 * sqrt((indice * (1 - indice)) / n())),
            sup = indice + (1.96 * sqrt((indice * (1 - indice)) / n())))
tabla


ggplot(tabla, aes(x = año, y = indice, colour = region, group = region)) + 
  geom_line() +
  geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.2, linetype = 3, col = "transparent") +
  theme_minimal() +
  labs(title = "Evolución del índice xxxx",
       caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")

# Por nivel educativo

tabla = datos3 %>% 
  group_by(año, educacion) %>% 
  summarise(indice = mean(indice_acp, na.rm = T),
            inf = indice - (1.96 * sqrt((indice * (1 - indice)) / n())),
            sup = indice + (1.96 * sqrt((indice * (1 - indice)) / n()))) %>% 
  arrange(educacion)
tabla

ggplot(tabla, aes(x = año, y = indice, colour = educacion, group = educacion)) + 
  geom_line() +
  geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.2, linetype = 3, col = "transparent") +
  theme_minimal() +
  labs(title = "Evolución del índice xxxx",
       caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")


######## eb
tabla_eb = datos3 %>% 
  filter(cuentaprop==0) %>% 
  group_by(año) %>% 
  summarise(indice = mean(indice_acp, na.rm = T),
            inf = indice - (1.96 * sqrt((indice * (1 - indice)) / n())),
            sup = indice + (1.96 * sqrt((indice * (1 - indice)) / n())))

tabla_eb
ggplot(tabla_eb, aes(x = año, y = indice)) + 
  geom_line() +
  theme_minimal() +
  labs(title = "Evolución del índice xxxx",
       caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")

ggplot(tabla, aes(x = año, y = indice)) + 
  geom_point() +geom_smooth(method = "lm")+
  theme_minimal() +
  labs(title = "Evolución del índice xxxx",
       caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")

