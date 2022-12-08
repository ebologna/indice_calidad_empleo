rm(list=ls())
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
# generomos las variables necesarias para la construcción del índice
# para los cuentapropistas, vamos a considerar sólo los que tienen calificación
# Operativa o No calificada, según el 5to dígito del CNO.
datos$calificacion = substr(datos$PP04D_COD, 5, 5)
datos$sobrecalificado = ifelse(((datos$calificacion=="3" | datos$calificacion=="4") & datos$NIVEL_ED==6)|
                                 (datos$calificacion=="4" & (datos$NIVEL_ED==4 | datos$NIVEL_ED==5)), 1, 0)
datos$sobreocupado_demandante = ifelse(datos$PP03G==1 & datos$PP3E_TOT>35, 1, 0)
datos$subocupado_demandante = ifelse(datos$PP03G==1 & datos$PP3E_TOT<30, 1, 0)
datos$sobreocupado = ifelse(datos$PP3E_TOT > 40, 1, 0)
# variables para generar el índice de "seguridad" en el empleo (formalidad)
datos$vacaciones = ifelse(datos$PP07G1==1, 1, 0)
datos$aguinaldo = ifelse(datos$PP07G2==1, 1, 0)
datos$obra_social = ifelse(datos$PP07G4==1, 1, 0)
datos$dias_enferm = ifelse(datos$PP07G3==1, 1, 0)
datos$jubilacion = ifelse(datos$PP07H==1, 1, 0)
attach(datos)
datos$seguridad = (vacaciones + aguinaldo + obra_social + dias_enferm + jubilacion)/5
hist(datos$seguridad) # casi dicotómica, muchos con nada
prop.table(table(datos[año=="2021",]$seguridad)) # en 2021 casi la mitad no tienen ninguna

# # ingreso por hora A ESTO NO LO SIGO

datos$ingreso_hora = ifelse(datos$P21<=0 | datos$PP3E_TOT==0, NA,
                            datos$P21/(((as.numeric(strftime(
                              paste0(datos[1,]$año,"-12-31"),
                              format = "%V")))/12)*datos$PP3E_TOT))

summary(datos[año=="2021",]$ingreso_hora)


# # cantidad de ocupaciones
datos$cantidad_ocupaciones = ifelse(datos$PP03C==1, 1, datos$PP03D)
datos$cantidad_ocupaciones = ifelse(datos$cantidad_ocupaciones==0, NA, datos$cantidad_ocupaciones)
# horas trabajadas
datos$PP3E_TOT = ifelse(datos$PP3E_TOT==0 | datos$PP3E_TOT > 100, NA, datos$PP3E_TOT)
# decil de ingreso de la ocupación principal
datos$key = paste0(datos$año, "-", datos$REGION)
umbral = datos %>%
  filter(ingreso_hora>0) %>% 
  group_by(key) %>% 
  summarise(q = quantile(ingreso_hora, probs = seq(0,1,0.25), na.rm = TRUE)[[2]])
datos = left_join(datos, umbral)
datos$p_25 = ifelse(datos$ingreso_hora <= datos$q, 1, 0)
table(datos$p_25)

datos %>% 
  group_by(REGION) %>% 
  summarise(posicion_p25=prop.table(table(p25)))


summary(datos[año=="2021",]$cantidad_ocupaciones)
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
esc <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}

# agregamos las variables a nivel micro

# datos2 = datos %>%
#   group_by(año) %>% 
#   mutate(seguridad = esc(seguridad),
#          horas_trabajadas = esc(PP3E_TOT),
#          ingreso_hs = esc(ingreso_hora),
#          ocupaciones = esc(cantidad_ocupaciones),
#          cuentaprop = esc(cuentapropista),
#          decil = esc(decil),
#          publico = esc(publico))

names(datos)
# Contrucción de índice con PCA

indice = datos[,c("seguridad","sobrecalificado","subocupado_demandante",
                   "sobreocupado","p_25","año","region","sexo","educacion")]

##### Prueba
summary(indice)
indice = na.omit(indice)
summary(indice)
attach(indice)
indice$index = 1 - (((1-seguridad) + sobrecalificado + subocupado_demandante +
                       sobreocupado + p_25) / 4)
mean(indice$index, na.rm = T)
table(is.na(indice$index))

tabla = indice %>% 
  group_by(año, sexo) %>% 
  summarise(indice = mean(index, na.rm = T),
            inf = indice - (1.96 * sd(index) / sqrt(n())),
            sup = indice + (1.96 * sd(index) / sqrt(n())))
tabla


ggplot(tabla, aes(x = año, y = indice, colour = sexo, group = sexo)) + 
  geom_line() +
  geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.2, linetype = 3, col = "transparent") +
  theme_minimal() +
  labs(title = "Evolución del índice xxxx",
       caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")

#### nivel educativo

summary(indice)
indice = na.omit(indice)
summary(indice)
attach(indice)
indice$index = 1 - (((1-seguridad) + sobrecalificado + subocupado_demandante +
                       sobreocupado + p_25) / 4)
mean(indice$index, na.rm = T)
table(is.na(indice$index))
tabla = indice %>% 
  group_by(año, educacion) %>% 
  summarise(indice = mean(index, na.rm = T),
            inf = indice - (1.96 * sd(index) / sqrt(n())),
            sup = indice + (1.96 * sd(index) / sqrt(n())))
tabla


ggplot(tabla, aes(x = año, y = indice, colour = educacion, group = educacion)) + 
  geom_line() +
  geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.2, linetype = 3, col = "transparent") +
  theme_minimal() +
  labs(title = "Evolución del índice xxxx",
       caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")

###########################################################################################################
# 
# # escalo las variables
# pre_proceso <- preProcess(indice, method = c("center", "scale")) 
# indice <- predict(pre_proceso, indice) 
# summary(indice) 
# # elimino NA
# indice = na.omit(indice)
# # calculo las CP
# library(factoextra)
# set.seed (7) 
# res.pca <- prcomp(indice, scale = TRUE) 
# # extraigo los vectores propios
# eig.val <- get_eigenvalue(res.pca)
# eig.val <- round(eig.val, digits = 2)
# eig.val = subset(eig.val, eig.val$cumulative.variance.percent < 90)
# ncomp = dim(eig.val)[1]
# # reescalo los valores propios
# eig.val$varianza_reescalada = eig.val$variance.percent/eig.val[nrow(eig.val), c("cumulative.variance.percent")]
# # hago la rotación ortogonal
# loadings_rotada <- varimax(res.pca$rotation[,1:ncomp])
# matrix_varimax = as.matrix(loadings_rotada$loadings) 
# # hago el producto matricial entre la matriz rotada y vector de valores propios
# ponderador = matrix_varimax %*% eig.val$varianza_reescalada
# # Calculo el índice
# indice =  as.data.frame(as.matrix(indice) %*% ponderador)
# # esto lo hago porque me parece que los signos del PCA están al reves. Le da valores negativos a cosas con efecto "positivo".
# indice = esc(1-indice)
# summary(indice)
# # se distribuye bien el índice
# hist(indice$V1)
# # pego el índice a la base de datos original
# datos3 = datos2[,c("seguridad","horas_trabajadas","ingreso_hs","ocupaciones","cuentaprop",
#                    "decil","publico","año","sexo","region","educacion")]
# summary(datos3)
# datos3 = na.omit(datos3)
# datos3$indice_acp = indice$V1
# summary(datos3)
# 
# # Por sexo
# 
# tabla = datos3 %>% 
#   group_by(año, sexo) %>% 
#   summarise(indice = mean(indice_acp, na.rm = T),
#             inf = indice - (1.96 * sqrt((indice * (1 - indice)) / n())),
#             sup = indice + (1.96 * sqrt((indice * (1 - indice)) / n())))
# tabla
# 
# ggplot(tabla, aes(x = año, y = indice, colour = sexo, group = sexo)) + 
#   geom_line() +
#   geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.2, linetype = 3, col = "transparent") +
#   theme_minimal() +
#   labs(title = "Evolución del índice xxxx",
#        caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")
# 
# # Por region
# 
# tabla = datos3 %>% 
#   group_by(año, region) %>% 
#   summarise(indice = mean(indice_acp, na.rm = T),
#             inf = indice - (1.96 * sqrt((indice * (1 - indice)) / n())),
#             sup = indice + (1.96 * sqrt((indice * (1 - indice)) / n())))
# tabla
# 
# 
# ggplot(tabla, aes(x = año, y = indice, colour = region, group = region)) + 
#   geom_line() +
#   geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.2, linetype = 3, col = "transparent") +
#   theme_minimal() +
#   labs(title = "Evolución del índice xxxx",
#        caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")
# 
# # Por nivel educativo
# 
# tabla = datos3 %>% 
#   group_by(año, educacion) %>% 
#   summarise(indice = mean(indice_acp, na.rm = T),
#             inf = indice - (1.96 * sqrt((indice * (1 - indice)) / n())),
#             sup = indice + (1.96 * sqrt((indice * (1 - indice)) / n()))) %>% 
#   arrange(educacion)
# tabla
# 
# ggplot(tabla, aes(x = año, y = indice, colour = educacion, group = educacion)) + 
#   geom_line() +
#   geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.2, linetype = 3, col = "transparent") +
#   theme_minimal() +
#   labs(title = "Evolución del índice xxxx",
#        caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")
# 
# 
# 
