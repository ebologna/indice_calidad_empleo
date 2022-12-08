rm(list=ls())
library(eph)
library(dplyr)
library(caret)
datos <- get_microdata(year = 2005:2021, 
                    trimester = 2,
                    type = 'individual',
                    vars = c('ANO4', 'CH04', 'ESTADO','CAT_OCUP','PP07G1','PP07G2',
                             'PP07G3','PP07G4','PP07H','PP08D1','P21','PP3E_TOT',
                             'PP03C','PP03D','REGION','NIVEL_ED','CH06','PP04D_COD'))
datos <- rename(datos, año = ANO4)
datos$sexo = ifelse(datos$CH04==1, "Varón", "Mujer")
# generomos las variables necesarias para la construcción del índice
datos$activo = ifelse(datos$ESTADO==1 | datos$ESTADO==2, 1, 0)
datos$desocupado = ifelse(datos$activo & datos$ESTADO==2, 1, 0)
datos$desocupado = ifelse(datos$activo==0, NA, datos$desocupado)
# para los cuentapropistas, vamos a considerar sólo los que tienen calificación
# Operativa o No calificada, según el 5to dígito del CNO.
datos$calificacion = substr(datos$PP04D_COD, 5, 5)
datos$cuentapropista = ifelse(datos$CAT_OCUP==2 & (datos$calificacion=="3" | datos$calificacion==4), 1, 0)
datos$cuentapropista = ifelse(datos$ESTADO==1, datos$cuentapropista, NA)
datos$vacaciones = ifelse(datos$PP07G1==1, 1, 0)
datos$aguinaldo = ifelse(datos$PP07G2==1, 1, 0)
datos$obra_social = ifelse(datos$PP07G4==1, 1, 0)
datos$dias_enferm = ifelse(datos$PP07G3==1, 1, 0)
datos$jubilacion = ifelse(datos$PP07H==1, 1, 0)
attach(datos)
datos$seguridad = (vacaciones + aguinaldo + obra_social + dias_enferm + jubilacion)/5
datos$seguridad = ifelse(datos$PP08D1 > 0, datos$seguridad, NA)
datos$ingreso_hora = ifelse(datos$P21<=0 | datos$PP3E_TOT==0, NA, datos$P21/(((as.numeric(strftime(paste0(datos[1,]$año,"-12-31"), format = "%V")))/12)*
                                                           datos$PP3E_TOT))
# No tiene sentido sumar el ingreso monetario bruto, si ya sumamos el ingreso por hora
# lo reemplacemos por la cantidad de ocupaciones
datos$cantidad_ocupaciones = ifelse(datos$PP03C==1, 1, datos$PP03D)
datos$cantidad_ocupaciones = ifelse(datos$cantidad_ocupaciones==0, NA, datos$cantidad_ocupaciones)
datos$PP3E_TOT = ifelse(datos$PP3E_TOT==0 | datos$PP3E_TOT > 100, NA, datos$PP3E_TOT)

# tabla = datos %>% 
#   group_by(año, sexo) %>% 
#   summarise(tasa_actividad = mean(activo, na.rm = T),
#             tasa_desempleo = mean(desocupado, na.rm = T),
#             prop_cuentapropistas = mean(cuentapropista, na.rm = T),
#             seguridad = mean(seguridad, na.rm = T),
#             horas_trabajadas = mean(PP3E_TOT, na.rm = T),
#             ingreso_hs = mean(ingreso_hora, na.rm = T),
#             percentile_90 = mean(quantile(ingreso_hora, probs = c(0.9), na.rm = T), na.rm = T) /
#               mean(quantile(ingreso_hora, probs = c(0.1), na.rm = T), na.rm = T),
#             ocupaciones = mean(cantidad_ocupaciones, na.rm = T))
# tabla
# 
# esc <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
# tabla_esc = datos %>% 
#   group_by(año, sexo) %>% 
#   summarise(tasa_actividad = mean(esc(activo), na.rm = T),
#             tasa_desempleo = mean(esc(desocupado), na.rm = T),
#             prop_cuentapropistas = mean(esc(cuentapropista), na.rm = T),
#             seguridad = mean(esc(seguridad), na.rm = T),
#             horas_trabajadas = mean(esc(PP3E_TOT), na.rm = T),
#             ingreso_hs = mean(esc(ingreso_hora), na.rm = T),
#             percentile_90 = mean(quantile(esc(ingreso_hora), probs = c(0.9), na.rm = T), na.rm = T) /
#               mean(quantile(esc(ingreso_hora), probs = c(0.1), na.rm = T), na.rm = T) / 100,
#             ocupaciones = mean(esc(cantidad_ocupaciones), na.rm = T))
# 
# attach(tabla_esc)
# tabla_esc$index = (seguridad + (1 - horas_trabajadas) + ingreso_hs + (1 - ocupaciones) +
#                   tasa_actividad + (1 - tasa_desempleo) + (1 - prop_cuentapropistas) + (1 - percentile_90)) / 8
# tabla_esc
# 
# # Interesante cómo se nota el foco de la política social en las mujeres durante la pandemia
# library(ggplot2)
# ggplot(tabla_esc, aes(x = año, y = index, colour = sexo, group = sexo)) + 
#   geom_line() +
#   geom_point() +
#   theme_minimal() +
#   labs(title = "Evolución del índice xxxx",
#        caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2011-2021")

#############
#############
# Otro approach más interesante para calcular intervalos.
# El índice a nivel individual.
# Esto ayudaría a PCA para ponderar, no lo logro hacer aún
#############
#############
esc <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
datos2 = datos %>%
  group_by(año) %>% 
  mutate(seguridad = esc(seguridad),
         horas_trabajadas = esc(PP3E_TOT),
         ingreso_hs = esc(ingreso_hora),
         ocupaciones = esc(cantidad_ocupaciones)) %>% 
  group_by(sexo) %>% 
  mutate(tasa_actividad = mean(activo, na.rm = T),
         tasa_desempleo = mean(desocupado, na.rm = T),
         prop_cuentapropistas = mean(cuentapropista, na.rm = T),
         percentile_90 = mean(quantile(esc(ingreso_hora), probs = c(0.9), na.rm = T), na.rm = T) /
           mean(quantile(esc(ingreso_hora), probs = c(0.1), na.rm = T), na.rm = T) / 100,)

# Esto no me gusta. Deberíamos ver cómo obtener estas ponderaciones de otra manera... pca, varimax?
attach(datos2)
datos2$index = (seguridad + (1 - horas_trabajadas) + ingreso_hs + (1 - ocupaciones) +
  tasa_actividad + (1 - tasa_desempleo) + (1 - prop_cuentapropistas) + (1 - percentile_90)) / 8

summary(datos2$index)

# rara esa bimodal...
hist(datos2$index)

# por año no es
datos2 %>% 
  group_by(año) %>% 
  summarise(indice = median(index, na.rm = T))

# por sexo no es
datos2 %>% 
  group_by(sexo) %>% 
  summarise(indice = median(index, na.rm = T))

# por la edad no parece ser, si bien hay problemas en los extremos (evaluar acotar rango etario a 18-70)
plot(datos2 %>% 
  group_by(CH06) %>% 
  summarise(indice = median(index, na.rm = T)))

# por region no es
datos2 %>% 
  group_by(REGION) %>% 
  summarise(indice = mean(index, na.rm = T))

# por cuentapropista puede ser
datos2 %>% 
  group_by(cuentapropista) %>% 
  summarise(indice = median(index, na.rm = T))

# acá está... es por la formalidad!
datos2 %>% 
  group_by(aguinaldo) %>% 
  summarise(indice = median(index, na.rm = T))

# Calculamos la tabla por año y por sexo, con intervalos de confianza ¿para la proporción?
tabla = datos2 %>% 
  group_by(año, sexo) %>% 
  summarise(indice = mean(index, na.rm = T),
            inf = indice - (1.96 * sqrt((indice * (1 - indice)) / n())),
            sup = indice + (1.96 * sqrt((indice * (1 - indice)) / n())))
tabla

# Grafiquito
ggplot(tabla, aes(x = año, y = indice, colour = sexo, group = sexo)) + 
  geom_line() +
  geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.2, linetype = 3, col = "transparent") +
  theme_minimal() +
  labs(title = "Evolución del índice xxxx",
       caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")

# A ver por región?
datos2 = datos %>%
  group_by(año) %>% 
  mutate(seguridad = esc(seguridad),
         horas_trabajadas = esc(PP3E_TOT),
         ingreso_hs = esc(ingreso_hora),
         ocupaciones = esc(cantidad_ocupaciones)) %>% 
  group_by(REGION) %>% 
  mutate(tasa_actividad = mean(activo, na.rm = T),
         tasa_desempleo = mean(desocupado, na.rm = T),
         prop_cuentapropistas = mean(cuentapropista, na.rm = T),
         percentile_90 = mean(quantile(esc(ingreso_hora), probs = c(0.9), na.rm = T), na.rm = T) /
           mean(quantile(esc(ingreso_hora), probs = c(0.1), na.rm = T), na.rm = T) / 100,)

attach(datos2)
datos2$index = (seguridad + (1 - horas_trabajadas) + ingreso_hs + (1 - ocupaciones) +
                  tasa_actividad + (1 - tasa_desempleo) + (1 - prop_cuentapropistas) + (1 - percentile_90)) / 8


summary(datos2$index)
table(is.na(datos2$index))

# sigue rara esa bimodal...
hist(datos2$index)

tabla = datos2 %>% 
  group_by(año, REGION) %>% 
  summarise(indice = mean(index, na.rm = T),
            inf = indice - (1.96 * sqrt((indice * (1 - indice)) / n())),
            sup = indice + (1.96 * sqrt((indice * (1 - indice)) / n())))

tabla$REGION <- case_when(tabla$REGION == 1  ~ "GBA",
                          tabla$REGION == 40 ~ "NOA",
                          tabla$REGION == 41 ~ "NEA",
                          tabla$REGION == 42 ~ "Cuyo",
                          tabla$REGION == 43 ~ "Pampeana",
                          tabla$REGION == 44 ~ "Patagonia")
tabla



ggplot(tabla, aes(x = año, y = indice, colour = REGION, group = REGION)) + 
  geom_line() +
  geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.2, linetype = 3, col = "transparent") +
  theme_minimal() +
  labs(title = "Evolución del índice xxxx",
       caption = "Elaboración propia en base a la Encuesta Permanente de Hogares, INDEC 2005-2021")

