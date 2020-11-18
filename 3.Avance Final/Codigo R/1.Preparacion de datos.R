

library(tidyverse)

base <- read.csv("train.csv")


#Preparacion de datos

#Seleccionando solo los casos del cabeza de hogar pues posee la informacion completa
hogar <- base %>% 
  filter(parentesco1==1)


#Recalcular la variable objetivo de 4 a 2 categorias (pobre y no pobre)
hogar <- hogar %>% 
  #filter(Target!=3) %>% 
  mutate(Target = if_else(Target>=3,0,1))


#Prefiltrado de variables por criterio manual. 

nombres <- c("pobre","renta","banno","refri","tablet", "homb_men_12",
             "homb_may_12","muj_men_12","muj_may_12","tot_men_12","tot_may_12",
             "escolari","paredblock","paredzocalo","paredpreb","pisoceram",
             "pisocemento","cielorazo","sani_alcant","sani_tanque","cocina_elec",
             "cocina_gas","pared_buen","techo_buen","piso_buen","sexo",
             "union_lib","casadx","divorc","separadx","viudx","solterx","ninnos",
             "adult_may","tam_hogar","dependencia","educ_adult","sin_educ",
             "primaria_inc","primaria_comp","secund_inco","secund_comp",
             "tecnico_inc","tecnico_comp","grado","posgrado","cuartos",
             "hacinamiento","casa_propia","casa_prest","alquiler","PC","TV",
             "cant_telef","Central","Chorotega","Pacifico", "Brunca","Huetar_Atl",
             "Huetar_Nort","urbano","edad")

hogar <- hogar %>%
  select(
    c(
      Target,
      v2a1,
      v14a,
      refrig,
      v18q,
      r4h1,
      r4h2,
      r4m1,
      r4m2,
      r4t1,
      r4t2,
      escolari,
      paredblolad,
      paredzocalo,
      paredpreb,
      pisomoscer,
      pisocemento,
      cielorazo,
      sanitario2,
      sanitario3,
      energcocinar2,
      energcocinar3,
      epared3,
      etecho3,
      eviv3,
      female,
      estadocivil2,
      estadocivil3,
      estadocivil4,
      estadocivil5,
      estadocivil6,
      estadocivil7,
      hogar_nin,
      hogar_mayor,
      hogar_total,
      dependency,
      meaneduc,
      instlevel1,
      instlevel2,
      instlevel3,
      instlevel4,
      instlevel5,
      instlevel6,
      instlevel7,
      instlevel8,
      instlevel9,
      bedrooms,
      overcrowding,
      tipovivi1,
      tipovivi2,
      tipovivi3,
      computer,
      television,
      qmobilephone,
      lugar1,
      lugar2,
      lugar3,
      lugar4,
      lugar5,
      lugar6,
      area1,
      age
    )
  ) %>%
  `colnames<-`(nombres) %>%
  mutate(
    dependencia = case_when(
      dependencia == 'yes' ~ 1,
      dependencia == 'no' ~ 0,
      TRUE ~ as.numeric(dependencia)
    ),
    renta = if_else(is.na(renta), 0, renta / 1000)
  ) %>%
  filter(!((alquiler == 1 | casa_prest == 1) & renta == 0))

#Eliminando los casos con NAs
hogar <- hogar[complete.cases(hogar),]

#Recalculando nuevas variables
hogar <- hogar %>%
  mutate(
    adult_may = if_else(adult_may > 0, 1, 0),
    renta_individuo = renta / tam_hogar,
    renta_menores = if_else(
      is.nan(renta / tot_men_12) |
        is.infinite(renta / tot_men_12)
      ,
      0,
      (renta / tot_men_12)
    ),
    prop_men12 = tot_men_12 / tam_hogar,
    prop_homb = (homb_men_12 + homb_may_12) / tam_hogar,
    prop_mujer = (muj_men_12 + muj_may_12) / tam_hogar,
    men12_por_adult = tot_men_12 / tot_may_12,
    primaria = primaria_inc + primaria_comp,
    secundaria = secund_inco + secund_comp + tecnico_inc + tecnico_comp,
    universidad = grado + posgrado
  ) %>%
  select(c(
    pobre,
    ninnos, 
    cocina_elec,
    banno, 
    sin_educ,
    cocina_elec, 
    urbano,
    renta, 
    sani_alcant,
    casadx,
    dependencia,
    casa_propia
    
  ))
  # select(
  #   -c(
  #     primaria_comp,
  #     primaria_inc,
  #     secund_inco,
  #     secund_comp,
  #     tecnico_inc,
  #     tecnico_comp,
  #     grado,
  #     posgrado,
  #     homb_may_12,
  #     muj_may_12,
  #     muj_men_12,
  #     homb_men_12,
  #     Central,
  #     Chorotega,
  #     Pacifico,
  #     Brunca,
  #     Huetar_Atl,
  #     Huetar_Nort,
  #     paredzocalo,
  #     paredzocalo,
  #     paredpreb,
  #     escolari,
  #     sin_educ,
  #     primaria,
  #     secundaria,
  #     universidad,
  #     casa_prest,
  #     renta_menores,
  #     men12_por_adult,
  #     banno,
  #     pisocemento,
  #     pisoceram,
  #     sani_alcant,
  #     sani_tanque,
  #     tot_men_12,
  #     tot_may_12,
  #     solterx
  #   )
  # )

save(hogar,file="hogar.RData")


