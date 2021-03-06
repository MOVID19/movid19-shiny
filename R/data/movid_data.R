message("R/data/process_data.R")

# paquetes ----------------------------------------------------------------
library(dplyr)
library(stringr)

# lectura de datos --------------------------------------------------------
# Script que procesa movid18.csv
mv <- data.table::fread("data/movid19.csv", encoding = "UTF-8")

mv <- as_tibble(mv)

# mv %>% 
#   filter(fecha_obs %>% as.Date() >= lubridate::ymd(20200803)) %>% 
#   {
#     table(.$fecha_obs %>% as.Date(), .$semana)
#   }
#   
# table(mv$fecha_obs %>% as.Date(), mv$semana)


# mv$r5_educ

# glimpse(mv)

# renombrando -------------------------------------------------------------
# Movid: Recodificaciones generales (Monica)
mv <- mv %>% 
  rename(
    fecha = fecha_obs,
    sexo = r2_sexo,
    # firstName = r1_nombre
    region = u1_region,
    comuna = u2_comuna,
    # calle = u3_calle,
    educ = r5_educ,
    tra_salud = pr1_wrk_salud,
    prev = pr2_prevision,
    # pob_id = X.U.FEFF.X.U.FEFF.pob_id
  )

# recodificacion ----------------------------------------------------------
mv <- mv %>% 
  mutate(
    fecha_ymd = as.Date(fecha)
  )

mv$fecha_ymd <- as.Date(mv$fecha)

mv$tra_salud_dic <- ifelse(mv$tra_salud=="Sí", 1, 
                           ifelse(mv$tra_salud=="No", 0, NA))

mv$sexo_trasalud <- ifelse(mv$sexo=="Femenino" & mv$tra_salud=="Sí", "Mujer trabajadora de salud",
                           ifelse(mv$sexo=="Femenino" & mv$tra_salud=="No", "Mujer no trabajadora de salud",  
                                  ifelse(mv$sexo=="Masculino" & mv$tra_salud=="Sí", "Hombre trabajador de salud",
                                         ifelse(mv$sexo=="Masculino" & mv$tra_salud=="No", "Hombre no trabajador de salud", NA))))


mv$educ_4cat <- ifelse(mv$educ=="Sin estudios" | mv$educ=="Educación Básica (primaria o preparatoria)", "Basica o sin estudios",
                       ifelse(mv$educ == "Educación Media (Humanidades)", "Media",
                              ifelse(mv$educ == "Educación Profesional (Carreras de 4 o más años)", "Profesional",
                                     ifelse(mv$educ == "Educación Técnica Nivel Superior (Carreras de 1 a 3 años)", "Tecnica", NA))))
mv$educ_3cat <- ifelse(mv$educ=="Sin estudios" | mv$educ=="Educación Básica (primaria o preparatoria)" | mv$educ == "Educación Media (Humanidades)", "Media o menos",
                       ifelse(mv$educ == "Educación Profesional (Carreras de 4 o más años)", "Profesional",
                              ifelse(mv$educ == "Educación Técnica Nivel Superior (Carreras de 1 a 3 años)", "Técnica", NA)))

mv$educ_2cat <- ifelse(mv$educ=="Sin estudios" | mv$educ=="Educación Básica (primaria o preparatoria)" | mv$educ == "Educación Media (Humanidades)", "Media o menos",
                       ifelse(mv$educ == "Educación Profesional (Carreras de 4 o más años)" | mv$educ == "Educación Técnica Nivel Superior (Carreras de 1 a 3 años)", "Más que media", NA))


# mv$tertil_ingre_c <- ifelse(mv$tertil_ingre==1, "Ingresos bajos", 
#                             ifelse(mv$tertil_ingre==2, "Ingresos medios",
#                                    ifelse(mv$tertil_ingre==3, "Ingresos altos", NA)))

mv$sexo_edad <- ifelse(mv$sexo=="Masculino" & mv$edad<65, "Hombre menor a 65",
                       ifelse(mv$sexo=="Masculino" & mv$edad>64, "Hombre mayor a 65",
                              ifelse(mv$sexo=="Femenino" & mv$edad<65, "Mujer menor a 65",
                                     ifelse(mv$sexo=="Femenino" & mv$edad>64, "Mujer mayor a 65", NA))))

mv$dic_trabajo <- ifelse(mv$p1_pra_trabajo==0, 0,
                         ifelse(mv$p1_pra_trabajo>0, 1, NA))

mv$dic_tramite <- ifelse(mv$p1_pra_tramite==0, 0,
                         ifelse(mv$p1_pra_tramite>0, 1, NA))

mv$dic_visita <- ifelse(mv$p1_pra_visita==0, 0,
                        ifelse(mv$p1_pra_visita>0, 1, NA))

mv$dic_recrea <- ifelse(mv$p1_pra_recrea==0, 0,
                        ifelse(mv$p1_pra_recrea>0, 1, NA))

mv$dic_transporte <- ifelse(mv$p1_pra_transporte==0, 0,
                            ifelse(mv$p1_pra_transporte>0, 1, NA))

mv$dic_invitado <- ifelse(mv$p1_pra_invitado==0, 0,
                          ifelse(mv$p1_pra_invitado>0, 1, NA))

mv$dic_otro <- ifelse(mv$p1_pra_otro==0, 0,
                      ifelse(mv$p1_pra_otro>0, 1, NA))

mv$dic_practicas <- ifelse((mv$dic_trabajo==0 & mv$dic_tramite==0 & mv$dic_invitado==0 &
                              mv$dic_recrea==0 & mv$dic_transporte==0 & mv$dic_visita==0), 0,
                           ifelse((mv$dic_trabajo>0 | mv$dic_tramite>0 | mv$dic_invitado>0 |
                                     mv$dic_recrea>0 | mv$dic_transporte>0 | mv$dic_visita>0), 1, NA))

mv$n_salidas <- (mv$p1_pra_trabajo+mv$p1_pra_recrea+mv$p1_pra_tramite+mv$p1_pra_transporte)


mv$sintoma <- ifelse((mv$s1_snt_fiebre==1 | mv$s1_snt_anosmia==1 | mv$s1_snt_disnea==1 | mv$s1_snt_tos==1 |
                        mv$s1_snt_mialgias==1 | mv$s1_snt_odinofagia==1 | mv$s1_snt_dol_torax==1 |
                        mv$s1_snt_cefalea==1 | mv$s1_snt_diarrea==1 | mv$s1_snt_disgeusia==1), 1,
                     ifelse((mv$s1_snt_fiebre==0 & mv$s1_snt_anosmia==0 & mv$s1_snt_disnea==0 & mv$s1_snt_tos==0 &
                               mv$s1_snt_mialgias==0 & mv$s1_snt_odinofagia==0 & mv$s1_snt_dol_torax==0 &
                               mv$s1_snt_cefalea==0 & mv$s1_snt_diarrea==0 & mv$s1_snt_disgeusia==0), 0, NA))
mv$sintoma <- ifelse(mv$s1_snt_null==1, 0, mv$sintoma)

mv$edad_3cat <- ifelse(mv$edad<40, "18 a 39",
                       ifelse(mv$edad<65 & mv$edad>39, "40 a 64",
                              ifelse(mv$edad>64, "65 y más", NA)))

mv$semana <- ifelse(mv$semana==15, 16, mv$semana)

# Actividad mezclando actividad de semana referencia y normalmente en el pasado
mv <- mv %>% 
  mutate(actividad = case_when(
    str_detect(pr4_wrk_lastw, "No realicé ") & educ_2cat == "Media o menos" & 
      str_detect(pr6_ocup_normal, "cuenta propia") ~ "Cuenta propia baja",
    str_detect(pr4_wrk_lastw, "No realicé ") & educ_2cat == "Más que media" & 
      str_detect(pr6_ocup_normal, "cuenta propia") ~ "Cuenta propia alta",
    educ_2cat == "Media o menos" & 
      str_detect(pr4_wrk_lastw, "cuenta propia") ~ "Cuenta propia baja",
    educ_2cat == "Más que media" & 
      str_detect(pr4_wrk_lastw, "cuenta propia") ~ "Cuenta propia alta",
    is.na(educ_2cat) & str_detect(pr4_wrk_lastw, "cuenta propia") ~ NA_character_,
    str_detect(pr4_wrk_lastw, "doméstico") | 
      str_detect(pr6_ocup_normal, "doméstico") ~ "Casa particular",
    str_detect(pr4_wrk_lastw, "público") | 
      str_detect(pr6_ocup_normal, "público") ~ "Asalariado/a público",
    str_detect(pr4_wrk_lastw, "privada") | 
      str_detect(pr6_ocup_normal, "privada") ~ "Asalariado/a privado",
    str_detect(pr4_wrk_lastw, "propia empresa") | 
      str_detect(pr6_ocup_normal, "propia empresa") ~ "Empleador",
    !is.na(pr4_wrk_lastw) ~ pr4_wrk_lastw,
    !is.na(pr6_ocup_normal) ~ pr6_ocup_normal,
    TRUE ~ pr6_ocup_normal),
    actividad2 = case_when(
      actividad == "Asalariado/a privado" & 
        educ_2cat == "Media o menos" ~ "Privado baja",
      actividad == "Asalariado/a privado" & 
        educ_2cat == "Más que media" ~ "Privado alta",
      actividad == "Asalariado/a privado" ~ NA_character_,
      TRUE ~ actividad),
    actividad3 = case_when(
      pr3_ocupacion == "Desempleado o desempleada" ~ "Desempleado",
      TRUE ~ actividad
    )
  )


# 20200805 ----------------------------------------------------------------
# miercoles de la semana anterior

# mv$fecha_ultima_obs
# table( mv$fecha_ultima_obs %>% as.Date(), mv$semana )
# 
# lubridate::wday(Sys.Date(), week_start = 1)

# mv <- mv %>% 
#   mutate(
#     semana_fecha_miercoles = as.Date(paste(2020, semana - 1, 3, sep="-"), "%Y-%U-%u"),
#     semana_fecha_miercoles = if_else(
#       lubridate::wday(as.Date(fecha), week_start = 1) > 3,
#       semana_fecha_miercoles + lubridate::days(7),
#       semana_fecha_miercoles
#       )
#   ) %>% 
#   rename(semana_fecha = semana_fecha_miercoles)

mv <- mv %>%
  mutate(
    semana_fecha = as.Date(paste(2020, semana - 1, 3, sep="-"), "%Y-%U-%u"),
    fecha_date = as.Date(fecha)
    )

# 20200827 ----------------------------------------------------------------
# considerar semana completas
semanas_incompletas <- mv %>% 
  count(semana, fecha_date) %>%
  group_by(semana) %>% 
  mutate(dias = n()) %>%
  filter(dias < 7) %>% 
  distinct(semana)

mv <- mv %>% 
  anti_join(semanas_incompletas, by = "semana")




# mv %>% 
#   count(as.Date(fecha)) %>% 
#   tail(10)

# 20200810 ----------------------------------------------------------------
mv <- mv %>% 
  mutate(
    caso_probable2 =  contacto == 1 & sosp_minsal0530 == 1
  )

# 20200811 ----------------------------------------------------------------
# variable auxiliar para el selector dedesagregación
mv <- mv %>% 
  mutate(
    todo = "Total"
  )

# 20200814 ----------------------------------------------------------------
mv <- mv %>% 
  mutate(
    prev = ifelse(stringr::str_detect(prev, "Otra"), "Otra", prev)
  )


# percepcion de legitmidad ------------------------------------------------

mv$soc1_bienestar <- car::recode(mv$soc1_bienestar, c("1='Muy de acuerdo';2='De acuerdo';3='Ni de acuerdo ni en desacuerdo';4= 'En desacuerdo'; 5='Muy en desacuerdo'"), as.factor = T,
                                 levels = c('Muy de acuerdo','De acuerdo', 'Ni de acuerdo ni en desacuerdo', 'En desacuerdo', 'Muy en desacuerdo'))

mv$soc2_obedecer <- car::recode(mv$soc2_obedecer, c("1='Muy de acuerdo';2='De acuerdo';3='Ni de acuerdo ni en desacuerdo';4= 'En desacuerdo'; 5='Muy en desacuerdo'"), as.factor = T,
                                 levels = c('Muy de acuerdo','De acuerdo', 'Ni de acuerdo ni en desacuerdo', 'En desacuerdo', 'Muy en desacuerdo'))
mv$soc3_desigualdad <- car::recode(mv$soc3_desigualdad, c("1='Muy de acuerdo';2='De acuerdo';3='Ni de acuerdo ni en desacuerdo';4= 'En desacuerdo'; 5='Muy en desacuerdo'"), as.factor = T,
                                 levels = c('Muy de acuerdo','De acuerdo', 'Ni de acuerdo ni en desacuerdo', 'En desacuerdo', 'Muy en desacuerdo'))
mv$soc4_represion <- car::recode(mv$soc4_represion, c("1='Muy de acuerdo';2='De acuerdo';3='Ni de acuerdo ni en desacuerdo';4= 'En desacuerdo'; 5='Muy en desacuerdo'"), as.factor = T,
                                 levels = c('Muy de acuerdo','De acuerdo', 'Ni de acuerdo ni en desacuerdo', 'En desacuerdo', 'Muy en desacuerdo'))

mv %>% 
  select(pob_id, semana_fecha, soc1_bienestar)

mv %>% 
  count(soc1_bienestar)


dmensual <- mv %>% 
  distinct(anio = lubridate::year(semana_fecha), mes = lubridate::month(semana_fecha), semana_fecha) %>% 
  arrange(anio, mes, semana_fecha) %>% 
  group_by(anio, mes) %>% 
  filter(semana_fecha  == max(semana_fecha)) %>% 
  ungroup()
  
dsoc <- mv %>% 
  group_by(pob_id, anio = lubridate::year(semana_fecha), mes = lubridate::month(semana_fecha)) %>% 
  summarise_at(
    vars(soc1_bienestar, soc2_obedecer, soc3_desigualdad, soc4_represion),
   ~last(na.omit(.x))
  )

dsoc <- left_join(dsoc, dmensual) %>%
  ungroup() %>% 
  select(-anio, -mes) %>% 
  filter(semana_fecha >= lubridate::ymd(20200901)) 

dsoc <- dsoc %>% 
  arrange(pob_id, semana_fecha)

# dsoc %>% 
#   # group_by(pob_id) %>% 
#   filter(semana_fecha >= lubridate::ymd(20201001)) %>% 
#   filter(!is.na(soc1_bienestar))
# 
# dsoc %>% 
#   group_by(pob_id) %>%
#   filter(length(na.omit(soc1_bienestar)) >= 2)

#   filter(pob_id == "0000daa20a9d6b34502b0e9c80318cd1a9d8fe456b3ef5c9e3c7d18121b65c6b") %>% 
#   View()
#                   
# 
# mv %>% 
#   filter(pob_id == "0000daa20a9d6b34502b0e9c80318cd1a9d8fe456b3ef5c9e3c7d18121b65c6b") %>% 
#   select(fecha, semana_fecha, starts_with("soc")) %>% 
#   View()

# -------------------------------------------------------------------------
# mv <- mv %>%
#   select(
#     -contains("TEXT"),
#     -starts_with("sa3"),
#     -starts_with("sa2"),
#     -starts_with("c7"),
#     -starts_with("c6"),
#     -starts_with("pr"),
#     -edad_65,
#     -RM,
#     -comuna,
#     -semana,
#     -semana0,
#     -region
#     )


# exportar ----------------------------------------------------------------
saveRDS(mv, "data/movid.rds")
saveRDS(dsoc, "data/dsoc.rds")

