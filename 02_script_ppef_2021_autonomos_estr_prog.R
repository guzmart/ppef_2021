test <- estructura_programática %>% 
  select(id_ramo, desc_ramo, tipo_cambio, desc_pp_2020) %>% 
  pivot_wider(names_from = tipo_cambio,
              values_from = desc_pp_2020)

estructura_programática %>% 
  distinct(id_unica_2021, .keep_all = T) %>% 
  count(tipo_cambio)


estructura_programática %>% 
  mutate(id_unica_2021 = case_when(
    is.na(id_unica_2021) & tipo_cambio != "CAMBIO DE MODALIDAD" ~ id_unica_2020,
    T ~ id_unica_2021
  )) %>% 
  distinct(id_unica_2021, .keep_all = T) %>% 
  count(tipo_cambio) 

openxlsx::write.xlsx(
  estructura_programática %>% 
    mutate(id_unica_2021 = case_when(
      is.na(id_unica_2021) & tipo_cambio != "CAMBIO DE MODALIDAD" ~ id_unica_2020,
      T ~ id_unica_2021
    )) %>% 
    distinct(id_unica_2021, .keep_all = T) %>% 
    count(tipo_cambio), 
  paste0(out, "03_cambios_programaticos/00_tipo_cambio.xlsx")
)

openxlsx::write.xlsx(
  estructura_programática %>% 
    mutate(id_unica_2021 = case_when(
      is.na(id_unica_2021) & tipo_cambio != "CAMBIO DE MODALIDAD" ~ id_unica_2020,
      T ~ id_unica_2021
    )) %>% 
    distinct(id_unica_2021, .keep_all = T) %>% 
    count(desc_ramo) %>% 
    arrange(-n), 
  paste0(out, "03_cambios_programaticos/01_desc_ramo.xlsx")
)
