

ur_ramos_pef2020apr_ppef2021 <- ur_ramos %>% 
  filter(ciclo != 2019) %>% 
  filter(filter_ramos_apf(id_ramo)) %>% 
  mutate(
    monto_comparativo = case_when(
      is.na(monto_proyecto) ~ monto_aprobado,
      T ~ monto_proyecto
    )
  ) %>% 
  select(ciclo, id_ramo, desc_ramo, id_ur, desc_ur, monto_comparativo) %>% 
  group_by(ciclo, id_ramo, desc_ramo, id_ur, desc_ur) %>% 
  summarise(monto_comparativo = sum(monto_comparativo, na.rm = T)) %>% 
  pivot_wider(
    names_from = ciclo, 
    values_from = monto_comparativo,
    names_glue = "monto_{ciclo}"
  ) %>% 
  mutate(
    monto_diferencia = (monto_2021 - monto_2020),
    monto_diferencia = case_when(
      is.na(monto_diferencia) & is.na(monto_2021) ~ monto_2020*-1,
      is.na(monto_diferencia) & is.na(monto_2020) ~ monto_2021,
      T ~ monto_diferencia
    ),
    prop_monto = round((monto_2021 - monto_2020)/monto_2020*100),
    prop_monto = case_when(
      is.na(monto_2021) & monto_2020*-1==monto_diferencia ~ -100,
      monto_2021==monto_diferencia ~ 100,
      T ~ prop_monto
    ),
    tipo = case_when(
      monto_diferencia<=0 ~ "Perdedor",
      monto_diferencia>0 ~ "Ganador",
      T ~ "check"
    ),
    tipo = factor(tipo, 
                  levels = c("Ganador", "Perdedor"),
                  labels = c("Ganador", "Perdedor")
    ),
    ramo_acronimo = ramos_to_acr(id_ramo),
    ramo_cat = ramos_to_cat(id_ramo),
    ramo_cat = factor(ramo_cat, 
                      levels = c("la APF Centralizada", "la APF Paraestatal", "los Ramos Generales"),
                      labels = c("la APF Centralizada", "la APF Paraestatal", "los Ramos Generales"))
  ) %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(round(./1000000))
  ) %>% 
  arrange(as.numeric(id_ramo)) %>% 
  ungroup()



id_ramo_loop <- unique(ur_ramos_pef2020apr_ppef2021$id_ramo)

fiuf <- "Variación del presupuesto para unidades responsables del gasto de "
fiuff <- "Monto aprobado en PEF2020 vs Monto de proyecto en PPEF 2021"


for(i in 1:length(id_ramo_loop)){
  a <- ur_ramos_pef2020apr_ppef2021 %>% 
    filter(id_ramo==id_ramo_loop[i]) 
  
  
  ggplot(
    a, 
    aes(
      x=monto_diferencia, 
      y=reorder(str_wrap(str_trunc(desc_ur, 80), 35), as.numeric(monto_diferencia)), 
      label=paste0(
        "$", prettyNum(monto_diferencia, big.mark = ","), "\n[ ",
        prop_monto,"% ]"
      ),
      fill = tipo
    )
  ) + 
    geom_col(width = 0.5) + 
    geom_text(hjust = "outward", size = 8 ) +
    scale_x_continuous(labels = scales::comma,
                       limits = c((max(abs(a$monto_diferencia))*-1)-10,
                                  max(abs(a$monto_diferencia))+10)) + 
    scale_fill_manual("", values = c(mcci_discrete[1],
                                     mcci_discrete[5])) +
    scale_color_manual("", values = c(mcci_discrete[1],
                                      mcci_discrete[5])) +
    labs(title= str_wrap(paste0(
      fiuf, str_to_title(unique(a$desc_ramo))
    ), width = 65),
    subtitle = fiuff,
    caption = paste0("Nota: no se tomaron en cuenta aquellos ramos cuyo monto absoluto de diferencia haya sido menor a 5 MDP\n",
                     fiuffi),
    x = "Millones de pesos (constantes 2021)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 50, face = "bold" , hjust = 0.5),
          plot.subtitle = element_text(size = 40,  hjust = 0.5),
          plot.caption = element_text(size = 35),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Arial Narrow"),
          axis.title.x = element_text(size = 30),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 27),
          axis.text.y = element_text(size = 25),
          legend.title = element_blank(),
          legend.position = "none")
  ggsave(filename = paste0(out, "01_APF/06_", unique(a$id_ramo), "_pef2020apr_ppef2021.png"),
         width = 30, height = 50, dpi = 100, limitsize = FALSE)
}


ur_ramos_pef2020mod_ppef2021 <- ur_ramos %>% 
  filter(ciclo != 2019) %>% 
  filter(filter_ramos_apf(id_ramo)) %>% 
  mutate(
    monto_comparativo = case_when(
      is.na(monto_proyecto) ~ monto_modificado,
      T ~ monto_proyecto
    )
  ) %>% 
  select(ciclo, id_ramo, desc_ramo, id_ur, desc_ur, monto_comparativo) %>% 
  group_by(ciclo, id_ramo, desc_ramo, id_ur, desc_ur) %>% 
  summarise(monto_comparativo = sum(monto_comparativo, na.rm = T)) %>% 
  pivot_wider(
    names_from = ciclo, 
    values_from = monto_comparativo,
    names_glue = "monto_{ciclo}"
  ) %>% 
  mutate(
    monto_diferencia = (monto_2021 - monto_2020),
    monto_diferencia = case_when(
      is.na(monto_diferencia) & is.na(monto_2021) ~ monto_2020*-1,
      is.na(monto_diferencia) & is.na(monto_2020) ~ monto_2021,
      T ~ monto_diferencia
    ),
    prop_monto = round((monto_2021 - monto_2020)/monto_2020*100),
    prop_monto = case_when(
      is.na(monto_2021) & monto_2020*-1==monto_diferencia ~ -100,
      monto_2021==monto_diferencia ~ 100,
      T ~ prop_monto
    ),
    tipo = case_when(
      monto_diferencia<=0 ~ "Perdedor",
      monto_diferencia>0 ~ "Ganador",
      T ~ "check"
    ),
    tipo = factor(tipo, 
                  levels = c("Ganador", "Perdedor"),
                  labels = c("Ganador", "Perdedor")
    ),
    ramo_acronimo = ramos_to_acr(id_ramo),
    ramo_cat = ramos_to_cat(id_ramo),
    ramo_cat = factor(ramo_cat, 
                      levels = c("la APF Centralizada", "la APF Paraestatal", "los Ramos Generales"),
                      labels = c("la APF Centralizada", "la APF Paraestatal", "los Ramos Generales"))
  ) %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(round(./1000000))
  ) %>% 
  arrange(as.numeric(id_ramo)) %>% 
  ungroup()
