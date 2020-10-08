# Paquetes ----
require(foreign)
require(haven)
require(ggalt)
require(readxl)
require(ggthemes)
require(lubridate)
require(plotly)
require(RColorBrewer)
require(ggnewscale)
require(openxlsx)
require(tidyverse)

# Directorios ----
inp <- "Github/ppef_2021/01_datos/"
out <- "Github/ppef_2021/03_productos/"
fiuffi <- "Fuente: elaboración propia con datos de @TPresupuestaria | @guzmart_"

# Paleta de colores ----
mcci_discrete <- c(
  '#000c2d', '#0E9A9D', '#1EBB97', '#ecd756', '#f72732', '#F58C33'
)

# Desactivar notación científica
options(scipen=999)


# 1. RAMOS 2019-2021 ----
ramos <- readRDS(paste0(inp, "2019_cp_defl2021.rds")) %>% 
  select(
    ciclo, contains("ramo"), contains("monto")
  ) %>% 
  bind_rows(
    readRDS(paste0(inp, "2020_pef_defl2021.rds")) %>% 
      select(
        ciclo, contains("ramo"), contains("monto")
      )
  ) %>% 
  bind_rows(
    readRDS(paste0(inp, "2021_ppef_defl2021.rds")) %>% 
      select(
        ciclo, contains("ramo"), contains("monto")
      )
  )

ramos_2019_2021 <- ramos %>% 
  mutate(
    monto_comparativo = case_when(
      is.na(monto_proyecto) ~ monto_aprobado,
      T ~ monto_proyecto
    )
  ) %>% 
  select(ciclo, id_ramo, desc_ramo, monto_comparativo) %>% 
  group_by(ciclo, id_ramo, desc_ramo) %>% 
  summarise(monto_comparativo = sum(monto_comparativo, na.rm = T))  %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(round(./1000000))
  ) %>% 
  mutate(tipo = "Aprobado") %>% 
  bind_rows(
    ramos %>% 
      mutate(
        monto_comparativo = case_when(
          is.na(monto_proyecto) ~ monto_modificado,
          T ~ monto_proyecto
        )
      ) %>% 
      select(ciclo, id_ramo, desc_ramo, monto_comparativo) %>% 
      group_by(ciclo, id_ramo, desc_ramo) %>% 
      summarise(monto_comparativo = sum(monto_comparativo, na.rm = T))  %>% 
      mutate_at(
        vars(starts_with("monto")),
        funs(round(./1000000))
      ) %>% 
      mutate(tipo = "Modificado") 
  ) %>% 
  bind_rows(
    ramos %>% 
      filter(ciclo==2021) %>% 
      mutate(
        monto_comparativo = case_when(
          is.na(monto_proyecto) ~ monto_modificado,
          T ~ monto_proyecto
        )
      ) %>% 
      select(ciclo, id_ramo, desc_ramo, monto_comparativo) %>% 
      group_by(ciclo, id_ramo, desc_ramo) %>% 
      summarise(monto_comparativo = sum(monto_comparativo, na.rm = T))  %>% 
      mutate_at(
        vars(starts_with("monto")),
        funs(round(./1000000))
      ) %>% 
      mutate(tipo = "Proyecto") 
  ) %>% 
  mutate(
    etiqueta = case_when(
      ciclo < 2021 ~ paste0("$", prettyNum(monto_comparativo, big.mark = ",")),
      ciclo == 2021 & tipo == "Proyecto" ~ paste0("$", prettyNum(monto_comparativo, big.mark = ",")),
      T ~ ""
    )
  )

id_ramo_loop <- unique(ramos_2019_2021$id_ramo)

fiuf <- "Progresión del presupuesto público de "
fiuff <- "Montos aprobados y modificados en CP2019 y PEF2020T02 vs\nMonto de proyecto en PPEF 2021"

for(i in 1:length(id_ramo_loop)){
  a <- ramos_2019_2021 %>% 
    filter(id_ramo==id_ramo_loop[i])
  
  ggplot(
    a,
    aes(
      x = as.factor(ciclo),
      y = monto_comparativo,
      col = tipo,
      group = tipo,
      label = etiqueta
    )
  ) +
    geom_line(size = 4.5) + 
    geom_point(size = 6) +
    ggrepel::geom_label_repel(show.legend = F, na.rm = T, size = 10) +
    scale_color_manual("", values = c(
      mcci_discrete[1],
      mcci_discrete[2],
      mcci_discrete[5]
    )) +
    scale_y_continuous(
      limits = c(0, max(a$monto_comparativo)+abs(quantile(a$monto_comparativo, 0.01)-max(a$monto_comparativo))),
      labels = scales::comma
    ) +
    labs(title= str_wrap(paste0(
      fiuf, str_to_title(unique(a$desc_ramo))
    ), width = 65),
    subtitle = fiuff,
    caption = fiuffi,
    y = "Millones de pesos (constantes 2021)",
    x = "Ciclo presupuestario") +
    theme_minimal() +
    theme(plot.title = element_text(size = 50, face = "bold" , hjust = 0.5),
          plot.subtitle = element_text(size = 40,  hjust = 0.5),
          plot.caption = element_text(size = 35),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Arial Narrow"),
          axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 30),
          axis.text.x = element_text(size = 27),
          axis.text.y = element_text(size = 25),
          legend.title = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 27))
  
  
  ggsave(filename = paste0(out, "00_progresión_gasto/", unique(a$id_ramo), "_progresión_pef2020apr_ppef2021.png"),
         width = 20, height = 15, dpi = 100, limitsize = FALSE)
}


# 2. RAMOS 2008-2021 ----
cps_pef_ppef_2008_2021 <- c(
  "2008_cp_defl2021.rds",
  "2009_cp_defl2021.rds",
  "2010_cp_defl2021.rds",
  "2011_cp_defl2021.rds",
  "2012_cp_defl2021.rds",
  "2013_cp_defl2021.rds",
  "2014_cp_defl2021.rds",
  "2015_cp_defl2021.rds",
  "2016_cp_defl2021.rds",
  "2017_cp_defl2021.rds",
  "2018_cp_defl2021.rds",
  "2019_cp_defl2021.rds",
  "2020_pef_defl2021.rds",
  "2021_ppef_defl2021.rds"
)

ramos <- data.frame()

for(i in 1:length(cps_pef_ppef_2008_2021)){
  tempo <- readRDS(paste0(inp, cps_pef_ppef_2008_2021[i])) %>% 
    select(
      ciclo, contains("ramo"), contains("monto")
    )
  
  ramos <- bind_rows(ramos, tempo)
  
}

ramos <- ramos %>% 
  mutate(
    id_ramo = str_replace_all(id_ramo, "TOQ|TZZ", "18"),
    id_ramo = str_replace_all(id_ramo, "GYR", "50"),
    id_ramo = str_replace_all(id_ramo, "GYN", "51"),
    id_ramo = str_replace_all(id_ramo, "17", "49"),
    desc_ramo = case_when(
      id_ramo == "49" ~ "Fiscalía General de la República (antes PGR)",
      T ~ desc_ramo
    )
  )


ramos_2008_2021 <- ramos %>% 
  mutate(
    monto_comparativo = case_when(
      is.na(monto_proyecto) ~ monto_aprobado,
      T ~ monto_proyecto
    )
  ) %>% 
  select(ciclo, id_ramo, desc_ramo, monto_comparativo) %>% 
  group_by(ciclo, id_ramo, desc_ramo) %>% 
  summarise(monto_comparativo = sum(monto_comparativo, na.rm = T))  %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(round(./1000000))
  ) %>% 
  mutate(tipo = "Aprobado") %>% 
  bind_rows(
    ramos %>% 
      mutate(
        monto_comparativo = case_when(
          ciclo == 2008 ~ monto_ejercido,
          ciclo == 2009 ~ monto_ejercido,
          ciclo == 2010 ~ monto_ejercido,
          ciclo == 2011 ~ monto_ejercido,
          ciclo == 2012 ~ monto_ejercido,
          ciclo == 2013 ~ monto_ejercicio,
          ciclo == 2014 ~ monto_ejercicio,
          ciclo == 2015 ~ monto_ejercicio,
          ciclo == 2016 ~ monto_ejercicio,
          ciclo == 2017 ~ monto_ejercicio,
          ciclo == 2018 ~ monto_ejercicio,
          ciclo == 2019 ~ monto_modificado,
          ciclo == 2020 ~ monto_modificado,
          ciclo == 2021 ~ monto_proyecto,
          T ~ NA_real_
        )
      ) %>% 
      select(ciclo, id_ramo, desc_ramo, monto_comparativo) %>% 
      group_by(ciclo, id_ramo, desc_ramo) %>% 
      summarise(monto_comparativo = sum(monto_comparativo, na.rm = T))  %>% 
      mutate_at(
        vars(starts_with("monto")),
        funs(round(./1000000))
      ) %>% 
      mutate(tipo = "Ejercido o modificado") 
  ) %>% 
  bind_rows(
    ramos %>% 
      filter(ciclo==2021) %>% 
      mutate(
        monto_comparativo = monto_proyecto
      ) %>% 
      select(ciclo, id_ramo, desc_ramo, monto_comparativo) %>% 
      group_by(ciclo, id_ramo, desc_ramo) %>% 
      summarise(monto_comparativo = sum(monto_comparativo, na.rm = T))  %>% 
      mutate_at(
        vars(starts_with("monto")),
        funs(round(./1000000))
      ) %>% 
      mutate(tipo = "Proyecto") 
  ) %>% 
  mutate(
    etiqueta = case_when(
      ciclo < 2021 ~ paste0("$", prettyNum(monto_comparativo, big.mark = ",")),
      ciclo == 2021 & tipo == "Proyecto" ~ paste0("$", prettyNum(monto_comparativo, big.mark = ",")),
      T ~ ""
    )
  )


id_ramo_loop <- unique(ramos_2008_2021$id_ramo)

fiuf <- "Progresión del presupuesto público de "
fiuff <- "Montos aprobados y modificados en CP2008-CP2019) y PEF2020T02 vs\nMonto de proyecto en PPEF 2021"

for(i in 1:length(id_ramo_loop)){
  a <- ramos_2008_2021 %>% 
    filter(id_ramo==id_ramo_loop[i])
  
  ggplot(
    a,
    aes(
      x = as.factor(ciclo),
      y = monto_comparativo,
      col = tipo,
      group = tipo,
      label = etiqueta
    )
  ) +
    geom_line(size = 4.5) + 
    geom_point(size = 6) +
    ggrepel::geom_label_repel(show.legend = F, na.rm = T, size = 10) +
    scale_color_manual("", values = c(
      mcci_discrete[1],
      mcci_discrete[2],
      mcci_discrete[5]
    )) +
    scale_y_continuous(
      limits = c(0, max(a$monto_comparativo)+abs(quantile(a$monto_comparativo, 0.01)-max(a$monto_comparativo))),
      labels = scales::comma
    ) +
    labs(title= str_wrap(paste0(
      fiuf, str_to_title(unique(a$desc_ramo))
    ), width = 65),
    subtitle = fiuff,
    caption = fiuffi,
    y = "Millones de pesos (constantes 2021)",
    x = "Ciclo presupuestario") +
    theme_minimal() +
    theme(plot.title = element_text(size = 50, face = "bold" , hjust = 0.5),
          plot.subtitle = element_text(size = 40,  hjust = 0.5),
          plot.caption = element_text(size = 35),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Arial Narrow"),
          axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 30),
          axis.text.x = element_text(size = 27),
          axis.text.y = element_text(size = 25),
          legend.title = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 27))
  
  
  ggsave(filename = paste0(out, "00_progresión_gasto_2008_2021/", unique(a$id_ramo), "_progresión_pef2020apr_ppef2021.png"),
         width = 30, height = 25, dpi = 100, limitsize = FALSE)
}
