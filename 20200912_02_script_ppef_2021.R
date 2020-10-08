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

# Desactivar notación científica
options(scipen=999)

# Directorios ----
inp <- "Github/ppef_2021/01_datos/"
out <- "Github/ppef_2021/03_productos/"

fiuffi <- "Fuente: elaboración propia con datos de @TPresupuestaria | @guzmart_"


# Datos ----
cp_2019 <- readRDS(paste0(inp, "2019_cp_sindefl.rds"))
pef_2020 <- readRDS(paste0(inp, "2020_pef_sindefl.rds"))
ppef_2021 <-readRDS(paste0(inp, "2021_ppef_sindefl.rds"))

tribble(
  ~variable_1, ~variable2, ~variable_3, ~variable_n,
  "margarita", "yaris", 2020, "méxico",
  "girasoles", "vocho", 2020, "méxico",
  "rosas", "tsuru", 2020, "méxico",
  "lavanda", "jetta", 2020, "méxico"
)

deflactor_2021 <- 
  tribble(
    ~año, ~def,
    "2000",  264.3805/100,
    "2001",  254.5415/100,
    "2002",  238.3775/100,
    "2003",  231.0361/100,
    "2004",  210.5657/100,
    "2005",  201.2366/100,
    "2006",  189.3730/100,
    "2007",  176.4373/100,
    "2008",  170.7035/100,
    "2009",  162.3738/100,
    "2010",  154.6858/100,
    "2011",  143.8631/100,
    "2012",  142.5077/100,
    "2013",  139.4495/100,
    "2014",  133.5881/100,
    "2015",  129.7219/100,
    "2016",  120.7418/100,
    "2017",  114.8619/100,
    "2018",  109.4587/100,
    "2019",  107.1113/100,
    "2020",  103.4333/100,
    "2021",  100.0000/100
  )


deflactor_2021$def[deflactor_2021$año=="2012"]


# DEFLACTAR A MANO ----
data_cp_2019 <- cp_2019 %>% 
  # deshacerse de columnas X
  select(-starts_with("X")) %>% 
  # volver minúsculas las variables
  rename_all(tolower) %>% 
  # deflactar los montos
  mutate_at(
    # ¿Qué variables?
    vars(starts_with("monto")),
    # ¿Qué les hago?
    funs(.*deflactor_2021$def[deflactor_2021$año==unique(cp_2019$CICLO)])
  ) %>% 
  mutate(
    # width = cuánto quieres que mida, side = de qué lado, pad = relleno
    id_ramo = str_pad(id_ramo, width = 2, side = "left", pad = "0")
  )
  

data_pef_2020 <- pef_2020 %>% 
  rename_all(tolower) %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(
      (.*deflactor_2021$def[deflactor_2021$año=="2020"])
    )
  ) %>% 
  mutate(
    id_ramo = str_pad(id_ramo, 2, "l", "0")
  )


data_ppef_2021 <- ppef_2021 %>% 
  rename_all(tolower) %>% 
  mutate(
    id_ramo = str_pad(id_ramo, 2, "l", "0")
  ) %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(
      (as.numeric(str_remove_all(., ","))*deflactor_2021$def[deflactor_2021$año=="2021"])
    )
  )

# DEFLACTAR CON FUNCIÓN ----

# ¿Cómo hacer más sencillo el proceso para deflactar?
# ¡Con una función!
nombre_de_la_función <- 
  function(argumento_de_la_función){
    "¿qué le voy a hacer al argumento?"
  }


mi_función <- 
  function(argumento){
    print(argumento)
  }

mi_función("Katia")


mi_función_2 <- 
  function(argumento, argumento_2){
    print(paste(argumento, argumento_2))
  }

mi_función_2("hola", "sof")


deflacta_2021 <- 
  function(monto, ciclo){
    (monto*deflactor_2021$def[deflactor_2021$año==ciclo])
  }

deflacta_2021(monto = 500000000000, ciclo = 2013)

data_cp_2019 <- cp_2019 %>% 
  # deshacerse de columnas X
  select(-starts_with("X")) %>% 
  # volver minúsculas las variables
  rename_all(tolower) %>% 
  # deflactar los montos
  mutate_at(
    # ¿Qué variables?
    vars(starts_with("monto")),
    # ¿Qué les hago?
    funs(deflacta_2021(monto = ., ciclo = unique(cp_2019$CICLO)))
  ) %>% 
  mutate(
    # width = cuánto quieres que mida, side = de qué lado, pad = relleno
    id_ramo = str_pad(id_ramo, width = 2, side = "left", pad = "0")
  )


data_pef_2020 <- pef_2020 %>% 
  rename_all(tolower) %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(
      deflacta_2021(monto = ., ciclo = unique(pef_2020$CICLO))
    )
  ) %>% 
  mutate(
    id_ramo = str_pad(id_ramo, 2, "l", "0")
  )


data_ppef_2021 <- ppef_2021 %>% 
  rename_all(tolower) %>% 
  mutate(
    id_ramo = str_pad(id_ramo, 2, "l", "0")
  ) %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(
      deflacta_2021(monto = as.numeric(str_remove_all(., ",")), ciclo = unique(ppef_2021$CICLO))
    )
  )

