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

# Datos ----
cp_2019 <-readRDS(paste0(inp, "2019_cp_sindefl.rds"))
pef_2020 <- readRDS(paste0(inp, "2020_pef_sindefl.rds"))
ppef_2021 <-readRDS(paste0(inp, "2021_ppef_sindefl.rds"))

estructura_programática <- read_excel(paste0(inp, "2021_estructura_programática.xlsx"))

# Deflactar ----
data_cp_2019 <- cp_2019 %>% 
  select(-starts_with("X")) %>% 
  rename_all(tolower) %>% 
  mutate(
    id_ramo = str_pad(id_ramo, 2, "l", "0")
  ) %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(
      deflacta(., 2019, 2021)
    )
  )

data_pef_2020 <- pef_2020 %>% 
  rename_all(tolower) %>% 
  mutate(
    id_ramo = str_pad(id_ramo, 2, "l", "0")
  )%>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(
      deflacta(., 2020, 2021)
    )
  )

data_ppef_2021 <- ppef_2021 %>% 
  rename_all(tolower) %>% 
  mutate(
    id_ramo = str_pad(id_ramo, 2, "l", "0")
  ) %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(
      deflacta(as.numeric(str_remove_all(., ",")), 2021, 2021)
    )
  )

# saveRDS(data_cp_2019, paste0(inp, "2019_cp_defl2021.rds"))
# saveRDS(data_pef_2020, paste0(inp, "2020_pef_defl2021.rds"))
# saveRDS(data_ppef_2021, paste0(inp, "2021_ppef_defl2021.rds"))



# Funciones ----
# Desactivar notación científica
options(scipen=999)

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}

filter_ramos_apf <- 
  function(id_ramo) {
    str_detect(id_ramo, "02|04|05|06|07|08|09|10|11|12|13|14|15|16|18|20|21|23|25|27|28|31|32|33|36|37|38|47|48|49|50|51|52|53")
  }

filter_ramos_auton <- 
  function(id_ramo) {
    str_detect(id_ramo, "01|03|22|35|41|43|44|49")
  }

ramos_to_cat <-
  function(id_ramo){
    case_when(
      str_detect(id_ramo,"02|04|05|06|07|08|09|10|11|12|13|14|15|16|18|20|21|27|31|32|36|37|38|48|49") ~ "la APF Centralizada",
      str_detect(id_ramo,"47|50|51|52|53") ~ "la APF Paraestatal",
      str_detect(id_ramo,"23|25|28|33") ~ "los Ramos Generales",
      T ~ NA_character_
    )
  }

ramos_to_acr <- 
  function(id_ramo){
    case_when(
      id_ramo=="02" ~ "OFICINA DE LA PRESIDENCIA",
      id_ramo=="04" ~ "GOBERNACIÓN",
      id_ramo=="06" ~ "SHCP",
      id_ramo=="05" ~ "SRE",
      id_ramo=="07" ~ "SEDENA",
      id_ramo=="08" ~ "AGRICULTURA",
      id_ramo=="09" ~ "SCT",
      id_ramo=="10" ~ "ECONOMÍA",
      id_ramo=="11" ~ "SEP",
      id_ramo=="12" ~ "SALUD",
      id_ramo=="13" ~ "MARINA",
      id_ramo=="14" ~ "STPS",
      id_ramo=="15" ~ "SEDATU",
      id_ramo=="16" ~ "SEMARNAT",
      id_ramo=="18" ~ "ENERGÍA",
      id_ramo=="20" ~ "BIENESTAR",
      id_ramo=="21" ~ "TURISMO",
      id_ramo=="23" ~ "RAMO 23",
      id_ramo=="25" ~ "RAMO 25",
      id_ramo=="27" ~ "FUNCIÓN PÚBLICA",
      id_ramo=="28" ~ "RAMO 28",
      id_ramo=="31" ~ "TRIB. AGRARIOS",
      id_ramo=="32" ~ "TFJA",
      id_ramo=="33" ~ "RAMO 33",
      id_ramo=="36" ~ "SPC",
      id_ramo=="37" ~ "CONSEJERÍA JURÍDICA DEL EJECUTIVO FEDERAL",
      id_ramo=="38" ~ "CONACyT",
      id_ramo=="47" ~ "RAMO 47",
      id_ramo=="48" ~ "CULTURA",
      id_ramo=="49" ~ "FGR",
      id_ramo=="50" ~ "IMSS",
      id_ramo=="52" ~ "PEMEX",
      id_ramo=="51" ~ "ISSSTE",
      id_ramo=="53" ~ "CFE",
      T ~ NA_character_
    )
  }

ids_modalidad <- 
  function(id_unica){
    case_when(
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][1] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][1],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][2] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][2],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][3] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][3],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][4] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][4],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][5] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][5],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][6] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][6],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][7] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][7],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][8] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][8],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][9] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][9],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][10] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][10],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][11] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][11],
      T ~ id_unica
    )
  }



ids_fusion <- 
  function(id_unica){
    case_when(
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][1] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="FUSIÓN"][1],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][2] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="FUSIÓN"][2],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][3] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="FUSIÓN"][3],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][4] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="FUSIÓN"][4],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][5] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="FUSIÓN"][5],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][6] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="FUSIÓN"][6],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][7] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="FUSIÓN"][7],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][8] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="FUSIÓN"][8],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][9] ~ estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="FUSIÓN"][9],
      T ~ id_unica
    )
  }

desc_pp_fusion <- 
  function(id_unica, desc_pp){
    case_when(
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][1] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="FUSIÓN"][1],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][2] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="FUSIÓN"][2],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][3] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="FUSIÓN"][3],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][4] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="FUSIÓN"][4],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][5] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="FUSIÓN"][5],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][6] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="FUSIÓN"][6],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][7] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="FUSIÓN"][7],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][8] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="FUSIÓN"][8],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="FUSIÓN"][9] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="FUSIÓN"][9],
      T ~ desc_pp
    )
  }

desc_pp_denominacion <- 
  function(id_unica, desc_pp){
    case_when(
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][1] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][1],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][2] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][2],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][3] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][3],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][4] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][4],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][5] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][5],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][6] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][6],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][7] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][7],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][8] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][8],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][9] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][9],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][10] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][10],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][11] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][11],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][12] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][12],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][13] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][13],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][14] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][14],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][15] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][15],
      id_unica == estructura_programática$id_unica_2020[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][16] ~ estructura_programática$desc_pp_2021[estructura_programática$tipo_cambio=="CAMBIO DE DENOMINACIÓN"][16],
      T ~ desc_pp
    )
  }

estr_prog_2020 <- estructura_programática %>% 
  select(id_unica_2020, tipo_cambio) %>% 
  distinct(id_unica_2020, .keep_all = T) %>% 
  drop_na(id_unica_2020)

tipo_cambio_2020 <- 
  function(id_unica){
    case_when(
      id_unica == estr_prog_2020$id_unica_2020[1] ~ estr_prog_2020$tipo_cambio[1],
      id_unica == estr_prog_2020$id_unica_2020[2] ~ estr_prog_2020$tipo_cambio[2],
      id_unica == estr_prog_2020$id_unica_2020[3] ~ estr_prog_2020$tipo_cambio[3],
      id_unica == estr_prog_2020$id_unica_2020[4] ~ estr_prog_2020$tipo_cambio[4],
      id_unica == estr_prog_2020$id_unica_2020[5] ~ estr_prog_2020$tipo_cambio[5],
      id_unica == estr_prog_2020$id_unica_2020[6] ~ estr_prog_2020$tipo_cambio[6],
      id_unica == estr_prog_2020$id_unica_2020[7] ~ estr_prog_2020$tipo_cambio[7],
      id_unica == estr_prog_2020$id_unica_2020[8] ~ estr_prog_2020$tipo_cambio[8],
      id_unica == estr_prog_2020$id_unica_2020[9] ~ estr_prog_2020$tipo_cambio[9],
      id_unica == estr_prog_2020$id_unica_2020[10] ~ estr_prog_2020$tipo_cambio[10],
      id_unica == estr_prog_2020$id_unica_2020[11] ~ estr_prog_2020$tipo_cambio[11],
      id_unica == estr_prog_2020$id_unica_2020[12] ~ estr_prog_2020$tipo_cambio[12],
      id_unica == estr_prog_2020$id_unica_2020[13] ~ estr_prog_2020$tipo_cambio[13],
      id_unica == estr_prog_2020$id_unica_2020[14] ~ estr_prog_2020$tipo_cambio[14],
      id_unica == estr_prog_2020$id_unica_2020[15] ~ estr_prog_2020$tipo_cambio[15],
      id_unica == estr_prog_2020$id_unica_2020[16] ~ estr_prog_2020$tipo_cambio[16],
      id_unica == estr_prog_2020$id_unica_2020[17] ~ estr_prog_2020$tipo_cambio[17],
      id_unica == estr_prog_2020$id_unica_2020[18] ~ estr_prog_2020$tipo_cambio[18],
      id_unica == estr_prog_2020$id_unica_2020[19] ~ estr_prog_2020$tipo_cambio[19],
      id_unica == estr_prog_2020$id_unica_2020[20] ~ estr_prog_2020$tipo_cambio[20],
      id_unica == estr_prog_2020$id_unica_2020[21] ~ estr_prog_2020$tipo_cambio[21],
      id_unica == estr_prog_2020$id_unica_2020[22] ~ estr_prog_2020$tipo_cambio[22],
      id_unica == estr_prog_2020$id_unica_2020[23] ~ estr_prog_2020$tipo_cambio[23],
      id_unica == estr_prog_2020$id_unica_2020[24] ~ estr_prog_2020$tipo_cambio[24],
      id_unica == estr_prog_2020$id_unica_2020[25] ~ estr_prog_2020$tipo_cambio[25],
      id_unica == estr_prog_2020$id_unica_2020[26] ~ estr_prog_2020$tipo_cambio[26],
      id_unica == estr_prog_2020$id_unica_2020[27] ~ estr_prog_2020$tipo_cambio[27],
      id_unica == estr_prog_2020$id_unica_2020[28] ~ estr_prog_2020$tipo_cambio[28],
      id_unica == estr_prog_2020$id_unica_2020[29] ~ estr_prog_2020$tipo_cambio[29],
      id_unica == estr_prog_2020$id_unica_2020[30] ~ estr_prog_2020$tipo_cambio[30],
      id_unica == estr_prog_2020$id_unica_2020[31] ~ estr_prog_2020$tipo_cambio[31],
      id_unica == estr_prog_2020$id_unica_2020[32] ~ estr_prog_2020$tipo_cambio[32],
      id_unica == estr_prog_2020$id_unica_2020[33] ~ estr_prog_2020$tipo_cambio[33],
      id_unica == estr_prog_2020$id_unica_2020[34] ~ "NO SE REGISTRÓ CAMBIO",
      id_unica == estr_prog_2020$id_unica_2020[35] ~ estr_prog_2020$tipo_cambio[35],
      id_unica == estr_prog_2020$id_unica_2020[36] ~ estr_prog_2020$tipo_cambio[36],
      id_unica == estr_prog_2020$id_unica_2020[37] ~ estr_prog_2020$tipo_cambio[37],
      id_unica == estr_prog_2020$id_unica_2020[38] ~ estr_prog_2020$tipo_cambio[38],
      id_unica == estr_prog_2020$id_unica_2020[39] ~ estr_prog_2020$tipo_cambio[39],
      id_unica == estr_prog_2020$id_unica_2020[40] ~ estr_prog_2020$tipo_cambio[40],
      id_unica == estr_prog_2020$id_unica_2020[41] ~ estr_prog_2020$tipo_cambio[41],
      id_unica == estr_prog_2020$id_unica_2020[42] ~ estr_prog_2020$tipo_cambio[42],
      id_unica == estr_prog_2020$id_unica_2020[43] ~ estr_prog_2020$tipo_cambio[43],
      id_unica == estr_prog_2020$id_unica_2020[44] ~ estr_prog_2020$tipo_cambio[44],
      id_unica == estr_prog_2020$id_unica_2020[45] ~ estr_prog_2020$tipo_cambio[45],
      id_unica == estr_prog_2020$id_unica_2020[46] ~ estr_prog_2020$tipo_cambio[46],
      id_unica == estr_prog_2020$id_unica_2020[47] ~ estr_prog_2020$tipo_cambio[47],
      id_unica == estr_prog_2020$id_unica_2020[48] ~ estr_prog_2020$tipo_cambio[48],
      id_unica == estr_prog_2020$id_unica_2020[49] ~ estr_prog_2020$tipo_cambio[49],
      id_unica == estr_prog_2020$id_unica_2020[50] ~ estr_prog_2020$tipo_cambio[50],
      id_unica == estr_prog_2020$id_unica_2020[51] ~ estr_prog_2020$tipo_cambio[51],
      id_unica == estr_prog_2020$id_unica_2020[52] ~ estr_prog_2020$tipo_cambio[52],
      id_unica == estr_prog_2020$id_unica_2020[53] ~ estr_prog_2020$tipo_cambio[53],
      id_unica == estr_prog_2020$id_unica_2020[54] ~ estr_prog_2020$tipo_cambio[54],
      id_unica == estr_prog_2020$id_unica_2020[55] ~ estr_prog_2020$tipo_cambio[55],
      id_unica == estr_prog_2020$id_unica_2020[56] ~ estr_prog_2020$tipo_cambio[56],
      id_unica == estr_prog_2020$id_unica_2020[57] ~ estr_prog_2020$tipo_cambio[57],
      id_unica == estr_prog_2020$id_unica_2020[58] ~ estr_prog_2020$tipo_cambio[58],
      id_unica == estr_prog_2020$id_unica_2020[59] ~ estr_prog_2020$tipo_cambio[59],
      
      # IDS CAMBIO DE MODALIDAD EN 2021 PERO EXISTENTES EN 2020
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][1] ~ "CAMBIO DE MODALIDAD",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][2] ~ "CAMBIO DE MODALIDAD",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][3] ~ "CAMBIO DE MODALIDAD",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][4] ~ "CAMBIO DE MODALIDAD",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][5] ~ "CAMBIO DE MODALIDAD",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][6] ~ "CAMBIO DE MODALIDAD",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][7] ~ "CAMBIO DE MODALIDAD",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][8] ~ "CAMBIO DE MODALIDAD",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][9] ~ "CAMBIO DE MODALIDAD",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][10] ~ "CAMBIO DE MODALIDAD",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="CAMBIO DE MODALIDAD"][11] ~ "CAMBIO DE MODALIDAD",
      
      # IDS ALTA EN 2021 PERO EXISTENTES EN AVANCE 2020
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][1] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][2] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][3] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][4] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][5] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][6] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][7] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][8] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][9] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][10] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][11] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][12] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][13] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][14] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][15] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][16] ~ "ALTA",
      id_unica == estructura_programática$id_unica_2021[estructura_programática$tipo_cambio=="ALTA"][17] ~ "ALTA",
      
      
      T ~ "NO SE REGISTRÓ CAMBIO"
    )
  }

estr_prog_2021 <- estructura_programática %>% 
  select(id_unica_2021, tipo_cambio) %>% 
  distinct(id_unica_2021, .keep_all = T) %>% 
  drop_na(id_unica_2021)

tipo_cambio_2021 <- 
  function(id_unica){
    case_when(
      id_unica == estr_prog_2021$id_unica_2021[1] ~ estr_prog_2021$tipo_cambio[1],
      id_unica == estr_prog_2021$id_unica_2021[2] ~ estr_prog_2021$tipo_cambio[2],
      id_unica == estr_prog_2021$id_unica_2021[3] ~ estr_prog_2021$tipo_cambio[3],
      id_unica == estr_prog_2021$id_unica_2021[4] ~ estr_prog_2021$tipo_cambio[4],
      id_unica == estr_prog_2021$id_unica_2021[5] ~ estr_prog_2021$tipo_cambio[5],
      id_unica == estr_prog_2021$id_unica_2021[6] ~ estr_prog_2021$tipo_cambio[6],
      id_unica == estr_prog_2021$id_unica_2021[7] ~ estr_prog_2021$tipo_cambio[7],
      id_unica == estr_prog_2021$id_unica_2021[8] ~ estr_prog_2021$tipo_cambio[8],
      id_unica == estr_prog_2021$id_unica_2021[9] ~ estr_prog_2021$tipo_cambio[9],
      id_unica == estr_prog_2021$id_unica_2021[10] ~ estr_prog_2021$tipo_cambio[10],
      id_unica == estr_prog_2021$id_unica_2021[11] ~ estr_prog_2021$tipo_cambio[11],
      id_unica == estr_prog_2021$id_unica_2021[12] ~ estr_prog_2021$tipo_cambio[12],
      id_unica == estr_prog_2021$id_unica_2021[13] ~ estr_prog_2021$tipo_cambio[13],
      id_unica == estr_prog_2021$id_unica_2021[14] ~ estr_prog_2021$tipo_cambio[14],
      id_unica == estr_prog_2021$id_unica_2021[15] ~ estr_prog_2021$tipo_cambio[15],
      id_unica == estr_prog_2021$id_unica_2021[16] ~ estr_prog_2021$tipo_cambio[16],
      id_unica == estr_prog_2021$id_unica_2021[17] ~ estr_prog_2021$tipo_cambio[17],
      id_unica == estr_prog_2021$id_unica_2021[18] ~ estr_prog_2021$tipo_cambio[18],
      id_unica == estr_prog_2021$id_unica_2021[19] ~ estr_prog_2021$tipo_cambio[19],
      id_unica == estr_prog_2021$id_unica_2021[20] ~ estr_prog_2021$tipo_cambio[20],
      id_unica == estr_prog_2021$id_unica_2021[21] ~ estr_prog_2021$tipo_cambio[21],
      id_unica == estr_prog_2021$id_unica_2021[22] ~ estr_prog_2021$tipo_cambio[22],
      id_unica == estr_prog_2021$id_unica_2021[23] ~ estr_prog_2021$tipo_cambio[23],
      id_unica == estr_prog_2021$id_unica_2021[24] ~ estr_prog_2021$tipo_cambio[24],
      id_unica == estr_prog_2021$id_unica_2021[25] ~ estr_prog_2021$tipo_cambio[25],
      id_unica == estr_prog_2021$id_unica_2021[26] ~ estr_prog_2021$tipo_cambio[26],
      id_unica == estr_prog_2021$id_unica_2021[27] ~ estr_prog_2021$tipo_cambio[27],
      id_unica == estr_prog_2021$id_unica_2021[28] ~ estr_prog_2021$tipo_cambio[28],
      id_unica == estr_prog_2021$id_unica_2021[29] ~ estr_prog_2021$tipo_cambio[29],
      id_unica == estr_prog_2021$id_unica_2021[30] ~ "NO SE REGISTRÓ CAMBIO",
      id_unica == estr_prog_2021$id_unica_2021[31] ~ "NO SE REGISTRÓ CAMBIO",
      id_unica == estr_prog_2021$id_unica_2021[32] ~ "NO SE REGISTRÓ CAMBIO",
      id_unica == estr_prog_2021$id_unica_2021[33] ~ estr_prog_2021$tipo_cambio[33],
      id_unica == estr_prog_2021$id_unica_2021[34] ~ estr_prog_2021$tipo_cambio[34],
      id_unica == estr_prog_2021$id_unica_2021[35] ~ estr_prog_2021$tipo_cambio[35],
      id_unica == estr_prog_2021$id_unica_2021[36] ~ estr_prog_2021$tipo_cambio[36],
      id_unica == estr_prog_2021$id_unica_2021[37] ~ estr_prog_2021$tipo_cambio[37],
      id_unica == estr_prog_2021$id_unica_2021[38] ~ estr_prog_2021$tipo_cambio[38],
      id_unica == estr_prog_2021$id_unica_2021[39] ~ estr_prog_2021$tipo_cambio[39],
      id_unica == estr_prog_2021$id_unica_2021[40] ~ estr_prog_2021$tipo_cambio[40],
      id_unica == estr_prog_2021$id_unica_2021[41] ~ estr_prog_2021$tipo_cambio[41],
      id_unica == estr_prog_2021$id_unica_2021[42] ~ estr_prog_2021$tipo_cambio[42],
      id_unica == estr_prog_2021$id_unica_2021[43] ~ estr_prog_2021$tipo_cambio[43],
      id_unica == estr_prog_2021$id_unica_2021[44] ~ estr_prog_2021$tipo_cambio[44],
      id_unica == estr_prog_2021$id_unica_2021[45] ~ estr_prog_2021$tipo_cambio[45],
      id_unica == estr_prog_2021$id_unica_2021[46] ~ estr_prog_2021$tipo_cambio[46],
      id_unica == estr_prog_2021$id_unica_2021[47] ~ estr_prog_2021$tipo_cambio[47],
      id_unica == estr_prog_2021$id_unica_2021[48] ~ estr_prog_2021$tipo_cambio[48],
      id_unica == estr_prog_2021$id_unica_2021[49] ~ estr_prog_2021$tipo_cambio[49],
      id_unica == estr_prog_2021$id_unica_2021[50] ~ estr_prog_2021$tipo_cambio[50],
      id_unica == estr_prog_2021$id_unica_2021[51] ~ estr_prog_2021$tipo_cambio[51],
      T ~ "NO SE REGISTRÓ CAMBIO"
    )
  }


fiuffi <- "Fuente: elaboración propia con datos de @TPresupuestaria | @guzmart_"

# Paleta de colores ----
mcci_discrete <- c(
  '#000c2d', '#0E9A9D', '#1EBB97', '#ecd756', '#f72732', '#F58C33'
)

# ADMINISTRACIÓN PÚBLICA FEDERAL ----
# 1. RAMOS ----
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

# 1.1. Sin categorizar ----
# ** Aprobado2020 vs Proyecto2021 ----
ramos_pef2020apr_ppef2021 <- ramos %>% 
  filter(ciclo != 2019) %>% 
  filter(filter_ramos_apf(id_ramo)) %>% 
  mutate(
    monto_comparativo = case_when(
      is.na(monto_proyecto) ~ monto_aprobado,
      T ~ monto_proyecto
    )
  ) %>% 
  select(ciclo, id_ramo, desc_ramo, monto_comparativo) %>% 
  group_by(ciclo, id_ramo, desc_ramo) %>% 
  summarise(monto_comparativo = sum(monto_comparativo, na.rm = T)) %>% 
  pivot_wider(
    names_from = ciclo, 
    values_from = monto_comparativo,
    names_glue = "monto_{ciclo}"
  ) %>% 
  mutate(
    monto_diferencia = (monto_2021 - monto_2020)/1000000,
    tipo = case_when(
      monto_diferencia<0 ~ "Perdedor",
      T ~ "Ganador"
    ),
    ramo_acronimo = ramos_to_acr(id_ramo),
    ramo_cat = ramos_to_cat(id_ramo),
    ramo_cat = factor(ramo_cat, 
                      levels = c("la APF Centralizada", "la APF Paraestatal", "los Ramos Generales"),
                      labels = c("la APF Centralizada", "la APF Paraestatal", "los Ramos Generales"))
  ) %>% 
  ungroup() %>% 
  arrange(-monto_diferencia)

fiuf <- "Variación del presupuesto de la Administración Pública Federal"
fiuff <- "Monto aprobado en PEF2020 vs Monto de proyecto en PPEF 2021"
ggplot(ramos_pef2020apr_ppef2021 %>% 
         filter(abs(monto_diferencia)>200), 
       aes(
         x=monto_diferencia, 
         y=reorder(str_wrap(toupper(ramo_acronimo),15), as.numeric(monto_diferencia)), 
         label=paste0("$", prettyNum(round(monto_diferencia), big.mark = ",")),
         fill = tipo
       )) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = "outward", size = 6) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(-75000,75000)) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = paste0("Nota: no se tomaron en cuenta aquellos ramos cuyo monto absoluto de diferencia haya sido menor a 200 MDP\n",
                        fiuffi),
       x = "Millones de pesos (constantes 2021)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_text(size = 21),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(out, "01_APF/01_00_ramos_pef2020apr_ppef2021.png"),
       width = 17, height = 20, dpi = 100)

# ** Modificado2020T02 vs Proyecto2021 ----
ramos_pef2020mod_ppef2021 <- ramos %>% 
  filter(ciclo != 2019) %>% 
  filter(filter_ramos_apf(id_ramo)) %>% 
  mutate(
    monto_comparativo = case_when(
      is.na(monto_proyecto) ~ monto_modificado,
      T ~ monto_proyecto
    )
  ) %>% 
  select(ciclo, id_ramo, desc_ramo, monto_comparativo) %>% 
  group_by(ciclo, id_ramo, desc_ramo) %>% 
  summarise(monto_comparativo = sum(monto_comparativo, na.rm = T)) %>% 
  pivot_wider(
    names_from = ciclo, 
    values_from = monto_comparativo,
    names_glue = "monto_{ciclo}"
  ) %>% 
  mutate(
    monto_diferencia = (monto_2021 - monto_2020)/1000000,
    tipo = case_when(
      monto_diferencia<0 ~ "Perdedor",
      T ~ "Ganador"
    ),
    ramo_acronimo = ramos_to_acr(id_ramo),
    ramo_cat = ramos_to_cat(id_ramo),
    ramo_cat = factor(ramo_cat, 
                      levels = c("la APF Centralizada", "la APF Paraestatal", "los Ramos Generales"),
                      labels = c("la APF Centralizada", "la APF Paraestatal", "los Ramos Generales"))
  ) %>% 
  ungroup() %>% 
  arrange(-monto_diferencia)

fiuf <- "Variación del presupuesto de la Administración Pública Federal"
fiuff <- "Monto modificado en PEF2020 (avance del gasto al T02) vs Monto de proyecto en PPEF 2021"
ggplot(ramos_pef2020mod_ppef2021 %>% 
         filter(abs(monto_diferencia)>200), 
       aes(
         x=monto_diferencia, 
         y=reorder(str_wrap(toupper(ramo_acronimo),15), as.numeric(monto_diferencia)), 
         label=paste0("$", prettyNum(round(monto_diferencia), big.mark = ",")),
         fill = tipo
       )) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = "outward", size = 6) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(-70000,70000),
                     breaks = seq(-60000,60000,20000)) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = paste0("Nota: no se tomaron en cuenta aquellos ramos cuyo monto absoluto de diferencia haya sido menor a 200 MDP\n",
                        fiuffi),
       x = "Millones de pesos (constantes 2021)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_text(size = 21),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(out, "01_APF/02_00_ramos_pef2020mod_ppef2021.png"),
       width = 17, height = 20, dpi = 100)


# 1.2. Categorizada ----
# * Aprobado2020 vs Proyecto2021 ----
# ** APF Centralizada ----
i = 1
ramos_cats_loop <- levels(ramos_pef2020apr_ppef2021$ramo_cat)
fiuf <- paste0("Variación del presupuesto de ", ramos_cats_loop[1])
fiuff <- "Monto aprobado en PEF2020 vs Monto de proyecto en PPEF 2021"
ggplot(ramos_pef2020apr_ppef2021 %>% 
         filter(ramo_cat==ramos_cats_loop[1], abs(monto_diferencia)>100), 
       aes(
         x=monto_diferencia, 
         y=reorder(str_wrap(toupper(ramo_acronimo),15), as.numeric(monto_diferencia)), 
         label=paste0("$", prettyNum(round(monto_diferencia), big.mark = ",")),
         fill = tipo
       )) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = "outward", size = 6) +
  scale_x_continuous(labels = scales::comma) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(-40000,40000),
                     breaks = seq(-30000,30000,10000)) + 
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = paste0("Nota: no se tomaron en cuenta aquellos ramos cuyo monto absoluto de diferencia haya sido menor a 100 MDP\n",
                        fiuffi),
       x = "Millones de pesos (constantes 2021)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_text(size = 21),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(out, "01_APF/01_01_apfcent_pef2020apr_ppef2021.png"),
       width = 15, height = 20, dpi = 100)

# ** APF Paraestatal ----
i = 2
ramos_cats_loop <- levels(ramos_pef2020apr_ppef2021$ramo_cat)
fiuf <- paste0("Variación del presupuesto de ", ramos_cats_loop[i])
fiuff <- "Monto aprobado en PEF2020 vs Monto de proyecto en PPEF 2021"
ggplot(ramos_pef2020apr_ppef2021 %>% 
         filter(ramo_cat==ramos_cats_loop[i]), 
       aes(
         x=monto_diferencia, 
         y=reorder(str_wrap(toupper(ramo_acronimo),15), as.numeric(monto_diferencia)), 
         label=paste0("$", prettyNum(round(monto_diferencia), big.mark = ",")),
         fill = tipo
       )) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = "outward", size = 6) +
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(-70000,70000),
                     breaks = seq(-60000,60000,20000)) + 
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = fiuffi,
       x = "Millones de pesos (constantes 2021)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_text(size = 21),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(out, "01_APF/01_02_apfparaest_pef2020apr_ppef2021.png"),
       width = 15, height = 20, dpi = 100)

# ** Ramos Generales ----
i = 3
ramos_cats_loop <- levels(ramos_pef2020apr_ppef2021$ramo_cat)
fiuf <- paste0("Variación del presupuesto de ", ramos_cats_loop[i])
fiuff <- "Monto aprobado en PEF2020 vs Monto de proyecto en PPEF 2021"
ggplot(ramos_pef2020apr_ppef2021 %>% 
         filter(ramo_cat==ramos_cats_loop[i]), 
       aes(
         x=monto_diferencia, 
         y=reorder(str_wrap(toupper(ramo_acronimo),15), as.numeric(monto_diferencia)), 
         label=paste0("$", prettyNum(round(monto_diferencia), big.mark = ",")),
         fill = tipo
       )) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = "outward", size = 6) +
  scale_x_continuous(labels = scales::comma) + 
  scale_fill_manual("", values = c(mcci_discrete[5])) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(-70000,70000),
                     breaks = seq(-60000,60000,20000)) + 
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = fiuffi,
       x = "Millones de pesos (constantes 2021)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_text(size = 21),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(out, "01_APF/01_03_apfparaest_pef2020apr_ppef2021.png"),
       width = 15, height = 20, dpi = 100)

# * Modificado2020T02 vs Proyecto2021 ----
# ** APF Centralizada ----
i = 1
ramos_cats_loop <- levels(ramos_pef2020mod_ppef2021$ramo_cat)
fiuf <- paste0("Variación del presupuesto de ", ramos_cats_loop[i])
fiuff <- "Monto modificado en PEF2020 (avance del gasto al T02) vs Monto de proyecto en PPEF 2021"
ggplot(ramos_pef2020mod_ppef2021 %>% 
         filter(ramo_cat==ramos_cats_loop[i], abs(monto_diferencia)>100), 
       aes(
         x=monto_diferencia, 
         y=reorder(str_wrap(toupper(ramo_acronimo),15), as.numeric(monto_diferencia)), 
         label=paste0("$", prettyNum(round(monto_diferencia), big.mark = ",")),
         fill = tipo
       )) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = "outward", size = 6) +
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(-40000,40000),
                     breaks = seq(-30000,30000,10000)) + 
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = paste0("Nota: no se tomaron en cuenta aquellos ramos cuyo monto absoluto de diferencia haya sido menor a 100 MDP\n",
                        fiuffi),
       x = "Millones de pesos (constantes 2021)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_text(size = 21),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(out, "01_APF/02_01_apfcent_pef2020apr_ppef2021.png"),
       width = 15, height = 20, dpi = 100)

# ** APF Paraestatal ----
i = 2
ramos_cats_loop <- levels(ramos_pef2020mod_ppef2021$ramo_cat)
fiuf <- paste0("Variación del presupuesto de ", ramos_cats_loop[i])
fiuff <- "Monto modificado en PEF2020 (avance del gasto al T02) vs Monto de proyecto en PPEF 2021"
ggplot(ramos_pef2020mod_ppef2021 %>% 
         filter(ramo_cat==ramos_cats_loop[i]), 
       aes(
         x=monto_diferencia, 
         y=reorder(str_wrap(toupper(ramo_acronimo),15), as.numeric(monto_diferencia)), 
         label=paste0("$", prettyNum(round(monto_diferencia), big.mark = ",")),
         fill = tipo
       )) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = "outward", size = 6) +
  scale_x_continuous(labels = scales::comma) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(-70000,70000),
                     breaks = seq(-60000,60000,20000)) + 
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = fiuffi,
       x = "Millones de pesos (constantes 2021)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_text(size = 21),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(out, "01_APF/02_02_apfparaest_pef2020apr_ppef2021.png"),
       width = 15, height = 20, dpi = 100)

# ** Ramos Generales ----
i = 3
ramos_cats_loop <- levels(ramos_pef2020mod_ppef2021$ramo_cat)
fiuf <- paste0("Variación del presupuesto de ", ramos_cats_loop[i])
fiuff <- "Monto modificado en PEF2020 (avance del gasto al T02) vs Monto de proyecto en PPEF 2021"
ggplot(ramos_pef2020mod_ppef2021 %>% 
         filter(ramo_cat==ramos_cats_loop[i]), 
       aes(
         x=monto_diferencia, 
         y=reorder(str_wrap(toupper(ramo_acronimo),15), as.numeric(monto_diferencia)), 
         label=paste0("$", prettyNum(round(monto_diferencia), big.mark = ",")),
         fill = tipo
       )) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = "outward", size = 6) +
  scale_x_continuous(labels = scales::comma) + 
  scale_fill_manual("", values = c(mcci_discrete[5])) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(-70000,70000),
                     breaks = seq(-60000,60000,20000)) + 
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = fiuffi,
       x = "Millones de pesos (constantes 2021)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_text(size = 21),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(out, "01_APF/01_03_apfparaest_pef2020apr_ppef2021.png"),
       width = 15, height = 20, dpi = 100)

# 2. MODALIDADES ----
mods <- readRDS(paste0(inp, "2019_cp_defl2021.rds")) %>% 
  select(
    ciclo, id_ramo, contains("modalidad"), contains("monto")
  ) %>% 
  bind_rows(
    readRDS(paste0(inp, "2020_pef_defl2021.rds")) %>% 
      select(
        ciclo, id_ramo, contains("modalidad"), contains("monto")
      )
  ) %>% 
  bind_rows(
    readRDS(paste0(inp, "2021_ppef_defl2021.rds")) %>% 
      select(
        ciclo, id_ramo, contains("modalidad"), contains("monto")
      )
  )

# ** Aprobado2020 vs Proyecto2021 ----
mods_pef2020apr_ppef2021 <- mods %>% 
  filter(ciclo != 2019) %>% 
  filter(filter_ramos_apf(id_ramo)) %>% 
  filter(!id_modalidad=="Y") %>% 
  mutate(
    monto_comparativo = case_when(
      is.na(monto_proyecto) ~ monto_aprobado,
      T ~ monto_proyecto
    )
  ) %>% 
  select(ciclo, id_modalidad, desc_modalidad, monto_comparativo) %>% 
  group_by(ciclo, id_modalidad, desc_modalidad) %>% 
  summarise(monto_comparativo = sum(monto_comparativo, na.rm = T)) %>% 
  pivot_wider(
    names_from = ciclo, 
    values_from = monto_comparativo,
    names_glue = "monto_{ciclo}"
  ) %>% 
  mutate(
    monto_diferencia = (monto_2021 - monto_2020)/1000000,
    tipo = case_when(
      monto_diferencia<0 ~ "Perdedor",
      T ~ "Ganador"
    )
  ) %>% 
  ungroup() %>% 
  arrange(-monto_diferencia) 

fiuf <- "Variación del presupuesto por Modalidades"
fiuff <- "Monto aprobado en PEF2020 vs Monto de proyecto en PPEF 2021"
ggplot(mods_pef2020apr_ppef2021, 
       aes(
         x=monto_diferencia, 
         y=reorder(
           str_wrap(paste0(desc_modalidad,"\n[",id_modalidad,"]"),30), as.numeric(monto_diferencia)
         ), 
         label=paste0("$", prettyNum(round(monto_diferencia), big.mark = ",")),
         fill = tipo
       )) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = "outward", size = 6) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(-165000,165000),
                     breaks = seq(-150000,150000,50000)) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = fiuffi,
       x = "Millones de pesos (constantes 2021)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_text(size = 21),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(out, "01_APF/03_mods_pef2020apr_ppef2021.png"),
       width = 15, height = 22, dpi = 100)


# ** Modificado2020T02 vs Proyecto2021 ----
mods_pef2020mod_ppef2021 <- mods %>% 
  filter(ciclo != 2019) %>% 
  filter(filter_ramos_apf(id_ramo)) %>% 
  filter(!id_modalidad=="Y") %>% 
  mutate(
    monto_comparativo = case_when(
      is.na(monto_proyecto) ~ monto_modificado,
      T ~ monto_proyecto
    )
  ) %>% 
  select(ciclo, id_modalidad, desc_modalidad, monto_comparativo) %>% 
  group_by(ciclo, id_modalidad, desc_modalidad) %>% 
  summarise(monto_comparativo = sum(monto_comparativo, na.rm = T)) %>% 
  pivot_wider(
    names_from = ciclo, 
    values_from = monto_comparativo,
    names_glue = "monto_{ciclo}"
  ) %>% 
  mutate(
    monto_diferencia = (monto_2021 - monto_2020)/1000000,
    tipo = case_when(
      monto_diferencia<0 ~ "Perdedor",
      T ~ "Ganador"
    )
  ) %>% 
  ungroup() %>% 
  arrange(-monto_diferencia) 

fiuf <- "Variación del presupuesto por Modalidades"
fiuff <- "Monto modificado en PEF2020 (avance del gasto al T02) vs\nMonto de proyecto en PPEF 2021"
ggplot(mods_pef2020mod_ppef2021, 
       aes(
         x=monto_diferencia, 
         y=reorder(
           str_wrap(paste0(desc_modalidad,"\n[",id_modalidad,"]"),30), as.numeric(monto_diferencia)
         ), 
         label=paste0("$", prettyNum(round(monto_diferencia), big.mark = ",")),
         fill = tipo
       )) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = "outward", size = 6) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(-150000,150000),
                     breaks = seq(-125000,125000,50000)) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = fiuffi,
       x = "Millones de pesos (constantes 2021)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_text(size = 21),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(out, "01_APF/03_mods_pef2020mod_ppef2021.png"),
       width = 15, height = 22, dpi = 100)

# 3. PROGRAMAS PRESUPUESTARIOS POR RAMOS (CON MODALIDAD) ----
pp_ramos <- readRDS(paste0(inp, "2019_cp_defl2021.rds")) %>% 
  select(
    ciclo, contains("ramo"), contains("modalidad"), ends_with("_pp"), contains("monto")
  ) %>% 
  bind_rows(
    readRDS(paste0(inp, "2020_pef_defl2021.rds")) %>% 
      select(
        ciclo, contains("ramo"), contains("modalidad"), ends_with("_pp"),contains("monto")
      )
  ) %>% 
  bind_rows(
    readRDS(paste0(inp, "2021_ppef_defl2021.rds")) %>% 
      select(
        ciclo, contains("ramo"), contains("modalidad"), ends_with("_pp"),contains("monto")
      )
  ) %>% 
  mutate(
    id_pp = str_pad(id_pp,3,"l","0"),
    id_unica = paste0(id_ramo, id_modalidad, id_pp)
  )

# ** Aprobado2020 vs Proyecto2021 ----
# Nota: la escisión del PP 11S247 a 11S307:11S309 fue calificada como sin cambios
pp_ramos_pef2020apr_ppef2021 <- pp_ramos %>% 
  filter(ciclo == 2020) %>% 
  mutate(
    tipo_cambio = tipo_cambio_2020(id_unica),
    id_unica = ids_modalidad(id_unica),
    id_unica = ids_fusion(id_unica),
    desc_pp = desc_pp_denominacion(id_unica, desc_pp),
    desc_pp = desc_pp_fusion(id_unica, desc_pp)
  ) %>% 
  bind_rows(
    pp_ramos %>% 
      filter(ciclo == 2021) %>% 
      mutate(
        tipo_cambio = tipo_cambio_2021(id_unica)
      )
  ) %>% 
  mutate(
    monto_comparativo = case_when(
      is.na(monto_proyecto) ~ monto_aprobado,
      T ~ monto_proyecto
    ),
    desc_pp = str_trim(desc_pp),
    desc_pp = str_replace_all(desc_pp,"[:whitespace:]", "_"),
    desc_pp = str_replace_all(desc_pp,"_", " "),
    desc_pp = toupper(desc_pp),
    desc_pp = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", desc_pp, perl=TRUE),
    desc_pp = case_when(
      id_unica == "47S249" ~ "PROGRAMA PARA EL BIENESTAR DE LOS PUEBLOS INDÍGENAS Y AFROMEXICANO",
      T ~ desc_pp
    ),
    tipo_cambio = factor(
      tipo_cambio,
      levels = c("NO SE REGISTRÓ CAMBIO", "ALTA", "ELIMINACIÓN", 
                 "CAMBIO DE MODALIDAD", "CAMBIO DE DENOMINACIÓN", 
                 "FUSIÓN"),
      labels = c("NO SE REGISTRÓ CAMBIO", "ALTA", "ELIMINACIÓN", 
                 "CAMBIO DE MODALIDAD", "CAMBIO DE DENOMINACIÓN", 
                 "FUSIÓN")
    )
  ) %>% 
  select(ciclo, id_unica, id_ramo, desc_ramo, desc_pp, monto_comparativo, tipo_cambio) %>% 
  filter(filter_ramos_apf(id_ramo)) %>% 
  group_by(ciclo, id_unica, id_ramo, desc_ramo, desc_pp, tipo_cambio) %>% 
  summarise(monto_comparativo = sum(monto_comparativo, na.rm = T)) %>% 
  pivot_wider(
    names_from = ciclo, 
    values_from = monto_comparativo,
    names_glue = "monto_{ciclo}"
  ) %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(case_when(is.na(.)~0,T~.))
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
      tipo_cambio == "ELIMINACIÓN" ~ -100,
      tipo_cambio == "FUSIÓN" & is.na(monto_2021) ~ -100,
      tipo_cambio == "ALTA" ~ 100,
      prop_monto==Inf~ 100,
      is.na(prop_monto)~0,
      T ~ prop_monto
    ),
    ramo_acronimo = ramos_to_acr(id_ramo),
    ramo_cat = ramos_to_cat(id_ramo),
    ramo_cat = factor(ramo_cat, 
                      levels = c("la APF Centralizada", "la APF Paraestatal", "los Ramos Generales"),
                      labels = c("la APF Centralizada", "la APF Paraestatal", "los Ramos Generales")),
    tipo = case_when(
      monto_diferencia<0 ~ "Perdedor",
      T ~ "Ganador"
    )
  ) %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(round(./1000000))
  ) %>% 
  arrange(as.numeric(id_ramo)) %>% 
  ungroup() %>% 
  mutate(
    drop = case_when(
      tipo_cambio == "NO SE REGISTRÓ CAMBIO" & monto_2020 == 0 & monto_2021 == 0 ~ 1,
      T ~ 0
    )
  ) %>% 
  filter(!drop==1)



id_ramo_loop <- unique(pp_ramos_pef2020apr_ppef2021$id_ramo)

fiuf <- "Variación del presupuesto para programas presupuestarios de "
fiuff <- "Monto aprobado en PEF2020 vs Monto de proyecto en PPEF 2021"

for(i in 1:length(id_ramo_loop)){
  a <- pp_ramos_pef2020apr_ppef2021 %>% 
    filter(id_ramo==id_ramo_loop[i])
  
  
  ggplot(
    a, 
    aes(
      x=monto_diferencia, 
      y=reorder(str_wrap(str_trunc(desc_pp, 80), 35), as.numeric(monto_diferencia)), 
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
                       limits = c((max(abs(a$monto_diferencia))*-1)-50,
                                  max(abs(a$monto_diferencia))+50)) + 
    scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
    scale_color_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
    labs(title= str_wrap(paste0(
      fiuf, str_to_title(unique(a$desc_ramo))
    ), width = 65),
    subtitle = fiuff,
    caption = fiuffi,
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
  ggsave(filename = paste0(out, "01_APF/04_", unique(a$id_ramo), "_pp_pef2020apr_ppef2021.png"),
         width = 30, height = 40, dpi = 100, limitsize = FALSE)
}


# ** Modificado2020T02 vs Proyecto2021 ----
pp_ramos_pef2020mod_ppef2021 <- pp_ramos %>% 
  filter(ciclo == 2020) %>% 
  mutate(
    tipo_cambio = tipo_cambio_2020(id_unica),
    id_unica = ids_modalidad(id_unica),
    id_unica = ids_fusion(id_unica),
    desc_pp = desc_pp_denominacion(id_unica, desc_pp),
    desc_pp = desc_pp_fusion(id_unica, desc_pp)
  ) %>% 
  bind_rows(
    pp_ramos %>% 
      filter(ciclo == 2021) %>% 
      mutate(
        tipo_cambio = tipo_cambio_2021(id_unica)
      )
  ) %>% 
  mutate(
    monto_comparativo = case_when(
      is.na(monto_proyecto) ~ monto_modificado,
      T ~ monto_proyecto
    ),
    desc_pp = str_trim(desc_pp),
    desc_pp = str_replace_all(desc_pp,"[:whitespace:]", "_"),
    desc_pp = str_replace_all(desc_pp,"_", " "),
    desc_pp = toupper(desc_pp),
    desc_pp = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", desc_pp, perl=TRUE),
    desc_pp = case_when(
      id_unica == "47S249" ~ "PROGRAMA PARA EL BIENESTAR DE LOS PUEBLOS INDÍGENAS Y AFROMEXICANO",
      T ~ desc_pp
    ),
    tipo_cambio = factor(
      tipo_cambio,
      levels = c("NO SE REGISTRÓ CAMBIO", "ALTA", "ELIMINACIÓN", 
                 "CAMBIO DE MODALIDAD", "CAMBIO DE DENOMINACIÓN", 
                 "FUSIÓN"),
      labels = c("NO SE REGISTRÓ CAMBIO", "ALTA", "ELIMINACIÓN", 
                 "CAMBIO DE MODALIDAD", "CAMBIO DE DENOMINACIÓN", 
                 "FUSIÓN")
    )
  ) %>% 
  select(ciclo, id_unica, id_ramo, desc_ramo, desc_pp, monto_comparativo, tipo_cambio) %>% 
  filter(filter_ramos_apf(id_ramo)) %>% 
  group_by(ciclo, id_unica, id_ramo, desc_ramo, desc_pp, tipo_cambio) %>% 
  summarise(monto_comparativo = sum(monto_comparativo, na.rm = T)) %>% 
  pivot_wider(
    names_from = ciclo, 
    values_from = monto_comparativo,
    names_glue = "monto_{ciclo}"
  ) %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(case_when(is.na(.)~0,T~.))
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
      tipo_cambio == "ELIMINACIÓN" ~ -100,
      tipo_cambio == "FUSIÓN" & is.na(monto_2021) ~ -100,
      tipo_cambio == "ALTA" & monto_2020==0 ~ 100,
      prop_monto==Inf~ 100,
      is.na(prop_monto)~0,
      T ~ prop_monto
    ),
    ramo_acronimo = ramos_to_acr(id_ramo),
    ramo_cat = ramos_to_cat(id_ramo),
    ramo_cat = factor(ramo_cat, 
                      levels = c("la APF Centralizada", "la APF Paraestatal", "los Ramos Generales"),
                      labels = c("la APF Centralizada", "la APF Paraestatal", "los Ramos Generales")),
    tipo = case_when(
      monto_diferencia<0 ~ "Perdedor",
      T ~ "Ganador"
    )
  ) %>% 
  mutate_at(
    vars(starts_with("monto")),
    funs(round(./1000000))
  ) %>% 
  arrange(as.numeric(id_ramo)) %>% 
  ungroup() %>% 
  mutate(
    drop = case_when(
      tipo_cambio == "NO SE REGISTRÓ CAMBIO" & monto_2020 == 0 & monto_2021 == 0 ~ 1,
      T ~ 0
    )
  ) %>% 
  filter(!drop==1)



id_ramo_loop <- unique(pp_ramos_pef2020mod_ppef2021$id_ramo)

fiuf <- "Variación del presupuesto para programas presupuestarios de "
fiuff <- "Monto modificado en PEF2020 (avance del gasto al T02) vs\nMonto de proyecto en PPEF 2021"

for(i in 1:length(id_ramo_loop)){
  a <- pp_ramos_pef2020mod_ppef2021 %>% 
    filter(id_ramo==id_ramo_loop[i])
  
  
  ggplot(
    a, 
    aes(
      x=monto_diferencia, 
      y=reorder(str_wrap(str_trunc(desc_pp, 80), 35), as.numeric(monto_diferencia)), 
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
                       limits = c((max(abs(a$monto_diferencia))*-1)-50,
                                  max(abs(a$monto_diferencia))+50)) + 
    scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
    scale_color_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
    labs(title= str_wrap(paste0(
      fiuf, str_to_title(unique(a$desc_ramo))
    ), width = 65),
    subtitle = fiuff,
    caption = fiuffi,
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
  ggsave(filename = paste0(out, "01_APF/05_", unique(a$id_ramo), "_pp_pef2020mod_ppef2021.png"),
         width = 30, height = 40, dpi = 100, limitsize = FALSE)
}


# 4. UNIDAD RESPONSABLE ----
ur_ramos <- readRDS(paste0(inp, "2019_cp_defl2021.rds")) %>% 
  select(
    ciclo, contains("ramo"), ends_with("_ur"), contains("monto")
  ) %>% 
  bind_rows(
    readRDS(paste0(inp, "2020_pef_defl2021.rds")) %>% 
      select(
        ciclo, contains("ramo"), ends_with("_ur"), contains("monto")
      )
  ) %>% 
  bind_rows(
    readRDS(paste0(inp, "2021_ppef_defl2021.rds")) %>% 
      select(
        ciclo, contains("ramo"), ends_with("_ur"), contains("monto")
      )
  ) 

# ** Aprobado2020 vs Proyecto2021 ----
ur_ramos_pef2020apr_ppef2021 <- ur_ramos %>% 
  filter(ciclo != 2019) %>% 
  filter(filter_ramos_apf(id_ramo)) %>% 
  mutate(
    monto_comparativo = case_when(
      is.na(monto_proyecto) ~ monto_aprobado,
      T ~ monto_proyecto
    ),
    desc_ur = case_when(
      id_ramo =="14" & str_starts(desc_ur, "Delegación Federal del Trabajo en") ~ str_replace_all(desc_ur, "Delegación Federal del Trabajo en", "Representación en"),
      id_ramo =="14" & str_starts(desc_ur, "Oficina de Representación Federal del Trabajo en") ~ str_replace_all(desc_ur, "Oficina de Representación Federal del Trabajo en", "Representación en"),
      T ~ desc_ur
    ),
    desc_ur = str_trim(desc_ur),
    desc_ur = str_replace_all(desc_ur,"[:whitespace:]", "_"),
    desc_ur = str_replace_all(desc_ur,"_", " "),
    desc_ur = toupper(desc_ur),
    desc_ur = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", desc_ur, perl=TRUE),
    desc_ur = str_replace_all(desc_ur, "BELIZE", "BELICE")
  ) %>% 
  select(ciclo, id_ramo, desc_ramo, desc_ur, monto_comparativo) %>% 
  group_by(ciclo, id_ramo, desc_ramo, desc_ur) %>% 
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


for(i in c(1:25, 28:length(id_ramo_loop))){
  a <- ur_ramos_pef2020apr_ppef2021 %>% 
    filter(id_ramo==id_ramo_loop[i], 
           abs(monto_diferencia) > 10)
  
  
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
    caption = paste0("Nota: no se tomaron en cuenta aquellos ramos cuyo monto absoluto de diferencia haya sido menor a 10 MDP\n",
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

a <- ur_ramos_pef2020apr_ppef2021 %>% 
  filter(id_ramo==id_ramo_loop[26])


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
  caption = fiuffi,
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


a <- ur_ramos_pef2020apr_ppef2021 %>% 
  filter(id_ramo==id_ramo_loop[27])

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
  caption = fiuffi,
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


# ** Modificado2020T02 vs Proyecto2021 ----
ur_ramos_pef2020mod_ppef2021 <- ur_ramos %>% 
  filter(ciclo != 2019) %>% 
  filter(filter_ramos_apf(id_ramo)) %>% 
  mutate(
    monto_comparativo = case_when(
      is.na(monto_proyecto) ~ monto_modificado,
      T ~ monto_proyecto
    ),
    desc_ur = case_when(
      id_ramo =="14" & str_starts(desc_ur, "Delegación Federal del Trabajo en") ~ str_replace_all(desc_ur, "Delegación Federal del Trabajo en", "Representación en"),
      id_ramo =="14" & str_starts(desc_ur, "Oficina de Representación Federal del Trabajo en") ~ str_replace_all(desc_ur, "Oficina de Representación Federal del Trabajo en", "Representación en"),
      T ~ desc_ur
    ),
    desc_ur = str_trim(desc_ur),
    desc_ur = str_replace_all(desc_ur,"[:whitespace:]", "_"),
    desc_ur = str_replace_all(desc_ur,"_", " "),
    desc_ur = toupper(desc_ur),
    desc_ur = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", desc_ur, perl=TRUE),
    desc_ur = str_replace_all(desc_ur, "BELIZE", "BELICE")
  ) %>% 
  select(ciclo, id_ramo, desc_ramo, desc_ur, monto_comparativo) %>% 
  group_by(ciclo, id_ramo, desc_ramo, desc_ur) %>% 
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



id_ramo_loop <- unique(ur_ramos_pef2020mod_ppef2021$id_ramo)

fiuf <- "Variación del presupuesto para unidades responsables del gasto de "
fiuff <- "Monto modificado en PEF2020 (avance del gasto al T02) vs Monto de proyecto en PPEF 2021"


for(i in c(1:25, 28:length(id_ramo_loop))){
  a <- ur_ramos_pef2020mod_ppef2021 %>% 
    filter(id_ramo==id_ramo_loop[i], 
           abs(monto_diferencia) > 10)
  
  
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
    caption = paste0("Nota: no se tomaron en cuenta aquellos ramos cuyo monto absoluto de diferencia haya sido menor a 10 MDP\n",
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
  ggsave(filename = paste0(out, "01_APF/07_", unique(a$id_ramo), "_pef2020mod_ppef2021.png"),
         width = 30, height = 50, dpi = 100, limitsize = FALSE)
}

a <- ur_ramos_pef2020mod_ppef2021 %>% 
  filter(id_ramo==id_ramo_loop[26])


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
  caption = fiuffi,
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
ggsave(filename = paste0(out, "01_APF/07_", unique(a$id_ramo), "_pef2020mod_ppef2021.png"),
       width = 30, height = 50, dpi = 100, limitsize = FALSE)


a <- ur_ramos_pef2020mod_ppef2021 %>% 
  filter(id_ramo==id_ramo_loop[27])

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
  caption = fiuffi,
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
ggsave(filename = paste0(out, "01_APF/07_", unique(a$id_ramo), "_pef2020mod_ppef2021.png"),
       width = 30, height = 50, dpi = 100, limitsize = FALSE)


