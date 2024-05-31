# pacotes -----------------------------------------------------------------
library(dplyr)
library(purrr)
library(data.table)
library(googlesheets4)
library(tidyr)
library(ggplot2)
library(magrittr)
library(stringr)
library(read.dbc)
library(foreign)
library(RCurl)
library(glue)
library(readr)
library(survey)
library(haven)
library(httr)
library(tidytext)

# variáveis ---------------------------------------------------------------
dir_dbc <- "dados/brutos/SUS/"
dir_csv <- "dados/interim/SUS/"
dir_ibge <- "dados/interim/SUS/IBGE/"

dir_sim_dbc <-glue("{dir_dbc}/SIM")
dir_sinan_dbc <- glue("{dir_dbc}/SINAN")
dir_sih_dbc <- glue("{dir_dbc}/SIHSUS")
dir_sia_dbc <- glue("{dir_dbc}/SIASUS")
dir_ibge_dbc <- glue("{dir_dbc}/IBGE")

dir_sim_csv <-glue("{dir_csv}/SIM")
dir_sinan_csv <- glue("{dir_csv}/SINAN")
dir_sih_csv <- glue("{dir_csv}/SIHSUS")
dir_sia_csv <- glue("{dir_csv}/SIASUS")
dir_ibge_csv <- glue("{dir_csv}/IBGE")

dir_pns <- "dados/brutos/PNS/"
dir_pisa <- "dados/brutos/PISA/" 

dir_graficos <- "dados/processados/graficos/"
dir_tabelas <- "dados/processados/tabelas/"

age_ranges <- c(10, 14, 19, Inf)
age_labels <- c("10 a 14 anos", "15 a 19 anos", "20 anos ou mais")




# cria diretorios ---------------------------------------------------------

c(dir_sim_dbc, dir_sinan_dbc, dir_sih_dbc, dir_sia_dbc, dir_ibge_dbc, 
  dir_sim_csv, dir_sinan_csv, dir_sih_csv, dir_sia_csv, dir_ibge_dbc, 
  dir_pns, dir_pisa, 
  dir_graficos, dir_tabelas) %>% 
  map(~ if(!dir.exists(.x)) dir.create(.x, recursive = TRUE))


# funções -----------------------------------------------------------------

##
## PNS
##
le_microdados_pns <- function(path_micro, path_dic, age_ranges, age_labels){
  
  dicionario <- path_dic %>% 
    readLines(encoding = "latin-1", warn = FALSE) %>% 
    keep(str_starts, "@") %>% 
    str_split("\\s{2,}|\t") %>% 
    map(magrittr::extract, c(2, 3)) %>% 
    map(setNames, c("names", "width")) %>% 
    bind_rows() %>% 
    mutate(width = str_match(width, "\\d+")) %>% 
    mutate(width = as.numeric(width))
  
  # Dicionário das variáveis:
  #     UPA_PNS - unidades primárias de amostragem
  #     C008 - Idade do morador na data de referência
  #     V00291 - Peso do morador selecionado com calibração
  #     V0024 - Estrato
  #     C006 - Sexos (1 = Hhomem; 2 = Mulher)
  #     Q092- Algum médico ou profissional de saúde mental 
  #           (como psiquiatra ou psicólogo) já lhe deu o diagnóstico 
  #            de depressão? (1 = Sim; 2 = Não)
  df <- path_micro %>% 
    read_fwf(col_positions = fwf_widths(widths = dicionario$width,
                                        col_names = dicionario$names), 
             col_select = c(UPA_PNS, C008, V00291, V0024, C006, Q092))  %>%
    rename(idade = C008,  peso = V00291, stratum = V0024, sexo = C006) %>% 
    filter(peso != ".") %>% 
    drop_na(peso) %>%
    mutate_at(vars(idade, peso), as.numeric) %>% 
    filter(idade >= 18) %>% 
    mutate(faixa_etaria = cut(idade, breaks = age_ranges, labels = age_labels, 
                              include.lowest = TRUE)) %>% 
    mutate(dignosticado_depressao = Q092 == 1)  %>% 
    select(-Q092)
  return(df)
}


processa_survey_pns <- function(design, ano) {
  tbl <- svyby(~dignosticado_depressao, ~faixa_etaria, 
               design, 
               svymean, 
               na.rm = TRUE, 
               vartype = "ci") %>%
    pivot_longer(cols = -faixa_etaria) %>%
    filter(grepl("TRUE", name)) %>%
    mutate(value = value * 100) %>%
    pivot_wider(names_from = "name", values_from = "value") %>%
    rename(pct_depressao = dignosticado_depressaoTRUE, 
           intervalo_de_confianca_superior = ci_u.dignosticado_depressaoTRUE,
           intervalo_de_confianca_inferior = ci_l.dignosticado_depressaoTRUE) %>%
    mutate(sexo = "todos",
           ano = ano)
  return(tbl)
}

processa_survey_pns_sexo <- function(design, ano) {
  tbl <- svyby(~dignosticado_depressao, ~faixa_etaria + sexo, 
               design, 
               svymean, 
               na.rm = TRUE, 
               vartype = "ci") %>%
    pivot_longer(cols = -c(faixa_etaria, sexo)) %>%
    filter(grepl("TRUE", name)) %>%
    mutate(value = value * 100) %>%
    pivot_wider(names_from = "name", values_from = "value") %>%
    rename(pct_depressao = dignosticado_depressaoTRUE, 
           intervalo_de_confianca_superior = ci_u.dignosticado_depressaoTRUE,
           intervalo_de_confianca_inferior = ci_l.dignosticado_depressaoTRUE) %>%
    mutate(sexo = ifelse(sexo == 1, "M", "F"),
           ano = ano)
  return(tbl)
}

##
## SIH
##
#     "Segundo as normas do SIH/SUS, as internações provocadas por causas externas 
#     devem ser classificadas, no diagnóstico principal, segundo o tipo de traumatismo,
#     ou seja, pelo capítulo XIX (causas S e T). No diagnóstico secundário, deve ser 
#     codificado segundo a origem da causa externa, ou seja, o que a provocou, 
#     utilizando-se, então o capítulo XX (causas V a Y). Existem situações em que 
#     é permitido que o diagnóstico principal seja classificado diretamento pelo
#     capítulo XX." 
# Fonte: http://tabnet.datasus.gov.br/cgi/sih/eidescr.htm
# Por isso vamos procurar no diagnóstico secundário o CID-10 de lesão 
# autroprovocada

# N_AIH: Número da AIH (Autorização de Internação Hospitalar)
# DT_INTER: Data de internação
# COD_IDADE:  Unidade de medida da idade (4 = anos)
# IDADE: Idade
# SEXO: Sexo do paciente
# DIAG_PRINC: Código do diagnóstico principal (CID10)
# DIAG_SECUN: Código do diagnóstico secundário (CID10)
# DIAGSEC1: Diagnóstico secundário 1

# Códigos CID-10 para lesão auto provocada
rgx_alesao <- "X6[0-9]|X7[0-9]|X8[0-4]"
le_filtra_formata_sih <- function(x){
  x %>% 
    fread(select = c("N_AIH", "DT_INTER", "COD_IDADE", "SEXO", "IDADE",
                     "DIAG_PRINC", "DIAG_SECUN", "DIAGSEC1"),
          colClasses = "character") %>%
    rename_all(tolower) %>%
    distinct(n_aih, .keep_all = TRUE) %>% 
    filter(cod_idade == "4") %>% 
    mutate(idade = as.numeric(idade)) %>% 
    filter(idade > 9) %>%
    filter_at(vars(starts_with("diag")), any_vars(str_detect(.,rgx_alesao))) %>% 
    mutate(faixa_etaria = cut(idade, breaks = age_ranges, labels = age_labels, 
                              include.lowest = TRUE)) %>% 
    mutate(ano = substring(dt_inter, 1, 4)) %>% 
    mutate(sexo = case_when(sexo == 1 ~ "M", 
                            sexo == 3 ~ "F")) %>% 
    select(n_aih, ano, sexo, faixa_etaria)
}


##
## PISA
##
le_microdados_pisa <- function(path_micro, path_dic){
  
  dic_lines <- readLines(path_dic, encoding = "latin-1")
  
  dic_start <- dic_lines %>%
    str_detect("DATA LIST") %>%
    which() %>% 
    add(1)
  
  dic_end <- dic_lines %>% 
    is_in(c("", ".")) %>% 
    which() %>% 
    keep(is_greater_than, dic_start) %>% 
    head(1) %>% 
    subtract(1)
  
  dic <- dic_lines %>% 
    magrittr::extract(c(dic_start:dic_end)) %>% 
    str_trim() %>% 
    keep(str_detect, "\\d+\\s*-\\s*\\d+")
  
  start_var <- str_extract(dic, "\\s+(\\d+).*-", group = 1) %>% 
    as.numeric()
  end_var <- str_extract(dic, "-.*?(\\d+)", group = 1) %>% 
    as.numeric() 
  names_var <- str_extract(dic, "^(.*?) ", group = 1) 
  
  
  micro <- read_fwf(path_micro, 
                    fwf_positions(start = start_var,
                                  end = end_var,
                                  col_names = names_var), 
                    show_col_types = FALSE)
}

recodifica_resposta_pisa <- function(x, ano, tipo){
  if(tipo == "belong"){
    if(ano == 2000){
      x <- x %>% 
        mutate(resposta =  case_when(resposta == 1 ~  "Discordo plenamente",
                                     resposta == 2 ~  "Discordo",
                                     resposta == 3 ~  "Concordo",
                                     resposta == 4 ~  "Concordo plenamente"))
    } else{
      x <- x %>% 
        mutate(resposta =  case_when(resposta == 1 ~  "Concordo plenamente",
                                     resposta == 2 ~  "Concordo",
                                     resposta == 3 ~  "Discordo",
                                     resposta == 4 ~  "Discordo plenamente"))
    }  
  } else {
    x <- x %>% 
      mutate(resposta =  case_when(resposta == 1 ~  "Toda aula",
                                   resposta == 2 ~  "Maioria das aulas",
                                   resposta == 3 ~  "Algumas aulas",
                                   resposta == 4 ~  "Nunca ou quase nunca"))
  }
  
  return(x)
}


##
## SUS
##
tabula_sus <- function(df_sus, df_pop, variaveis){
  
  # Tabulação por sexo
  tbl_por_sexo <- df_sus %>%
    drop_na(sexo) %>% 
    filter(sexo != "") %>% 
    group_by(across(all_of(c(variaveis, "sexo")))) %>% 
    summarise(n = n(), .groups = "keep") %>% 
    ungroup() %>% 
    # Pondera pela população
    left_join(tbl_pop, by = c("faixa_etaria", "ano", "sexo")) %>% 
    mutate(tx_100mil = n/pop * 100000)
  
  # Tabulacao todos
  tbl_todos <- tbl_por_sexo %>% 
    group_by(across(all_of(variaveis))) %>% 
    summarise(n = sum(n), 
              pop = sum(pop), .groups = "keep") %>% 
    ungroup() %>% 
    mutate(tx_100mil = n/pop * 100000) %>% 
    mutate(sexo = "todos") 
  
  # Junta
  tbl <- tbl_por_sexo %>% 
    bind_rows(tbl_todos)  %>% 
    select(all_of(variaveis), "sexo", "tx_100mil")
  
  # O quanto aumentou em cada groupo em cada ano
  tbl <- tbl %>% 
    group_by(across(all_of(c(keep(variaveis, variaveis != "ano"), 
                             "sexo")))) %>% 
    mutate(aumento = tx_100mil %>%
             subtract(keep(tx_100mil, ano == min(ano))) %>% 
             divide_by(keep(tx_100mil, ano == min(ano))) %>% 
             multiply_by(100)) %>% 
  ungroup() 
  
  return(tbl)
}

download_with_retry <- function(url, destfile, max_retries = 3) {
  for (attempt in 1:max_retries) {
    try_out <- try(download.file(url, destfile))
    if("try-error" %in% class(try_out)){
      Sys.sleep(10)
      next
    }
    break
  }
}

