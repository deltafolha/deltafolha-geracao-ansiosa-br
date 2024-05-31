##
## Calcula a porcentagem e os intervalos de confiança das pessoas que disseram
## terem sido diagnosticadas com depressão na PNS
##


# carrega variáveis e funções ---------------------------------------------
source("codigos/00_config.R")

# variáveis ---------------------------------------------------------------

# Nesse caso vamos usar uma faixa etária diferente da usada nas análises do SUS
# porque a pergunta de interesse só foi feita ara pessoas com mais de 18 anos
age_ranges <- c(18, 21, Inf)
age_labels <- c("18 a 21 anos", "22 anos ou mais")

nome_tabela <- "depressao_pns"

path_pns_microdados_2013 = glue("{dir_pns}/PNS_2013.txt")
path_pns_dicionario_2013 = glue("{dir_pns}/input_PNS_2013.txt")

path_pns_microdados_2019 = glue("{dir_pns}/PNS_2019.txt")
path_pns_dicionario_2019 = glue("{dir_pns}/input_PNS_2019.txt")


# le os microdados --------------------------------------------------------
microdados_2013 <- le_microdados_pns(path_pns_microdados_2013, 
                                     path_pns_dicionario_2013, 
                                     age_ranges, age_labels) 
microdados_2019 <- le_microdados_pns(path_pns_microdados_2019, 
                                     path_pns_dicionario_2019, 
                                     age_ranges, age_labels)

# formata -----------------------------------------------------------------
design_2013 <- svydesign(ids = ~UPA_PNS, 
                         strata = ~stratum, 
                         weights = ~peso, 
                         data = microdados_2013, 
                         nest = TRUE)

design_2019 <- svydesign(ids = ~UPA_PNS, 
                         strata = ~stratum, 
                         weights = ~peso, 
                         data = microdados_2019, 
                         nest = TRUE)

# análise -----------------------------------------------------------------
tbl_2013 <- processa_survey_pns(design_2013, 2013)
tbl_2019 <- processa_survey_pns(design_2019, 2019)

tbl_2013_sexo <- processa_survey_pns_sexo(design_2013, 2013)
tbl_2019_sexo <- processa_survey_pns_sexo(design_2019, 2019)

# formata resultado -------------------------------------------------------
tbl <- tbl_2013 %>% 
  bind_rows(tbl_2019) %>% 
  bind_rows(tbl_2013_sexo) %>% 
  bind_rows(tbl_2019_sexo) %>% 
  select(faixa_etaria, sexo, ano, pct_depressao, 
         intervalo_de_confianca_inferior,  
         intervalo_de_confianca_superior) %>% 
  group_by(faixa_etaria, sexo) %>% 
  mutate(aumento = pct_depressao %>% 
           subtract(keep(pct_depressao, ano == min(ano))) %>% 
           divide_by(keep(pct_depressao, ano == min(ano))) %>% 
           multiply_by(100)) %>% 
  ungroup()


# plota -------------------------------------------------------------------
tbl_formatado_plot <- tbl %>% 
  mutate(sexo = recode(sexo,  
                       "F" =  "Mulheres", 
                       "M" =  "Homens", 
                       "todos" = 'Homens + Mulheres')) %>% 
  mutate(ano = as.factor(ano))


tbl_formatado_plot %>%
  ggplot(aes(x = ano, y = pct_depressao, fill = faixa_etaria)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~sexo) + 
  xlab("Ano") + 
  ylab("Porcentagem de depressão (%)") + 
  labs(fill = "Faixa etária") +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal()

ggsave(glue("{dir_graficos}/{nome_tabela}_tx.png"))

tbl_formatado_plot %>%
  filter(ano == 2019) %>% 
  ggplot(aes(x = faixa_etaria, y = aumento, fill = sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Aumento em depressão por sexo ente 2013 - 2019",
       x = "Faixa etária", y = "Aumento (%)",
       fill = "Sexo") +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal()

ggsave(glue("{dir_graficos}/{nome_tabela}_aumento.png"))


# salva -------------------------------------------------------------------
path_tabela <- glue("{dir_tabelas}/{nome_tabela}.csv")
write.csv(tbl, path_tabela, row.names = FALSE)
