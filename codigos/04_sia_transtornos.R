##
## Calcula a taxa por 100 mil de transtornos selecionados por faixa etárias 
## e sexo ao longo do tempo
##


# carrega variáveis e funções ---------------------------------------------
source("codigos/00_config.R")
source("codigos/00_tbl_pop.R")

# variáveis ---------------------------------------------------------------
nome_tabela <- "atendimentos_transtornos_sus"

# lê as tabelas -----------------------------------------------------------

# Arquivos do SIA que começam com PS: ARQUIVO DE RAAS – PSICOSSOCIAL

# CNS_PAC: Carteira Nacional de Saúde do Paciente (criptografia)
# DT_ATEND: Mês do Atendimento (AAAAMM)
# TPIDADEPAC: Tipo da Idade do Paciente em anos, meses ou dias.
#             Calculado a partir da data de nascimento (4 = anos)
# IDADEPAC: Idade do Paciente em anos
# SEXOPAC: Sexo do Paciente
# CIDPRI: CID Principal
sia <- dir_sia_csv %>% 
  list.files(full.names = TRUE, recursive = TRUE, pattern = "^PS") %>% 
  map(fread, 
      select = c("CNS_PAC", "DT_ATEND", "TPIDADEPAC", 
                 "IDADEPAC", "SEXOPAC", "CIDPRI"), 
      colClasses = "character") %>% 
  bind_rows()


# formata e filtra --------------------------------------------------------
sia <- sia %>% 
  mutate(IDADEPAC = as.numeric(IDADEPAC)) %>% 
  filter(TPIDADEPAC == "4") %>% 
  filter(IDADEPAC > 9) %>% 
  mutate(transtorno = case_when(grepl("F4[01]", CIDPRI) ~ "Ansiedade",
                                grepl("F3([23]|41)", CIDPRI) ~ "Depressão",
                                grepl("F90", CIDPRI) ~ "TDAH",
                                grepl("F42", CIDPRI) ~ "TOC",
                                grepl("F31", CIDPRI) ~ "Bipolar",
                                grepl("F50", CIDPRI) ~ "Distúrbio alimentar",
                                grepl("F2[015]", CIDPRI) ~ "Esquizofrenia",
                                TRUE ~ "Outras")) %>% 
  mutate(ano = substring(DT_ATEND, 1, 4)) %>%
  mutate(ano = as.integer(ano)) %>% 
  mutate(faixa_etaria = cut(IDADEPAC, breaks = age_ranges, labels = age_labels, 
                            include.lowest = TRUE)) %>% 
  rename(sexo = SEXOPAC) %>% 
  distinct(transtorno, CNS_PAC, ano, .keep_all = TRUE) %>% 
  # O primeiro e último ano estão incompletos
  filter(!ano %in% range(ano)) %>% 
  filter(transtorno != "Outras")


# tabula ------------------------------------------------------------------
tbl <- tabula_sus(df_sus = sia,
                  df_pop = tbl_pop, 
                  variaveis = c("faixa_etaria", "ano", "transtorno"))

# plota -------------------------------------------------------------------
tbl_formatado_plot <- tbl %>% 
  mutate(sexo = recode(sexo,  
                       "F" =  "Mulheres", 
                       "M" =  "Homens", 
                       "todos" = 'Homens + Mulheres')) %>% 
    mutate(ano = as.factor(ano))

# Taxa por 100 mil
tbl_formatado_plot %>% 
  ggplot(aes(x = ano, y = tx_100mil, colour = transtorno, group = transtorno)) + 
  geom_line() + 
  geom_point() + 
  scale_color_brewer(palette = "Dark2")  + 
  facet_wrap(~faixa_etaria + sexo) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  ylab(label = c("Taxa de pacientes atendidos por transtorno\n(por 100 mil)")) +
  xlab("Ano") 
ggsave(glue("{dir_graficos}/{nome_tabela}_tx.png"))

# Variação
tbl_formatado_plot %>% 
  ggplot(aes(x = ano, y = aumento, 
             colour = transtorno, group = transtorno)) + 
  geom_line() + 
  geom_point() + 
  scale_color_brewer(palette = "Dark2")  +
  facet_wrap(~faixa_etaria + sexo) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  ylab(label = glue("Variação desde {min(tbl$ano)}")) +
  xlab("Ano")
ggsave(glue("{dir_graficos}/{nome_tabela}_aumento.png"))


# salva -------------------------------------------------------------------
path_tabela <- glue("{dir_tabelas}/{nome_tabela}.csv")
write.csv(tbl, path_tabela, row.names = FALSE)