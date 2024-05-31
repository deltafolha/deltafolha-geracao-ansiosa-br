##
## Calcula a taxa por 100 mil de lesões autoprovocadas por faixa etárias 
## e sexo ao longo dos anos
##


# carrega variáveis e funções ---------------------------------------------
source("codigos/00_config.R")
source("codigos/00_tbl_pop.R")


# variáveis ---------------------------------------------------------------
nome_tabela <- "lesao_autoinfligida_atendimentos"

# lê a tabela -------------------------------------------------------------

# Arquivos do SINAN que começam com VIOL: Violência Interpessoal/Autoprovocada

# LES_AUTOP: Informar se a lesão foi autoprovocada  (1 = "Sim")
# OUT_VEZES: Informar se a violência é de repetição (1 = "Sim")
# NU_IDADE_N: Idade (primeiro dígito diz a unidade. 4 = "Anos", 
#             demais a quantidade da unidade)
# DT_NOTIFIC: Data de preenchimento da ficha de notificação.
# DT_OCOR: Data da ocorrência da violência (dd/mm/aaaa)
# CS_SEXO: Sexo do paciente 

viol <- dir_sinan_csv %>% 
  list.files(full.names = TRUE, recursive = TRUE, pattern = "^VIOL")  %>% 
  map(fread, 
      select = c("LES_AUTOP", "OUT_VEZES", "NU_IDADE_N", 
                 "DT_OCOR", "CS_SEXO"), 
      colClasses = "character") %>% 
  bind_rows()


# formata e filtra --------------------------------------------------------

lesao_autoprovocadas <-  viol %>% 
  # Somente lesões autoprovocadas
  filter(LES_AUTOP == 1) %>% 
  # Para evitar contar mais de uma vez o mesmo
  filter(OUT_VEZES != 1) %>% 
  drop_na(NU_IDADE_N) %>% 
  filter(NU_IDADE_N != "") %>% 
  filter(str_starts(NU_IDADE_N, "4")) %>% 
  mutate(idade = substring(NU_IDADE_N, 2)) %>% 
  mutate(idade = as.numeric(idade)) %>% 
  filter(idade > 9) %>% 
  mutate(faixa_etaria = cut(idade, breaks = age_ranges, labels = age_labels, 
                            include.lowest = TRUE)) %>% 
  mutate(ano = substring(DT_OCOR, 1, 4)) %>%
  mutate(ano = as.numeric(ano)) %>% 
  # A partir de 2011 é obrigatório reportar
  filter(ano >= 2011) %>% 
  rename(sexo = CS_SEXO) %>% 
  filter(sexo %in% c("M", "F"))


# tabula ------------------------------------------------------------------
tbl <- tabula_sus(df_sus = lesao_autoprovocadas,
                  df_pop = tbl_pop, 
                  variaveis = c("faixa_etaria", "ano"))


# plota -------------------------------------------------------------------
tbl_formatado_plot <- tbl %>% 
  mutate(sexo = recode(sexo,  
                       "F" =  "Mulheres", 
                       "M" =  "Homens", 
                       "todos" = 'Homens + Mulheres')) %>% 
  mutate(ano = as.factor(ano))

# Taxa por 100 mil
tbl_formatado_plot %>% 
  ggplot(aes(x = ano, y = tx_100mil, colour = sexo, group = sexo)) + 
  geom_line() + 
  geom_point() + 
  scale_color_brewer(palette = "Dark2")  + 
  facet_wrap(~faixa_etaria) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  ylab(label = c("Taxa de registro de lesão auto-provocadas (por 100 mil))")) +
  xlab("Ano") 
ggsave(glue("{dir_graficos}/{nome_tabela}_tx.png"))

# Variação
tbl_formatado_plot %>% 
  ggplot(aes(x = ano, y = aumento, 
             colour = sexo, group = sexo)) + 
  geom_line() + 
  geom_point() + 
  scale_color_brewer(palette = "Dark2")  +
  facet_wrap(~faixa_etaria) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  ylab(label = glue("Variação desde {min(tbl$ano)}")) +
  xlab("Ano")
ggsave(glue("{dir_graficos}/{nome_tabela}_aumento.png"))




# salva -------------------------------------------------------------------
path_tabela <- glue("{dir_tabelas}/{nome_tabela}.csv")
write.csv(tbl, path_tabela, row.names = FALSE)

