##
## Calcula a taxa por 100 mil de internações por lesões autoprovocadas por 
## faixa etárias e sexo ao longo dos anos
##


# carrega variáveis e funções ---------------------------------------------
source("codigos/00_config.R")
source("codigos/00_tbl_pop.R")

# variáveis ---------------------------------------------------------------
nome_tabela <- "lesao_autoinfligida_internacao"

# lê as tabelas -----------------------------------------------------------

# Arquivos RD: contém registros correspondentes a cada Autorização de Internação
# Hospitalar paga na Unidade da Federação no período, com os campos mais utilizados
sih <- dir_sih_csv %>% 
  list.files(full.names = TRUE, recursive = TRUE, pattern = "^RD") %>%   
  map(le_filtra_formata_sih) %>% 
  bind_rows() %>% 
  # Para não contar a mesma internação diversas vezes
  distinct(n_aih, .keep_all = TRUE) %>%
  mutate(ano = as.numeric(ano)) %>% 
  # A partir do momento que virou obrigatório reportar lesões autoinfligidas
  filter(ano >= 2011) %>%
  # Retira 2024 que está incompleto
  filter(ano != max(ano))

# tabula ------------------------------------------------------------------
tbl <- tabula_sus(df_sus = sih,
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
  ylab(label = c("Taxa de internação por lesão auto-provocadas (por 100 mil))")) +
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
