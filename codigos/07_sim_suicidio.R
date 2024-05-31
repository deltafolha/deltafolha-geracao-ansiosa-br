source("codigos/00_config.R")
##
## Calcula a taxa por 100 mil de suicídio por  faixa etárias e sexo ao longo 
## dos anos
##

# carrega variáveis e funções ---------------------------------------------
source("codigos/00_config.R")
source("codigos/00_tbl_pop.R")

# variáveis ---------------------------------------------------------------
nome_tabela <- "suicidio_sus"


# lê a tabela -------------------------------------------------------------

## IDADE: Idade (primeiro dígito diz a unidade: 4 = anos; demais dígitos diz a
##        quantidade de unidade)
## CAUSABAS: Causa básica, conforme a Classificação Internacional de Doença 
##          (CID), 10a. Revisão
## DTOBITO: Data do óbito, no formato ddmmaaaa
## SEXO: Sexo (Nominal, com as seguintes classificações: Masculino; Feminino; 
##            Ignorado)
sim <- dir_sim_csv %>% 
  list.files(full.names = TRUE, recursive = TRUE) %>%
  map(fread, 
      select = c("IDADE", "CAUSABAS", "DTOBITO", "SEXO"), 
      colClasses = "character") %>%  
  bind_rows()



# formata -----------------------------------------------------------------
sim <- sim %>% 
  filter(str_starts(IDADE, "4")) %>% 
  filter(str_starts(CAUSABAS, "X(6[0-9]|7[0-9]|8[0-4])")) %>% 
  mutate(idade = substring(IDADE, 2)) %>% 
  mutate(idade = as.numeric(idade)) %>% 
  filter(idade > 9) %>%
  mutate(ano = ifelse(nchar(DTOBITO) == 4, DTOBITO, substring(DTOBITO, 5))) %>%
  mutate(ano = as.numeric(ano)) %>% 
  mutate(faixa_etaria = cut(idade, breaks = age_ranges, labels = age_labels, 
                            include.lowest = TRUE)) %>% 
  mutate(sexo = case_when(SEXO == 1 ~ "M", 
                          SEXO == 2 ~ "F", 
                          TRUE ~ NA))

# tabula ------------------------------------------------------------------
tbl <- tabula_sus(df_sus = sim,
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
  ylab(label = c("Taxa de suicídio (por 100 mil))")) +
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
