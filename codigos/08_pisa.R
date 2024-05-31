source("codigos/00_config.R")

# variáveis -------------------------------------------
nome_tabela <- "pertencimento_a_escola_pisa"

# Em qual coluna está cada questão em cada ano
questoes = list("Sinto-me só na escola" = c(
  "2000" = "ST31Q06",
  "2003" = "ST27Q06",
  "2012" = "ST87Q06",
  "2015" = "ST034Q06TA",
  "2018" = "ST034Q06TA",
  "2022" = "ST034Q06TA"),
  "Os outros alunos parecem gostar de mim" = c(
    "2000" = "ST31Q05",
    "2003" =  "ST27Q05",
    "2012" =  "ST87Q05",
    "2015" =  "ST034Q05TA",
    "2018" =  "ST034Q05TA",
    "2022" =  "ST034Q05TA"),
  "Sinto-me como um estranho e que não faço parte do grupo na escola" =  c(
    "2000" =  "ST31Q04",
    "2003" =  "ST27Q04",
    "2012" =  "ST87Q04",
    "2015" =  "ST034Q04TA",
    "2018" =  "ST034Q04TA",
    "2022" =  "ST034Q04TA"),
  "Sinto que faço parte do grupo na escola" =  c(
    "2000" =  "ST31Q03",
    "2003" =  "ST27Q03",
    "2012" =  "ST87Q03",
    "2015" =  "ST034Q03TA",
    "2018" =  "ST034Q03TA",
    "2022" =  "ST034Q03TA"),
  "Faço amigos com facilidade na escola" =  c(
    "2000" =  "ST31Q02",
    "2003" =  "ST27Q02",
    "2012" =  "ST87Q02",
    "2015" =  "ST034Q02TA",
    "2018" =  "ST034Q02TA",
    "2022" =  "ST034Q02TA"),
  "Sinto-me como um estranho (ou deixam-me fora de tudo) na escola" = c(
    "2000" =  "ST31Q01",
    "2003" =  "ST27Q01",
    "2012" =  "ST87Q01",
    "2015" =  "ST034Q01TA",
    "2018" =  "ST034Q01TA",
    "2022" =  "ST034Q01TA"))



# análise por ano ---------------------------------------------------------
anos <- questoes %>% 
  map(names) %>% 
  unlist %>% 
  unique

# Lista que irá receper as tabulações de cada pergunta para cada país
tabulacoes <- list()

for(ano in anos){
  cat(ano, "\n")
  
  paths <- list.files(dir_pisa, full.names = TRUE, 
                      pattern = ano)
  

  n_files <- length(paths) 
  
  # Questões sobre pertencimento
  colunas_questoes <- questoes %>% 
    map(magrittr::extract(ano)) %>% 
    unlist() %>% 
    tolower()
  
  # Adiciona questões sobre celular se o ano for 2022
  if(ano == 2022){
    colunas_questoes <- colunas_questoes %>%
      c("Quão frenquente você se distrai com seu aparelho" =  "ST273Q06JA",
        "Quão frenquente você se distrai com aparelho dos outros" =  "ST273Q07JA") %>% 
      tolower()
  }
  
  ##
  ## Lê os microdados
  ##
  # Nos primeiros anos eram dois arquivos textos
  # Depois tem um ano que são dois arquivos sav (nesse caso as questões que 
  # queremos está no segundo). E daí para frente é só um arquivo sav por ano
  
  # Extensão dos arquivos
  ext <- str_remove(paths, ".*\\.") %>% 
    unique
  
  if(ext == "txt"){
    micro <- le_microdados_pisa(path_micro = paths[2], path_dic = paths[1])
  } else{
    if(n_files == 2){
      paths <- paths[2]
    }
    micro <- read_sav(paths, col_select = c(toupper(colunas_questoes),
                                            "CNT",
                                            starts_with("W_"))) %>% 
      mutate(across(everything(), as_vector))
  }
  micro <- rename_all(micro, tolower)
  
  
  # Faz a análise por pais
  paises <- micro$cnt %>% 
    as.vector() %>% 
    unique
  
  for(pais in paises){
    
    cat("    |-", pais, "\n")
    
    # Microdados do país
    micro_pais <- micro %>% 
      filter(cnt == pais)
    
    # Colunas com pesos para replicação
    colunas_rep <- colnames(micro_pais) %>% 
      keep(str_detect, "^w_.*\\d$") %>% 
      head(1) %>% 
      str_replace("\\d+$", "[0-9]+")
    
    
    micro_design_pais <- svrepdesign(
      data = micro_pais,
      weights = as.numeric(micro_pais$w_fstuwt),  
      repweights = colunas_rep,  
      type = "BRR")
    
    # Para cada questão....
    for(q_n in names(colunas_questoes)){
      
      # Quando a questão é sobre pertencimento aplica uma recodificação nas 
      # respostas (concorda ou não com a afirmação), quando é sobre aparelho
      # aplica outra codificação (quão frequente é a distração)
      if(q_n %in% names(questoes)){
        tipo <- "belong"
      } else {
        tipo <-  "aparelho"
      }
      q <- colunas_questoes[[q_n]]
      
      # Tabula as repostas pelo pais
      cat("        |-", q_n, "\n")
      tabulacao_q <- svytable(as.formula(glue("~{q}")) ,
                              micro_design_pais,
                              na.rm = TRUE) 
      
      # Alguns países em alguns anos não tiveram as perguntas de pertencimento
      if(nrow(tabulacao_q) == 0) next
      
      # formata
      tabulacao_q <- tabulacao_q %>% 
        as_tibble() %>%
        setNames(c("resposta", "n")) %>% 
        recodifica_resposta_pisa(ano, tipo) %>%
        mutate(p = n/sum(n) * 100) %>%
        mutate(pergunta = q_n) %>% 
        mutate(pais = pais) %>% 
        mutate(ano = ano) %>% 
        ungroup()  
      
      # Adiciona na lista
      tabulacoes[[length(tabulacoes) + 1]] <- tabulacao_q
    }
  } 
}



# tabulacao ---------------------------------------------------------------

# Agregamos todas as repostas "concordo" em só uma opção e agregamos todas as 
# respostas que disseram que se distraem nas aulas em uma reposta.
tbl <- tabulacoes %>% 
  bind_rows() %>% 
  drop_na(resposta) %>% 
  mutate(resposta = case_when(str_detect(resposta, "Discordo") ~ "Discordo",
                              str_detect(resposta, "Concordo") ~ "Concordo",
                              str_detect(resposta, "Toda") ~ "Me distraio",
                              str_detect(resposta, "Maioria") ~ "Me distraio",
                              str_detect(resposta, "Algumas") ~ "Me distraio",
                              str_detect(resposta, "Nunca") ~ "Não me distraio")) %>%
  group_by(resposta, pergunta, pais, ano) %>% 
  summarise(n = sum(n), .groups = "keep") %>% 
  ungroup() %>% 
  group_by(pergunta, pais, ano) %>% 
  mutate(p = n/sum(n) * 100) %>% 
  ungroup() %>%
  filter(resposta %in% c("Concordo", "Me distraio"))  



# plota -------------------------------------------------------------------

tbl_formatado_plot <- tbl %>% 
  mutate(ano = as.factor(ano))

# Senso de pertencimento ao longo do tempo
tbl_formatado_plot %>% 
  filter(resposta == "Concordo") %>%
  filter(pais == "BRA") %>%
  ggplot(aes(x = ano, y = p, colour = pergunta, group = pergunta)) + 
  geom_line() + 
  geom_point() + 
  scale_color_brewer(palette = "Dark2")  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  ylab(label = c("% que concordam))")) +
  xlab("Ano")

ggsave(glue("{dir_graficos}/{nome_tabela}_senso_de_pertencimento.png"))

# Uso de celular
tbl_formatado_plot %>% 
  filter(resposta == "Me distraio") %>% 
  ggplot(aes(y = reorder_within(pais, p, pergunta), x = p, 
             fill = ifelse(pais == "BRA", "green", "gray"))) +
  geom_bar(stat = "identity") +
  facet_wrap(~pergunta, scales = "free_y") +
  scale_y_reordered() +
  labs(y = "País") +  
  scale_fill_identity() + 
  theme_minimal() + 
  xlab("% que respondeu que se distraem pelo menos 'algumas vezes'")

ggsave(glue("{dir_graficos}/{nome_tabela}_distracao_aparelhos.png"))


# salva -------------------------------------------------------------------
path_tabela <- glue("{dir_tabelas}/{nome_tabela}.csv")
write.csv(tbl, path_tabela, row.names = FALSE)
