source("codigos/00_config.R")

tbl_pop <- dir_ibge %>%
  list.files(full.names = TRUE, recursive = TRUE) %>%
  map(fread) %>%
  bind_rows() %>%
  # O código da faixa etária da projeção é:
  # * Primeiro caractere: unidade - Sempre "A" --> Anos
  # * Segundo e terceiro dígito: idade de início da faixa etária
  # * Quarto e quinta dígito: idade fim da faixa etária
  # Em todos casos, menos na faixa "A9099" o início e fim da faixa etária é o mesmo
  # Como todo intervalo de "A9099"  vai no última classe que estamos criando
  # não é um problema tratar todo mundo como tendo 90 anos
  mutate(idade = substring(FXETARIA, 2, 3)) %>%
  mutate(idade = as.numeric(idade)) %>%
  # Idades menores que 10 anos não serão analisadas e aqui são removidas
  filter(idade > 9) %>%
  mutate(faixa_etaria = cut(idade, breaks = age_ranges, labels = age_labels,
                            include.lowest = TRUE))  %>%
  group_by(ANO, faixa_etaria, SEXO) %>%
  summarise(pop = sum(POPULACAO), .groups = "keep") %>%
  ungroup() %>%
  rename_all(tolower)
