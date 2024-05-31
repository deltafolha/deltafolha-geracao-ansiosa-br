# Códigos utilizados para análises da matéria _Registros de ansiedade entre crianças e jovens superam os de adultos pela 1ª vez_  na Folha de São Paulo


A matéria adaptou análises presentes no livro "The Anxious Generation" de Jonathan Haidt para o Brasil, utilizando dados do SUS, IBGE e Pisa.

Na pasta `dados > processados` encontram-se as tabelas e gráficos com os resultados da análise.

Os dados brutos e intermediários não estão aqui porque são muito pesados, mas podem ser baixados com os códigos:

* `codigos > 01_baixa_dados.R`
* `codigos > 02_dados_sus_em_csv.R`

Abaixo, um resumo da metodologia para cada análise e os códigos em que foram realizadas:

## 1. Sentimento de pertencimento (Pisa/OCDE) - código 08_pisa.R:

A Organização para a Cooperação e Desenvolvimento Econômico (OCDE) define "sentimento de pertencimento" como a "necessidade de formar e manter pelo menos um número mínimo de relações interpessoais baseadas em confiança, aceitação, amor e apoio"[*](https://www.oecd-ilibrary.org/sites/d69dc209-en/index.html?itemId=/content/component/d69dc209-en). Para medir essa variável nos estudos do Pisa, utilizam-se seis perguntas:

1. Sinto-me só na escola.
2. Os outros alunos parecem gostar de mim.
3. Sinto-me como um estranho e que não faço parte do grupo na escola.
4. Sinto que faço parte do grupo na escola.
5. Faço amigos com facilidade na escola.
6. Sinto-me como um estranho (ou deixam-me fora de tudo) na escola.

As respostas possíveis são:

* Concordo plenamente
* Concordo
* Discordo
* Discordo plenamente

1. Respostas inválidas ou não respondidas foram excluídas da análise.
2. Assim como no [estudo feito pela OCDE](https://www.oecd-ilibrary.org/sites/d69dc209-en/index.html?itemId=/content/component/d69dc209-en), calculamos a porcentagem de alunos que "concordam plenamente" ou "concordam" com cada afirmação em cada ano de aplicação do teste, exceto em 2006 e 2009.
3. A amostra foi ponderada pelo peso dos estudantes.

## 2. Distração por celulares (Pisa/OCDE) - código 08_pisa.R:

No Pisa de 2022, foi incluída a seguinte pergunta sobre distração nas aulas de Matemática:

Com que frequência as seguintes situações ocorrem nas suas aulas de Matemática?

* Os estudantes se distraem usando recursos tecnológicos (ex.: smartphones, sites, aplicativos).
* Os estudantes se distraem com outros estudantes que estão usando recursos tecnológicos (ex.: smartphones, sites, aplicativos).

As respostas possíveis eram:

* Em todas as aulas
* Em muitas aulas
* Em algumas aulas
* Nunca ou quase nunca

1. [Assim como no relatório da OCDE](https://www.oecd.org/pisa/PISA%202022%20Insights%20and%20Interpretations.pdf), em nossa análise reportamos a porcentagem de alunos que escolheram as três primeiras opções.
2. Desconsideramos respostas inválidas ou não respondidas.
3. A amostra foi ponderada pelo peso dos estudantes.

## 3. Prevalência de depressão (PNS/IBGE) - código 03_pns_depressao.R:

Foram utilizadas as duas edições da Pesquisa Nacional de Saúde (2013 e 2019). A pergunta que utilizamos aqui é:

> "Algum médico ou profissional de saúde mental (como psiquiatra ou psicólogo) já lhe deu o diagnóstico de depressão?"

Essa pergunta foi feita somente para moradores selecionados. Como para ser selecionado em 2013 era necessário ser maior de 18 anos, utilizamos esse mesmo recorte para 2019. Fizemos o grupo de 18 a 21 anos porque no livro esse gráfico é feito com alunos de universidades ("undergraduates"). A análise foi feita a partir dos microdados disponibilizados pelo IBGE. A amostra foi ponderada pelo peso do morador ("Peso do morador selecionado com calibração").

Essa metodologia é semelhante à publicada no artigo [Prevalência de depressão autorreferida no Brasil: Pesquisa Nacional de Saúde 2019 e 2013](https://www.scielo.br/j/ress/a/YJthwW4VYj6N59BjdS94FJM/?lang=pt#), publicado em 2022, que chegou a conclusão semelhante utilizando a faixa etária mais jovem de 18-29 anos.

## 4. Taxa de atendimento por transtornos psiquiátricos (SIA/SUS) - código 04_sia_transtornos.R:

Foram utilizados os microdados de procedimentos ambulatoriais do SUS que geraram um registro de acompanhamento, monitoramento e apoio aos processos de trabalho dos Centros de Atenção Psicossocial (RAAS - Psicossocial). Os transtornos foram classificados de acordo com o campo "CID Principal" (CID se refere à Classificação Internacional de Doenças - CID-10). Os códigos utilizados aqui foram (os caracteres se referem aos dois ou três primeiros dígitos do campo):

**Ansiedade**:
* F40 (Transtornos fóbico-ansiosos)
* F41 (Outros transtornos ansiosos)

**Depressão**:
* F32 (Episódios depressivos)
* F33 (Transtorno depressivo recorrente)
* F341 (Distimia)

**TDAH**:
* F90 (Transtornos hipercinéticos)

**Bipolar**:
* F31 (Transtorno afetivo bipolar)

**Distúrbio alimentar**:
* F50 (Transtornos da alimentação)

**TOC**:
* F42 (Transtorno obsessivo-compulsivo)

**Esquizofrenia**:
* F20 (Esquizofrenia)
* F21 (Transtorno esquizotípico)
* F25 (Transtornos esquizoafetivos)

Pacientes únicos por transtornos em cada ano foram filtrados removendo duplicidades dos campos "CNS_PAC" (número do Cartão Nacional de Saúde), do transtorno (como classificado acima) e do ano do atendimento (quatro primeiros dígitos do campo "DT_ATEND"). Isso foi feito para não contar múltiplas vezes o mesmo paciente que passou por vários atendimentos devido ao mesmo transtorno.

Foi tabulado o número de atendimentos de acordo com o sexo, ano do atendimento, faixa etária dos pacientes e transtorno. Esse número foi ponderado pela população da faixa etária segundo a projeção do IBGE de 2018 presente no site do Datasus para cada ano.

## 5. Atendimento de ferimentos auto infligidos (SINAN/SUS) - código 05_sinan_lesao_auto_infligida_atendimentos.R:

A partir de 2011, é compulsória a notificação de violência autoprovocada. Os casos são registrados no Sistema de Informação de Agravos de Notificação (SINAN). Utilizando os microdados para notificações de violências desse sistema, filtramos para casos de violências autoprovocadas (quando o campo "LES_AUTOP" = "1") e casos não recorrentes (quando "OUT_VEZES" != "1").

Foi tabulado o número de atendimentos de acordo com o sexo, data da ocorrência e faixa etária dos pacientes, e foi ponderado pela população da faixa etária segundo a projeção do IBGE presente no site do Datasus para cada ano.

## 6. Internação de ferimentos auto infligidos (SINAN/SUS) - código 06_sih_lesao_auto_infligida_internacao.R:

A análise partiu dos microdados do Sistema de Informação Hospitalar do SUS. Foram considerados casos em que o CID-10 da causa primária ou secundária tinham códigos CID-10 de X60 a X84 (O uso de causa secundária é justificado pelas normas do SIH/SUS). Cada internação foi individualizada através do número da Autorização de Internação Hospitalar (AIH).

Foi tabulado o número de internações de acordo com o sexo, data da internação e faixa etária dos pacientes, e foi ponderado pela população da faixa etária segundo a projeção do IBGE presente no site do Datasus para cada ano.

## 7. Taxa de Suicídios (SIM/SUS) - código 07_sim_suicidio.R:

Tabulação feita a partir dos microdados do Sistema de Informação de Mortalidade (SIM) do SUS. Foram filtrados casos onde a causa principal tinha códigos CID-10 de X60 a X84.

Foi tabulado o número de mortes de acordo com o sexo, data da ocorrência e faixa etária dos mortos, e foi ponderado pela população da faixa etária segundo a projeção do IBGE presente no site do Datasus para cada ano.

