##
## Baixa dados do SUS, PNS (IBGE) e PISA
##

# carrega variáveis e funções ---------------------------------------------
source("codigos/00_config.R")

# variáveis ---------------------------------------------------------------

ftp_datasus <-  "ftp.datasus.gov.br"

##
## URLs dos diretórios FTP, regex para baixar só os arquivos de interesse e
## e diretório onde salvar cada arquivo do SUS
##
bds_sus <- list("SIM" = c("dir_local" = dir_sim_dbc,
                          "dir_ftp" = "/dissemin/publicos/SIM/CID10/DORES/",
                          "regex" = "^DO(?!BR)..2"),
                "SINAN" = c("dir_local" = dir_sinan_dbc,
                            "dir_ftp" = "/dissemin/publicos/SINAN/DADOS/FINAIS/",
                            "regex" = "^VIOL"),
                "SIHSUS" = c("dir_local" = dir_sih_dbc,
                             "dir_ftp" = "/dissemin/publicos/SIHSUS/200801_/Dados/",
                             "regex" = "^RD"),
                "SIASUS" = c("dir_local" = dir_sia_dbc,
                             "dir_ftp" = "/dissemin/publicos/SIASUS/200801_/Dados/",
                             "regex" = "^PS"),
                "IBGE" = c("dir_local" = dir_ibge_dbc, 
                           "dir_ftp" = "/dissemin/publicos/IBGE/projpop/",
                           "regex" = "^projbr"))

# URLs para arquivo da Pesquina Nacional de Saúde no IBGE 
pns_2013_url <- "http://ftp.ibge.gov.br/PNS/2013/Microdados/Dados/PNS_2013.zip"
pns_2019_url <- "https://ftp.ibge.gov.br/PNS/2019/Microdados/Dados/PNS_2019_20220525.zip"


# No Pisa alguns anos tem arquivos separados para os dados e para o dicionário
bd_pisa <- list("2000" = c("dado" = "https://www.oecd.org/pisa/pisaproducts/intstud_math.zip", 
                           "dicionario" = "https://www.oecd.org/pisa/pisaproducts/PISA2000_SPSS_student_mathematics.txt"),
                "2003" = c("dado" = "https://www.oecd.org/pisa/pisaproducts/INT_stui_2003_v2.zip", 
                           "dicionario" = "https://www.oecd.org/pisa/pisaproducts/PISA2003_SPSS_student.txt"),
                "2012" = c("dado" = "https://www.oecd.org/pisa/pisaproducts/INT_STU12_DEC03.zip", 
                           "dicionario" = "https://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_student.txt"),
                "2015" = c("dado" = "https://webfs.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_STU_QQQ.zip"),
                "2018" = c("dado" = "https://webfs.oecd.org/pisa2018/SPSS_STU_QQQ.zip"),
                "2022" = c("dado" = "https://webfs.oecd.org/pisa2022/STU_QQQ_SPSS.zip"))

##
## OpçÕes de como fazer o download
##
options(download.file.method = "libcurl", 
        extra = paste0("--header='User-Agent: Mozilla/5.0 ", 
                       "(Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ", "
                       (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3'"))
options(timeout = 100000)


# sus ---------------------------------------------------------------------
for(bd in names(bds_sus)){
  
  cat(bd, "\n")
  
  bd_info <- bds_sus[[bd]]
  
  dlocal <- bd_info["dir_local"]
  dftp <- bd_info["dir_ftp"] %>% 
    paste0(ftp_datasus, .)
  rgx <- bd_info[["regex"]]
  
  # Lista os arquivos do bo banco de dados
  ftp_files <- dftp %>% 
    getURL(ftp.use.epsv = TRUE) %>% 
    str_split("\n") %>% 
    unlist %>% 
    discard(equals, "") %>% 
    str_split("\\s+") %>% 
    unlist() %>% 
    # Filtra para a regex
    keep(str_detect, rgx)
  
  # Baixa os arquivos
  n_files <- length(ftp_files)
  for(i_ff in seq_len(n_files)){
    ff <- ftp_files[[i_ff]]
    
    glue("    |- {ff} ({i_ff}/{n_files})") %>% 
      cat("\n")
    
    url_path <- glue("{dftp}/{ff}")
    local_path <- glue("{dlocal}/{ff}")
    
    if(file.exists(local_path)) {
      cat("        |- Já baixado\n")
      next
    }
    download.file(url_path, local_path)
  }
}



# baixa dados do PNS ------------------------------------------------------

path_pns_zip_2013 <- glue("{dir_pns}/2013.zip")
if(!file.exists(path_pns_zip_2013)){
  download.file(pns_2013_url, destfile = path_pns_zip_2013)
  unzip(path_pns_zip_2013, exdir = dir_pns)
}

path_pns_zip_2019 <- glue("{dir_pns}/2019.zip")
if(!file.exists(path_pns_zip_2019)){
  download.file(pns_2019_url, destfile = path_pns_zip_2019)
  unzip(path_pns_zip_2013, exdir = dir_pns)
}

# baixa dados do PISA -----------------------------------------------------
for(ano in names(bd_pisa)){
  
  files_ano <- bd_pisa[[ano]]
  
  n_files <- length(files_ano)
  
  
  if(n_files == 1){
    
    zip_file <- glue("{dir_pisa}/{ano}.zip")
    
    check_file <- list.files(dir_pisa, 
                             pattern = glue("{ano}(_\\d)?\\.sav")) %>% 
      length() %>% 
      as.logical()
    
    if(check_file) next
    
    download_with_retry(files_ano, zip_file)
    files_unziped <- unzip(zip_file, exdir = dir_pisa) %>%
      keep(str_detect, "sav$")
    n_files_uz <- length(files_unziped)
    for(i_fuz in seq_len(n_files_uz)){
      fuz <- files_unziped[i_fuz]
      fuz_novo_nome <- ifelse(n_files_uz == 1, 
                              glue("{ano}.sav"),
                              glue("{ano}_{i_fuz}.sav")) %>%
        paste0(dir_pisa,  .)
      file.rename(fuz, fuz_novo_nome)
    }
    file.remove(zip_file)
  } else { 
    # Baixa microdados
    zip_dados <- glue("{dir_pisa}/{ano}.zip")
    md_txt <- glue("{dir_pisa}/{ano}.txt")
    if(!file.exists(md_txt)){
      dado_url <- files_ano[["dado"]]
      download_with_retry(url = dado_url, destfile = zip_dados)
      fuz <- unzip(zip_dados, exdir = dir_pisa)
      file.remove(zip_dados)
      file.rename(fuz, md_txt)
    
    }
    # Baixa o dicionario
    txt_dic <- glue("{dir_pisa}/{ano}_dic.txt")
    if(!file.exists(txt_dic)){
      dic_url <- files_ano[["dicionario"]]
      download_with_retry(dic_url, txt_dic) 
    }
  }
}