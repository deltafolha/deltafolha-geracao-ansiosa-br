##
## Converte os dados baixados do SUS que estão em dbc ou dbf em csv
##

# carrega variáveis e funções ---------------------------------------------
source("codigos/00_config.R")

# unzipa ------------------------------------------------------------------
paths_zips <- dir_dbc  %>%
  list.files(full.names = TRUE, recursive = TRUE, pattern = "zip$") 

for(z in paths_zips){
  dir_ex <- str_remove(z, "/[^/]*\\.zip$")
  unzip(z, exdir = dir_ex)
  file.remove(z)
}


# db[c ou f] para csv -----------------------------------------------------
paths_dbcf <- dir_dbc  %>%
  list.files(full.names = TRUE, recursive = TRUE, pattern = "db[cf]") 

n_dbcf <- length(paths_dbcf)

for(i_dbcf in seq_len(n_dbcf)){
  
  cat(i_dbcf, "/", n_dbcf, "\n", sep = "")
  
  path_dbcf <- paths_dbcf[i_dbcf]
  path_csv <- path_dbcf %>% 
    str_replace(dir_dbc, dir_csv) %>%
    str_replace("db[cf]$", "csv")
  
  if(file.exists(path_csv)) {
    cat("    |- arquivo já convertido\n")
    next
  }
  
  if(str_ends(path_dbcf, "f")){
    df <- read.dbf(path_dbcf, as.is = TRUE)
  } else {
    df <- read.dbc(path_dbcf, as.is = TRUE)
  }
  fwrite(df, path_csv, row.names = FALSE)
}