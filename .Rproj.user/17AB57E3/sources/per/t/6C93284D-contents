# setup do script ---------------------------------------------------------


list.of.packages <- c("tidyverse","readxl","dplyr","tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)})




# Funcoes -----------------------------------------------------------------
filtro <- function(df){
  # OBS: 
  # Seleciona os paises que tem dados balanceados
  # É necessário que a coluna com os paises tenha o nome de "Pais"
  
  lista_paises <- as.character(unique(df$Pais))
  paises_completos <- vector(mode = "character",length = length(lista_paises))
  
  for(i in seq_along(lista_paises)){
    pais_atual <- df %>% 
      filter(Pais == lista_paises[i])
    
    if(any(is.na(pais_atual))==F){
      paises_completos[i] <- lista_paises[i]
    } else {
      paises_completos[i] <- NA
    }
  }
  dados <- df %>% 
    filter(Pais %in% paises_completos)
  return(dados)
}




# Instituições ------------------------------------------------------------

## fonte: http://knoema.com/GII2017Jun/global-innovation-index


gii <- as_tibble(read.csv("Dados/institutions.csv",
                          header = T,sep = ';',dec = '.'))
gii <- gii[c(1:1272),-3]
colnames(gii) <- c("Pais","Variavel",2013:2017)

## verificando quais paises estão completos

gii <- filtro(gii) %>% 
  gather(key = Ano,value = valor,-c(Pais,"Variavel")) %>% 
  spread(key = Variavel,value = valor)


# Percepção de corrupção --------------------------------------------------


## fonte: http://knoema.com/CPI2014/corruption-perceptions-index-by-transparency-international

ipc <- as_tibble(read.csv("Dados/cpi.csv",
                          header = T,dec = '.',sep = ';'))
colnames(ipc) <- c("sigla","Pais",2011:2017)

ipc <- filtro(ipc) %>% 
  gather(key = Ano,value = ipc,-c(sigla,Pais))

# IDH ---------------------------------------------------------------------

## fonte: http://knoema.com/HDREPT2016/human-development-report

idh <- as_tibble(read.csv("Dados/idh.csv",
                          header = T,sep=';',dec = '.'))
colnames(idh) <- c("Pais",2008:2015)
idh <- filtro(idh) %>% 
  mutate(`2016` = (1/3)*`2015`+(1/3)*`2014`+(1/3)*`2013`,
         `2017` = (1/3)*`2016`+(1/3)*`2015`+(1/3)*`2014`) %>% 
  gather(key = Ano, value = idh,-Pais)



# juntando dados ----------------------------------------------------------

data_set <- inner_join(x = ipc,y = gii,c("Pais","Ano"))
data_set <- inner_join(data_set, idh,c("Pais","Ano"))


# liberdade de imprensa ---------------------------------------------------
press_freedom <- readxl::read_xlsx(path = "Dados/press freedom.xlsx") %>% 
  select(Country, `2013`,`2014`, `2015`,`2016`,`2017`) %>% 
  gather(key = "Ano",
         value = "press_freedom",
         -Country)

data_set %>% 
  mutate(Country = sigla) %>% 
  left_join(press_freedom,
            by = c("Country","Ano")) %>% 
  select(-c(Country,`2`,`3`)) %>% 
  rename(politico = `1.1.`,
         regulatorio = `1.2.`,
         negocio = `1.3.`,
         sof_mercado = `4`,
         sof_negocio = `5`, 
         imprensa = press_freedom) -> dados_tcc

# salvando csv ------------------------------------------------------------
write_csv(x = dados_tcc,
          path = "Dados/dados_tcc.csv")

