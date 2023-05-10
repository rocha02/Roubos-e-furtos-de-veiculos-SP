#carrega pacotes
library(tidyverse)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)

#Define a pasta onde est?o os arquivos
caminho_pasta <- "D:\\Dropbox\\Acad?micos\\Artigos e Apresenta??es\\ANPOCS 2020 - Roubos e furtos de ve?culos\\dados\\microdados_RV"
setwd(caminho_pasta)

#Abre todos os arquivos
rv2018_1 <- read.csv(file ="DadosBO_2018_1(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2018_2 <- read.csv(file ="DadosBO_2018_2(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2018_3 <- read.csv(file ="DadosBO_2018_3(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2018_4 <- read.csv(file ="DadosBO_2018_4(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2018_5 <- read.csv(file ="DadosBO_2018_5(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2018_6 <- read.csv(file ="DadosBO_2018_6(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2018_7 <- read.csv(file ="DadosBO_2018_7(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2018_8 <- read.csv(file ="DadosBO_2018_8(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2018_9 <- read.csv(file ="DadosBO_2018_9(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2018_10 <- read.csv(file ="DadosBO_2018_10(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2018_11 <- read.csv(file ="DadosBO_2018_11(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2018_12 <- read.csv(file ="DadosBO_2018_12(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2019_1 <- read.csv(file ="DadosBO_2019_1(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2019_2 <- read.csv(file ="DadosBO_2019_2(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2019_3 <- read.csv(file ="DadosBO_2019_3(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2019_4 <- read.csv(file ="DadosBO_2019_4(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2019_5 <- read.csv(file ="DadosBO_2019_5(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2019_6 <- read.csv(file ="DadosBO_2019_6(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2019_7 <- read.csv(file ="DadosBO_2019_7(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2019_8 <- read.csv(file ="DadosBO_2019_8(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2019_9 <- read.csv(file ="DadosBO_2019_9(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2019_10 <- read.csv(file ="DadosBO_2019_10(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2019_11 <- read.csv(file ="DadosBO_2019_11(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
rv2019_12 <- read.csv(file ="DadosBO_2019_12(ROUBO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)


#unifica todas as bases de RV( ta dando erro porque os bancos est?o com diferen?as de vari?veis)
base_RV_2018 <- rbind(rv2018_1,rv2018_2,rv2018_3,rv2018_4,rv2018_5,rv2018_6,rv2018_7,rv2018_8,rv2018_9,rv2018_10,rv2018_11,rv2018_12)
rm(rv2018_1,rv2018_2,rv2018_3,rv2018_4,rv2018_5,rv2018_6,rv2018_7,rv2018_8,rv2018_9,rv2018_10,rv2018_11,rv2018_12)

base_RV_2019_1 <- rbind(rv2019_1,rv2019_2,rv2019_3,rv2019_4,rv2019_5) 
base_RV_2019_1 <- base_RV_2019_1 %>% 
  mutate(
    PARENTESCO = 99,
    RELACIONAMENTO = 99,
    HORAOCORRENCIA = 99
  )

base_RV_2019_2 <- rbind(rv2019_6,rv2019_7)
base_RV_2019_2 <-base_RV_2019_2 %>% 
  mutate( HORAOCORRENCIA = 99)
base_RV_2019_3 <- rbind(rv2019_8,rv2019_9,rv2019_10,rv2019_11,rv2019_12)
rm(rv2019_1,rv2019_2,rv2019_3,rv2019_4,rv2019_5,rv2019_6,rv2019_7,rv2019_8,rv2019_9,rv2019_10,rv2019_11,rv2019_12)
base_RV_2019 <- rbind(base_RV_2019_1,base_RV_2019_2,base_RV_2019_3)
base_RV <- rbind(base_RV_2018,base_RV_2019)
rm(base_RV_2019_1,base_RV_2019_2,base_RV_2019_3,base_RV_2018,base_RV_2019)


#muda o caminho para consolidar os dados de FV
caminho_pasta <- "D:\\Dropbox\\Acad?micos\\Artigos e Apresenta??es\\ANPOCS 2020 - Roubos e furtos de ve?culos\\dados\\microdados_FV"
setwd(caminho_pasta)

fv2019_1 <- read.csv(file ="DadosBO_2019_1(FURTO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
fv2019_2 <- read.csv(file ="DadosBO_2019_2(FURTO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
fv2019_3 <- read.csv(file ="DadosBO_2019_3(FURTO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
fv2019_4 <- read.csv(file ="DadosBO_2019_4(FURTO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
fv2019_5 <- read.csv(file ="DadosBO_2019_5(FURTO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
fv2019_6 <- read.csv(file ="DadosBO_2019_6(FURTO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
fv2019_7 <- read.csv(file ="DadosBO_2019_7(FURTO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
fv2019_8 <- read.csv(file ="DadosBO_2019_8(FURTO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
fv2019_9 <- read.csv(file ="DadosBO_2019_9(FURTO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
fv2019_10 <- read.csv(file ="DadosBO_2019_10(FURTO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
fv2019_11 <- read.csv(file ="DadosBO_2019_11(FURTO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
fv2019_12 <- read.csv(file ="DadosBO_2019_12(FURTO DE VE?CULOS).csv", sep = ";", header = T, stringsAsFactors = F)
fv2018_1 <- read.delim("DadosBO_2018_1(FURTO DE VE?CULOS).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)
fv2018_2 <- read.delim("DadosBO_2018_2(FURTO DE VE?CULOS).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)
fv2018_3 <- read.delim("DadosBO_2018_3(FURTO DE VE?CULOS).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)
fv2018_4 <- read.delim("DadosBO_2018_4(FURTO DE VE?CULOS).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)
fv2018_5 <- read.delim("DadosBO_2018_5(FURTO DE VE?CULOS).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)
fv2018_6 <- read.delim("DadosBO_2018_6(FURTO DE VE?CULOS).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)
fv2018_7 <- read.delim("DadosBO_2018_7(FURTO DE VE?CULOS).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)
fv2018_8 <- read.delim("DadosBO_2018_8(FURTO DE VE?CULOS).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)
fv2018_9 <- read.delim("DadosBO_2018_9(FURTO DE VE?CULOS).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)
fv2018_10 <- read.delim("DadosBO_2018_10(FURTO DE VE?CULOS).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)
fv2018_11 <- read.delim("DadosBO_2018_11(FURTO DE VE?CULOS).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)
fv2018_12 <- read.delim("DadosBO_2018_12(FURTO DE VE?CULOS).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)

base_FV_2018 <- rbind(fv2018_1,fv2018_2,fv2018_3,fv2018_4,fv2018_5,fv2018_6,fv2018_7,fv2018_8,fv2018_9,fv2018_10,fv2018_11,fv2018_12)
rm(fv2018_1,fv2018_2,fv2018_3,fv2018_4,fv2018_5,fv2018_6,fv2018_7,fv2018_8,fv2018_9,fv2018_10,fv2018_11,fv2018_12)

base_FV_2019_1 <- rbind(fv2019_1,fv2019_2,fv2019_3,fv2019_4,fv2019_5) 
base_FV_2019_1 <- base_FV_2019_1 %>% 
  mutate(
    PARENTESCO = 99,
    RELACIONAMENTO = 99,
    HORAOCORRENCIA = 99
  )

base_FV_2019_2 <- rbind(fv2019_6,fv2019_7)
base_FV_2019_2 <-base_FV_2019_2 %>% 
  mutate( HORAOCORRENCIA = 99)
base_FV_2019_3 <- rbind(fv2019_8,fv2019_9,fv2019_10,fv2019_11,fv2019_12)
rm(fv2019_1,fv2019_2,fv2019_3,fv2019_4,fv2019_5,fv2019_6,fv2019_7,fv2019_8,fv2019_9,fv2019_10,fv2019_11,fv2019_12)
base_FV_2019 <- rbind(base_FV_2019_1,base_FV_2019_2,base_FV_2019_3)
base_FV <- rbind(base_FV_2018,base_FV_2019)
rm(base_FV_2019_1,base_FV_2019_2,base_FV_2019_3,base_FV_2018,base_FV_2019)

# Se tudo deu certo ao chegar aqui temos as 2 bases, de RV e de FV relativa aos anos de 2018 e 2019.
# Est? conclu?da a fase de importa??o dos dados.

#Cria uma ID a partir de 4 variaveis
base_FV <- base_FV %>% 
  unite(col = "ID",
        "ANO_BO","NUM_BO","DELEGACIA_NOME","PLACA_VEICULO",
        sep = "-", remove = FALSE) %>% 
        mutate (tipo = "FV")

base_RV <- base_RV %>% 
  unite(col = "ID",
        "ANO_BO","NUM_BO","DELEGACIA_NOME","PLACA_VEICULO",
        sep = "-", remove = FALSE)%>% 
  mutate (tipo = "RV")


# Junta as bases de RV e FV.
base_total <- rbind(base_FV,base_RV)


## Criar as colunas das macrorregi?es na base total

base_total <- base_total %>% 
  mutate(MACROREG = case_when(
    CIDADE %in% "S.PAULO" ~ "Capital",
    TRUE ~ "Interior"))
    
base_total <- base_total %>% 
  mutate(MACROREG = case_when(
    CIDADE %in% c("ARUJA", "BARUERI", "BIRITIBA-MIRIM", "CAIEIRAS", "CAJAMAR", "CARAPICUIBA", "COTIA", "DIADEMA", 
      "EMBU DAS ARTES", "EMBU-GUACU", "FERRAZ DE VASCONCELOS", "FRANCISCO MORATO", "FRANCO DA ROCHA", "GUARAREMA",
      "GUARULHOS", "ITAPECERICA DA SERRA", "ITAPEVI", "ITAQUAQUECETUBA", "JANDIRA", "JUQUITIBA", "MAIRIPORA",
      "MAUA", "OSASCO", "MOGI DAS CRUZES", "POA", "RIBEIRAO PIRES", "RIO GRANDE DA SERRA", "SALESOPOLIS", 
      "PIRAPORA BOM JESUS", "S.ISABEL", "SANTANA DE PARNAIBA", "S.ANDRE", "S.BERNARDO DO CAMPO", "SUZANO",
      "S.CAETANO DO SUL", "S.LOURENCO DA SERRA", "TABOAO DA SERRA", "VARGEM GRANDE PAULISTA") ~ "Regi?o Metropolitana", 
    TRUE ~ as.character(MACROREG)))

saveRDS(base_total, "base_total.rds")

# Cria base de ocorr?ncias a partir da base_total
base_ocorrencias <- base_total %>% 
  filter (RUBRICA %in% c("Furto (art. 155) - VEICULO", "Roubo (art. 157) - VEICULO",
                         "Furto qualificado (art. 155, ?4o.) - VEICULO","A.I.-Roubo (art. 157) - VEICULO",
                         "A.I.-Furto (art. 155) - VEICULO","A.I.-Furto qualificado (art. 155, ?4o.) - VEICULO")) 


# Remove as linhas com placa em branco
base_ocorrencias$PLACA_VEICULO[base_ocorrencias$PLACA_VEICULO == ""] <- NA

base_ocorrencias <- base_ocorrencias %>% 
  filter(!is.na(PLACA_VEICULO))

# Remove as linhas com BO Principal
base_ocorrencias <- base_ocorrencias %>% 
  filter (NUMERO_BOLETIM_PRINCIPAL == "")

# Remove os ID repetidos
base_ocorrencias <- base_ocorrencias[!duplicated(base_ocorrencias$ID), ]

saveRDS(base_ocorrencias, "base_ocorrencias.rds")

# Cria base de recupera??o  a partir da base_total
base_recuperacao <- base_total %>% 
  filter (RUBRICA %in% c("Localiza??o/Apreens?o e Entrega de ve?culo", "Localiza??o/Apreens?o e Entrega de ve?culo"))

# Remove as linhas com placa em branco                      
base_recuperacao$PLACA_VEICULO[base_recuperacao$PLACA_VEICULO == ""] <- NA

base_recuperacao <- base_recuperacao %>% 
  filter(!is.na(PLACA_VEICULO))

# Remove as linhas com BO Principal
base_recuperacao <- base_recuperacao %>% 
  filter (NUMERO_BOLETIM_PRINCIPAL == "")

# Remove os ID repetidos
base_recuperacao <- base_recuperacao[!duplicated(base_recuperacao$ID), ]

saveRDS(base_recuperacao, "base_recuperacao.rds")

#faz o join da subtra??o com a recupera??o
base_full <- left_join(base_ocorrencias, base_recuperacao, by = "PLACA_VEICULO")

saveRDS(base_full, "base_full.rds")

base_full <- readRDS("base_full.rds")
base_recuperacao <- readRDS("base_recuperacao.rds")
base_ocorrencias <- readRDS("base_ocorrencias.rds")

#Cria uma base s? com os casos em que temos roubo e recupera??o
base_sub_recup <- base_full %>% 
  filter(!is.na(ANO_BO.y))

base_sub_recup <- base_sub_recup %>% 
  

#Carrega a base cartografica de delegacias
#muda o caminho para puxar o shapefile
caminho_pasta <- "C:\\Users\\leonardo\\Desktop\\dados\\shape_dp_sp"
setwd(caminho_pasta)
base_DP_SP <- st_read("DPS_ESTADO_SP.shp", quiet = TRUE)
base_DP_SP %>%
  ggplot() +
  geom_sf()

# An?lise de frequ?ncias b?sicas para os bancos de ocorr?ncias e recupera??o

#Descri??o numero de ocorr?ncias e distribui??o

descricao <- base_ocorrencias %>% 
  group_by(tipo,MACROREG) %>% 
  summarise(n())

#Sobre o tipo de Veiculo

base_ocorrencias<- base_ocorrencias %>% 
  mutate(TIPO_VEI_CONSOLIDADO = case_when(
    DESCR_TIPO_VEICULO %in% "AUTOMOVEL" ~ "Autom?vel", 
    TRUE ~ as.character(DESCR_TIPO_VEICULO)))

base_ocorrencias<- base_ocorrencias %>% 
  mutate(TIPO_VEI_CONSOLIDADO = case_when(
    DESCR_TIPO_VEICULO %in% c("CAMINHONETE","CAMIONETA","UTILIT?RIO") ~ "Caminhonetes e utilit?rios", 
    TRUE ~ as.character(TIPO_VEI_CONSOLIDADO)))

base_ocorrencias<- base_ocorrencias %>% 
  mutate(TIPO_VEI_CONSOLIDADO = case_when(
    DESCR_TIPO_VEICULO %in% c("CAMINH?O TRATOR","CAMINH?O") ~ "Caminh?o", 
    TRUE ~ as.character(TIPO_VEI_CONSOLIDADO)))

base_ocorrencias<- base_ocorrencias %>% 
  mutate(TIPO_VEI_CONSOLIDADO = case_when(
    DESCR_TIPO_VEICULO %in% c("CICLOMOTO","MOTOCICLO","MOTONETA") ~ "Motocicletas e similares", 
    TRUE ~ as.character(TIPO_VEI_CONSOLIDADO)))

base_ocorrencias<- base_ocorrencias %>% 
  mutate(TIPO_VEI_CONSOLIDADO = case_when(
    DESCR_TIPO_VEICULO %in% c("INEXIST.","N?o Informado") ~ "N?o informado", 
    TRUE ~ as.character(TIPO_VEI_CONSOLIDADO)))

base_ocorrencias<- base_ocorrencias %>% 
  mutate(TIPO_VEI_CONSOLIDADO = case_when(
    DESCR_TIPO_VEICULO %in% c("MICRO-ONIBUS","ONIBUS") ~ "?nibus", 
    TRUE ~ as.character(TIPO_VEI_CONSOLIDADO)))

base_ocorrencias<- base_ocorrencias %>% 
  mutate(TIPO_VEI_CONSOLIDADO = case_when(
    DESCR_TIPO_VEICULO %in% c("MOTOR CASA","REBOQUE", "SEMI-REBOQUE", "SIDE-CAR", "TRATOR MISTO", "TRATOR RODAS", 
                              "TRICICLO") ~ "Outros", 
    TRUE ~ as.character(TIPO_VEI_CONSOLIDADO)))

tipo_veic <- base_ocorrencias %>% 
  group_by(tipo, MACROREG,TIPO_VEI_CONSOLIDADO) %>% 
  summarise(n())


#Sobre a ocorrencia
#periodo do dia
periodo <- base_ocorrencias %>% 
  group_by(tipo,MACROREG,PERIDOOCORRENCIA) %>% 
  summarise(n())

#dia da semana 

base_ocorrencias$DATAOCORRENCIA <- dmy(base_ocorrencias$DATAOCORRENCIA)

dia_semana <- base_ocorrencias%>%
  mutate(dia_sem = (weekdays(DATAOCORRENCIA))) %>% 
  group_by(tipo,MACROREG,dia_sem) %>% 
  summarise(n())

#tipo de local

base_ocorrencias<- base_ocorrencias %>% 
  mutate(LOCAL_CONSOLIDADO = case_when(
    DESCRICAOLOCAL %in% c("Area n?o ocupada", "Estacionamento p?blico", "Estrada de ferro", "Favela", "Rodovia/Estrada",
                          "Terminal/Esta??o", "Unidade rural", "Ve?culo em movimento", "Via p?blica", "Via P?blica") 
    ~ "Local/via P?blica", TRUE ~ as.character(DESCRICAOLOCAL)))

base_ocorrencias<- base_ocorrencias %>% 
  mutate(LOCAL_CONSOLIDADO = case_when(
    DESCRICAOLOCAL %in% c("Carro Forte", "Local clandestino/ilegal", "Outros") ~ "Outros", 
    TRUE ~ as.character(LOCAL_CONSOLIDADO)))

base_ocorrencias<- base_ocorrencias %>% 
  mutate(LOCAL_CONSOLIDADO = case_when(
    DESCRICAOLOCAL %in% c("Centro Comerc./Empresarial", "Com?rcio e servi?os", "Condominio Comercial", 
                          "Entidade assistencial", "Escrit?rio", "Estabelecimento banc?rio", "Estabelecimento de ensino",
                          "Estabelecimento industrial", "Estabelecimento prisional", "Estacionamento com vigil?ncia", 
                          "Estacionamento particular", "Hospedagem", "Lazer e recrea??o", "Reparti??o P?blica", 
                          "Restaurante e afins", "Sa?de", "Servi?os e bens p?blicos", "Shopping Center", "Sindicato", 
                          "Templo e afins") ~ "Com?rcio e servi?os", 
    TRUE ~ as.character(LOCAL_CONSOLIDADO)))

base_ocorrencias<- base_ocorrencias %>% 
  mutate(LOCAL_CONSOLIDADO = case_when(
    DESCRICAOLOCAL %in% c("Condominio Residencial", "Garagem coletiva de pr?dio", "Garagem ou abrigo de resid?ncia",
                          "Resid?ncia") ~ "Resid?ncias", 
    TRUE ~ as.character(LOCAL_CONSOLIDADO)))

tipo_local <- base_ocorrencias %>% 
  group_by(tipo,MACROREG,LOCAL_CONSOLIDADO) %>% 
  summarise(n())

# Modelo do ve?culo

tipo_modelo_reg <- base_ocorrencias %>% 
  group_by(tipo, MACROREG,DESCR_MARCA_VEICULO) %>% 
  summarise(n()) %>% 
  top_n(11)

tipo_modelo <- base_ocorrencias %>% 
  group_by(tipo, DESCR_MARCA_VEICULO) %>% 
  summarise(n()) %>% 
  top_n(11)

# Ano modelo

base_ocorrencias$ANO_MODELO <- as.double(base_ocorrencias$ANO_MODELO)

base_ocorrencias %>%
  group_by(tipo,MACROREG) %>% 
  summarise(
    mean = mean(ANO_MODELO,na.rm=TRUE))

ano_modelo <- base_ocorrencias %>%
  group_by(tipo,MACROREG) %>% 
  summarise(
    mediana = median(ANO_MODELO,na.rm=TRUE))

base_ocorrencias %>%
  group_by(tipo) %>% 
  summarise(
    mediana = median(ANO_MODELO,na.rm=TRUE))

flagrante <- base_ocorrencias %>% 
  group_by(tipo,MACROREG,FLAGRANTE) %>% 
  summarise(n())


glimpse(base_ocorrencias)

# Exporta as contagens das frequencias em xlsx

writexl::write_xlsx(descricao, "descricao.xlsx")
writexl::write_xlsx(periodo, "periodo.xlsx")
writexl::write_xlsx(tipo_local, "tipo_local.xlsx")
writexl::write_xlsx(tipo_veic, "tipo_veic.xlsx")
writexl::write_xlsx(dia_semana, "dia_semana.xlsx")
writexl::write_xlsx(tipo_modelo, "tipo_modelo.xlsx")
writexl::write_xlsx(tipo_modelo_reg, "tipo_modelo_reg.xlsx")
writexl::write_xlsx(ano_modelo, "ano_modelo.xlsx")
writexl::write_xlsx(flagrante, "flagrante.xlsx")

base_ocorrencias %>% 
  filter(ANO_BO > 2017) %>% 
  filter (RUBRICA %in% c("Furto (art. 155) - VEICULO", 
                         "Furto qualificado (art. 155, ?4o.) - VEICULO",
                         "A.I.-Furto (art. 155) - VEICULO",
                         "A.I.-Furto qualificado (art. 155, ?4o.) - VEICULO")) %>% 
  group_by(MACROREG) %>% 
  ggplot(aes(
    x = ANO_BO, fill = MACROREG)) +
  geom_bar(stat="count", position="dodge")+
  scale_x_continuous(breaks = seq(2018, 2019)) +
  scale_y_continuous(breaks = seq(0, 38000, 5000)) +
  labs(
    x = "Ano do roubo",
    y = "Total de roubos",
    color = "Macrorregi?o",
    title = "Total de roubos por regi?o do estado - 2018 e 2019"
  ) 


  
  
  
  ggplot(as.data.frame(tbl), aes(factor(Depth), Freq, fill = Species)) +     
  
  
  contagem_ocorr <- table(base_ocorrencias$ID)
barplot(contagem_ocorr, main="Car Distribution",
        xlab="Number of Gears")



geom_bar(aes(fill = factor(MACROREG)),stat="identity",position = "dodge") +
  ggtitle("Matrix Sort Performance") + 
  scale_fill_brewer(palette = "Dark2")

glimpse(base_ocorrencias)



