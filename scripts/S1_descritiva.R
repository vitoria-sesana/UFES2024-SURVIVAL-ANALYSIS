# Análises Descritivas 
# Vitória Sesana


# pacotes -----------------------------------------------------------------

library(dplyr) # manipulação
library(ggplot2) # gráficos
library(lubridate) # variáveis temporais

# leitura e tratamento ----------------------------------------------------

base <- read.csv("bases/dados_intestino.csv")

base_tratada <- base %>% 
  mutate(
    # calculo do tempo observado em meses
    DATA_ULTIMA_INFO = as_date(DATA_ULTIMA_INFO),
    data_diag = as_date(data_diag),
    tempo_total = interval(ymd(data_diag),ymd(DATA_ULTIMA_INFO)),
    tempo_total = as.period(tempo_total, unit = "months"),
    tempo_meses = month(tempo_total),
    
    # criação das variáveis indicadoras de evento e censura
    id_evento = case_when(
      ULTINFO == 3 | ULTINFO == 4 ~ 1,
      .default = 0
    ), 
    id_censura = case_when(
      ULTINFO == 1 | ULTINFO == 2 ~ 1,
     .default = 0
    ), 
  )

# descritiva --------------------------------------------------------------

# IDADE
hist(base_tratada$IDADE) # boxplot talvez
boxplot(base_tratada$IDADE, horizontal = TRUE)


# SEXO
base_tratada %>% 
  group_by(SEXO) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd/sum(qntd)) %>% 
  ggplot(aes(SEXO, prop)) +
  geom_col(position = "fill")

# ESCOLARIDADE
base_tratada %>% 
  group_by(ESCOLARI) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd/sum(qntd))

# CATEATED (Categoria de atendimento ao diagnóstico)
base_tratada %>% 
  group_by(CATEATEND) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd/sum(qntd))

# ECGRUP (Grupo do estadiamento clínico)
base_tratada %>% 
  group_by(ECGRUP) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd/sum(qntd))

# TOPOGRUP (Grupo da topografia)
base_tratada %>% 
  group_by(TOPOGRUP) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd/sum(qntd))

# DESCTOPO (Descrição da topografia)
base_tratada %>% 
  group_by(DESCTOPO) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd/sum(qntd))
