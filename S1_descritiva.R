# Análises Descritivas 
# Vitória Sesana


# pacotes -----------------------------------------------------------------

library(dplyr) # manipulação
library(ggplot2) # gráficos
library(lubridate) # variáveis temporais

# leitura e tratamento ----------------------------------------------------

base <- read.csv("bases/dados_intestino.csv")


# precisamos da indicadora censura e indicadora tempo de falha
# também precisa desse tempo

# descritiva --------------------------------------------------------------

# IDADE
hist(base$IDADE) # boxplot talvez
boxplot(base$IDADE, horizontal = TRUE)


# SEXO
base %>% 
  group_by(SEXO) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd/sum(qntd)) %>% 
  ggplot(aes(SEXO, prop)) +
  geom_col(position = "fill")

# ESCOLARIDADE
base %>% 
  group_by(ESCOLARI) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd/sum(qntd))

