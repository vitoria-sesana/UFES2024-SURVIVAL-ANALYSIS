# pacotes -----------------------------------------------------------------

library(dplyr)
library(survival)
library(survminer)
library(rcompanion)
library(tidymodels)

# Avaliando associação entre variáveis ------------------------------------

base_categoricos <- base_modelo %>% select(-c("idade", "tempo_meses","id_evento"))

# Função para calcular matriz de V de Cramer
cramerV_matrix <- function(data) {
  cat_vars <- data %>% select_if(is.factor)
  
  cat_var_names <- names(cat_vars)
  
  n <- length(cat_var_names)
  cramer_matrix <- matrix(NA, n, n, dimnames = list(cat_var_names, cat_var_names))
  
  # Calcular V de Cramer para todas as combinações de variáveis
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      tabela <- table(cat_vars[[i]], cat_vars[[j]])
      cramer_matrix[i, j] <- cramerV(tabela)
      cramer_matrix[j, i] <- cramer_matrix[i, j]
    }
  }
  
  return(cramer_matrix)
}

# V de Cramer entre todas variáveis categóricas

cramer_matriz <- cramerV_matrix(base_categoricos)
# OBS: aparentemente baixa associação

# Treinamento/Teste/Validação ----------------------------------------------

set.seed(0)
split<- initial_split(base_modelo, prop = 0.7, strata=tempo_meses)

base_treino <- training(split)
base_teste <- testing(split)

folds<- vfold_cv(base_treino, v=5, strata=tempo_meses)

# Aplicar modelos com base de treinamento aplicar crosss validation
# Weibull e Exponencial ---------------------------------------------------

model_weibull <- survreg(Surv(tempo_meses,id_evento) ~ ., data = base_modelo,
                         dist = "weibull")

model_exp <- survreg(Surv(tempo_meses,id_evento) ~ ., data = base_modelo,
                     dist = "exponential")

# Testando se escala = 1 usando teste de razão de verossimilhança

trv_exp <- -2*(model_exp$loglik[2] - model_weibull$loglik[2])

p_trv_exp <- pchisq(trv_exp, 1, lower.tail = FALSE)

if (p_trv_exp > 0.05) {
  cat("Não rejeitamos a hipótese nula. O modelo exponencial é adequado.\n")
} else {
  cat("Rejeitamos a hipótese nula. O modelo exponencial não é adequado.\n")
}

# Log Logistica -----------------------------------------------------------

model_logit <- survreg(Surv(tempo_meses,id_evento) ~ ., data = base_modelo,
                       dist = "logistic")

# Log Normal --------------------------------------------------------------

model_normal <- survreg(Surv(tempo_meses,id_evento) ~ ., data = base_modelo,
                       dist = "gaussian")
