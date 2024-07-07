# pacotes -----------------------------------------------------------------

library(dplyr)
library(survival)
library(survminer)
library(rcompanion)

# Avaliando associação entre variáveis ------------------------------------

base_categoricos <- base_modelo %>% select(-c("IDADE", "tempo_meses"))

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

# Weibull e Exponencial ---------------------------------------------------

model_weibull <- survreg(base_surv ~ ., data = base_modelo, dist = "weibull")

model_exp <- survreg(base_surv ~ ., data = base_modelo, dist = "exponential")

# Testando se escala = 1 usando teste de razão de verossimilhança

trv_exp <- -2*(model_exp$loglik[2] - model_weibull$loglik[2])

p_trv_exp <- pchisq(trv_exp, 1, lower.tail = FALSE)

if (p_trv_exp > 0.05) {
  cat("Não rejeitamos a hipótese nula. O modelo exponencial é adequado.\n")
} else {
  cat("Rejeitamos a hipótese nula. O modelo exponencial não é adequado.\n")
}

# Log Logistica -----------------------------------------------------------

#modelo_logit <- survreg(base_surv ~ ., data = base_modelo_scaled,
#                               dist = "logistic",
#                               control = survreg.control(maxiter = 1000))

model_logit <- survreg(base_surv ~ ., data = base_modelo, dist = "logistic")

# Log Normal --------------------------------------------------------------

#model_normal <-  survreg(base_surv ~ ., data = base_modelo_scaled,
#                         dist = "gaussian",
#                         control = survreg.control(maxiter = 1000))