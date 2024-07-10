# pacotes -----------------------------------------------------------------

library(dplyr) # manipulação de dados
library(survival) # modelo de sobrevivencia
library(survminer)
library(rcompanion)
library(tidymodels)
library(flexsurv) # modelo gama generalizada

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
                       dist = "loglogistic")

# Log Normal --------------------------------------------------------------

model_normal <- survreg(Surv(tempo_meses,id_evento) ~ ., data = base_modelo,
                       dist = "lognormal")

# Gráficos de Linearização ------------------------------------------------

time <- fit$time

st <- fit$surv

invst <-qnorm(st)

par(mfrow=c(1,3))
plot(time, -log(st),pch=16,xlab="Tempos",
     ylab="-log(S(t))", main = "Exponencial")
plot(log(time),log(-log(st)),pch=16,xlab="log(tempos)",
     ylab="log(-log(S(t))",main = "Weibull")
plot(log(time),invst,pch=16,xlab="log(tempos)",
     ylab=expression(Phi^-1*(S(t))), main = "LogNormal")

# Gráficos  de Kaplan Meyer vs Modelo Paramétrico ------------------------

# Função para calcular as sobrevivências ajustadas
calc_surv <- function(modelo, dados) {
  linear_predictor <- predict(modelo, newdata = dados,
                              type = "lp")
  scale <- modelo$scale
  if (modelo$dist == "weibull") {
    scale <- 1 / scale
    surv <- exp(- (dados$tempo_meses / exp(linear_predictor)) ^ scale)
  } else if (modelo$dist == "exponential") {
    surv <- exp(- dados$tempo_meses / exp(linear_predictor))
  } else if (modelo$dist == "loglogistic") {
    surv <- 1 / (1 + exp((log(dados$tempo_meses) - linear_predictor) / scale))
  } else if (modelo$dist == "lognormal") {
    surv <- (log(dados$tempo_meses) - linear_predictor) / scale
  }
  return(surv)
}

# Calculando as sobrevivências ajustadas
base_modelo$surv_weibull <- calc_surv(model_weibull, base_modelo)
base_modelo$surv_exp <- calc_surv(model_exp, base_modelo)
base_modelo$surv_logit <- calc_surv(model_logit, base_modelo)
base_modelo$surv_normal <- calc_surv(model_normal, base_modelo)

par(mfrow = c(1,2 ))

# Kaplan-Meier vs. Modelo Exponencial
plot(st, base_modelo$surv_exp[match(time, base_modelo$tempo_meses)], pch = 16,
     ylim = range(c(0.0, 1)), xlim = range(c(0, 1)), xlab = "S(t): Kaplan-Meier",
     ylab = "S(t): Exponencial")
lines(c(0, 1), c(0, 1), type = "l", lty = 1)

# Kaplan-Meier vs. Modelo Weibull
plot(st, base_modelo$surv_weibull[match(time, base_modelo$tempo_meses)], pch = 16,
     ylim = range(c(0.0, 1)), xlim = range(c(0, 1)), xlab = "S(t): Kaplan-Meier",
     ylab = "S(t): Weibull")
lines(c(0, 1), c(0, 1), type = "l", lty = 1)

# Kaplan-Meier vs. Modelo Log-Logistico
plot(st, base_modelo$surv_logit[match(time, base_modelo$tempo_meses)], pch = 16,
     ylim = range(c(0.0, 1)), xlim = range(c(0, 1)), xlab = "S(t): Kaplan-Meier",
     ylab = "S(t): Log-Logistico")
lines(c(0, 1), c(0, 1), type = "l", lty = 1)

# Kaplan-Meier vs. Modelo Log Normal
plot(st, base_modelo$surv_normal[match(time, base_modelo$tempo_meses)], pch = 16,
     ylim = range(c(0.0, 1)), xlim = range(c(0, 1)), xlab = "S(t): Kaplan-Meier",
     ylab = "S(t): Log-Normal")
lines(c(0, 1), c(0, 1), type = "l", lty = 1)


# Teste de Razão de Verossimilhança para melhor modelo --------------------

# Ajustando modelo Gama generalizada

model_gengamma <- flexsurvreg(Surv(tempo_meses, id_evento) ~ ., data = base_modelo,
                               dist = "gengamma")

# weibull x gama generalisada

trv_weibull <- -2*(model_weibull$loglik[2] - logLik(model_gengamma))

p_trv_weibull <- pchisq(trv_weibull, df = length(coef(model_gengamma)) - model_weibull$df,
                        lower.tail = FALSE)

if (p_trv_weibull > 0.05) {
  cat("Não rejeitamos a hipótese nula. O modelo é adequado.\n")
} else {
  cat("Rejeitamos a hipótese nula. O modelo não é adequado.\n")
}

# log normal

trv_normal <- -2*(model_normal$loglik[2] - logLik(model_gengamma))

p_trv_normal <- pchisq(trv_normal,df = length(coef(model_gengamma)) - model_normal$df,
                       lower.tail = FALSE)

if (p_trv_normal > 0.05) {
  cat("Não rejeitamos a hipótese nula. O modelo é adequado.\n")
} else {
  cat("Rejeitamos a hipótese nula. O modelo não é adequado.\n")
}





