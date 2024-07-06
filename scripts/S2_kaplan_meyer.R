# pacotes -----------------------------------------------------------------

library(dplyr)
library(survival)
library(survminer)
library(gridExtra)

# leitura e tratamento  ---------------------------------------------------

base_modelo <- base_tratada %>% select("ESCOLARI", "IDADE", "SEXO", "CATEATEND",
                                       "ECGRUP", "DESCTOPO", "tempo_meses",
                                       "id_evento")
# kaplan meyer ------------------------------------------------------------

# Criando objeto de sobrevivência
base_surv <- Surv(time = base_modelo$tempo_meses, base_modelo$id_evento)

# Estimação de Kaplan Meyer

fit <- survfit(base_surv ~ 1, data = base_modelo)

fit_escolaridade <- survfit(base_surv ~ ESCOLARI, data = base_modelo)

fit_sexo <- survfit(base_surv ~ SEXO, data = base_modelo)

fit_atendimento <- survfit(base_surv ~ CATEATEND, data = base_modelo)

fit_grupo <- survfit(base_surv ~ ECGRUP, data = base_modelo)

fit_topografia <- survfit(base_surv ~ DESCTOPO, data = base_modelo)

# Gráficos de curva de Kaplan Meyer

km_plot <- ggsurvplot(fit, data = base_modelo,
                      pval = TRUE, conf.int = TRUE, conf.int.style = "step",
                      ylab = "Sobrevida", xlab = "Tempo em dias",
                      legend.title = "Curva de Sobrevivência")

km_plot2 <- ggsurvplot(fit_escolaridade, data = base_modelo,
                       pval = TRUE, conf.int = TRUE, conf.int.style = "step",
                       ylab = "Sobrevida", xlab = "Tempo em dias",
                       legend.title = "Curva de Sobrevivência por Escolaridade")

km_plot3 <- ggsurvplot(fit_sexo, data = base_modelo,
                       pval = TRUE, conf.int = TRUE, conf.int.style = "step",
                       ylab = "Sobrevida", xlab = "Tempo em dias",
                       legend.title = "Curva de Sobrevivência por Sexo")

km_plot4 <- ggsurvplot(fit_atendimento, data = base_modelo,
                       pval = TRUE, conf.int = TRUE, conf.int.style = "step",
                       ylab = "Sobrevida", xlab = "Tempo em dias",
                       legend.title = "Curva de Sobrevivência por Atendimento")

km_plot5 <- ggsurvplot(fit_grupo, data = base_modelo,
                       pval = TRUE, conf.int = TRUE, conf.int.style = "step",
                       ylab = "Sobrevida", xlab = "Tempo em dias",
                       legend.title = "Curva de Sobrevivência por Grupo")

km_plot6 <- ggsurvplot(fit_topografia, data = base_modelo,
                       pval = TRUE, conf.int = TRUE, conf.int.style = "step",
                       ylab = "Sobrevida", xlab = "Tempo em dias",
                       legend.title = "Curva de Sobrevivência por Topografia")

