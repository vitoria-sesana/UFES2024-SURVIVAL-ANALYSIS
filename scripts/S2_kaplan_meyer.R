# pacotes -----------------------------------------------------------------

library(dplyr)
library(survival)
library(survminer)
library(forcats)
library(janitor)

# leitura e tratamento  ---------------------------------------------------

base_modelo <- base_tratada %>% select("ESCOLARI", "IDADE", "SEXO", "CATEATEND",
                                       "ECGRUP", "DESCTOPO", "tempo_meses",
                                       "id_evento") %>%
  mutate(DESCTOPO = case_when(
    DESCTOPO == "INTESTINO DELGADO SOE" ~ "ID SOE",
    DESCTOPO == "ILEO EXCLUI VALVULA ILEOCECAL C180" ~ "IEVIC C180",
    DESCTOPO == "DUODENO" ~ "DUO",
    DESCTOPO == "JEJUNO" ~ "JEJ",
    DESCTOPO == "INTESTINO DELGADO LESAO SOBREPOSTA DO" ~ "ID LESAO SOBREPOSTA",
    TRUE ~ DESCTOPO)) %>%  # Renomeando essa coluna com diminutivos para praticidade
  mutate(across(c(ESCOLARI, SEXO, CATEATEND, ECGRUP, DESCTOPO),
                as.character)) %>%
  clean_names()
    
# kaplan meyer ------------------------------------------------------------

# Criando objeto de sobrevivência
base_surv <- Surv(time = base_modelo$tempo_meses, base_modelo$id_evento)

# Estimação de Kaplan Meyer

fit <- survfit(base_surv ~ 1, data = base_modelo)

fit_escolaridade <- survfit(base_surv ~ escolari, data = base_modelo)

fit_sexo <- survfit(base_surv ~ sexo, data = base_modelo)

fit_atendimento <- survfit(base_surv ~ cateatend, data = base_modelo)

fit_grupo <- survfit(base_surv ~ ecgrup, data = base_modelo)

fit_topografia <- survfit(base_surv ~ desctopo, data = base_modelo)

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

# testes de curvas -------------------------------------------------

logrank_escolaridade <- survdiff(base_surv ~ escolari, data = base_modelo)
wilcoxon_escolaridade <- survdiff(base_surv ~ escolari, data = base_modelo,
                                  rho = 1)

logrank_sexo <- survdiff(base_surv ~ sexo, data = base_modelo)
wilcoxon_sexo <- survdiff(base_surv ~ sexo, data = base_modelo, rho = 1)

logrank_atendimento <- survdiff(base_surv ~ cateatend, data = base_modelo)
wilcoxon_atendimento <- survdiff(base_surv ~ cateatend, data = base_modelo,
                                 rho = 1)

logrank_grupo <- survdiff(base_surv ~ ecgrup, data = base_modelo)
wilcoxon_grupo <- survdiff(base_surv ~ ecgrup, data = base_modelo, rho = 1)

logrank_topografia <- survdiff(base_surv ~ desctopo, data = base_modelo)
wilcoxon_topografia <- survdiff(base_surv ~ desctopo, data = base_modelo,
                                rho = 1)

