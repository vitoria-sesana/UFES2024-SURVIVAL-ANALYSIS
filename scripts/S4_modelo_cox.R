# Modelo de cox


# bibliotecas -------------------------------------------------------------

require(survival)
require(survminer)


# modelo ------------------------------------------------------------------
attach(base_tratada)

fit <- coxph(Surv(tempo_meses, id_censura)~factor(ESCOLARI)+IDADE+factor(SEXO)+
               factor(CATEATEND)+factor(ECGRUP),
                data = base_tratada)

summary(fit)


# testes e adequabilidade -------------------------------------------------
cox.zph(fit, transform = "identity")
par(mfrow=c(1,4))
plot(cox.zph)


anova(fit, test = "Chisq")

# teste de riscos proporcionais
test.ph <- survival::cox.zph(fit);test.ph
ggcoxzph(test.ph)

ggcoxdiagnostics(fit, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

ggcoxdiagnostics(fit, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
