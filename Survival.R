####Análisis de supervivencia desde R###
##Ponentes: Carlos Fermin Martinez y Neftali Eduardo Antonio-Villa
##17-Junio-2021

####----Cargar paquetes----####
pacman::p_load(survival, survminer, tidyverse, mice)


####----Modelo de riesgos proporcionales de Cox----####
#Modelo de regresión que permite estudiar el tiempo a un evento. A diferencia de KM,
#permite cuantificar el efecto que tiene un predictor sobre el tiempo de supervivencia

#RIESGOS PROPORCIONALES
#Este modelo asume que la razón de riesgo entre los individuos o grupos estudiados
#se mantiene constante con el tiempo (las curvas de supervivencia acumulada no deben cruzarse)


#1. Base de datos con la que vamos a trabajar
data(lung)
str(lung)

summary(lung$time) #Tiempo de supervivencia en días

table(lung$status) #Status (censurado o muerto)
lung$status <- ifelse(lung$status==2, 1, 0) #C=0, M=1

summary(lung$age) #Edad en años

table(lung$sex) #Sexo
lung$sex <- ifelse(lung$sex==2, 1, 0) #H=0, M=1

table(lung$ph.ecog) #Escala ECOG 
#0= asintomático, 1= ambulatorio, 2= en cama <50% del día, 3= en cama >50%, 4= completamente postrado

summary(lung$ph.karno) #Escala de rendimiento de Karnofsky
summary(lung$meal.cal) #Calorías consumidas en cada comida
summary(lung$wt.loss) #Pérdida de peso en los últimos 6 meses

apply(apply(lung,2,is.na),2,sum)

lung%>%drop_na
nrow(lung); nrow(lung%>%drop_na)

imp1 <- mice::mice(lung, m=1, maxit=5); lung2 <- complete(imp1, "long")
nrow(lung); nrow(lung2)
apply(apply(lung2,2,is.na),2,sum)


#2. Modelos de Cox (interpretación y comparación)
survival::coxph(formula = Surv(time, status) ~ sex, data = lung2)
Surv(lung2$time, lung2$status)

m1 <- survival::coxph(formula = Surv(time, status) ~ sex, data = lung2)
summary(m1)
#HR =1 No hay diferencia
#HR <1 Menor riesgo
#HR >1 Mayor riesgo
1-0.588
concordance(m1)
summary(m1)$concordance

m2 <- coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung2)
summary(m2)

m3 <- coxph(Surv(time, status) ~ sex + ph.ecog, data = lung2)
summary(m3)

summary(m2)$concordance; summary(m3)$concordance
anova(m3, m2)
BIC(m2); BIC(m3) #El modelo 3 (solo sexo y ECOG) es mejor


m4 <- coxph(Surv(time, status) ~ sex + ph.ecog + ph.karno + wt.loss, data = lung2)
summary(m4)

summary(m4)$concordance; summary(m3)$concordance
anova(m4, m3)
BIC(m4); BIC(m3) #El modelo 3 (solo sexo y ECOG) es mejor


#Predecir riesgo o supervivencia
predict(m3, type="risk") #Riesgo de cada individuo vs su riesgo basal
predict(m3, type="survival") #Supervivencia estimada para cada individuo

median(predict(m3, type="survival"))

survfit(m3, data = lung2)
survfit(Surv(time, status) ~ sex + ph.ecog, data = lung2)

fit1 <- survfit(m3, data = lung2)
fit2 <- survfit(Surv(time, status) ~ sex + ph.ecog, data = lung2)

summary(fit1, times=c(30, 90, 180, 365))
summary(fit2, times=c(30, 90, 180, 365))

?survival::lung


#3. Cumplimiento de supuestos

#RIESGOS PROPORCIONALES
#Este modelo asume que la razón de riesgo entre los individuos o grupos estudiados
#se mantiene constante con el tiempo (las curvas de supervivencia acumulada no deben cruzarse)

#H0: Los riesgos son proporcionales  /// H1: Los riesgos NO son proporcionales
cox.zph(m3)

#Residuos de Schoenfeld 
survminer::ggcoxzph(cox.zph(m3),ggtheme = ggpubr::theme_pubclean(),
                    point.col = "midnightblue", point.alpha = 0.5)

#Residuos de Martingale
survminer::ggcoxdiagnostics(m3) + ggpubr::theme_pubclean()

#Kaplan-Meier
ggsurvplot(survfit(Surv(time, status) ~ sex , data = lung2), 
           ggtheme = ggpubr::theme_pubclean(), conf.int = T, palette = "nejm",
           pval=T, pval.coord=c(25, 0.80), fun = "cumhaz", risk.table = T,
           legend.labs=c("Male", "Female"))







