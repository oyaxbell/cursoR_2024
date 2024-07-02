#### Regresión Poisson ###
## Ponentes: Omar Yaxmehen Bello Chavolla
## 07/Mayo/2024

#### Cargamos el dataset ####
require(pacman)
pacman::p_load(multcomp,pander,MASS,tidyverse, nortest, lmtest)

dataset <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")

dataset$prog <- factor(dataset$prog,
                       levels = 1:3,
                       labels = c("General", "Academic", "Vocational"))

## Regresión Poisson lidia con datos discretos ->
## Valores NO pueden ser negativos, NO tiene valores intermedios
## Conteos -> # Número de muertes, # Número de casos nuevos, # Número de eventos
## Estudios que calculan incidencia 

View(dataset)

summary(dataset)
glimpse(dataset)

#### Visualizando la distribución ####
dataset %>%
  ggplot(aes(x=num_awards))+geom_histogram()+theme_bw()+
  ylab("Conteo de premios")+xlab("Número de premios")

## ¿Cuál es la relación entre el número de premios y 
## los puntajes de matemáticas?#

## Ver la relación entre las dos variables
dataset %>%
  ggplot(aes(y=num_awards, x=math))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  ylab("Número de premios")+xlab("Puntaje en matemáticas")

dataset %>%
  ggplot(aes(y=num_awards, x=math, col=prog))+
  geom_point()+
  geom_smooth()+
  theme_bw()+
  ylab("Número de premios")+xlab("Puntaje en matemáticas")

#### Ajustando un modelo ####

m0<-lm(num_awards ~ math, data=dataset)
summary(m0)

## Supuesto de normalidad de errores
hist(m0$residuals)

ad.test(m0$residuals)
## No valido homoscedasticidad de varianza
bptest(m0)

## Modelo Poisson
m1 <- glm(num_awards ~ math+offset(log(estudiantes)), family = poisson, data = dataset)
summary(m1)

exp(coef(m1))##--> IRR

## Comparar el modelo lineal con el modelo Poisson
BIC(m0) # Lineal
BIC(m1) # Poisson

## Predicción del conteo de premios para una escuela que tiene puntaje de 46

round(exp(predict(m1, newdata = data.frame(math=c(46)))))

## Entre más pequeño sea el BIC, mejor es el modelo

## Modelo con dos covariables ##
m2 <- glm(num_awards ~ prog + math, family = poisson, data = dataset)
summary(m2)

## Prueba de devianza-> Compara dos modelos para ver cual es el mejor
anova(m1, m2, dispersion = "chisq")
BIC(m1)
BIC(m2)
## Diferencia de 2 puntos en el BIC para considerar que mejora el modelo

## Interpretando modelos ##
## Calular # de premios en función de un puntaje de matemáticas de 46 en una escuela con programa académico
num_awards <- exp(-5.24712 + 1.08386 + 0.07015 * dataset$math)

data.frame(pred=round(num_awards), y=dataset$num_awards)

#### Prueba de sobredispersión ####

## Distribución de Poisson
## Parámetro lambda -> Intensidad
## Varianza lambda-> lambda
## varianza=Lambda -> Varianza =/=Lambda -> Sobredispersión

## H0-> No sobredispersión

with(
  m2,
  cbind(
    res.deviance = deviance,
    df = df.residual,
    ratio=deviance/df.residual,
    p = pchisq(deviance, df.residual, lower.tail = FALSE)))

with(
  m1,
  cbind(
    res.deviance = deviance,
    df = df.residual,
    ratio=deviance/df.residual,
    p = pchisq(deviance, df.residual, lower.tail = FALSE)))

#### Modelos errores estándares robustos ####
## ¿Que pasaría si tengo sobredispersión?
## Corrección de Sandwich

cov.m1 <- sandwich::vcovHC(m2, type = "HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(
  Estimate = coef(m2), "Robust SE" = std.err,
  "Pr(>|z|)" = 2 * pnorm(abs(coef(m2) / std.err), lower.tail = FALSE),
  LL = coef(m1) - 1.96 * std.err,
  UL = coef(m1) + 1.96 * std.err
)
r.est

#### Modelo Quasipoisson ####
modeliq <- glm(num_awards ~ math + prog, family = quasipoisson, data = dataset)
summary(modeliq)

#### Modelo Binomial negativo ####

## Comparando modelos ##
modelinb <- glm.nb(num_awards ~ math + prog, data = dataset)
summary(modelinb)

## Zero-inflated Poisson ##

drop_in_dev <- anova(modeliq, modelinb, test = "F")

#### Predicción de modelos ####
data <- dataset

# Calculamos los valores predichos
data$pred <- predict(m2, type = "response")

data %>%
  ggplot(aes(pred))+geom_histogram()+theme_bw()

# Ordenamos el dataset por programa y luego por math
data <- data %>% arrange(prog, math)
View(data)
# Hacemos el gráfico de medias marginales
ggplot(data, aes(x = math, y = pred, color = prog)) +
  # plots the line of the marginal means
  geom_line(size = 1) +
  # plots the fitted values, we use 'jitter' to make it prettier
  geom_point(aes(y = num_awards), alpha = .5, position = position_jitter(h = .2)) +
  labs(title = "Medias marginales ajustadas", x = "Puntaje en matemáticas", y = "Numero predicho de premios")+
  theme_bw()


#### Incidence Rate Ratio ####

coef(m2)
exp(coef(m2))
confint(m2)
exp(confint(m2))


