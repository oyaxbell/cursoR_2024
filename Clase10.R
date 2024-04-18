#### Regresión lineal y ANOVA ###
## Ponentes: Omar Yaxmehen Bello Chavolla
## 18/Abril/2024

#### Regresión lineal ####
library(tidyverse); library(ggpubr); library(nortest); library(car)
library(lmtest)
load("~/Downloads/data/parenthood.Rdata")

## Horas de sueño en función de qué tan 
## malhumorada está la persona

parenthood %>%
  ggplot(aes(x=dan.grump, y=dan.sleep))+geom_point()+
  theme_pubclean()+
  ylab("Hora de sueño (hrs)")+xlab("Enojado (0-100, %)")

## Coeficiente de correlación

cor.test(parenthood$dan.grump, parenthood$dan.sleep)

## Si hoy estoy enojado a un 76% ¿cuántas horas voy
## a dormir?

## Regresión lineal

## Línea que describe la relación entre 2 variables

## Minimización - Mínimos cuadrados ordinarios


## y~beta0+beta1*x1
## beta0 <- intercepto -> Cuanto vale y cuando x=0
## beta1 <- pendiente -> Cuanto cambia y cuando cambia x
## y=mx+b

parenthood %>%
  ggplot(aes(x=dan.grump, y=dan.sleep))+geom_point()+
  theme_pubclean()+
  ylab("Hora de sueño (hrs)")+xlab("Enojado (0-100, %)")+
  geom_smooth(method="lm")


 lm(y~x) ## La regresión de y en función de x
 lm(dan.sleep ~ dan.grump,  data = parenthood) 

 ## Conforme x aumenta, y disminuye
 
# y~ 12.78318 - 0.09132*x
 12.78318 - (0.09132*76)

# Error= Observado-Predicho -> Residuos

 #Observado y un valor predicho
 
 ## Significancia estadística
 
 # beta0, beta1 -> Distribución t de Student
 # Prueba t
 
 m1<-lm(dan.sleep ~ dan.grump,  data = parenthood)
 m1
 summary(m1)
 confint(m1)
 
 ## Haciendo predicciones del modelo
 predict(m1, newdata=data.frame(dan.grump=c(76,65,12)))
 
## Supuestos
## Residuos deben tener una distribución normal
 
hist(m1$residuals)
 
## Anderson-Darling
## H0=Normal <- NO RECHAZAR
ad.test(m1$residuals)

## Linealidad
## Falta de ajuste-> Cuando la única relación factible es lineal
## Polinomios ortogonales (2o, 3er, nthorden)

m2<-lm(dan.sleep ~ poly(dan.grump,3),  data = parenthood)
summary(m2)

## Independencia

## Metodológico -> Si las observaciones son dependientes

## Homoscedasticidad de varianza
bptest(m1)


## Calcualr los componentes del modelo

# y ~ x

X <- parenthood$dan.grump  # the predictor
Y <- parenthood$dan.sleep  # the outcome
Y.pred <- 12.78 - 0.091 * X

## De donde viene la R2
SS.resid <- sum( (Y - Y.pred)^2 ) ## Suma de errores residuales
print( SS.resid )
SS.tot <- sum( (Y - mean(Y))^2 )
print( SS.tot )
R.squared <- 1 - (SS.resid / SS.tot) ## Que tanta información captura mi modelo -> R2
## % DE VARIABILIDAD CAPTURADO POR EL MODELO
print( R.squared )*100

## Modelo de regresión lineal simple -> 1 predictor (1x)

r <- cor(X, Y)  # calculate the correlation
print( r^2 )    # print the squared correlation


## Regresión lineal múltiple
## +1 predictor x

## Si yo tomo en cuenta su nivel de enojo y cuanto duerme su bebé, cuanto dormirá Dan?
regression.2 <- lm( dan.sleep ~  dan.grump + baby.sleep,  
                    data = parenthood )
summary(regression.2)

## Residuales 

hist(regression.2$residuals)
ad.test(regression.2$residuals)

## Linealidad
parenthood %>%
  ggplot(aes(x=baby.sleep, y=dan.sleep))+geom_point()+
  theme_pubclean()+
  ylab("Hora de sueño (hrs)")+xlab("Hora de sueño del bebe (hrs)")+
  geom_smooth(method="lm")

m2 <- lm( dan.sleep ~  dan.grump + poly(baby.sleep,3),  
                    data = parenthood )
summary(m2)

## Independencia -> NO CUMPLE

## Multicolinealidad

# Si las variables están midiendo lo mismo
# Peso -> IMC -> Matriz de diseño de rango completo
m2 <- lm( dan.sleep ~  dan.grump + poly(baby.sleep,3),  
          data = parenthood )
summary(m2)

# Factor de inflación de la varianza VIF
vif(regression.2) 
# VIF >5 -> Mulcolinealidad

## Homoscedasticidad de varianza
## Prueba de Breusch-Pagan
## H0: Hay homoscedasticidad varianza

bptest(regression.2)






#### ANOVA ####
#A) Descripción de la prueba
##Extensión de la prueba de t para comparar medias cuando hay más de 2 grupos.
##Se agrupan datos a través de un factor.

#Hipótesis: Si el valor esperado para cada nivel de factor es el mismo
#H0: control=grupo1=grupo2
#Ha: al menos uno es distinto

#B) Supuestos
#Las observaciones se obtienen completamenta al azar y de forma independiente.
#Homocedasticidad de varianza
#Normalidad de residuos.
#Simetría de variables.

#Algoritmo básico
##Determinar varianza intramuestra (residual)
##Determinar varianza entre medias.
##Producción de estadístico de prueba

#Spoiler alert: La ANOVA es un modelo lineal de efectos fijos en variables nulas

#Creamos datos 
my_data <- PlantGrowth


levels(my_data$group)

#Estadísticos de resumen

group_by(my_data, group) %>% summarise(count = n(),
                                       mean = mean(weight, na.rm = TRUE),
                                       sd = sd(weight, na.rm = TRUE))

ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")

par(mfrow=c(1,3))
hist((my_data%>%filter(group=="ctrl"))$weight)
hist((my_data%>%filter(group=="trt1"))$weight)
hist((my_data%>%filter(group=="trt2"))$weight)
par(mfrow=c(1,1))

ggarrange()

#Mecánica de la prueba
res.aov <- aov(weight ~ group, data = my_data)
m3 <- lm(weight ~ group, data = my_data)

#Interpretación de la prueba
summary(res.aov)

#Validación de supuestos
hist(res.aov$residuals)
ad.test(res.aov$residuals)
qqPlot(res.aov$residuals, id = FALSE)

par(mfrow=c(2,2))
plot(res.aov)
par(mfrow=c(1,1))

leveneTest(weight~group, my_data, center=mean)

#Tukey (ANOVA)
#Creación de intervalos mñultiples de confianza.
#El algoritmo construye intervalos múltiples de confianza para diferencia de medias de "p" poblaciones.
#Define un estadístico llamado de rango estudiantizado.
#Se toma en cuenta el número de comparaciones y grados de libertad.

TukeyHSD(res.aov)
plot(TukeyHSD(res.aov))

#ANOVA de dos vías

data1<-ToothGrowth

ggboxplot(data1, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))

ggline(data1, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))

res.aov2 <- aov(len ~ factor(supp) + factor(dose), data = data1)
summary(res.aov2)

res.aov3 <- aov(len ~ factor(supp)*factor(dose), data = data1)
summary(res.aov3)

res.aov3 <- aov(len ~ factor(supp)+factor(dose)+factor(supp):factor(dose), data = data1)
summary(res.aov3)

TukeyHSD(res.aov3)
plot(TukeyHSD(res.aov3))

#Validación de supuestos
hist(res.aov3$residuals)
ad.test(res.aov3$residuals)
qqPlot(res.aov3$residuals, id = FALSE)

par(mfrow=c(2,2))
plot(res.aov3)
par(mfrow=c(1,1))

leveneTest(len~supp*dose, data1, center=mean)

#####Kruskal-Wallis====
#A) Descrpción de la prueba
#Alternativa no paramétrica cuando hay que comparar más de dos grupos.
#1 variable nominal y una variable rankeada.
#No se cumple el supuesto de la simetría/ normalidad de la ANOVA

#B) Supuestos
#Homocedasticidad
#Todos los grupos deben tener la misma distribución

#Se construye un estadístico H (Mann-Whitney construye U)

#Hipótesis
##H0=Las medias de rango son distintas.

#Estadísticos de resumen
group_by(my_data, group) %>% summarise(
  count = n(),
  mean = mean(weight, na.rm = TRUE),
  sd = sd(weight, na.rm = TRUE),
  median = median(weight, na.rm = TRUE),
  IQR = IQR(weight, na.rm = TRUE))

#Mecánica de la prueba
kruskal.test(weight ~ group, data = my_data)

#Validción de supuestos

leveneTest(weight~group, my_data, center=median)
