#### Pruebas de hipótesis continuas y ANOVA ###
## Ponentes: Omar Yaxmehen Bello Chavolla
## 02/Abril/2024

##### Pruebas de hipótesis ####
##Procedimiento mediante el cual se decide si aceptar la hipótesis nula como no verdadera (RECHAZO)
##Hipótesis: Enunciado que hace referencia a los valores que asume uno o más parámetros de una o más poblaciones.
##Matriz de confusión: Estándar de prueba contra esquema de hipótsis nula.
###Tipo I (Alfa): Rechazo de H0 cuando H0 es verdadera.
###Tipo II (Beta): No rechazo H0 cuando H0 es falsa.
###Ambos son valores de probabilidad.

#Elección de la prueba adecuada
##1. ¿Qué datos quiero obtener?
##2. ¿Cuál es mi hipótesis?
##3. ¿Cuál es mi tamaño de muestra?
##4. ¿Qué tipo de datos tengo?
# Establecer hipótesis

#Paramétrica vs No paramétrica
#La no paramétrica conlleva menos supuestos, y posiblemente menos poder.
#Utilizar no paramétrica cuando la variable dependiente es categórica u ordinal.
###Hay muchos outliers y la distribución es asimétrica
###Cuando el tamaño de muestra es pequeño es buena alternativa (TLC)

##### Paquetes a cargar#####
library(tidyverse); library(ggpubr); library(haven); 
library(nortest); library(dunn.test); library(datasets)
library(vcd)
library(moments)
library(car)
library(PairedData); library(DescTools)

##### T muestras independientes ####

# A) Descripción de la prueba
# La prueba t de Welch se utiliza para comparar las medias de dos conjuntos de datos o para comparar una media observada con una media teórica

# Resumen de Hipotesis
# Hipótesis nula (H0): La distribución de ambos grupos 
# Hipótesis alternativa (H1): las variables de fila y columna son dependientes

# B. Supuestos de la prueba
# Datos Continuos: La escala de medición aplicada a los datos sigue una escala continua u ordinal
# Muestra Representativa: La muestra pertenece a una porción representativa y seleccionada aleatoriamente de la población total.
# Simetria de los datos
# Homogeneidad de la varianza: Los datos siguen una homogeneidad de la varianza. (Prueba de Levene)
# Independencia de los grupos: No hay relación entre las observaciones en cada grupo.

#1. Dataset a trabajar

df_t<-MASS::survey #Asignar df
?MASS::survey #Evaluar elementos del df survey
head(df_t) #Observar el df
df_t_1<-df_t %>%
  dplyr::select(Sex,Height,Age,Pulse) #Seleccionar variables del df categoricas

#2. Visualización de la información

summary(df_t_1)

#Histogramas
ggplot(df_t_1,aes(x=Pulse))+ 
  geom_histogram() +
  facet_wrap(~Sex)

ggplot(df_t_1,aes(x=Height))+ 
  geom_histogram() +
  facet_wrap(~Sex)

ggplot(df_t_1,aes(x=Age))+ 
  geom_histogram()+
  facet_wrap(~Sex)



#3. Definir prueba a usar. Recordar supuestos.
##Pregunta: ¿Existirán diferencias de pulso, estatura y edad entre sexos?

#Evaluar homogeneidad de la varianza

##Hipótesis nula (Ho): La varianza entre grupos es similar. 
##Hipótesis alternativa (H1): hay una diferencia entre las varianzas de los dos grupos. 
#NOTA IMPORTANTE: La prueba de t, asume  igualdad de varianzas.

#Prueba F (var.test)
#El estadístico de la prueba F se puede obtener calculando la razón de las dos varianzas Var (A) / Var (B). 
#Cuanto más se desvía esta relación de 1, más fuerte es la evidencia de variaciones desiguales de la población.

var.test(Pulse ~ Sex, data = df_t_1)
var.test(Height ~ Sex, data = df_t_1)
var.test(Age ~ Sex, data = df_t_1)

#Prueba de Bartlett (bartlett.test)
#La prueba de Bartlett se utiliza para probar la homogeneidad de las varianzas en muestras k, donde k puede ser más de dos. 
#Está adaptado para datos distribuidos simetricamente

bartlett.test(Pulse ~ Sex, data = df_t_1)
bartlett.test(Height ~ Sex, data = df_t_1)
bartlett.test(Age ~ Sex, data = df_t_1)

#Prueba de Levene (leveneTest)
#La prueba de Levene es una alternativa a la prueba de Bartlett cuando los datos no se distribuyen simetricamente

leveneTest(Pulse ~ Sex, data = df_t_1)
leveneTest(Height ~ Sex, data = df_t_1)
leveneTest(Age ~ Sex, data = df_t_1)

#Prueba de T de Welch
#Realizar siguiendo los supuestos previamente mencionados

#Evaluar prueba t "clasica" 
#Por "default", la prueba asume una correccion de Welch
t.test(Pulse~ Sex, data = df_t_1)
t.test(Height~ Sex, data = df_t_1)
t.test(Age~ Sex, data = df_t_1)

#La correccion puede modificarse
t.test(Pulse~ Sex, data = df_t_1,var.equal=T)
t.test(Height~ Sex, data = df_t_1,var.equal=T)
t.test(Age~ Sex, data = df_t_1,var.equal=T)

#Realizar un grafico de ggplot
df_t_1%>%filter(!is.na(Sex))%>%
  ggplot(aes(x=Sex,y=Pulse))+
  geom_boxplot()+
  stat_compare_means(method = "t.test")

df_t_1%>%filter(!is.na(Sex))%>%
  ggplot(aes(x=Sex,y=Height))+
  geom_boxplot()+
  stat_compare_means(method = "t.test")

df_t_1%>%filter(!is.na(Sex))%>%
  ggplot(aes(x=Sex,y=Age))+
  geom_boxplot()+
  stat_compare_means(method = "t.test")

##### T pareada #####
#A) Descripción de la prueba
#Se utiliza para comparar las medias entre dos grupos relacionados. 
#En este caso, se tiene dos valores medidos para las mismas muestras.

##Resumen de Hipotesis

#B. Supuestos de la prueba
# Datos Continuos: Los datos son continuos (no discretos).
# Simetria de los datos: Los datos, es decir, las diferencias para los pares emparejados, siguen una distribución de probabilidad normal.
# Muestra Representativa: #Cada individuo de la población tiene la misma probabilidad de ser seleccionado en la muestra.


#1. Dataset a trabajar

# Weight of the mice before treatment
before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
# Weight of the mice after treatment
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
# Create a data frame
df_tp_1<- data.frame( 
  group = rep(c("before", "after"), each = 10),
  weight = c(before,  after)
)

#2. Visualización de la información

summary(df_tp_1)

#Histogramas
ggplot(df_tp_1,aes(x=weight))+ 
  geom_histogram() +
  facet_wrap(~group)

#Graficos qqplot
ggqqplot(df_tp_1, x = "weight", facet.by = "group")

#Evaluar varianza de los datos

var.test(weight ~ group, data = df_tp_1)
leveneTest(weight ~ group, data = df_tp_1)

#Aplicar Prueba de T pareada

t.test(weight~ group, data = df_tp_1,paired = TRUE)

#Realizar un grafico de ggplot
ggpaired(df_tp_1, x = "group", y = "weight",
        line.color = "gray", line.size = 0.4,
        palette = "jco")+
  stat_compare_means(method = "t.test",paired = TRUE)

##### Wilcoxon independiente y pareado ####
#1. Independiente

#A) Descripción de la prueba
##Alternatuva a prueba de t no paramétrica
##Se basa en el orden de las observaciones (rankings). Grupos pueden o no tener el mismo número de datos.
##También conocida como Mann-Whitney
##Se combinan las muestras a una sola rankeada.

##Hipótesis:
##H0: las dos poblaciones no son diferentes. Probabilidad de que alguien de una población extraído al azar supere a un miembro de la otra población

#B) Supuestos
#1. No hay empates en los promedios de Walsh en la prueba pareada.
#2. Distribuciones deben tener la misma forma.

# Creamos datos
women_weight<-c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight<-c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 
# Creamos data frame
my_data<-data.frame(group = rep(c("Woman", "Man"), each = 9),
                    weight = c(women_weight,  men_weight))
#Estadísticos de resumen
group_by(my_data, group) %>%summarise(count = n(),
                                      median = median(weight, na.rm = TRUE),
                                      IQR = IQR(weight, na.rm = TRUE))

#Visualizamos datos

ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

ggplot(aes(x=weight, fill=factor(group)), data=my_data)+
  geom_histogram(position = "dodge", binwidth = 15)

#Mecánica de la prueba
res <- wilcox.test(women_weight, men_weight)

#Interpretación de la prueba
res

#Alternativa a una cola
wilcox.test(weight ~ group, data = my_data, 
            exact = FALSE, alternative = "less")

#2. Pareados
#Las diferencias entre las muestras pareadas deben tener una distribución simétrica alrededor de la mediana

#Creamos datos

# Peso antes de tratamiento
antes<-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
# Peso después de tratamiento
despues<-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#Creación de data frame
my_data <- data.frame(group = rep(c("antes", "despues"), each = 10),
                      weight = c(antes,  despues))

#Estadísticos de resumen
group_by(my_data, group) %>% summarise(count = n(),
                                       median = median(weight, na.rm = TRUE),
                                       IQR = IQR(weight, na.rm = TRUE))

#Visualización

ggpaired(my_data, x = "group", y = "weight", 
         color = "group", palette = c("#00AFBB", "#E7B800"),
         ylab = "Weight", xlab = "Groups", line.color = "gray")+
  stat_compare_means(paired=TRUE)

ggplot(aes(x=weight, fill=factor(group)), data=my_data)+
  geom_histogram(position = "dodge", binwidth = 30)

#Mecánica de la prueba
res<-wilcox.test(antes, despues, paired = TRUE)
res


##### ANOVA ####
#A) Descripción de la prueba
## Extensión de la prueba de t para comparar medias cuando hay más de 2 grupos.
## Se agrupan datos a través de un factor.

# Hipótesis: Si el valor esperado para cada nivel de factor es el mismo
# H0: control=grupo1=grupo2
# Ha: al menos uno es distinto

# B) Supuestos
# Las observaciones se obtienen completamenta al azar y de forma independiente.
# Homocedasticidad de varianza
# Normalidad de residuos.
# Simetría de variables.

#Algoritmo básico
## Determinar varianza intramuestra (residual)
## Determinar varianza entre medias.
## Producción de estadístico de prueba

#Spoiler alert: La ANOVA es un modelo de efectos fijos en variables nulas

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

##### Kruskal-Wallis #####
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

##### Correlaciones ####

my_data <- mtcars
head(my_data, 6)


ggscatter(my_data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

## Correlación de Pearson
res <- cor.test(my_data$wt, my_data$mpg, 
                method = "pearson")
res

## Correlación de Kendall
res2 <- cor.test(my_data$wt, my_data$mpg,  method="kendall")
res2

## Correlación de Spearman
res2 <-cor.test(my_data$wt, my_data$mpg,  method = "spearman")
res2

## Intervalo de confianza para la correlación de Spearman
SpearmanRho(my_data$wt, my_data$mpg, conf.level=0.95)

