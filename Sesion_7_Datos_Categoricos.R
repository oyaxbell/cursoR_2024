###Tablas de contingencia y pruebas de hipótesis para datos categóricos en R#
###Curso: Programación Estadística en R – 2024
###Instructor: Dr. Neftali Eduardo Antonio-Villa###
###Twitter: @NeftaliAntonioV


#####Seccion 1: Librerias, directorio de trabajo y cargar Datos#####

library(readr)
library(haven)
library(readxl)
library(dplyr)
library(tidyverse)
library(rstatix)
library(ggstatsplot)
library(epiR)
library(pROC)

#Establecer el directorio de trabajo#
#Recuerda
#Session -> Set Working Directory -> Choose Directory -> Copiar -> Pegar Script

setwd("/Users/nefoantonio/Library/CloudStorage/OneDrive-UNIVERSIDADNACIONALAUTÓNOMADEMÉXICO/Clases/Curso R Cardiologia")

#Recuerda
#Existen funciones por default que permitiran cargar bases por default
##En Excel

fram <- read_excel("framingham.xls")

#####Seccion 2: Codificar la base de datos#####

#Recodificación de base de datos#

fram1<-fram%>%rename("Sexo_Masculino"=male,
                     "Edad"=age,
                     "Educacion"=education,
                     "Tabaquismo_Actual"=currentSmoker,
                     "Cigarros_por_Dia"=cigsPerDay,
                     "Uso_Antihipertensivos"=BPMeds,
                     "Evento_Vascular_Cerebral_Previa"=prevalentStroke,
                     "Hipertension_Previa"=prevalentHyp,
                     "Diabetes_Previa"=diabetes,
                     "Colesterol_Total"=totChol,
                     "Presion_Sistolica"=sysBP,
                     "Presion_Diastolica"=diaBP,
                     "Indice_Masa_Corporal"=BMI,
                     "Frecuencia_Cardiaca"=heartRate,
                     "Glucosa_Serica"=glucose,
                     "Riesgo_Falla_Cardiaca"=TenYearCHD)

#Ver que las variables continuas sean continuas

fram1$Edad<-as.numeric(fram1$Edad)
fram1$Cigarros_por_Dia<-as.numeric(fram1$Cigarros_por_Dia)
fram1$Presion_Sistolica<-as.numeric(fram1$Presion_Sistolica)
fram1$Presion_Diastolica<-as.numeric(fram1$Presion_Diastolica)
fram1$Colesterol_Total<-as.numeric(fram1$Colesterol_Total)
fram1$Glucosa_Serica<-as.numeric(fram1$Glucosa_Serica)
fram1$Indice_Masa_Corporal<-as.numeric(fram1$Indice_Masa_Corporal)

#Crear variables categoricas

#Edad de 65 años
fram1$Edad_65<-NULL #Los puntos de corte deben de estar basados para el desenlace de interes
fram1$Edad_65[fram1$Edad<65]<-0
fram1$Edad_65[fram1$Edad>=65]<-1

#Hipercolesterolemia

fram1$HiperColesterolemia<-NULL
fram1$HiperColesterolemia[fram1$Colesterol_Total<200]<-0
fram1$HiperColesterolemia[fram1$Colesterol_Total>=200]<-1

#HiperGlucemia

fram1$HiperGlucemia<-NULL
fram1$HiperGlucemia[fram1$Glucosa_Serica<100]<-0
fram1$HiperGlucemia[fram1$Glucosa_Serica>=100]<-1

#Hipertensión Arterial Secundaria Actual (HAS_Actual)

fram1$HAS_Actual<-NULL
fram1$HAS_Actual[fram1$Presion_Sistolica>=140 | fram1$Presion_Diastolica>=90]<-1  #AND 
fram1$HAS_Actual[is.na(fram1$HAS_Actual)]<-0 #Aqui estamos haciendo un pequeño truco que es asignarle rapidamente a aquellos que tiene NA darles el valor de 0

#Categorias de Indice de Masa Corporal (IMC)

fram1$IMC_Categorias<-NULL
fram1$IMC_Categorias[fram1$Indice_Masa_Corporal<25]<-1 
fram1$IMC_Categorias[fram1$Indice_Masa_Corporal>=25 & fram1$Indice_Masa_Corporal<30]<-2
fram1$IMC_Categorias[fram1$Indice_Masa_Corporal>=30]<-3

##Etiquetar variables a conveniencia##

##Vamos a asignarle etiquetas a nuestras variables categoricas
#Las etiquetas deben ser: claras, breves, permitan identificar categorias mutuamente excluyentes y si aplica, indicar solo el grado de riesgo (P. ejemplo: I,II, III, IV en lugar de NYHA I, NYHA II, NYHA III, etc)

fram1$Sexo_Masculino<-factor(fram1$Sexo_Masculino,levels = c(0,1),labels = c("Femenino","Masculino")) 
fram1$Educacion<-factor(fram1$Educacion,levels = c(1,2,3,4),labels = c("Primaria/Secundaria","Preparatoria","Universidad","Posgrado"))
fram1$Tabaquismo_Actual<-factor(fram1$Tabaquismo_Actual,levels = c(0,1),labels = c("Sin Tabaquismo", "Tabaquismo Activo"))
fram1$Uso_Antihipertensivos<-factor(fram1$Uso_Antihipertensivos,levels = c(0,1),labels = c("Sin Uso de Antihipertensivos", "Usa Antihipertensivos"))
fram1$Evento_Vascular_Cerebral_Previa<-factor(fram1$Evento_Vascular_Cerebral_Previa,levels = c(0,1),labels = c("Sin EVC Previo", "Antecedente EVC"))
fram1$Hipertension_Previa<-factor(fram1$Hipertension_Previa,levels = c(0,1),labels = c("Sin HAS Previa", "Antecedente HAS"))
fram1$Diabetes_Previa<-factor(fram1$Diabetes_Previa,levels = c(0,1),labels = c("Sin Diabetes Previa", "Antecedente Diabetes"))
fram1$Riesgo_Falla_Cardiaca<-factor(fram1$Riesgo_Falla_Cardiaca,levels = c(0,1),labels = c("Sin Falla Cardiaca", "Falla Cardiaca Incidente"))
fram1$Edad_65<-factor(fram1$Edad_65,levels = c(0,1),labels = c("<65 Años", "≥65 Años"))
fram1$HiperColesterolemia<-factor(fram1$HiperColesterolemia,levels = c(0,1),labels = c("Sin Hipercolesterolemia", "Hipercolesterolemia"))
fram1$HiperGlucemia<-factor(fram1$HiperGlucemia,levels = c(0,1),labels = c("Sin Glucosa Alterada en Ayuno", "Glucosa Alterada en Ayuno"))
fram1$HAS_Actual<-factor(fram1$HAS_Actual,levels = c(0,1),labels = c("Sin HAS Actual", "HAS Actual"))
fram1$IMC_Categorias<-factor(fram1$IMC_Categorias,levels = c(1,2,3),labels = c("Normopeso","Sobrepeso","Obesidad"))

#Repitamos el proceso de conocer la estructura del df

summary(fram1)
nrow(fram1)

#####Sección 3: Evaluación datos categorico#####

#Datos Categoricos 
#Nota: Lo más importante es saber evaluar las tablas con sus respectivos porcentajes dependiendo del grupo de interes. 
#La funcion de table() nos dará la frecuencia de cada evento

table(fram1$Educacion,useNA = "always")
table(fram1$Sexo_Masculino, useNA = "always")

#Se le añadimos la función prop.table() nos brindará la proporción relativa con respecto al numero de elementos en una variable

prop.table(table(fram1$Educacion,useNA = "always"))*100
prop.table(table(fram1$Educacion))*100

prop.table(table(fram1$Sexo_Masculino))

#Si lo multiplicamos por 100, nos dará el porcentaje relativo

prop.table(table(fram1$Sexo_Masculino))*100

#Que pasa si tenemos datos perdidos
#Lo correcto de manera inicial es incluirlos en el porcentaje reportado como datos faltantes. 
#Como opciones se pueden realizar métodos de imputación.

table(fram1$Sexo_Masculino,useNA = "always") #En el caso del sexo, no tenemos datos perdidos

prop.table(table(fram1$Educacion,useNA = "always"))*100
table(fram1$Educacion,useNA = "always") #Pero si evaluamos el caso de educación, si tenemos datos perdidos

#Si queremos excluir alguna categoria se puede evaluar el comando de exclude= en table()
table(fram1$Educacion,useNA = "always",exclude = "Posgrado") 

#Podemos seleccionar cualquier elemento de la tabla y tenerlo como un vector independiente
prop.table(table(fram1$Educacion,useNA = "always"))[c(1)]
prop.table(table(fram1$Educacion,useNA = "always"))[c(2)]
prop.table(table(fram1$Educacion,useNA = "always"))[c(3)]
prop.table(table(fram1$Educacion,useNA = "always"))[c(4)]
prop.table(table(fram1$Educacion,useNA = "always"))[c(5)]

#Si le asignamos una etiqueta previa, se puede seleccionar de acuerdo a la categoria 
prop.table(table(fram1$Educacion,useNA = "always"))[c("Primaria/Secundaria")]
prop.table(table(fram1$Educacion,useNA = "always"))[c("Preparatoria")]
prop.table(table(fram1$Educacion,useNA = "always"))[c("Universidad")]
prop.table(table(fram1$Educacion,useNA = "always"))[c("Posgrado")]

#Tablas de contingencia
#Si queremos evaluar la frecuencia de una categoria con respecto a otra, podemos realizar una tabla de contingencia en con el mismo comando de table()
#El comando permite añadir una segunda variable para evaluar la frecuencia con respecto a dos categorias

table(fram1$Sexo_Masculino,fram1$Educacion)
table(fram1$Educacion,fram1$Sexo_Masculino) #Ve la diferencia entre ordenarlos dependiendo de tu variable de agrupación

#Nota: Como consejo, primero pon la variable de evaluación y despues la variable de agrupación. Esto se servirá muchisimo para el porcentaje relativo
table(fram1$Educacion,fram1$Sexo_Masculino) 
prop.table(table(fram1$Educacion,fram1$Sexo_Masculino),1)*100
prop.table(table(fram1$Hipertension_Previa,fram1$Riesgo_Falla_Cardiaca),1)*100


#Si queremos saber el porcentaje relativo, solo añadimos las variables a estudiar en prop.table
prop.table(table(fram1$Educacion,fram1$Sexo_Masculino, useNA = "always"))*100 #Algo te parece raro? ES PORQUE TENEMOS EL PORCENTAJE CON TOTAL!

#Si añadimos a esta linea una coma se puede identificar el grupo de porcentaje de interes ##OJO!! Solo dentro de la funcion de table()
#,1 <- Porcentaje por filas
#,2 <- Porcentaje por columnas

#LA INTERPRETACION ES COMPLETAMENTE DIFERENTE!!!
prop.table(table(fram1$Educacion,fram1$Sexo_Masculino, useNA = "always"),1)*100 #Porcentaje de mujeres/hombre con respecto a la categoria de educación
prop.table(table(fram1$Educacion,fram1$Sexo_Masculino, useNA = "always"),2)*100 #Porcentaje de educación dentro de cada categorias mujer/hombre. PARA FINES DE COMPARACION DE +2 GRUPOS, SE ELIGE ESTA OPCION!

#Igual podemos seleccionar solo el porcentaje que nos interese representar?
prop.table(table(fram1$Educacion,fram1$Sexo_Masculino, useNA = "always"),2)[,1]*100 #Porcentaje de educación por categorias en hombres 
prop.table(table(fram1$Educacion,fram1$Sexo_Masculino, useNA = "always"),2)[,2]*100 #Porcentaje de educación por categorias en mujeres

#Que pasa si queremos hacer una estratificacion por una TERCERA VARIABLE!
#Ejemplo: Evaluar la frecuencia de educación y sexo en población de adulta mayor

#Solo añadimos una tercera variable a la funcion de table()
table(fram1$Educacion,fram1$Sexo_Masculino,fram1$Edad_65) 
prop.table(table(fram1$Educacion,fram1$Sexo_Masculino,fram1$Edad_65),c(3,2))

#Esto lo podemos poner en porcentaje para que tengamos un mejor punto de comparación
prop.table(table(fram1$Educacion,fram1$Sexo_Masculino,fram1$Edad_65,useNA = "always"))*100

#Para que tengamos el porcentaje con respecto a cada grupo de estratificaron... tenemos que hacer una pequeña modificacion
prop.table(table(fram1$Educacion,fram1$Riesgo_Falla_Cardiaca,fram1$Edad_65,useNA = "always"),c(3,1))*100 #Porcentaje por fila ,c(3,1)
prop.table(table(fram1$Educacion,fram1$Riesgo_Falla_Cardiaca,fram1$Edad_65,useNA = "always"),c(3,2))*100 #Porcentaje por columna ,c(3,2)


#####Sección 4: Prueba de Ji-Cuadrada#####

#Pregunta de Investigación 1
#Paso 1: Establezca una pregunta de Investigación

#¿Existiran diferencias entre la proporcion de hipertension entre grupos de falla cardiaca? 

#Paso 2: Identifique sus variables dependiente e independiente y la clasificación

#Independiente: Sujetos con falla y sin falla cardiaca (Categorica Nominal)
#Dependiente: Hipertension Arterial (Categorica Nominal)

#Paso 3: Establezca sus planteamientos de hipotesis

#HO: La proporción entre hipertensos es independiente de la falla cardiaca
#HA: La proporción entre hipertensos NO ES independiente de la falla cardiaca

#Paso 3.1: Visualize la distribución
table(fram1$HAS_Actual,fram1$Riesgo_Falla_Cardiaca)
plot(fram1$HAS_Actual,fram1$Riesgo_Falla_Cardiaca)

#Paso 3.2: Realice una estimación de proporciones

prop.table(table(fram1$Hipertension_Previa,fram1$Riesgo_Falla_Cardiaca),2)*100


#Paso 4: Calcule su estadistico de prueba al nivel de significancia deseado

rstatix::chisq_test(table(fram1$Hipertension_Previa,fram1$Riesgo_Falla_Cardiaca))
rstatix::fisher_test(table(fram1$Hipertension_Previa,fram1$Riesgo_Falla_Cardiaca))

#Paso 5: Contraste con sus planteamientos de hipótesis

#Conclusion completa
#A un nivel de 95% de confianza, SI existe evidencia suficiente en nuestra muestra para rechazar la hipotesis nula de independencia entre la proporcion de hipertension y de falla cardiaca

#Conclusion simplificada
#La proporción de hipertension es dependiente de la falla cardiaca.

#Visualizacion (ggstatsplot)
ggbarstats(fram1, x = Riesgo_Falla_Cardiaca, y = Hipertension_Previa)

#Visualizacion (ggstatsplot) - Graficas de Pasteles
ggpiestats(
  data = fram1,
  x = Hipertension_Previa,
  y = Riesgo_Falla_Cardiaca)

#####Sección 5: Pruebas Diagnósticas######

#Cargamos los datos dummy

df <- as.table(matrix(c(1540,42,3,35), nrow = 2, byrow = TRUE))
colnames(df) <- c("COVID+","COVID-")
rownames(df) <- c("PRUEBA+","PRUEBA-")

#Evaluamos las métricas diagnósticas
epiR::epi.tests(df,method = "exact",conf.level = 0.95)

#####Sección 6: Curvas ROC#####

#Evaluar las personas con el desenlace
table(fram1$Riesgo_Falla_Cardiaca)

#Generar Curva ROC con Intervalos de Confianza
roc(fram1$Riesgo_Falla_Cardiaca, as.numeric(fram1$Presion_Sistolica))

#Generar Curva ROC con Intervalos de Confianza
R1<-roc(fram1$Riesgo_Falla_Cardiaca, as.numeric(fram1$Presion_Sistolica),ci=T)
plot(R1)

#Generar Curva ROC con Intervalos de Confianza y Bootstrap
R1.b<-roc(fram1$Riesgo_Falla_Cardiaca, as.numeric(fram1$Presion_Sistolica),ci=T,smooth=TRUE)

#Graficar la Curva ROC con R Base
plot(R1.b)

#Graficar la Curva ROC con ggplot2
roc.list<-list(R1.b)
names(roc.list)<-c("TAS")

pROC::ggroc(roc.list, aes("linetype", "color"),size=1)+
  theme(legend.position="right")+
  theme_minimal()+
  ggtitle("") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="black", linetype="solid")+
  xlab("Tasa de Falsos Positivos (1-Especificidad)") + 
  ylab("Tasa de Verdaderos Positivos (Sensibilidad)") +
  labs(title = "AUROC de TAS para Predecir Falla Cardiaca Incidente",
       col="Parámetros", 
       linetype="Parámetros",
       caption  = "")
