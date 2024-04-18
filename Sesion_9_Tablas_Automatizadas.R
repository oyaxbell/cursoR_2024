###Generación de tablas y reportes estadísticos descriptivos reproducibles
###Curso: Programación Estadística en R – 2024
###Instructor: Dr. Neftali Eduardo Antonio-Villa###
###Twitter: @NeftaliAntonioV


####Seccion 1: Librerias, directorio de trabajo y cargar datos#####

library(readxl)
library(dplyr)
library(tidyverse)
library(flextable)
library(gtsummary)
library(data.table)

#Establecer el directorio de trabajo#
#Recuerda
#Session -> Set Working Directory -> Choose Directory -> Copiar -> Pegar Script

setwd("/Users/nefoantonio/Library/CloudStorage/OneDrive-UNIVERSIDADNACIONALAUTÓNOMADEMÉXICO/Clases/Curso R (PECEM)")

##Carga tu df 
fram<-read_excel("framingham.xls") #Vamos a cargar un ejemplo de un dataframe del Estudio de Framingham


####Seccion 2: Presentación de datos en formatos tabulares####

#Introducción a Flextable
#Se desarrolló como un paquete que permite tener una representación grafica y exportable de tablas en formato dataframe en formatos para html, pptx, doc y pdf
#Veamos como es su función principal

#Seleccionemos las primeras dos columnas y los primeros 5 sujetos y guardemoslo como un objeto
df1<-fram[1:5,1:2]
#Ejecutemos la funcion de flextable()
flextable(df1)

#Cual es el reto? Crear una tabla que nos permita evaluar la frecuencia y distribución de nuestros eventos
table(fram$male)
flextable(table(fram$male)) #Porque hay un error? Porque hay que construir dataframes para esta visualizacion

#Puedes convertir una tabla en formato df con data.frame()
sexo<-as.data.frame(table(fram$male))
names(sexo)<-c("Categoria","Frecuencia") #Vamos a ponerles nombres a nuestras columnas.
flextable(sexo)

#Se puede concatenar un titulo a nuestra tabla con la funcion add_header_row()

flextable(sexo)%>%add_header_row(values = c("Frecuencia de Sexos"), colwidths = c(2))

#Se puede añadir una nota al pie de pagina

flextable(sexo)%>%add_header_row(values = c("Frecuencia de Sexos"), colwidths = c(2))%>%add_footer_lines("Estudio de Framingham, 1968")

#Para evitar que exista un exceso en lineas de codigo, guarda tu df como objeto
h1<-as.data.frame(table(fram$male))
names(h1)<-c("Categoria","Frecuencia")
flextable(h1)%>%add_header_row(values = c("Frecuencia de Sexos"), colwidths = c(2))%>%add_footer_lines("Estudio de Framingham, 1968")

####Seccion 3: Elementos basicos de gtsummary#####

#Afortunadamente Flextable permite acoplar una estructura amigable a flextable

fram %>% 
  dplyr::select(male,age,diabetes,education,totChol)%>%
  tbl_summary()

#¿Qué notan de particular?
#Hay que realizar unos ajustes previos a esta estructura

#Typos
#Eliminar typos o valores erroneos
#Ver que las variables continuas sean continuas
fram$age<-as.numeric(fram$age)
fram$cigsPerDay<-as.numeric(fram$cigsPerDay)
fram$sysBP<-as.numeric(fram$sysBP)
fram$diaBP<-as.numeric(fram$diaBP)
fram$totChol<-as.numeric(fram$totChol)
fram$glucose<-as.numeric(fram$glucose)
fram$BMI<-as.numeric(fram$BMI)

fram %>% 
  dplyr::select(male,age,diabetes,education,totChol)%>%
  tbl_summary()


#Etiqueta de Categorias
#Se pueden añadir el nombre de las categorias
#Sugerencia: Sólo hacerlo para variables categóricas ordinales o multinomiales

fram$education<-factor(fram$education,levels = c(1,2,3,4),labels = c("Primaria/Secundaria","Preparatoria","Universidad","Posgrado"))

fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary()

#Bonus: Se puede hacer explicito los valores pedidos
fram$education<-forcats::fct_explicit_na(factor(fram$education))

#Etiquetas
#Con la función data.table es posible asignar un cambio en la etiqueta de cómo se nombran las variables
#setattr(df$var, "label", "Nombre de Variable, (Unidad de Medición)")

setattr(fram$male, "label", "Sexo, (n, %)")
setattr(fram$age, "label", "Edad, (años)")
setattr(fram$diabetes, "label", "Diabetes, (n, %)")
setattr(fram$education, "label", "Educación, (n, %)")
setattr(fram$totChol, "label", "Colesterol Total, (n, %)")

fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary()


####Seccion 4: Modificaciónes básicas de tbl_summary()####

#Modifiquemos algunos parametros de tbl_summary

##Evaluación de missing values

#Solo en aquellas que tuvieran
fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(missing = "ifany")

#Siempre reportado
fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(missing = "always")


##Cambiar la etiqueta de presentación
fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(label = c(male ~ "Hombres",age ~ "Edad"))

##Redondeo Decimal

#Dos decimales
fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(digits = c(everything() ~ 2))

#Tres decimales
fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(digits = c(everything() ~ 3))

##Cambiar tipo de variables
#Forzar una variable continua a categorica
fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(type = list(totChol ~"categorical"))

##Porcentaje
#Por Fila (util en algunos contextos)
fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(percent = "row")

#Incluir variables especificas
#Seleccio posterior al dplyr::select()
fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(include = c("age","diabetes"))

##Estadístico a presentar
#Presentar media y desviación estandar
fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(statistic = list(c(age) ~ "{mean} ({sd})"))

#Presentar mediana, minimo y máximo
fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(statistic = list(c(age) ~ "{median} ({min},{max})"))

#Presentar percentilas
fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(statistic = list(c(age) ~ "{mean} ({p10},{p90})"))

#Presentar unicamente porcentaje
fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)"))


#Creación de tabla final

tab.1<-fram %>% 
  dplyr::select(male,age,age,diabetes,education,totChol)%>%
  tbl_summary(label = c(male ~ "Hombres",age ~ "Edad"),
              digits = c(everything() ~ 1),
              statistic = list(c(age) ~ "{mean} ({sd})"))%>%
  bold_labels()%>%
  modify_spanning_header(all_stat_cols() ~ "**Muestra**")%>%
  modify_header(all_stat_cols() ~ "**n=4,240**")%>%
  modify_caption(caption = "Estudio Framinham")%>%
  modify_footnote(all_stat_cols() ~ "**Datos Continuos: media (DE) o mediana (RIQ); Datos Categóricos: n (%)**")


#Guardar la tabla

tab.1%>%
as_flex_table()%>%
  flextable::save_as_docx(path="Table_1.docx")

####Seccion 5: Descripción de dos o más grupos de estudio####

# Comparación básica de dos grupos (e.g., pacientes con y sin diabetes)

fram %>%
  select(male,age,diabetes,education,totChol) %>%
  tbl_summary(
    by = diabetes, # Dividir la tabla por la variable 'diabetes'
    missing = "ifany" # Mostrar datos faltantes solo si los hay en alguna de las columnas
  ) 


fram %>%
  select(male,age,diabetes,education,totChol) %>%
  mutate(diabetes=factor(diabetes,labels = c("Negativo", "Positivo")))%>%
  tbl_summary(
    by = diabetes, # Dividir la tabla por la variable 'diabetes'
    missing = "ifany" # Mostrar datos faltantes solo si los hay en alguna de las columnas
  ) 

#Añadir una Comparación estadística

fram %>%
  select(male,age,diabetes,education,totChol) %>%
  mutate(diabetes=factor(diabetes,labels = c("Negativo", "Positivo")))%>%
  tbl_summary(
    by = diabetes, # Dividir la tabla por la variable 'diabetes'
    missing = "ifany" # Mostrar datos faltantes solo si los hay en alguna de las columnas
  ) %>%
  add_p()

#Crear una tabla final

tab.2<-fram %>%
  select(male,age,diabetes,education,totChol) %>%
  mutate(diabetes=factor(diabetes,labels = c("Negativo", "Positivo")))%>%
  tbl_summary(
    by = diabetes, # Dividir la tabla por la variable 'diabetes'
    missing = "ifany" # Mostrar datos faltantes solo si los hay en alguna de las columnas
  ) %>%
  add_p()%>%
  bold_labels()%>%
  modify_spanning_header(all_stat_cols() ~ "**Diabetes**")%>%
  modify_caption(caption = "Tabla 1: Características descriptivas de la población Framingham por Diabetes")%>%
  modify_footnote(all_stat_cols() ~ "**Datos Continuos: media (DE) o mediana (RIQ); Datos Categóricos: n (%)**")


#Guardar la Tabla

tab.2%>%
  as_flex_table()%>%
  flextable::save_as_docx(path="Table_2.docx")
