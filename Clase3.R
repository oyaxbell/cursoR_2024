## Clase 3: Cargado, manejo y manipulación de datos utilizando dplyr y tidydata ##
## Dr. Neftali Eduardo Antonio Villa ##
## 13 de febrero de 2024 ##


#### Sección 1: Instalación, carga y manual de paquetes ####

### ¿Cómo instalar paquetes? ##

## Método 1: Método más sencillo, descarga los paquetes desde CRAN (necesita conexión a internet),
##           paquete debe ir entre comillas
install.packages("gapminder")
install.packages("dplyr")
install.packages("tidyr")
install.packages("pacman")
install.packages("tidyverse")
install.packages("readr")
install.packages("haven")
install.packages("mice")

# Se pueden concatenar las líneas de código para correrlo de forma más utilizanodo ;
install.packages("gapminder"); install.packages("dplyr"); install.packages("tidyr"); install.packages("pacman")
install.packages("tidyverse"); install.packages("readr"); install.packages("haven")

## Método 2: En caso de aparecer algún error al tratar de instalar paquetes con el Método 1 (ej. "Versión
##           incompatible de R") descarga paquete compatible con versión de R desde la fuente (GitHUB) 
install.packages("tidyverse", type = "source")

## Método 3: Directo de la pestaña de "Packages" en la ventana de visualización.

#1) Dar click en boton de instalar
#2) Seleccionar la opcion de repositorio CRAN
#3) Buscar el paquete a descargar


### ¿Cómo puedo cargar paquetes? ###

## Método 1: Método más sencillo, útil cuando la función que deseamos utilizar no está repetida.
##           NO se pone paquete entre comillas
library(gapminder)
library(tidyverse)
library(dplyr)
library(tidyr)
library(pacman)
library(readr)
library(haven)
#library(xlsx)
library(mice)

# Igualmente se pueden concatenar las líneas de código
library(gapminder);library(dplyr);library(tidyr);library(pacman)
library(tidyverse);library(readr);library(haven);library(mice)

# Si hay un error en una de las funciones, la concatenación se detiene
library(tidyverse);library(redr);library(haven)
3+3; ufdu ;2+2

# El paquete "pacman" puede incluir la función de p_load para instalar aquellos paquetes que no se tengan.

pacman::p_load(gapminder, dplyr, tidyr, tidyverse, readr, haven, mice)


## Método 2: Útil cuando la función que deseamos utilizar está repetida entre paquetes:
##           [Nombre del paquete]::[función]

?filter
?dplyr::filter

# ¿Cómo saber qué funciones están repetidas entre paquetes?
library(dplyr)
?filter


### Si ya cargué un paquete ¿Puedo quitarlo sin tener que reiniciar R? ###
## Método 1: Dirígete al panel de Paquetes, da click en "Packages" y buscar el paquete que
##           queremos quitar y eliminar palomita

## Método 2: Uso de la función opuesta a library: detatch()
detach("package:tidyverse", unload= TRUE)
detach("package:mice", unload= TRUE)
detach("package:gapminder", unload= TRUE)


### ¿Dónde puedo encontrar las características/manuales de algún paquete? ###

## Método 1: ?[paquete]
?gapminder
?tidyr

## Método 2: Buscar el nombre del paquete en el panel de paquetes (Pestaña "Help")



#### Sección 2: Carga, guardado e importación de datos ####

### ¿Cómo cargo datos de R? ###
data("iris")
gapminder<-gapminder::gapminder

?iris
?gapminder

iris
gapminder

## Puedes asignar un nombre a tus datos una vez cargados
datos1 <- iris 
datos2 <- gapminder


### ¿Cómo guardar nuestros datos? ###

## PASO 1: Especificar nuestro directorio de trabajo (la consola nos indica en cuál estamos), hay 2 métodos:
#   Método 1: En el panel inferior a la derecha
#         - Dar clic en la pestaña "Files"
#         - Dar click en "..." y seleccionas la ubicación donde quieres el archivo, dar "Open"
#         - Dar click en "More" y seleccionar "Set as working directory"
#   Método 2: Se utiliza la función setwd
#         - Ir a carpeta donde guardaremos nuestros documentos y copiar la dirección en forma de texto
#         - Pegar dirección entre comillas en la función setwd() (OJO: diagonales deben cambiar de sentido)

setwd("/Users/nefoantonio/Library/CloudStorage/OneDrive-UNIVERSIDADNACIONALAUTÓNOMADEMÉXICO/Clases/Curso R (PECEM)")

## PASO 2: Guardar nuestros datos en el formato deseado con la función write.
##         Es importante verificar que estén cargados los paquetes.
#         - readr: lee y guarda en formatos .csv, .txt (paquete incluido en Tidyverse)
#         - haven: lee y guarda en .stata, .sas, .sav
#         - xlxs: lee y guarda en formatos .xls, .xlsx 

write_csv(datos1, "datos.csv")
write_csv(datos1, "datos.txt")
write_sav(datos1, "datos.sav") # SPSS
write_sas(datos2, "datos.sas")
#write.xlsx(datos1, "datos.xlsx")


### ¿Cómo cargar nuestros datos? ###

## Método 1: En la ventana de ambiente global
#         - Dar click en "Import DataSet" y seleccionar tipo de archivo deseado
#         - Poner dirección o dar click en "Browse", buscar el archivo deseado y dar en "open"
#         - Cambiar el nombre si se desea y dar click en "Import"

## Método 2: Uso de la función read
read_csv("datos.csv") # NO lo guarda en ambiente global

datos <- read_csv("datos.csv")
# Otras funciones son read_sas, read_sav, read_excel
haven::read_sas
haven::read_sav
readxl::read_excel


#### Sección 3: Exploración de nuestros datos ####

datos <- read_csv("datos.csv")

View(datos)

## Función head(x = [Objeto], n= [# de observaciones]), por default muestra las primeras 6 observaciones
head(x = datos)
head(x = datos, n = 3)

## Función tail(x = [Obeto], n= [# de observaciones]), por default muestra las últimas 6 observaciones
tail(datos)
tail(datos, 10) #Podemos escribirlo sin especificar argumentos, pero en el mismo orde

#Submuestras específicas: usar corchetes [] (primero filas y después columnas)
datos[7:15, 5]
datos[1, 5] #Primera Fila, Cuarta Columna
datos[5, 1] #Quinta Fila, Primera Columna


## Función str([Objeto]). Muestra de forma compacta la estructura de un objeto
#Recordatorio: Dataframe: Conjunto de vectores de distintas clases ordenadas en un arreglo matricial.

str(datos1)
glimpse(datos1)

## Función view(x, título). Genera una pestaña que muestra todos los datos
View(datos) # También se puede dar clic en la tabla que aparece en global environment después del dataset
View(datos, "Datos de Iris")

## Función summary() Muestra un resumen de todas tus variables.
# En variables cuantitativas te genera algunas medidas de tendencia central
summary(datos1)

#### Sección 4: Manipulación de base de datos utilizando tydr y dplyr ####

### Pipeline (Ctrl (Cmd) + Shift + M) toma objetos y los concatena con funciones
#-- Objeto %>% Función --#
#%>%: Definicion: una forma de encadenar múltiples operaciones de una manera concisa y expresiva.

str(datos1)
datos1 %>% str()

# Se pueden consultar todas las funciones que se pueden invocar con pipelines en paqueteria de tidyverse

### Reorganización de datos - tidyr/dplyr ###
View(datos, "Datos de Iris")

## Limpieza de variables con janitor
#Función clean_names(): Queremos poner el nombre de mis variables en minuscula y sin espacios
datos.dummy<-data.frame(Nombres_Estudiantes=c("Juan","Pedro","Ana","Rosa"),
                        `Edades de Estudiantes`=c(24,25,26,23),
                        `Calificación Final`=c(10,9,10,10))
datos.dummy
datos.dummy<-janitor::clean_names(datos.dummy)


## Función unite(): Queremos juntar las características de sépalo y pétalo en la misma columna
#-- unite(data,col,..., sep) --#

# data = Base de datos
# col = Nombre que tendrá la nueva columna, debe ir entre comillas
# ... = Nombres de las columnas que se quieren unir, SIN comillas
# sep = Carater que queremos que separe la información, entre comillas (ej. "", " ", "/")

#Es una función muy similar a paste() y paste0()
paste0(datos$Sepal.Length,"-",datos$Sepal.Width)

datos2 <- unite(datos, "Sépalo (Length-Width)", Sepal.Length, Sepal.Width, sep = " - ")
datos2 <- datos %>% unite("Sépalo (Length-Width)", Sepal.Length, Sepal.Width, sep = " - ")
View(datos2, "Unite")

#Es una función util cuando se quieren hacer IDs unicos.

# Concatenamos ambas funciones para hacer una línea de código más corta
datos2 <- datos %>% unite("Sépalo (Length-Width)", Sepal.Length, Sepal.Width, sep = " - ") %>% 
  unite("Pétalo (Length-Width)", Petal.Length, Petal.Width, sep= " - ")
View(datos2, "Unite")

#Como podemos ver, se juntan caracteristicas, desapareciendo las variables originales.

## Función separate(): Queremos separar las características de sépalo y pétalo en dos diferentes columnas
#-- separate(data,col, into, sep) --#

# data = Base de datos
# col = Nombre de la columna que queremos separar, SIN comillas
# into = Vector con los nombres de las columnas que queremos anexar, CON comillas
# sep = Es el caracter que define la separación

datos3 <- datos2 %>% separate(`Sépalo (Length-Width)`, c("Sepal Length", "Sepal Width"), sep = " - ") %>% 
  separate(`Pétalo (Length-Width)`, c("Petal Lenght", "Petal Width"), sep = " - ")
View(datos3, "Separate")

# Con separate, podemos regresar las variables a su estado original

## Función arrange(): Queremos ordenar nuestra base según la longitud del sépalo

# De menor a mayor
#-- arrange(data,by_group) --#

# data = Base de datos
# by_groups = Variable en la que se basará el orden
datos4 <- datos %>% arrange(Sepal.Length)
View(datos4, "arrange-+")


# De mayor a menor
#--- arrange(data,desc(by_group)) ---#

# data = Base de datos
# by_groups = Variable en la que se basará el orden
datos5 <- datos %>% arrange( desc(Sepal.Length) )
View(datos5, "arrange+-")


## Función rename(): Queremos cambiar el nombre de las columnas
#-- rename(data, ...) --#

# data = Base de datos
# ... = "Nuevo nombre" = Variable
datos6 <- datos %>% rename("S_Longitud" = Sepal.Length, "S_Ancho" = Sepal.Width,
                           "P_Longitud" = Petal.Length, "P_Ancho" = Petal.Width)

View(datos6, "rename")


### Depuración de base de datos - dplyr ###

## Función distinct(): Queremos eliminar observaciones que se encuentran repetidas
datos.7<-data.frame("A"=c(1,2,2,3,3,4,5), "B"=c("A","B","B","C","C","D","E"))
datos.7%>%distinct()


## Función filter(): Filtra el data.frame por una condición especifica
# data = Base de datos
# by. = Variable de seleccion con la condicion 
datos2%>% filter(Species=="virginica")

#Queremos sólo analizar las especies virgínica y versicolor que tengan una longitud de
##sépalo >5.5 (se filtran OBSERVACIONES)

#Metodo 1: Filtros Individuales
datos2<- datos2 %>% filter(Species %in% c("virginica","versicolor")) %>% filter(Sepal.Length >5.5)

#Metodo 2: Filtros Concatenados
datos3 <- datos2 %>% filter( (Species=="virginica"|Species=="versicolor")&(Sepal.Length>5.5) )

View(datos, "Original")
view(datos3, "Filter")


## Función sample(): Queremos obtener una muestra aleatoria
set.seed(123) # Para tener código reproducible 

# Método 1: Se establece la fracción de la muestra que queremos
#-- sample_frac(data, size, replace=TRUE/FALSE) --#

# data = Base de datos
# size = Fración que queremos (0 a 1)
datos4 <- datos3 %>% sample_frac(0.7, replace = T)
View(datos4, "Sample_Frac")

nrow(datos4)/nrow(datos3) * 100
ncol(datos4)

# Método 2: Se establece el número de observaciones que queremos
#-- sample_n(data, size, replace=TRUE/FALSE) --#

# data = Base de datos
# size = Número de observaciones que queremos
datos5 <- datos3 %>% sample_n(50, replace = T); View(datos5, "Sample_n")


## Función select (): Sólo nos interesa analizar las longitudes de sépalo y pétalo segun la especie
##                    (se seleccionan VARIABLES)
#-- select(data, ...) --#

# data = Base de datos
# ... = Nombre de las variables que deseamos seleccionar, en el orden que queremos
datos6 <- datos5 %>% select(Sepal.Length,Petal.Length,Species)
View(datos6, "Select")

datos7 <- datos5 %>% select(Species,Sepal.Length,Petal.Length)
View(datos7, "Select2")


# Realiza el siguiente ejercicion para extraer un data.frame con las siguientes caracteristicas:
#     - Incluye las variables: especie, longitud de sépalo y longitud de pétalo
#     - Son flores de la especie virginica o versicolor
#     - Sólo incluye una muestra aleatoria de 50 observaciones que contiene las siguientes
#     - Con longitud de sépalo >5.5
#     - No tiene que tener observaciones repetidas

# ¿Se puede hacer todo en una línea de código?

set.seed(123)
datos_final <- datos %>% 
  select(Species,Sepal.Length,Petal.Length)%>% 
  sample_n(50, replace = TRUE) %>%
  filter((Species=="virginica" | Species=="versicolor") & (Sepal.Length>5.5)) %>%
  distinct() 
  

View(datos_final, "Datos depurados")


### Creación de nuevas variables ###

## Función mutate(): Genera nuevas variables y las anexa como nuevas columnas
#-- mutate(data,...)--#

# data = Base de datos
# ... = Nombre de nueva variable = Operación 
datos_final2 <- datos_final %>% mutate("Diferencia_Longitud" = Sepal.Length - Petal.Length,
                                       "Longitud_Total" = Sepal.Length + Petal.Length)
View(datos_final2, "Mutate")

## Función transmute(): Genera nuevas variables y elimina el resto de las columnas
#-- transmute(data,...) --#
# data = Base de datos
# ... = Nombre de nueva variable = Operación 
datos_final3 <- datos_final %>% transmute("Diferencia_Longitud" = Sepal.Length - Petal.Length,
                                          "Longitud_Total" = Sepal.Length + Petal.Length)
View(datos_final3, "Transmute")



### Resumen de nuestros datos ###

### FUNCIONES DE RESUMEN ###
## min() - Valor más pequeño del vector
## max() - Valor más grande del vector
## mean() - Promedio del vector
## median() - Mediana del vector
## var() - Varianza del vector
## sd() - Desviación estánddar del vector
## IQR() - Rango intercuartil del vector
## quantile() - Arroja el cuantil que se especifique

quantile(datos_final$Sepal.Length, probs = c(0.9))

#Percentila 25 y 75
quantile(datos_final$Sepal.Length, probs = c(0.25,0.75))


## Sólo quiero resumir algunas variables de mi base de datos
#-- summarise(data, ...) --#

# data = Base de datos
# ... = Nombre de nueva variable = función de resumen(variable deseada)

resumen1 <- datos_final %>% summarise("meanLongitud_S"= mean(Sepal.Length),
                                      "varLongitd_S"= var(Sepal.Length),
                                      "sdLongitud_S"= sd(Sepal.Length))

View(resumen1, "Resumen 1")


## Quiero resumir TODAS las variables de mi base de datos (útil cuando todas son variables continuas)
#-- summarise_each(data,funs()) --#

# data = Base de datos
# funs = Funciones de resumen deseadas separadas por comas

resumen2 <- datos_final %>% summarise_each( funs(mean, sd) )
View(resumen2, "Resumen 2")



### Agrupación de datos ###

## Función group_by(): 
data("gapminder")
gapminder

#Primero filtramos la base para conservar solo países del continente africano
#en los años mayores a 1980 y menores a 1960
datos_africa <- gapminder %>% filter( (continent=="Africa" & year>1980) | 
                                        (continent=="Africa" & year<1960) )
View(datos_africa)

#Después agrupamos los datos por año con group_by
#Y con summarise obtenemos la media de expectativa de vida y PIB per capita
datos_agrupados <- datos_africa %>% group_by(year) %>% 
  summarise("meanExp"=mean(lifeExp), "meanGDP"=mean(gdpPercap))
View(datos_agrupados)


# Realiza el siguiente ejercicio con la base gapminder
#     - Evalua el promedio de PIB de los paises de Asia
#     - De los años 1980 a 2000
#     - Exluye a China de la Evaluacion
#     - Ordenalos de mayor a menor

gapminder%>%
  filter(continent==c("Asia"))%>%
  filter(year>=1980 |year<=2000)%>%
  filter(country!="China")%>%
  group_by(country)%>%
  summarise("meanExp"=mean(gdpPercap))%>%
  arrange(desc(meanExp))
  

#### Sección 5: Identificación de errores de codificación y variables faltantes ####

## Valores faltantes ##

#En R, los valores faltantes aparecen como "NA" (Not Available - No disponible)
data("boys")
?boys
boys #Esta base de datos tiene una gran cantidad de NA

#La función is.na regresa un valor lógico
#Nos arroja TRUE si el valor es un NA y FALSE si no lo es
is.na( c(NA, 2, NA, 4, NA, 6) ) 

is.na(boys) #Esto puede aplicarse a toda la base de datos
is.na(boys$bmi) #O a un solo vector

#Recordemos que el caracter "!" se emplea como un operador lógico que significa negación
!TRUE #Ejemplo 1: la negación de TRUE es FALSE

#Cuando negamos la función "is.na", la interpretación se invierte
#Ahora tendremos TRUE si el valor NO es NA y FALSE si sí es NA
!is.na(boys$bmi) 

#Esto nos sirve para cuantificar los NA que tenemos gracias a la función sum
#La función "sum" suma todos los elementos dentro de un vector
sum(c(2,10,100))

#En el caso de un vector lógico, TRUE equivale a 1 y FALSE equivale a 0
sum(c(TRUE,TRUE,FALSE)) #Entonces la función sum nos arroja el total de TRUE que hay

#Así, nosotros podemos cuantificar cuántos valores perdidos tenemos en un vector
sum(is.na(boys$bmi))
#Del mismo modo, usando la negación, podemos cuantificar los valores completos que tenemos
sum(!is.na(boys$bmi))

#Otra manera rápida de saber el número de NA en una base de datos es la función summary,
#Que además de los otros estadísticos descriptivos, arroja el número de NA
summary(boys)

#También podemos obtener el porcentaje de valores perdidos al dividirlo entre
#el número total de valores y multiplicar por 100%, en nuestro caso usaremos la función nrow
#para conocer el número de filas que tiene nuestra base de datos
( sum(is.na(boys$gen)) / nrow(boys) ) * 100

#Muchas funciones no son compatibles con NA de forma predeterminada
mean(boys$hgt)
mean(boys$hgt, na.rm = T) #Se debe especificar que deseamos que se quiten los NA


#Se pueden eliminar los NA de una base de datos para una sola variable con filter
boys2 <- boys %>% filter( !is.na(hgt) )
sum(is.na(boys2$hgt)) #Ahora hay 0 NA

#Podemos hacernos una idea de cuántos NA se eliminaron comparando el número de filas
nrow(boys) #Antes
nrow(boys2) #Después


#Alternativamente, se pueden eliminar TODOS los NA de TODAS las variables con la función "na.omit"
boys3 <- boys %>% na.omit()
nrow(boys)
nrow(boys3) #A veces esto es contraproducente pues elimina una enorme cantidad de información


#Para no eliminar cantidades tan grandes de datos, las mejores opciones son
#   1) Eliminar solo los valores perdidos de las variables que realmente usaremos
#   2) Hacer imputación de datos ("educated guess")

#EJEMPLO
boys4 <- boys %>% 
  select(hgt, age, wgt) %>% #Seleccionamos solo edad talla y peso
  na.omit() #Eliminamos los NA solamente de esas variables

nrow(boys)
nrow(boys4) #Se perdió menos información que cuando eliminábamos NA de todas las variables


#### Sección 6: Ejercicio integrando todo ####

# 1) Seleccionar solo edad, talla, peso y circunferencia de cabeza
boys %>% select(age, hgt, wgt, hc)

# 2) Eliminar valores perdidos de todas las variables seleccionadas
boys %>% select(age, hgt, wgt, hc) %>% na.omit ()
boysA <- boys %>% select(age, hgt, wgt, bmi) %>% na.omit () #Lo guardamos como boysA

# 3) Clasificar en función de su circunferencia de cintura (percentila 90)

quantile(boysA$bmi, 0.9) #Obtenemos la percentila 90 de la circunferencia de cabeza
P90 <- quantile(boysA$bmi, 0.9) #La guardamos en un objeto para usarla después
boysA$bmi>=P90 #Con esta línea podemos observar qué valores están por arriba de la percentila 90

#Incluso podemos transformarlo en factor para etiquetar como Pequeña o Grande circunferencia de cabeza
factor(boysA$bmi>=P90, labels=c("Sin Obesidad", "Obesidad"))
#Y podemos agregar este factor como una nueva columna de la base de datos (la guardamos como boysB)
boysB <- boysA %>% mutate("IMC_Categorico"= factor(bmi>=P90, labels=c("Sin Obesidad", "Obesidad")))
boysB


# 4) Agrupar y obtener la media y desviación estándar de cada variable
boys_FINAL <- boysB %>% 
  group_by(IMC_Categorico) %>% 
  summarise_each( funs(mean, sd) )

View(boys_FINAL)









