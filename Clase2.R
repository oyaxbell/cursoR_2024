## Clase 2: Funciones, loops y automatización de tareas en R ##
## Carlos Alberto Fermín Martínez ##
## 06 de febrero de 2024 ##

####---------- FUNCIONES ------------------- ####
###--- ¿Qué son las funciones? ---##
#Bloques fundamentales del funcionamiento del lenguaje R
#Sintaxis: nombre de la función seguido de paréntesis ()

c(1,2,3,4,5) #Función para crear un vector c()
data.frame(c(1,2,3,4,5), c(6,7,8,9,10)) #Crear un data frame
cbind(c(1,2,3), c(4,5,6)) #Unir vectores por columnas
rbind(c(1,2,3), c(4,5,6)) #Unir vectores por filas

#Algunos ejemplos de funciones
mean(c(9, 9, 10, 8)) #Calcular la media mean()
median(c(9, 9, 10, 8)) #Calcular la mediana
sum(c(9, 9, 10, 8)) #Sumar elementos de un vector
length(c(9, 9, 10, 8)) #Obtener la longitud de un vector length()

c("A", "B")
paste("Hola", "Y adiós") #Unir caracteres separados por un espacio
paste0("Hola", "Yadiós") #Unir caracteres sin separación

#Etc...

###--- Crear funciones ---##
#1. Crear nuestra propia función permite guardar en el ambiente
#global acciones que queramos repetir una gran cantidad de veces

funcion1 <- function(x) {
  x**2
}
funcion1(10)
funcion1(43)

#2. Nuestra función puede tener tantos argumentos como queramos
funcion2 <- function(x, y, z){ 
  ((x**2) + sqrt(y)) / z
}
funcion2(10, 2, 3)

#3. Al usar la función debemos especificar todos los argumentos
funcion2(10, 2, 3)
funcion2(x=10, y=2, z=3)
funcion2(y=2, z=3, x=10)
funcion2(3, 10, 2)
funcion2(10, 2) #¿Qué pasa si no especificamos el tercer argumento?

#Pero... podemos asignarle valores fijos a un argumento
funcion3 <- function(x, y, z=10){ #Z queda fijado como 10
  (x + y) / z
}
funcion3(x=10, y=10) #(10+10)/10 Ya no fue necesario especificar Z
funcion3(x=10, y=10, z=100) #Pero aún tenemos la flexibilidad de cambiarlo


#4. Las funciones pueden usarse para CUALQUIER cosa

#Ejemplo: Unir un número y un caracter
funcion4 <- function(n){
  paste0("Número de alumnos: ", n)
}
funcion4(100)

#Ejemplo: Crear una tabla resumiendo dos variables
women
str(women)
women$height
summary(women$height) #Resumen de esa variable

funcion5 <- function(var1, var2){
  talla <- summary(var1)
  peso <- summary(var2)
  cbind(talla, peso)
}
funcion5(women$height, women$weight)


#5. Las funciones son especialmente útiles cuando
# tenemos tareas que ocupan mucho espacio de código
10**2
summary(women)

height_cm <- women$height*2.54
weight_kg <- women$weight*0.4535
imc <- weight_kg/((height_cm/100)**2)


#Con la función
imc_units <- function(height, weight){
  talla_cm <- height*2.54
  peso_kg <- weight*0.4535
  imc <- peso_kg/((talla_cm/100)**2)
  imc
}

imc_units(women$height, women$weight)

imc_units()
imc_units()
imc_units()
imc_units()
imc_units()


####---------- MANEJO DE BASES ------------- ####
#1. INDEXACIÓN: Seleccionar casos en un vector o data frame
a <- c(1:100)
a[1]
a[1:10]
a[50:59]
a[c(1,33,78,100)]

letters[1]
letters[5:10]

#2. Para bases de datos esto cambia ligeramente
?iris
str(iris)
head(iris)

base <- head(iris)
base[1,1] #El primer número es la fila y el segundo la columnas
base[5,1]
base[1,5]

base[5,] #Elegir toda una fila
base[,1] #O toda una columna
base[5] #Si no pongo comas, elige columnas

base[1:3,] #Puedo elegir varias filas/columnas al mismo tiempo
base[,c(1,5)]
base[1:3]


#3. FILTROS: La indexación funciona con condiciones lógicas
c(1,2,3,4,5) >= 3

a>=90
a[a>=90]

base
base[base$Sepal.Length>=5,]
#Alternativa: usar la función filter del paquete dplyr

#Ejemplos
#base[base$glucose>=126,]
#base[base$PAS>=140,]


####---------- FAMILIA APPLY --------------- ####
###--- apply ---##
str(iris)
iris[,-5]
iris[-c(1:10),]

base2 <- iris[1:10,-5]

#Aplicar una función a lo largo de toda una fila o columna
apply(X = base2, #Base de datos
      MARGIN = 1, #Índice o margen: 1=por filas, 2=por columnas
      FUN = mean) #Función que quieres aplicar

apply(X = base2, MARGIN = 1, FUN = mean)
apply(X = base2, MARGIN = 2, FUN = mean)

apply(X = base2, MARGIN = 1, FUN = mean) #Media de cada fila
apply(X = base2, MARGIN = 1, FUN = sum) #Suma de cada fila
apply(X = base2, MARGIN = 2, FUN = sd) #Desviación estándar de cada columna

funcion_apply <- function(x){sum(x**2)/10}
apply(X = base2, MARGIN = 2, FUN = funcion_apply) #Funciones creadas

#Base de pacientes con comorbilidades
diabetes <- c(0,0,1)
hipertension <- c(1,0,1)
cancer <- c(0,0,0)
ecv <- c(1,1,1)

base10 <- data.frame(diabetes, hipertension, cancer, ecv)
apply(base10, MARGIN=1, sum) #Comorbilidades en cada paciente
apply(base10, MARGIN=2, sum) #Cuántos pacientes tienen cada comorbilidad

#Para escoger solo un caso
apply(base10, MARGIN=1, sum)[2]
sum(base10[2,])


###--- lapply, sapply ---##
head(iris)

data.frame(
  c(1:3),
  c(1:4))

list(
  c(1:3),
  c(1:4))

#Las listas no necesariamente deben ser objetos del mismo tamaño
x <- list(A = 1:10, B = log(1:7), C = c(1,0,1))
x
mean(x) #Pero no se le pueden aplicar funciones directamente a una lista

#lapply toma una lista, aplica la función y regresa una lista
lapply(X = x, #LISTA DE OBJETOS
       FUN = mean) #FUNCIÓN

#sapply hace lo mismo pero regresa un vector en vez de una lista
sapply(X = x, #LISTA DE OBJETOS
       FUN = mean) #FUNCIÓN

sapply(X = x, FUN = sum)
sapply(X = x, FUN = sd)
sapply(X = x, FUN = funcion_apply)

#La función debe de ser compatible con los datos
y <- list(A = LETTERS[1:10], B = c("A","B"), C = 1:3)
sapply(y, FUN=sum)
sapply(y, FUN=paste, collapse=" ")

#Para que esta línea funcionara faltaba el argumento "collapse" de
#la función paste, en el que básicamente indicas con qué caracter quieres
#unir/pegar el resto de caracteres. Puede ser un espacio, guión, ninguno...
sapply(y, FUN=paste, collapse=" ")
sapply(y, FUN=paste, collapse="-")
sapply(y, FUN=paste, collapse="")
sapply(y, FUN=paste, collapse=".")



###--- tapply ---##
str(iris)

#tapply toma una variable (X) y aplica una función (FUN) estratificando
#los niveles o clases de otra variable (INDEX)
tapply(X = iris$Sepal.Length, #VARIABLE A LA QUE LE VAMOS A APLICAR LA FUNCIÓN
       INDEX = iris$Species, #VARIABLE CON LA QUE VAMOS A ESTRATIFICAR
       FUN = mean) #FUNCIÓN QUE QUEREMOS APLICAR

#EJEMPLO (NO LES VA A CORRER PORQUE LOS DATOS NO EXISTEN)
tapply(X = base_md$PAS,
       INDEX = base_md$diabetes,
       FUN = mean)
#sanos   diabetes
#113      135

tapply(X = iris$Petal.Length, INDEX = iris$Species, FUN = sum)
tapply(X = iris$Sepal.Width, INDEX = iris$Species, FUN = sd)
tapply(X = iris$Petal.Width, INDEX = iris$Species, FUN = funcion_apply)



####---------- LOOPS ----------------------- ####
###--- for ---##
#Los loops se usan para iterar o repetir una acción o tarea
#una cantidad determinanada de veces, hasta obtener el resultado deseado

#EJEMPLO (NO LES VA A CORRER PORQUE LOS DATOS NO EXISTEN)
for (variable in sequence) {
  expression}

#ESTOS SI
for (x in 1:5) {
  print(x)}

for (x in 1:5) {
  print(x**2)}

base3 <- head(iris[-5])

for (x in 1:5) { #Imprimir la fila 1, luego la 2... hasta la 5
  print(base3[x,])}

nrow(base3)
ncol(base3)

for (x in 1:nrow(base3)) { #Impirmir fila 1, luego 2... hasta la última
  print(base3[x,])}

for (x in 1:ncol(base3)) { #Impirmir resumen de columna 1, luego 2...
  print(summary(base3[x]))}

mean.col <- function(base){ #Imprimir media columnas 1, 2 y 3
  means <- c()
  for (x in 1:4){
    new_mean <- apply(base[x], 2, mean)
    means <- c(means, new_mean)
  }
  means
}
mean.col(iris)

#Perdonen por mi falta de conocimiento en loops,
#pero estos videos los explican muy bien: 
#https://www.youtube.com/watch?v=h987LWDvqlQ
#https://www.youtube.com/watch?v=SJVrHumq0zc



###--- Función ifelse() ---##
imc <- imc_units(women$height, women$weight)
imc

imc2 <- imc+2
imc2>=25

ifelse(imc2>=25, #Condición lógica
       "Overweight", #Qué hacer si sí se cumple
       "Normal") #Qué hacer si no se cumple

var1 <- ifelse(imc2>=25, "Overweight", "Normal")
table(var1)

var2 <- ifelse(iris$Sepal.Length>=6.5, "Long", "Short")
table(var2)



####---------- TIPS ADICIONALES ------------ ####
#1. Menús en R
#Sirven para separar y organizar pedazos relevantes de código
# ("####") + texto + ("####")
#Colapsar menús en Mac: Cmd + option + O


#2. Nombres de variables
#Tener nombres sencillos y reproducibles facilita la sistematización del código
iris
base <- iris

#Poco recomendado
base_Species_setosa <- iris[iris$Species=="setosa",]
base_Species_virginica <- iris[iris$Species=="virginica",]
base_Species_versicolor <- iris[iris$Species=="versicolor",]

#Recomendado
base1 <- iris[iris$Species=="setosa",]
base2 <- iris[iris$Species=="virginica",]
base3 <- iris[iris$Species=="versicolor",]

mean(base2$Sepal.Length)
sd(base2$Sepal.Length)
sum(base2$Sepal.Length>=5.5)



#3. Encontrar y reemplazar
#Cmd + F // Ctrl + F
#Reduce el riesgo de cometer errores
#IMPORTANTE: NO hacerlo en todo el código, solo en fragmentos específicos

mean(base2$Sepal.Length)
sd(base2$Sepal.Length)
sum(base2$Sepal.Length>=5.5)

mean(base2$Sepal.Length)
sd(base2$Sepal.Length)
sum(base2$Sepal.Length>=5.5)

mean(base2$Sepal.Length)
sd(base2$Sepal.Length)
sum(base2$Sepal.Length>=5.5)


#IMPORTANTE: Cuidado con no sobreescribir objetos
#O tener cuidado de no correr el código en desorden

#Setosa
base1 <- iris[iris$Species=="setosa",]
mean(base1$Sepal.Length)
#Versicolor
base1 <- iris[iris$Species=="versicolor",]
mean(base1$Sepal.Length)


#4. FUNCIONES DE LOS PAQUETES DE TIDYVERSE (dplyr, ggplot2, tidyr)
install.packages("tidyverse")
tidyverse::tidyverse_packages()
#Funciones mucho más amigables e intuitivas para
#manejo de bases de datos y creación de gráficos


####---------- EXTRAS (AVANZADOS) ---------- ####

###--- Algunas funciones de tidyverse ---##
#1. case_when()
#Alternativa a ifelse() cuando se quiere usar más de una condición lógica
?case_when

#2. between()
#Alternativa a cut()
?between


###--- Otras funciones de la familia apply ---##
swirl::swirl() #Ejercicios 10 y 11

###--- Manejo y formato de fechas en R ---##
swirl::swirl() #Ejercicio 14
Sys.Date()
unclass(Sys.Date())

###--- Manejo avanzado de texto en R ---##
#1. Paquetes stringi y stringr
#https://www.rdocumentation.org/packages/stringi/versions/1.8.3
#https://www.rdocumentation.org/packages/stringr/versions/1.5.1

#2. Uso de expresiones de texto regulares
#https://uc-r.github.io/regex

#Ejemplo de uso: limpiar base de datos con fármacos antihipertensivos
#Propanolol, Propranolol, propranol, POPRANOLOL



