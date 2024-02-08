## Matrices y vectores

## Vector--> Colección de objetos

# Vectores de dos dimensions
c() # Este es el signo que indica un vector
c(5)
c(13,16) ## Numerico
c("Omar", "13") ## Caracter
c(TRUE, FALSE) ## Vectores lógico: TRUE, FALSE

##Vectores numéricos
# Hola, ¿cómo están todos?
3+3 # Sumar
3-3 # Resta
3*3 # Multiplicación
3/3 # División
3^2 # Exponente
exp(2) # Exponencial e^2
pi
sqrt(2) # Raíz cuadrada
log(2) # logaritmo natural
3^(1/5) # Raíz exponencial

## Reglas lógicas
3>2 ## Mayor que
2>3 ## Mayor que
2<3 ## Menor que 
2<=2 ## Menor o igual
2>=3 ## Mayor o igual
(70/(1.75^2))>=30

"Omar"=="omAr" ## Exactamente igual
"1aBc"=="1abc"
"Omar"!="Omar" ## No es igual


c(2,3)*c(4,5) #multiplicacion
c(2,3)*2
c(2,3)+c(4,5) #Suma
c(2,3)-c(4,5) #Resta
c(2,3)/c(4,5) #Division

# Vectores de dimensiones diferentes
c(2,3,4,6,1)*c(4, 5)

## Asignación de objetos
# Pueden usar el signo = o el signo compuesto <- para hacer asignaciones
a<-c(2,3,4,6,1)
a<-c(2,3,4,6,1)
a
b<-c(4,5,7,3,1)

peso<- c(73, 65)
talla<-c(1.71, 1.57)

imc<-peso/(talla^2)
vino_a_clase<-c(FALSE, TRUE, TRUE)
nombres_peliculas<-c("Inception", "Interstellar", "Across the Spiderverse")
genero_peliculas<-c("Sci-fi", "Sci-fi", "Comic")

## Operaciones con vectores asignados
(a*b)/(b-a)

## Vectores de diferentes elementos (tipos de objetos)
class(2) # Función que permite evaluar el tipo de objeto con el que trabajo
class(a)
class(nombres_peliculas)
class(vino_a_clase)
class(genero_peliculas)

genero_peliculas<-as.factor(genero_peliculas)
## Variable categorica

2.1 # Númericos, variable cuantitativa continua
c<-2.1
c1<-as.integer(c) # Las funciones as.XXX indican hacia que tipo de objeto XXX se de transformar otro
c<-as.integer(c)
class(c)
2 # Integer (Entero), variable cuantitativa discreta

## Categóricas
d<-c("23-45X-16B") # Character es un objeto de tipo cadena
class(d)
e<-c("rojo", "azul", "blanco", "blanco", "azul", "rojo", "blanco") #Categórica
class(e)
f<-as.factor(e)
class(f)
unique(f) ## Cuantos valores únicos tiene el vector
unique(e)
levels(f) ## Cuales son las categorías
e1<-factor(e, labels = c("red", "blue", "white"))
e1<-factor(e1, levels = c("white", "red", "blue"))

## Vectores boleanos o lógicos
class(TRUE) # Objetos lógicos
g<-c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)
g<-c(F, F, T, F, T, T)
#Los vectores lógicos asumen que FALSE=0, TRUE=1

h<-a>=4 ##Categorizar variables
h1<-a>=4
h+h1
h<-as.factor(h)
## Seleccionar caso en una base de datos
# Aplicar funciones solo a un conjunto de datos
# Hacer operaciones complejas
# Filtrar casos

## Vectores de diferentes de diferentes tipos de elementos
i<-c("gato", 1, 3.1, "liebre", TRUE)
# Los vectores NO pueden tener tipos de objetos diferentes
# Se guardan de tipo characters
class(i)


## Vector -> Variables
## Matriz -> Base de datos

## Matrices
?matrix() ## El signo ? me abre la ventana de ayuda de RStudio
?class()
l<-matrix(c(1,0,0,1), nrow=2, ncol=2) ## Generar matrices
k<-matrix(c(1, "FALSE", FALSE, 0), ncol=2, nrow=2)
class(k)
# Las matrices NO pueden tener tipos de objetos diferentes
# Se guardan de tipo character

## Data frames
# Aceptan tipos de objetos diferentes
# Renglones sujetos
# Columnas variables
# Compuestas de vectores

# Estudio de 6 sujetos, donde medí tres variables
var1<-c(24, 27, 78, 56, 44, 33)
var2<-c("h", "m", "m", "o", "h", "o")
var3<-c(67, 78, 61, 44,71, 57)
var4<-c(T, F, T, F, F, T)
length(var0)
?length
cbind() #Une columnas con numero similar de renglones
rbind() #Une renglones con número similar de columnas
remove(var0)

data1<-cbind(var1, var2, var3, var4)
class(data1)
?data.frame
data2<-data.frame(var1, var2, var3, var4) ## Función para hacer data frames
class(data2)
data2$var1
data2$var1 # El signo $ sirve para extraer variables de un data.frame

var1_1<-data2$var1*2 #Guardando objetos en ambiente global
var5<-data2$var1*data2$var3

data2$var5<-data2$var1*data2$var3 
#Con el signo $ puedo insertar variables nuevas en un data-frame
data2$var5<-var5

## Inspeccionando un data.frame
data2
View(data2)
head(data2, n = 2) #Primeras 5 observaciones si n>10
?head()
?str()
str(data2)

data(mtcars)
class(mtcars)
View(mtcars) #Abrir un dataset en una ventana de RStudio
?mtcars
str(mtcars) # Estructura de un data.frame

summary(data2) # Ver estadísticas descriptivas de un dataset 
summary(mtcars)

###
install.packages("swirl")
library(swirl)

