## Clase 4: Visualización de datos en R usando ggplot2 ##
## Carlos Alberto Fermín Martínez ##
## 20 de febrero de 2024 ##

####----------------------- PARTE 1 ----------------------- ####
####---- Paquetes y bases de datos------ ####
pacman::p_load(tidyverse, ggthemes, ggsci,
               ggpubr, gapminder, viridis,
               RColorBrewer)

#Inspeccionar nuestras bases de datos
?mtcars
str(mtcars)
summary(mtcars)

?iris
str(iris)
summary(iris)

?gapminder
str(gapminder)
summary(gapminder)



####---- Gráficos básicos: graphics----- ####
hist(x = iris$Sepal.Length) #Histograma
plot(x = iris$Petal.Length, y = iris$Petal.Width) #Gráfico de dispersión
boxplot(formula = iris$Sepal.Length~iris$Species) #Gráfico de caja y bigotes

hist(x=iris$Sepal.Leng, #Variable a graficar
     col="midnightblue", #Color
     border="white", #Borde
     main="Distribución de la longitud de sépalo", #Título
     xlab="Longitud del sépalo",ylab="Conteo (frecuencias)") #Etiquetas ejes

plot(iris$Petal.Length,iris$Petal.Width, #Variables
     col="gold2", #Color
     pch=16, #Forma del punto
     cex=1.15, #Tamaño del punto
     main = "Relación largo-ancho del pétalo", #Título
     xlab="Longitud",ylab="Anchura") #Etiquetas
abline(lm(Petal.Width~Petal.Length,iris),col="gold4") #Linea de regresion

boxplot(iris$Sepal.Length~iris$Species,
        col=c("indianred1","red2","red4"), #Colores (vector)
        main = "Longitud de sépalo según la especie", #T?tulo
        xlab="Especie",ylab="Longitud") #Etiquetas



####---- Introducción a ggplot2--------- ####
iris %>% ggplot(aes(x=Species, y=Sepal.Length, fill=Species)) + 
  geom_boxplot(size=0.75, alpha=0.5) + scale_fill_nejm() +
  labs(x="Sepal length (cm)", y="Iris species") + theme_minimal() +
  theme(legend.position = "bottom", axis.ticks = element_blank(),
        text=element_text(face="bold"), axis.text.x = element_blank())

#Los elementos para generar gráficos en ggplot se escriben EN CAPAS
#1) Base de datos: "data"
    #Ejemplos: mtcars, iris, gapminder
#2) Estéticas: "mapping = aes()"
    #Variables con las que se va a trazar nuestro gráfico
    #Ejemplos: ejes x/y, color, relleno, tamaño, formas...
#3) Elementos geométricos
    #Ejemplos: puntos, histogramas, boxplots, líneas...

#Base de datos
ggplot(data=mtcars)

#Estéticas "x" y "y"
ggplot(data=mtcars, mapping=aes(x=wt, y=mpg))
ggplot(mtcars, aes(x=wt, y=mpg)) 
ggplot(mtcars, aes(y=wt, x=mpg)) 
ggplot(mtcars, x=wt, y=mpg) #Las estéticas siempre deben ir dentro de aes()
mtcars %>% ggplot(aes(x=wt, y=mpg)) #Concatenar funciones con pipeline

#No es necesario volver a especificar la base de datos en cada variable
mtcars %>% ggplot(aes(x=mtcars$wt, y=mtcars$mpg))
mtcars %>% ggplot(aes(x=wt, y=mpg))

#Elementos geométricos (funciones geom)
##ggplot2 trabaja con CAPAS "+"
mtcars %>% ggplot(aes(x=wt, y=mpg)) + geom_point()
mtcars %>% ggplot(aes(x=wt, y=mpg)) + geom_point() + geom_smooth(method="lm")

mtcars %>% #Capa 1: base de datos
  ggplot(aes(x=wt, y=mpg)) + #Capa 2: estéticas
  geom_point() + #Capa 3
  geom_smooth(method="lm") #Capa 4...

#Nota: siempre tener presente qué tipo de variable estamos utilizando
#Cuantitativas
  #Continuas (peso, temperatura)
  #Discretas (número de enfermedades)
#Cualitativas
  #Categóricas (sexo, raza/etnicidad)
  #Ordinales (grado de escolaridad)

#Para fines prácticos, R las separa en continuas vs discretas
#Pueden dividirse en factores ordenados y no ordenados
#factor
mtcars$am
mtcars$am %>% factor()
mtcars$am %>% factor(levels=c(0,1), labels=c("Automatic", "Manual"))
#ordered
mtcars$gear
mtcars$gear %>% ordered()
mtcars$gear %>% ordered(levels=c(3,4,5), labels=c("three", "four", "five"))

#Esto cobra relevancia al momento de graficar
mtcars2 <- mtcars
#X debería ser categórica pero R asume que es continua
mtcars2 %>% ggplot(aes(x=am, y=mpg)) + geom_boxplot()
#Solución 1: agregar la estética "group"
mtcars2 %>% ggplot(aes(x=am, y=mpg, group=am)) + geom_boxplot()
#Solución 2: convertirlo en factor dentro del mismo gráfico
mtcars2 %>% ggplot(aes(x=factor(am), y=mpg)) + geom_boxplot()
#Solución 3: crear una nueva variable
mtcars2$am2 <- mtcars$am %>% factor(levels=0:1, labels=c("Automatic","Manual"))
mtcars2 %>% ggplot(aes(x=am2, y=mpg)) + geom_boxplot()



####---- Objetos geométricos------------ ####
##El objeto geométrico se deben elegir según las variables que estemos usando
str(iris)
summary(iris)

##UNA VARIABLE CONTINUA
ggplot(iris, aes(x=Sepal.Length)) + geom_histogram() #Histograma
ggplot(iris, aes(x=Sepal.Length)) + geom_density() #Gráfico de densidad
ggplot(iris, aes(x=Sepal.Length)) + geom_dotplot()
ggplot(iris, aes(x=Sepal.Length)) + geom_freqpoly()

##UNA VARIABLE CATEGÓRICA
ggplot(iris, aes(x=Species)) + geom_bar()
ggplot(mtcars2, aes(x=am2)) + geom_bar()

##UNA VARIABLE CONTINUA Y UNA CATEGÓRICA
ggplot(iris, aes(x=Species, y=Petal.Width)) + geom_point()
ggplot(iris, aes(x=Species, y=Petal.Width)) + geom_col()
ggplot(iris, aes(x=Species, y=Petal.Width)) + geom_boxplot()
ggplot(iris, aes(x=Species, y=Petal.Width)) + geom_violin()
ggplot(iris, aes(x=Species, y=Petal.Width)) +
  geom_dotplot(binaxis="y", stackdir="center", binwidth=0.05, stackratio=0.5)

##DOS VARIABLES CONTINUAS
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
#Medición limitada (HbA1c: 5.1%, 5.9%, 7.3%)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_jitter()

#El método default de geom_smooth es no lineal, esto debe especificarse
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_smooth()
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_smooth(method="lm")

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_line()
ggplot(iris, aes(x=sort(Sepal.Length), y=sort(Sepal.Width))) + geom_line()
ggplot(gapminder %>% filter(country=="Mexico"),
       aes(x=year, y=lifeExp)) + geom_line()

#Combinar capas
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) +
  geom_point() +
  geom_smooth(method="lm")

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, )) +
  geom_violin() +
  geom_point()



#NOTA: EVITAR CONFUSIONES CON LOS GRÁFICOS DE BARRAS
ggplot(mtcars2, aes(x=am2)) + geom_bar() #Una sola variable
ggplot(iris, aes(x=Species, y=Petal.Width)) + geom_col() #Dos variables

ggplot(iris, aes(x=Species, y=Petal.Width)) + geom_bar()
#Esto sucede porque geom_bar maneja los datos de forma distinta
#"stat=" argumento que indica qué transformación estadística se usará

#stat=bin: separa una variable continua en bloques y cuenta sus frecuencias
ggplot(iris, aes(x=Petal.Width)) + geom_bar(stat="bin")
ggplot(iris, aes(x=Petal.Width)) + geom_histogram()

#stat=count: cuenta el número de veces que se repite una variable (default)
ggplot(mtcars2, aes(x=am2)) + geom_bar(stat="count")
ggplot(mtcars2, aes(x=am2)) + geom_bar()

#stat=identity: deja las variables tal cuál como están
ggplot(iris, aes(x=Species, y=Petal.Width)) + geom_bar(stat="identity")
ggplot(iris, aes(x=Species, y=Petal.Width)) + geom_col()




####---- Estéticas---------------------- ####
mtcars3 <- mtcars %>% mutate("fam"=factor(am), "fcyl"=factor(cyl),
                             "fgear"=factor(gear), "fvs"=factor(vs))
str(mtcars3)

mtcars3 %>% ggplot(aes(x=wt)) #x
mtcars3 %>% ggplot(aes(y=mpg)) #y
mtcars3 %>% ggplot(aes(x=wt, y=mpg)) #x*y
mtcars3 %>% ggplot(aes(x=wt, y=mpg)) + geom_point()

mtcars %>% ggplot(aes(x=wt, y=mpg, color=am) ) + geom_point() #color
mtcars3 %>% ggplot(aes(x=wt, y=mpg, color=fam)) + geom_point() #color
mtcars3 %>% ggplot(aes(x=wt, y=mpg, alpha=fam)) + geom_point() #alpha
mtcars3 %>% ggplot(aes(x=wt, y=mpg, shape=fam)) + geom_point() #shape 
mtcars3 %>% ggplot(aes(x=wt, y=mpg, size=fam)) + geom_point() #size
mtcars3 %>% ggplot(aes(x=wt, y=mpg, linetype=fam)) +  #linetype
  geom_smooth(method="lm")

#Fill es para rellenos. Color es para puntos, líneas y contornos
mtcars3 %>% ggplot(aes(x=wt,y=mpg,fill=fam)) + #fill
  geom_smooth(method="lm", color="black")
mtcars3 %>% ggplot(aes(x=wt,y=mpg,color=fam)) + #color
  geom_smooth(method="lm")
mtcars3 %>% ggplot(aes(x=wt, y=mpg, color=fam, fill=fam)) +
  geom_smooth(method="lm") #Color + fill

#Podemos agregar todas las estéticas que queramos
mtcars3 %>% ggplot(aes(x=wt, y=mpg, color=fam, fill=fam, linetype=fam)) + 
  geom_smooth(method="lm")

#Si le asignamos las estéticas a una variable, se modifican TODOS los geom
#PERO se pueden asignar estéticas distintas en cada uno de los geom

#Ejemplo:
mtcars3 %>% ggplot(aes(x=wt, y=mpg, color=fcyl)) + 
  geom_point() + geom_smooth(method="lm")

#Se puede cambiar la estética de uno de los geom en específico
mtcars3 %>% ggplot(aes(x=wt, y=mpg)) +
  geom_point(aes(color=fcyl)) + 
  geom_smooth(method="lm", color="black")

#Estéticas vs Atributos
#Las estéticas se asignan a una variable, mientras que los atributos son fijos
mtcars3 %>% ggplot( aes(x=wt, y=mpg, color=fam) ) + geom_point(size=3, alpha=0.5) +
  geom_smooth(method="lm", color="black", fill="black", alpha=0.1)



####---- Personalización de estéticas--- ####
#Histogramas, dispersión, cajas y bigotes, barras

## 2A. Aspectos generales (atributos) ##
##color y fill:

#1) Nombre del color
iris %>% ggplot(aes(x=Sepal.Length)) + geom_histogram(fill="red")
iris %>% ggplot(aes(x=Sepal.Length)) + geom_histogram(fill="darkblue")
#Deben ser nombres reconocidos por R
iris %>% ggplot(aes(x=Sepal.Length)) + geom_histogram(fill="azul")
iris %>% ggplot(aes(x=Sepal.Length)) + geom_histogram(fill="goldenrod")
#http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

#2) Sistema RGB
#Cualquier combinación de 6 caracteres entre 0-9 y A-F
#Precedidos siepre por un "#"
iris %>% ggplot(aes(x=Sepal.Length)) + geom_histogram(fill="#6133B1")
iris %>% ggplot(aes(x=Sepal.Length)) + geom_histogram(fill="#3BBG55")
iris %>% ggplot(aes(x=Sepal.Length)) + geom_histogram(fill="#E87BEC",
                                                      color="black", size=1)
#https://www.rapidtables.com/web/color/RGB_Color.html
#https://www.w3schools.com/colors/colors_groups.asp


##shape:
ggpubr::show_point_shapes()
iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(shape=0)
iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(shape=16)
#Las formas 0-20 solo tienen color, pero 21-25 pueden tener color y relleno
iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point(size=3, shape=16, color="midnightblue")
iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point(size=3, shape=21, color="midnightblue", fill="steelblue")


##linetype:
ggpubr::show_line_types()
iris %>% ggplot(
  aes(x=Sepal.Length, y=Petal.Width)) + geom_smooth(method="lm", linetype=0)
iris %>% ggplot(
  aes(x=Sepal.Length, y=Petal.Width)) + geom_smooth(method="lm", linetype=1)
iris %>% ggplot(
  aes(x=Sepal.Length, y=Petal.Width)) + geom_smooth(method="lm", linetype=2)
iris %>% ggplot(
  aes(x=Sepal.Length, y=Petal.Width)) + geom_smooth(method="lm", linetype=3)
iris %>% ggplot(
  aes(x=Sepal.Length, y=Petal.Width)) + geom_smooth(method="lm", linetype=4)
iris %>% ggplot(
  aes(x=Sepal.Length, y=Petal.Width)) + geom_smooth(method="lm", linetype=5)
iris %>% ggplot(
  aes(x=Sepal.Length, y=Petal.Width)) + geom_smooth(method="lm", linetype=6)

#Alternativa secreta:
#8 caracteres hexadecimales (1-9 y A-F)
#- - - - #
#12345678#
iris %>% ggplot(aes(x=Sepal.Length, y=Petal.Width)) +
  geom_smooth(method="lm", linetype="11111111")
#https://stackoverflow.com/questions/25788945/
#how-to-define-more-line-types-for-graphs-in-r-custom-linetype


##size: variables continuas, pueden tomar cualquier valor numérico
iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(size=NA)
iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(size=0)
iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(size=0.75)
iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(size=1)
iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(size=2)
iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(size=100000)

##alpha: variables continuas, toma valores entre 0 y 1
iris %>% ggplot(aes(x=Sepal.Length)) + geom_histogram(fill="blue4", alpha=0)
iris %>% ggplot(aes(x=Sepal.Length)) + geom_histogram(fill="blue4", alpha=0.34)
iris %>% ggplot(aes(x=Sepal.Length)) + geom_histogram(fill="blue4", alpha=0.69)
iris %>% ggplot(aes(x=Sepal.Length)) + geom_histogram(fill="blue4", alpha=1)
iris %>% ggplot(aes(x=Sepal.Length)) + geom_histogram(fill="blue4", alpha=1000)


####---- Escalas------------------------ ####
ggplot(data = iris, aes(x=Species, y=Petal.Width, fill=Species)) +
  geom_boxplot(color="black", size=0.75)


##-- VARIABLES CATEGÓRICAS --##
#Personalizar manualmente las estéticas: funciones scale_"X"_manual
#Se tiene que especificar en la función: ¿qué es lo que se quiere cambiar?
#scale_fill_manual // scale_color_manual // scale_linetype_manual
#scale_shape_manual // scale_alpha_manual // etc...

#Se pueden especificar los argumentos: values, labels, name, limits

#values: cambiar la estética de cada una de las categorías
ggplot(data = iris, aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75) +
  scale_fill_manual(values = c("indianred1","red2","red4"))

#labels: cambiar la etiqueta de las categorías en la leyenda
ggplot(data = iris, aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75) +
  scale_fill_manual(values=c("indianred1","red2","red4"),
                    labels = c("Set","Ver","Vir"))

#name: cambiar el nombre de la leyenda
ggplot(data = iris, aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75)+
  scale_fill_manual(values=c("indianred1","red2","red4"),
                    labels=c("Set","Ver","Vir"),
                    name="Especie \n de flor") #"\n" significa salto de línea

#limits: cambiar los límites a los que la escala afecta
ggplot(data = iris, aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75)+
  scale_fill_manual(values=c("indianred1","red2","red4"),
                    labels=c("Set","Ver","Vir"),
                    name="Especie \n de flor",
                    limits=c("setosa", "virginica"))


#Si se quiere modificar otro parámetro se tienen que agregar exactamente
#los mismos argumentos. De otra forma, aparecerán dos leyendas diferentes
ggplot(data = iris, aes(x=Species, y=Petal.Width,
                        fill=Species, color=Species))+
  geom_point(size=2) + geom_boxplot(color="black", size=0.75) +
  scale_fill_manual(values=c("indianred1","red2","red4"),
                    labels=c("Set","Ver","Vir"), name="Flor") +
  scale_color_manual(values=c("indianred1","red2","red4"),
                     labels=c("Set","Ver","Vir"), name="Flor")



#PALETAS PREDETERMINADAS
#Asignan colores en automático, no requiere que se especifiquen manualmente

##PAQUETE GGPLOT2
#scale_x_brewer
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black") +
  ggplot2::scale_fill_brewer()

RColorBrewer::display.brewer.all()

ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+
  ggplot2::scale_fill_brewer(palette = "Spectral")

ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+
  ggplot2::scale_fill_brewer(palette = "Accent")

ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black") + 
  ggplot2::scale_fill_brewer(palette = "Set3")

#Aqué también se pueden modificar las etiquetas y nombre de la leyenda
mpg %>%  ggplot(aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black") +
  ggplot2::scale_fill_brewer(palette = "Set3",
                             name="Tipo", labels=LETTERS[1:7])

ggplot(mpg, aes(x= factor(class, labels = LETTERS[1:7]), y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black") +
  ggplot2::scale_fill_brewer(palette = "Set3",
                             name="Tipo", labels=LETTERS[1:7])


#scale_x_discrete
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggplot2::scale_fill_discrete()

#scale_x_grey
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggplot2::scale_fill_grey()

#scale_x_ordinal
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggplot2::scale_fill_ordinal()



##PAQUETE GGTHEMES
#scale calc
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_calc()

#scale colorblind
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_colorblind()

#scale economist
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_economist()

#scale excel
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_excel()

#scale gdocs
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_gdocs()

#scale hc
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_hc()

#scale pander
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_pander()

#scale ptol
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_ptol()

#scale stata
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_stata()



##PAQUETE GGSCI
#scale aaas
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black") + ggsci::scale_fill_aaas()

#scale jama
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_jama()

#scale lancet
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_lancet()

#scale nejm
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_nejm(name="GGG",labels=1:7)

#scale futurama
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_futurama()

#scale rickandmorty
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_rickandmorty()

#scale simpson
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_simpsons()

#scale startrek
ggplot(data = mpg, aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_startrek()


##-- VARIABLES CONTINUAS --##
ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2)

#Scale viridis
ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis() #Default (D)

ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "A") #Magma

ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "B") #Inferno

ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "C") #Plasma

ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "E") #Cividis



##ESCALAS PARA LOS EJES (x y)
#Me permite modificar cómo se visualizan los ejes x/y
#scale_x_""
#scale_y_""

ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "A")

#Logaritmo
ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") +
  scale_x_log10()

#Raíz cuadrada
ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") +
  scale_y_sqrt()

#Inverso
ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") +
  scale_x_reverse()

#Separar en segmentos
ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") +
  scale_y_binned(n.breaks = 5)

#Cambiar coordenadas
ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") +
  coord_flip()



#Aplicando todo
ggplot(data = iris, aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_jama(name="Tipo \nde flor")

ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) +
  geom_point(size=2, alpha=0.75) + geom_smooth(color="black", method="lm") +
  scale_color_jama(name="Tipo \nde flor")

ggplot(data = iris, aes(x=Species, y=Sepal.Length, fill=Species)) +
  geom_point(size=1.9, shape=21, alpha=1)+ geom_boxplot(color="black", alpha=0.9) +
  scale_fill_jama(name="Tipo \nde flor")+
  scale_color_jama(name="Tipo \nde flor")


####----------------------- PARTE 2 ----------------------- ####
####---- Títulos y etiquetas------------ ####

#AGREGAR TÍTULOS (ggtitle)
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_jama(name="Tipo \nde flor") +
  ggtitle("Distribución de la longitud de sépalo")

#MODIFICAR EJES (xlab, ylab)
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_jama(name="Tipo \nde flor") +
  ggtitle("Distribución de la longitud de sépalo")+
  xlab("Longitud de sépalo") + ylab("Densidad")

#Otra forma de modificar etiquetas (labs):
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_jama(name="Tipo \nde flor") +
  labs(title="Distribución de la longitud de sépalo", x="Longitud de sépalo", y="Densidad")


###Hacer anotaciones

##annotate
#-- annotate(geom, x, y, label)
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_jama(name="Tipo \nde flor") +
  labs(title="Distribución de la longitud de sépalo", x="Longitud de sépalo", y="Densidad") +
  annotate(geom="text", x=6, y=1.2, label="GGG")
#geom es el tipo de objeto, en este caso texto, xy son las coordenadas, label es lo que queremos anotar


##tapply
#-- tapply(X, INDEX, FUN)
tapply(X = iris$Sepal.Length, INDEX = iris$Species, FUN = mean)
#Permite aplicar una función (FUN) a una variable (X) agrupada en función de otra (INDEX) 

ann1 <- tapply(X = iris$Sepal.Length, INDEX = iris$Species, FUN = mean)

#función round
#-- round(x, digits)
round(x = 0.73653, digits = 3)
ann2 <- round(x = ann1, digits = 2)

#función paste0
paste0("Here","comes","the","sun")
paste0("Here"," comes"," the"," sun")
ann3 <- paste0("?=", ann2)

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_jama(name="Tipo \nde flor") +
  labs(title="Distribución de la longitud de sépalo", x="Longitud de sépalo", y="Densidad") +
  annotate(geom="text", x=c(5,5.9,6.5), y=c(1.3,0.9,0.8), label=ann3)




####---- Anotaciones y comparaciones---- ####
#Paste0 pega los objetos que se le indiquen como caracteres sin dejar espacios
paste0("Cruz","Azul","campe?n", 2021)
paste0("Cruz ","Azul ","campe?n ", 2021)
paste0("La letra n?mero ", 1:4, " del abecedario es ", LETTERS[1:4])

#Round redondea un n?mero con los decimales que se le indiquen
round(3.14159265359, 4)
round((1:10)/7, 2)

#Combinar round y paste0
paste0(1:10, " entre 7 es igual a ", round((1:10)/7, 2))


#HACER ANOTACIONES PARA CADA GR?FICO
F1A <- F1A + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)); F1A
F1B <- F1B + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)); F1B
F1C <- F1C + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)); F1C

#Mediana de longitud de s?palo para cada especie
ann1<-tapply(iris$Sepal.Length, iris$Species, median)
ann1<-paste0("M= ", ann1)

F1A + annotate("text", x= c(5,5.8,6.5), y=c(1.3,0.85,0.8), label=ann1, size=3.5)


#Coeficiente de correlaci?n
ann2 <- cor(x = iris$Petal.Length, y = iris$Petal.Width, method = "spearman")
ann2 <- round(ann2, 3)
ann2 <- paste0("Rho de \nSpearman= ", ann2)

F1C + annotate("text", x=2.25, y=2, label=ann2, size=3.5)


#stat compare means permite hacer pruebas de hip?tesis y plasmar el resultado en la gr?fica

F1B + ggpubr::stat_compare_means()
F1B + ggpubr::stat_compare_means(label.x = 2, label.y = 4.25, size=3.5)
F1B + ggpubr::stat_compare_means(label.x = 2, label.y = 4.25, size=3.5, method="anova")

F1B + ggpubr::stat_compare_means(size=3.5, 
                                 comparisons = list(c("setosa", "virginica"),
                                                    c("setosa", "versicolor"),
                                                    c("versicolor", "virginica")))

F1B + ggpubr::stat_compare_means(size=3.5, 
                                 comparisons = list(c("setosa", "virginica"),
                                                    c("setosa", "versicolor"),
                                                    c("versicolor", "virginica")),
                                 symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 0.1),
                                                    symbols = c("***", "**", "*", ".")))


F1A <- F1A + annotate("text", x= c(5,5.8,6.5), y=c(1.3,0.85,0.8), label=ann1, size=3.5)

F1B <- F1B + ggpubr::stat_compare_means(size=3.5, 
                                        comparisons = list(c("setosa", "virginica"),
                                                           c("setosa", "versicolor"),
                                                           c("versicolor", "virginica")),
                                        symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 0.1),
                                                           symbols = c("***", "**", "*", ".")))

F1C <- F1C + annotate("text", x=2.25, y=2, label=ann2, size=3.5)


####---- Paneles (faceting)------------- ####
#Usaremos la base de datos mpg
data(mpg)
?mpg
table(mpg$manufacturer)
table(mpg$year)
table(mpg$cyl)
summary(mpg$hwy)

ggplot(mpg, aes(x=displ, y=hwy)) + geom_point() #Graficamos las millas por hora en funci?n del desplazamiento del motor

#Podemos desglosar esta relaci?n seg?n el a?o de fabricaci?n con facet_grid
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(year)) #En filas
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(cols = vars(year)) #En columnas

#Tambi?n podemos utilizar la funci?n facet_wrap
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~year)

#Estos subgr?ficos pueden tener todos la misma escala (default)
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(cyl))
#O podemos permitir que tengan escalas distintas (scales="free")
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(cyl), scales="free")

#Tambi?n podemos desglosar la gr?fica en funci?n de 2 variables al mismo tiempo
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(year~cyl)
#Con facet_wrap se eliminan los subgr?ficos vac?os y se tiene m?s control sobre la distribuci?n del gr?fico
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(cyl~year, nrow=1, ncol=7)
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(cyl~year, nrow=2, ncol=4)
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(cyl~year, nrow=3, ncol=3)

####---- Temas-------------------------- ####

#Paquete ggplot
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_jama(name="Tipo \nde flor") + ggplot2::theme_get()

#Paquete ggthemes
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_stata() + ggthemes::theme_stata()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_economist() + ggthemes::theme_economist()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_wsj() + ggthemes::theme_wsj()


#Minimalistas
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_jama(name="Tipo \nde flor") + ggplot2::theme_minimal()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_jama(name="Tipo \nde flor") + ggthemes::theme_hc()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_jama(name="Tipo \nde flor") + ggpubr::theme_pubr()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_jama(name="Tipo \nde flor") + ggpubr::theme_pubclean()


#Todo junto
F1A <- ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_jama(name="Tipo \nde flor")+
  labs(title="Distribución de la \nlongitud de sépalo", x="Longitud de sépalo", y="Densidad") +
  ggpubr::theme_pubclean()

F1B <- ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Species)) +
  geom_point(size=2, alpha=0.75) + geom_smooth(color="black", method="lm") +
  scale_color_jama(name="Tipo \nde flor")+ 
  labs(title="Relaci?n de las dimensiones \ndel pétalo", x="Longitud de pétalo", y="Anchura de pétalo") +
  ggpubr::theme_pubclean()

F1C <- ggplot(data = iris, mapping = aes(x=Species, y=Sepal.Length, fill=Species)) +
  geom_point(size=1.9, shape=21, alpha=0.75)+ geom_boxplot(color="black", alpha=0.9) +
  scale_fill_jama(name="Tipo \nde flor")+ scale_color_jama(name="Tipo \nde flor")+
  labs(title="Longitud de sépalo por \n tipo de flor", x="", y="Longitud de sépalo") +
  ggpubr::theme_pubclean()



#La función "theme" (a secas) sirve para modificar el formato de la gráfica
#Tenemos que especificar:
# 1) Elemento de la gráfica que queremos cambiar (título, ejes, leyenda, etc.)
# 2) Tipo de objeto que le queremos asignar (elemento en blanco, elemento de texto, etc.)
# 3) parámetros que vamos a modificar (tipo de letra, justificación, etc.)

#Borrar elementos: element_blank
F1A + theme(plot.title = element_blank() ) #Título
F1A + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) #Texto del eje
F1A + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) #Título del eje
F1A + theme(axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) #líneas del eje
F1A + theme(legend.title = element_blank()) #Título de la leyenda
F1A + theme(legend.text = element_blank()) #Texto de la leyenda

#Cambiar posición de la leyenda
F1A + theme(legend.position = "top")
F1A + theme(legend.position = "bottom")
F1A + theme(legend.position = "right")
F1A + theme(legend.position = "left")
F1A + theme(legend.position = "none")
F1A + theme(legend.position = c(0.75,0.75) )

#Borrar todo
F1A + theme(plot.title = element_blank(), axis.title = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(),
            legend.position = "none")



#Element_text: fuente (family)
F1A + theme(plot.title = element_text(family = "sans")) #Arial (predeterminada)
F1A + theme(plot.title = element_text(family = "serif")) #Times New Roman
F1A + theme(plot.title = element_text(family = "mono")) #Monospace

#Element_text: formato (face)
F1A + theme(plot.title = element_text(face = "plain"))
F1A + theme(plot.title = element_text(face = "bold"))
F1A + theme(plot.title = element_text(face = "italic"))
F1A + theme(plot.title = element_text(face = "bold.italic"))

#Element_text: color
F1A + theme(plot.title = element_text(colour = "red4"))
F1A + theme(plot.title = element_text(colour = "midnightblue"))
F1A + theme(plot.title = element_text(colour = "gold4"))

#Element_text: tamaño
F1A + theme(plot.title = element_text(size = 1))
F1A + theme(plot.title = element_text(size = 10))
F1A + theme(plot.title = element_text(size = 20))

#Element_text: justificación horizontal
F1A + theme(plot.title = element_text(size=11, hjust = 0)) #Hasta la izquierda
F1A + theme(plot.title = element_text(size=11, hjust = 1)) #Hasta la derecha
F1A + theme(plot.title = element_text(size=11, hjust = 0.5)) #Centrado

#Element_text: justificación vertical
F1A + theme(plot.title = element_text(size=11, hjust = 0.5, vjust = 0)) #Hasta abajo
F1A + theme(plot.title = element_text(size=11, hjust = 0.5, vjust = 1)) #Hasta arriba

#Element_text: ángulo
F1A + theme(axis.text = element_text(angle = 0)) #Predeterminado
F1A + theme(axis.text = element_text(angle = 90))
F1A + theme(axis.text = element_text(angle = 180))
F1A + theme(axis.text = element_text(angle = 270))
F1A + theme(axis.text = element_text(angle = 320))


##Gráficos de barras
#Hace un conteo del número de elementos de nuestra variable de inter?s
iris %>% ggplot(aes(x=Species, fill=Species)) + geom_bar()

iris %>% filter(Sepal.Width>3) %>% 
  ggplot(aes(x=Species, fill=Species)) + geom_bar(color="black", alpha=0.9)


####---- Exportación de gráficas-------- ####

#Las gráficas se pueden guardar en objetos de R
F1D <- iris %>% filter(Sepal.Width>3) %>% 
  ggplot(aes(x=Species, fill=Species)) + geom_bar(color="black", alpha=0.9) +
  labs(title="Flores con anchura de sépalo >3", x="", y="Conteo") +
  scale_fill_jama(name="Tipo \nde flor")+ ggpubr::theme_pubclean()


#Y se pueden añadir nuevas capas a esos objetos (y sobreescribirse)
F1A <- F1A + theme(plot.title = element_text(face="bold", hjust=0.5))
F1B <- F1B + theme(plot.title = element_text(face="bold", hjust=0.5))
F1C <- F1C + theme(plot.title = element_text(face="bold", hjust=0.5))
F1D <- F1D + theme(plot.title = element_text(face="bold", hjust=0.5))

#Una vez guardadas en objetos, se pueden agrupar con ggarrange
ggarrange(F1A, F1B, F1C, F1D, ncol=2, nrow=2, labels = LETTERS[1:4])
#Puede ponerse una leyenda en común
ggarrange(F1A, F1B, F1C, F1D, ncol=2, nrow=2, labels = LETTERS[1:4],
          common.legend = T)
#Y modificarse la posición de la leyenda
F1 <- ggarrange(F1A, F1B, F1C, F1D, ncol=2, nrow=2, labels = LETTERS[1:4],
                common.legend = T, legend = "bottom")
F1


#Cuando tengamos el producto final, se pueden exportar como imágenes
setwd("C:/Users/facmed/Desktop/Script/Sesion_4")

ggsave(filename="FIGURA1.png", plot = F1, width = 40, height = 20, units="cm",
       dpi=300, limitsize=F)

ggsave(filename="FIGURA1.jpg", plot = F1, width = 40, height = 20, units="cm",
       dpi=300, limitsize=F )

ggsave(filename="FIGURA1.tiff", plot = F1, width = 40, height = 20, units="cm",
       dpi=300, limitsize=F )

ggsave(filename="FIGURA1.pdf", plot = F1, width = 40, height = 20, units="cm",
       dpi=300, limitsize=F )


####---- Integración con tidyverse------ ####

#Extra: Manejo de bases de datos + generación de gráficos

gapminder2 <- gapminder %>% #Usaremos la base de datos gapminder
  filter(year%in%c(1957, 1967, 1977, 1987, 1997, 2007)) %>% #Filtramos por año 
  group_by(year, continent) %>% #Agrupamos por año y continente 
  summarise( #Obtenemos la media de lifeExp y GDP por año
    "LifeExp"=mean(lifeExp), "GDP"=mean(gdpPercap)) %>% as.data.frame()

F2A <- gapminder2 %>% ggplot(
  aes(x=year, y=LifeExp, color=continent)) + #Colocamos las estéticas
  geom_line(linewidth=1.25) + #Agregamos un gráfico de líneas
  theme_economist() + #Cambiamos el tema
  scale_color_economist(name="") + #Cambiamos la paleta de colores
  labs(x="Year", y="Life expectancy", #Modificamos las etiquetas de los ejes
       title="Change in life expectancy (1957-2007)") + #Añadimos un título
  theme(legend.position = "left", #Cambiamos la posición de la leyenda
        plot.title = element_text(face="bold", hjust=0.5)) #Formato del título

F2B <- gapminder %>% filter(year%in%c(1957, 1967, 1977, 1987, 1997, 2007)) %>% 
  group_by(year, continent) %>% summarise(
    "LifeExp"=mean(lifeExp), "GDP"=mean(gdpPercap)) %>% as.data.frame() %>% 
  ggplot(aes(x=year, y=GDP, color=continent)) + geom_line(linewidth=1.25) +
  theme_economist() + scale_color_economist(name="") +
  labs(x="Year", y="GDP per capita",
       title="Change in GDP per capita (1957-2007)") +
  theme(legend.position = "left",
        plot.title = element_text(face="bold", hjust=0.5))

#Los datos crudos y fácil de interpretar
View(gapminder)
#Se pueden sintetizar en un gráfico vistoso y f?cil de comprender
ggarrange(F2A,F2B, common.legend = T, legend="bottom")





