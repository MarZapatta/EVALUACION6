rm(list = ls())
getwd()
setwd("E:/Escritorio/EVALUACION6/")
#### Evaluacion de la 6ta Clase ####

#### Pregunta 1 ####
##  Realice un analisis descriptivo del data frame iris (Libreria datasets) 
##  por especie : histograma, boxplot , diagrama de dispersion [12 graficos]
##  y en conjunto para las 3 especies : histograma, boxplot , diagrama de dispersion [4 graficos]
data(iris)
help(iris)
str(iris)
class(iris)
names(iris)
summary(iris)
attach(iris)
help("prop.table")
help("table")
frecuenciasabsolutas <- table(iris$Species)
frecuenciasrelativas <- prop.table(frecuenciasabsolutas)

#MEDIA DE Species
media1 <- mean(iris$Sepal.Length , na.rm = TRUE)
media2 <- mean(iris$Sepal.Width , na.rm = TRUE)

# VARIANZA Y DESVIACION, PERCENTILES, MAXIMO Y MINIMO DE SEPAL.LENGHT
var(iris$Sepal.Length , na.rm = TRUE)
sd(iris$Sepal.Length , na.rm = TRUE)
quantile(iris$Sepal.Length,probs = 0.25)
quantile(iris$Sepal.Length,probs = 0.75)
max(iris$Sepal.Length)
min(iris$Sepal.Length)

# VARIANZA Y DESVIACION, PERCENTILES, MAXIMO Y MINIMO DE SEPAL.WIDTH
var(iris$Sepal.Width , na.rm = TRUE)
sd(iris$Sepal.Width , na.rm = TRUE)
quantile(iris$Sepal.Width,probs = 0.25)
quantile(iris$Sepal.Width,probs = 0.75)
max(iris$Sepal.Width)
min(iris$Sepal.Width)


help("hist")
help("boxplot")


#HISTOGRAMA PARA LA VARIABLE SEPAL.LENGTH
png(filename = "HISTOGRAMASEPAL.LENGHT.png")
hist(table(iris$Sepal.Length), col = c("red") , main = "Histograma para la variable Sepal.Lenght" , xlab = "Sepal.Lenght" , ylab = "Frecuencia")
dev.off()

#HISTOGRAMA PARA LA VARIABLE SEPAL.WIDTH
png(filename = "HISTOGRAMASEPAL.WIDTH.png")
hist(table(iris$Sepal.Width), col = c("purple") , main = "Histograma para la variable Sepal.Width" , xlab = "Sepal.Width" , ylab = "Frecuencia")
dev.off()

#DIAGRAMA DE CAJAS Y BIGOTES PARA EDAD DE RANGO =1.5 Sepal.Lenght
png(filename = "DIAGRAMADECAJASYBIGOTESparaSLySWendiferentesRangos.png")
par(mfrow = c(2,2))
ylm = c(min(iris$Sepal.Length,iris$Sepal.Width) ,
        max(iris$Sepal.Length,iris$Sepal.Width))

boxplot(iris$Sepal.Length , xlab= "Sepal.Lenght" , main= "Cajas y bigotes para la variable Sepal.Lenght", range = 1.5 )
#DIAGRAMA DE CAJAS Y BIGOTES PARA EDAD DE RANGO =0.5 Sepal.Lenght
boxplot(iris$Sepal.Length , xlab= "Sepal.Lenght" , main= "Cajas y bigotes para la variable Sepal.Lenght", range = 0.5 )
#DIAGRAMA DE CAJAS Y BIGOTES PARA EDAD DE RANGO =1.5 Sepal.Width
boxplot(iris$Sepal.Width , xlab= "Sepal.Width" , main= "Cajas y bigotes para la variable Sepal.Width", range = 1.5 )
#DIAGRAMA DE CAJAS Y BIGOTES PARA EDAD DE RANGO =0.5 Sepal.Width
boxplot(iris$Sepal.Width , xlab= "Sepal.Width" , main= "Cajas y bigotes para la variable Sepal.width", range = 0.5 )
dev.off()

#BOXPLOT SPECIES
png(filename = "DIAGRAMADECAJASYBIGOTES-Species.png")
par(mfrow = c(2,2))
ylm = c(min(Sepal.Width~Species,Sepal.Length~Species,iris) ,
        max(Sepal.Width~Species,Sepal.Length~Species,iris))

boxplot(Sepal.Width~Species,ylab="Sepal.Width")
boxplot(Sepal.Length~Species,ylab="Sepal.Length")

#BOXPLOT IRIS
boxplot(x=iris[,1:4],main="Boxplots Iris")
dev.off()


#Histograma Sepal.Lenght y Sepal.Width y Species
library(ggplot2)

png(filename = "HISTOGRAMAS-SL.png")
ggplot(iris, aes(x=Sepal.Length))+
  geom_histogram(bins=10, color="black", fill="white")
dev.off()

png(filename = "HISTOGRAMAS-SW.png")
ggplot(iris, aes(x=Sepal.Width))+
  geom_histogram(bins=10, color="black", fill="white")
dev.off()


png(filename = "DIPERSIONSPECIES.png")
p <- ggplot(iris, aes(x=Species, y=Sepal.Length, fill=Species))
p + geom_point()
dev.off()


#DIAGRAMAS DE DISPERSION para Species
png(filename = "SPECIES-DISPERSION.png")

#ancho vs largo
plot(Sepal.Width~Sepal.Length, col=Species)
#equivalente
plot(Sepal.Width~Sepal.Length, col=Species, pch=as.numeric(Species))
# + leyenda
legend('topright', levels(Species), lty=1, col=1:3, bty='n', cex=.75)

dev.off()

# DIAGRAMA DE DISPERSION PARA SPECIES USANDO GGPLOT
png(filename = "SPECIES-DISPERSIONGGPLOT.png")

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species))+
  geom_point(size=3,shape=4)

dev.off()


#PARES DE IRIS
png(filename = "PARESIRIS.png")
pairs(iris[1:4],pch=as.numeric(iris$Species),col=iris$Species)
dev.off()

#DIAGRAMA DE SECTORES PARA LA VARIABLE SPECIES 
png(filename = "diagramaSpecies.png")
pie(table(iris$Species), col = c("yellow","orange","green", "red") ,main = "Diagrama de sectores para la variable Species")
dev.off()

#DIAGRAMA DE BARRAS PARA LA VARIABLE SPECIES
png(filename = "diagramaBarrasSpecies.png")
barplot(table(iris$Species), col = c("yellow","orange","red"),xlab = "Species",ylab = "FRECUENCIAS ABSOLUTAS" ,main = "Diagrama de sectores para la variable Species")
dev.off()

#### Pregunta 2 ####
##  Contruya un arbol de regresion para el data frame iris (Libreria datasets)  y genere las predicciones 
##  para el siguiente data frame :
NuevaEspecie <- data.frame(Sepal.Length = 6.5, Sepal.Width = 3.0,Petal.Length = 5.2, Petal.Width = 2.0)
#creamos el arbol
library(tree)
set.seed(1)
NuevaEspecie <- data.frame(Sepal.Length = 6.5, Sepal.Width = 3.0,Petal.Length = 5.2, Petal.Width = 2.0, Species=1)
IRIS1<-rbind(iris,NuevaEspecie)

# IRIS1<-IRIS1[,+5]

train <- sample(seq(length(IRIS1$Species)),length(IRIS1$Species)*0.70,replace=FALSE)
IRIS1.tree <- tree(formula=IRIS1$Species~.,
                   data=IRIS1, subset = train,
                   split = "deviance")
summary(IRIS1.tree)
png(filename = "TREE-IRIS.png")

plot(x=IRIS1.tree, type="proportional")
text(x=IRIS1.tree, splits = TRUE, pretty = 0, 
     cex=0.8, col="firebrick")
IRIS1.tree
dev.off()

IRIS1.tree_pruning <- prune.tree(tree=IRIS1.tree, best=3)
plot(x=IRIS1.tree,type="proportional")
text(x=IRIS1.tree, splits=TRUE, pretty=0,
     cex=0.8, col="firebrick")
prediccionesIRIS <- predict(IRIS1.tree_pruning,
                         newdata = IRIS1[-train,])
#PREDICCIONES
test_mseIRIS <- mean((prediccionesIRIS-IRIS1[-train,"Petal.Width"])^2)
paste("Error (mse) del arbol despues de la poda: ",
      round(test_mseIRIS,2))
# "Error (mse) del arbol despues de la poda:  1.66"

#usamos rpart

library(rpart)

#Creamos modelo de iris con nueva especie
set.seed(1)
modiris1b <- rpart(Sepal.Length~.,data=IRIS1)
class(modiris1b)
typeof(modiris1b)
modiris1$splits
modiris1b

# Visualizacion del arbol modiris1: prp[libreria rpart.plot]
library(rpart.plot)
png(filename = "RPART-IRIS.png")

prp(modiris1b)
dev.off()


#### Pregunta 3 ####
## Cree un arbol de clasificacion para predecir la variable "diabetes" en los siguientes dos escenarios
## Escenario 1 : no considere la variable "pedigree" 
## Escenario 2 : no considere la variable "glucose" 
## Para cada uno de los escenario, genere nuevos data frames para predecir sus nuevos resultados 
## (predecir la variable "diabetes" cuyos posibles resultados son pos-neg)
library(mlbench)
data("PimaIndiansDiabetes2")

#### Escenario 1 : no considere la variable "pedigree" ####
datos1 <- PimaIndiansDiabetes2[,c(-7)]
datos1 <- na.omit(datos1)
# datos1pos <- datos1[datos1$diabetes=="pos",] 
# datos1neg <- datos1[datos1$diabetes=="neg",] 
str(datos1)
summary(datos1)

#### Escenario 2 : no considere la variable "glucose" ####
datos2 <- PimaIndiansDiabetes2[,c(-2)]
datos2 <- na.omit(datos2)
# datos2pos <- datos2[datos2$diabetes=="pos",] 
# datos2neg <- datos2[datos2$diabetes=="neg",] 
str(datos2)
summary(datos2)

# CORRELACION ESCENARIO 1

help("cor")
png(filename = "CORRELACIONESCENARIO1-DIABETES.png")
cor(datos1[,-8])
library(corrplot)
corrplot(cor(datos1[,-8]))
dev.off()

# CORRELACION ESCENARIO 2

png(filename = "CORRELACIONESCENARIO2-DIABETES.png")
cor(datos2[,-8])
library(corrplot)
corrplot(cor(datos2[,-8]))
dev.off()

#TREE E1 DIABETES
library(tree)
set.seed(1233)

trainE1 <- sample(1:nrow(datos1),size = nrow(datos1)/2)
arbol_regresionE1 <- tree(formula=diabetes~.,
                         data=datos1, subset = trainE1,
                         split = "deviance")

summary(arbol_regresionE1)
png(filename = "ARBOLREGRESIONE1-DIABETES.png")

plot(x=arbol_regresionE1, type="proportional")
text(x=arbol_regresionE1, splits = TRUE, pretty = 0, 
     cex=0.8, col="firebrick")
arbol_regresionE1
dev.off()
# PREDICCION E1 DIABETES
arbol_regresionE1_pruning <- prune.tree(tree=arbol_regresionE1, best=8)
plot(x=arbol_regresionE1,type="proportional")
text(x=arbol_regresionE1, splits=TRUE, pretty=0,
     cex=0.8, col="firebrick")
prediccionesarbol_regresionE1 <- predict(arbol_regresionE1_pruning,
                            newdata = datos1[-trainE1,])

#PREDICCIONES E1
# test_regresionE1 <- mean((prediccionesarbol_regresionE1-datos1[-trainE1,"glucose"])^2)
test_regresionE1 <- mean((prediccionesarbol_regresionE1-datos1[-trainE1,"age"])^2)


paste("Error del arbol despues de la poda: ",
      round(test_regresionE1,2))
# "Error del arbol despues de la poda glucose:  15586.16"
# "Error del arbol despues de la poda age:  1038.135"

#TREE E2 DIABETES
set.seed(1233444)

trainE2 <- sample(1:nrow(datos1),size = nrow(datos2)/2)
arbol_regresionE2 <- tree(formula=diabetes~.,
                          data=datos2, subset = trainE2,
                          split = "deviance")

summary(arbol_regresionE2)
png(filename = "ARBOLREGRESIONE2-DIABETES.png")

plot(x=arbol_regresionE2, type="proportional")
text(x=arbol_regresionE2, splits = TRUE, pretty = 0, 
     cex=0.8, col="firebrick")
arbol_regresionE2
dev.off()
# PREDICCION E2 DIABETES
arbol_regresionE2_pruning <- prune.tree(tree=arbol_regresionE2, best=8)
plot(x=arbol_regresionE2,type="proportional")
text(x=arbol_regresionE2, splits=TRUE, pretty=0,
     cex=0.8, col="firebrick")
prediccionesarbol_regresionE2 <- predict(arbol_regresionE2_pruning,
                                         newdata = datos2[-trainE2,])

#PREDICCIONES E1
# test_regresionE2 <- mean((prediccionesarbol_regresionE2-datos2[-trainE2,"pedigree"])^2)
test_regresionE2 <- mean((prediccionesarbol_regresionE2-datos2[-trainE2,"age"])^2)


paste("Error del arbol despues de la poda: ",
      round(test_regresionE2,2))
#"Error del arbol despues de la poda pedigree:  0.3"
# "Error del arbol despues de la poda age:  1034.63"


#### Pregunta 4 ####
## Construya un arbol de regresion para el data frame Boston (libreria MASS) y genere las predicciones
# ## para los siguientes dos escenadios 
# Prueba1 <-c(1 ,3, 9,12, 14,15,  32,  36,  45 , 59 , 66  ,94 , 95 ,130 ,146, 149 ,171, 188 ,193, 194, 209,
#             210 ,218 ,227, 237 ,241, 255, 277,304 ,308 ,316 ,320, 334 ,349,366 ,367 ,371 ,378, 393 ,401 ,
#             406 ,422, 423 ,453 ,455 ,485, 496, 505)
# Prueba2 <- sample(primes::generate_primes(min=200,max = 506),size = 25)
# Escenario1 <- Boston[Prueba1,-14] # dataframe de prueba 1
# Escenario2 <- Boston[Prueba2,-14] # dataframe de prueba 2
# #   Deacuerdo al arbol de regresion y a los resultados obtenidos usando los data frames de prueba :  Escenario1 y 
# #   Escenario2, diga usted con cual de estos dos dataframes de prueba le fue "mejor" a las predicciones objtenidas
# #   Sustente su respuesta a esta pregunta con un indicador cuantitativo y con un grafico.


rm(list=ls())
library(MASS)
data("Boston")
help("Boston")
# Vamos a predecir el precio medio de una vivienda (medv) en funcion
# de las varibles disponibles
head(Boston)

library(tree)
set.seed(1997454)

Prueba1 <-c(1 ,3, 9,12, 14,15,  32,  36,  45 , 59 , 66  ,94 , 95 ,130 ,146, 149 ,171, 188 ,193, 194, 209,
            210 ,218 ,227, 237 ,241, 255, 277,304 ,308 ,316 ,320, 334 ,349,366 ,367 ,371 ,378, 393 ,401 ,
            406 ,422, 423 ,453 ,455 ,485, 496, 505)
Escenario1 <- Boston[Prueba1,-14] # dataframe de prueba 1

train <- sample(1:nrow(Escenario1),size = nrow(Escenario1)/2)
arbol_regresion <- tree(formula=lstat~.,
                        data=Escenario1, subset = train,
                        split = "deviance")
summary(train)
summary(arbol_regresion)
png(filename = "ARBOLREGRESIONE1-BOSTON.png")

plot(x=arbol_regresion, type="proportional")
text(x=arbol_regresion, splits = TRUE, pretty = 0, 
     cex=0.8, col="firebrick")
arbol_regresion
dev.off()

library(ggplot2)
library(ggpubr)

# resultados_cv <- data.frame(n_nodos = cv_arbol$size,
#                             deviance=cv_arbol$dev,
#                             alpha=cv_arbol$k)
# 
# p1 <- ggplot(data = resultados_cv,aes(x=n_nodos,y=deviance))+
#   geom_line()+
#   geom_point()+
#   labs(title = "Error vs tamaño del arbol") + theme_bw()
# 
# p2 <- ggplot(data = resultados_cv,aes(x=alpha,y=deviance))  +
#   geom_line()+
#   geom_point()+
#   labs(title = "Error vs hiperparametro alpha") + theme_bw()
# 
# ggarrange(p1,p2)

arbol_pruning <- prune.tree(tree=arbol_regresion, best=3)
plot(x=arbol_pruning,type="proportional")
text(x=arbol_pruning, splits=TRUE, pretty=0,
     cex=0.8, col="firebrick")
predicciones <- predict(arbol_pruning,
                        newdata = Boston[-train,])
test_mse <- mean((predicciones-Boston[-train,"lstat"])^2)
paste("Error (mse) del arbol despues de la poda: ",
      round(test_mse,2))
# "Error (mse) del arbol despues de la poda:  38.07"

# arbol 2
Prueba2 <- sample(primes::generate_primes(min=200,max = 506),size = 25)
Escenario2 <- Boston[Prueba2,-14] # dataframe de prueba 2


train2 <- sample(1:nrow(Escenario2),size = nrow(Escenario2)/2)
arbol_regresion2 <- tree(formula=lstat~.,
                         data=Escenario2, subset = train2,
                         split = "deviance")
summary(train2)
summary(arbol_regresion2)

png(filename = "ARBOLREGRESIONE2-BOSTON.png")

plot(x=arbol_regresion2, type="proportional")
text(x=arbol_regresion2, splits = TRUE, pretty = 0, 
     cex=0.8, col="firebrick")
arbol_regresion2
dev.off()

arbol_pruning2 <- prune.tree(tree=arbol_regresion2, best=2)
plot(x=arbol_pruning2,type="proportional")
text(x=arbol_pruning2, splits=TRUE, pretty=0,
     cex=0.8, col="firebrick")
predicciones2 <- predict(arbol_pruning2,
                         newdata = Boston[-train,])
test_mse2 <- mean((predicciones2-Boston[-train,"lstat"])^2)
paste("Error (mse) del arbol despues de la poda: ",
      round(test_mse2,2))
# "Error (mse) del arbol despues de la poda:  47.32"

# Obtuvimos el Mean Square Test de las predicciones de los arboles de regresion: 
# Escenario1=38.07 y Escenario2=47.32 
# Con la raiz tenemos:
# E1=6.17 E2=6.87 
# Por lo cual concluimos que la mejor prediccion fue la del E1 pues con 6.17
# estamos mas proximos del promedio del valor real 

